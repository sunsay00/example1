(module sql (sql-emit sqlresult->query sqlresult->params sqlresult->cursor get-command?)
  (import scheme chicken data-structures tools signatures matchable migrations)
  (require-extension srfi-13 srfi-1)

  (define (make-state name var? value) (list name var? value))
  (define state->name car)
  (define state->var? cadr)
  (define state->value caddr)
  (define ((set-name n) s) (make-state n (state->var? s) (state->value s)))
  (define ((get-name) s) (make-state (state->name s) (state->var? s) (state->name s)))
  (define ((set-var? v) s) (make-state (state->name s) v (state->value s)))
  (define ((get-var?) s) (make-state (state->name s) (state->var? s) (state->var? s)))
  (define ((set-value v) s) (make-state (state->name s) (state->var? s) v))
  ; monoid
  (define ((mempty) s) (pure))
  (define (pure) (make-state '() #f '()))
  (define ((mappend ma mb) s)
    (let ((sa (ma (pure))) (sb (mb (pure))))
      (make-state (state->name sa)
                  (or (state->var? sa) (state->var? sb))
                  (append (state->value sa) (state->value sb)))))
  ; monad
  (define ((>>= m mf) s) (let ((s1 (m s))) ((mf (state->value s1)) s1)))
  (define ((>> m mf) s) (let ((s1 (m s))) ((mf) s1)))
  (define (return v) (set-value v))

  (define-syntax doM
    (syntax-rules (doM if <- return let)
                  ((doM (if #t t f)) t)
                  ((doM (if #f t f)) f)
                  ((doM (if m t f)) (>>= m (lambda (p) (if p t f))))
                  ((doM (cond . body)) (cond . body))
                  ((doM (let b . body)) (let b . body))
                  ((doM (return x)) (return x))
                  ((doM (x <- m) . rest) (>>= m (lambda (x) (doM . rest))))
                  ((doM m . rest) (>> m (lambda () (doM . rest))))
                  ('_ (error "invalid doM syntax"))))
  (define (printM . vs) (return (apply print vs)))
  (define (fmap f ml) (doM (l <- ml) (return (f l))))
  (define (mconcat l) (foldr (lambda (i sum) (mappend (fmap list i) sum)) (mempty) l))
  (define (mapM mf l) (mconcat (map mf l))) ; (a -> m b) -> f a -> m (f a)

  (define (make-sqlresult query params cursor) (list query params cursor))
  (define sqlresult->query car)
  (define sqlresult->params cadr)
  (define sqlresult->cursor caddr)

  (define (param-name->method-type method name)
    (let ((param (method->params-assq name method)))
      (if (not param) #f
        (param->type param))))

  (define (param-name->model-type model name)
    (let ((typedef (model->typedef model)))
      (let ((result (typedef->params-assq typedef name)))
        (if (not result) #f
          (param->type (cadr result))))))

  (define (skip-subfields api method names)
    (map (lambda (param-name)
           (let ((param (method->params-assq param-name method)))
             (if (not param) param-name
               (let* ((type (param->type param))
                      (typedef (typedef-assq type api)))
                 (if (not typedef) param-name
                   (let ((unskipped-names (map fieldvalue->name (filter (compose not fieldvalue-default?) (method+typedef->fieldvalues method (typedef-assq! type api))))))
                     (if (null? unskipped-names)
                       param-name
                       (list "(p => {const {" (intersperse unskipped-names ", ") ", ...rest} = p; return rest;})(" param-name ")"))))))))
         names))

  (define (ev-orderby-items method exprs)
    (define (recur expr)
      (match expr
             (`(distanceSphere ,x ,y) (list "ST_DistanceSphere(" (recur x) ", " (recur y) ")"))
             (`(quote ,x)
               (let ((type (param-name->method-type method x)))
                 (match type
                        (Point (list "ST_SetSRID(ST_MakePoint(${this.S(" x ".lon)}, ${this.S(" x ".lat)}), 4326)"))
                        (x x))))
             (x x)))
    (map recur exprs))

  (define (make-sql api model method cmd)
    (define (push/default v def count?)
      (match v
             ('sub (list "${this.S($ctx." v ")}"))
             ('now (list "${this.S($" v ")}"))
             (else
               (let ((type (param-name->method-type method v)))
                 (match type
                        ('(Optional Point) (list "ST_SetSRID(ST_MakePoint(${!" v "?'" def ", " def "':`${this.S(" v ".lon)}, ${this.S(" v ".lat)}`}), 4326)"))
                        ('Point (list "ST_SetSRID(ST_MakePoint(${this.S(" v ".lon)}, ${this.S(" v ".lat)}), 4326)"))
                        ('(Optional Int)
                         (if count?
                           (list "${" v "==undefined?(" def "+1):this.S(Math.min(" def "+1, " v "+1))}")
                           (list "${" v "==undefined?" def ":this.S(" v ")}")))
                        ('Int
                         (if count?
                           (list "${" v "==undefined?(" def "+1):this.S(Math.min(" def "+1, " v "+1))}")
                           (list "${" v "==undefined?" def ":this.S(" v ")}")))
                        ('(Optional Boolean) (list "${this.S(" v "==undefined?false:" v ")}"))
                        ('(Optional String) (list "${!" v "&&" v "!=''?" def ":this.S(" v ")}"))
                        ('(Optional DateTime) (list "${!" v "?" def ":this.S(" v ")}"))
                        ('(Optional UserFields) (list "${!" v "?" def ":this.S(" v ")}"))
                        ('Boolean (list "${this.S(" v ")}"))
                        ('String (list "${this.S(" v ")}"))
                        ('DateTime (list "${this.S(" v ")}"))
                        (`(Array ,t) (list "${this.S(" v ")}"))
                        ('CommentItem (list "${this.S(" v ")}"))
                        ('Appointment (list "${this.S(" v ")}"))
                        ('UserFields (list "${this.S(" v ")}"))
                        (else (error (smoosh "(sql.import) unhandled push type " (->string type) " for " v))))))))
    (define (push v) (push/default v "null" #f))
    (define (push-list l) (list "${" l ".map(n => `${this.S(n)}`).join(', ')}"))
    (define (ev-returns exprs)
      (intersperse
        (map (lambda (expr) (ev-param expr 0)) exprs) ", "))
    (define (ev-param name depth)
      (cond
        ((number? name) name)
        (else (let ((type (param-name->model-type model name)))
                (match type
                       ('(Optional Point) (list "ST_AsText(" name ")" (if (= depth 0) (list " AS \"" (lower name) "\"") "")))
                       ('Point (list "ST_AsText(" name ")" (if (= depth 0) (list " AS \"" (lower name) "\"") "")))
                       (else (list "\"" (lower name) "\"")))))))
    (define (ev-distance-param name depth)
      (cond
        ((number? name) name)
        (else (match name
                     (,x (let ((type (param-name->method-type method x)))
                            (match type
                                   (Point (list "ST_SetSRID(ST_MakePoint(${this.S(" x ".lon)}, ${this.S(" x ".lat)}), 4326)"))
                                   (else (error (string-append "unknown type '" (->string type) " '" (->string name)))))))
                     (x x)))))
    (define (ev-select exprs)
      (define (recur exprs depth)
        (intersperse
          (map (lambda (expr)
                 (match expr
                        (('as x alias) (list (recur (list x) (+ 1 depth)) (if (= depth 0) (list " AS \"" (lower alias) "\"") "")))
                        (`(count ,x) (list "COUNT(\"" (lower x) "\")"))
                        (`(min ,x) (list "MIN(\"" (lower x) "\")" (if (= depth 0) (list " AS \"" (lower x) "\"") "")))
                        (`(max ,x) (list "MAX(\"" (lower x) "\")" (if (= depth 0) (list " AS \"" (lower x) "\"") "")))
                        (`(distinct ,x) (list "DISTINCT(\"" (lower x) "\")" (if (= depth 0) (list " AS \"" (lower x) "\"") "")))
                        (`(distanceSphere ,x ,y) (list "ST_DistanceSphere(" (ev-distance-param x (+ 1 depth)) ", " (ev-distance-param y (+ 1 depth)) ")"))
                        (`(* ,x ,y) (list (recur (list x) (+ 1 depth)) " * " (recur (list y) (+ 1 depth))))
                        (`(quote ,x) x)
                        (`(,? ,x) (error "no match for sql ev-select" expr))
                        (x (ev-param x depth))
                        (else (error "no match for sql ev-select" expr))))
               exprs) ", "))
      (recur exprs 0))

    (define (ev-where expr)
      (define (recur e def)
        (match e
               (('and . r)
                (doM (rr <- (mapM (lambda (i)
                                    (doM
                                      (j <- (recur i def))
                                      (name <- (get-name))
                                      (return (if (null? name) j (list "${!" name "?'true':`" j "`}")))))
                                  r))
                     (return (intersperse  rr " AND "))))
               (('or . r)
                (doM (rr <- (mapM (lambda (i)
                                    (doM
                                      (j <- (recur i def))
                                      (name <- (get-name))
                                      (return (if (null? name) j (list "${!" name "?'false':`" j "`}")))))
                                  r))
                     (return (intersperse rr " OR "))))
               (('in x ,ids)
                (doM (rx <- (recur x def))
                     (return (list "${" ids ".length == 0 ? 'true' : `" rx " IN (" (push-list ids) ")`}"))))
               (('= x y) (doM (rx <- (recur x def)) (ry <- (recur y def)) (return (list rx " = " ry))))
               (('<= x y) (doM (rx <- (recur x def)) (ry <- (recur y def)) (return (list rx " <= " ry))))
               (('>= x y) (doM (rx <- (recur x def)) (ry <- (recur y def)) (return (list rx " >= " ry))))
               (('< x y) (doM (rx <- (recur x def)) (ry <- (recur y def)) (return (list rx " < " ry))))
               (('> x y) (doM (rx <- (recur x def)) (ry <- (recur y def)) (return (list rx " > " ry))))
               (('like x y)
                (doM
                  (rx <- (recur x "'\\\'%\\\''"))
                  (namex <- (get-name))
                  (ry <- (recur y "'\\\'%\\\''"))
                  (namey <- (get-name))
                  (return (list
                            (if (null? namex) rx (list "${!" namex "?'\\\'%\\\'':`" rx "`}"))
                            " LIKE "
                            (if (null? namey) ry (list "${!" namey "?'\\\'%\\\'':`" ry "`}"))))))
               (('contains x y)
                (doM
                  (rx <- (recur x "true")) (varx? <- (get-var?))
                  (ry <- (recur y "true")) (vary? <- (get-var?))
                  (return
                    (cond
                      (varx? (list rx " = ANY(" ry ")"))
                      (vary? (list ry " = ANY(" rx ")"))
                      (else (error "bad contains case"))))))
               (('ci-contains x y)
                (doM
                  (rx <- (recur x "true")) (varx? <- (get-var?))
                  (ry <- (recur y "true")) (vary? <- (get-var?))
                  (return
                    (cond
                      (varx? (list rx " = ANY(" ry "::citext[])"))
                      (vary? (list ry " = ANY(" rx "::citext[])"))))))
               (('distanceSphere x y) (doM (rx <- (recur x def)) (ry <- (recur y def)) (return (list "ST_DistanceSphere(" rx ", " ry ")"))))
               (('* x y) (doM (rx <- (recur x def)) (ry <- (recur y def)) (return (list "(" rx " * " ry ")"))))
               (('upper x) (doM (r <- (recur x def)) (return (list "UPPER(" r ")"))))
               (('lower x) (doM (r <- (recur x def)) (return (list "LOWER(" r ")"))))
               ('$sub (return (push 'sub)))
               (#t (return (list "true")))
               (#f (return (list "false")))
               (,v (let ((type (param-name->method-type method v)))
                     (match type
                            ((Optional ?)
                             (doM
                               (set-var? #t)
                               (set-name v)
                               (return (push/default v def #f))))
                            (else 
                              (doM
                                (set-var? #t)
                                (return (push/default v def #f)))))))
               (x (return x))))
        (let ((ret ((recur expr "null") (pure))))
          (state->value ret)))

    (define (ev-limit expr)
      (match expr
             (,x (let ((type (param-name->method-type method x)))
                   (if (not type) ""
                     (list " LIMIT " (push/default x 50 #t)))))
             (else (error "no match for sql ev-limit" expr))))
    (define (ev-offset expr)
      (match expr
             (,x (push x))
             (else (error "no match for sql ev-offset" expr))))
    (define (ev-groupby exprs)
      (intersperse
        (map (lambda (expr)
               (match expr
                      (x x)))
             exprs) ", "))
    (define (ev-orderby exprs)
      (intersperse (ev-orderby-items method exprs) ", "))
    (define (ev-desc expr)
      (match expr
             (#t " DESC")
             (#f " ASC")
             (else (error "no match for sql ev-desc" expr))))
    (define (ev-insert-names exprs)
      (intersperse
        (map (lambda (expr)
               (match expr
                      ((n _) (list "\"" (lower n) "\""))
                      (else (error "no match for sql ev-insert-name" expr))))
             exprs) ", "))
    (define (ev-insert-values exprs)
      (intersperse
        (map (lambda (expr)
               (match expr
                      ((n ,v)
                       (let ((type (param-name->method-type method n)))
                         (cond
                           ((array? type)
                            (let ((basetype (base-type type)))
                              (cond
                                ((native-type? basetype)
                                 (match basetype
                                        ('String (push v))
                                        ;('String (list "ARRAY[" (push v) "]::text[]"))
                                        (? (list "ARRAY[" (push v) "]"))))
                                (else (push v)))))
                           (else (push v)))))
                      ((_ '$sub) (push 'sub))
                      ((_ '$now) (push 'now))
                      ((_ x) x)
                      (else (error "no match for sql ev-insert-values" expr))))
             exprs) ", "))
    (define (ev-excluding exprs)
      (intersperse
        (map (lambda (expr)
               (match expr
                      ((n _) (list n "=EXCLUDED." n))
                      (else (error "no match for sql ev-insert-name" expr))))
             exprs) ", "))
    (define (ev-update-sets exprs)
      (define (recur expr)
        (match expr
               (('= a ('push b)) (list a "=array_append(" a ", " (push b) ")"))
               (('= ('at a ,i) b) (list a "[${" i "+1}]=" a "[${" i "+1}]||" (push b)))
               (`(= ,a $sub) (list a "=" (push 'sub)))
               (`(= ,a $now) (list a "=" (push 'now)))
               (('= a ,b) (list a "=" (push b)))
               (('= ,a b) (list (push a) "=" b))
               (`(= ,a ,b) (list (recur a) "=" (recur b)))
               (`(- ,a ',b) (list (recur a) "+" (push/default b 0 #f)))
               (`(- ,a ,b) (list (recur a) "+" (recur b)))
               (`(+ ,a ',b) (list (recur a) "+" (push/default b 0 #f)))
               (`(+ ,a ,b) (list (recur a) "+" (recur b)))
               (? ?)
               (else (error "no match for sql ev-update-sets" expr))))
      (intersperse (map recur exprs) ", "))
    (define (ev-offset expr)
      (match expr
             (('+ a ,b) (list a "+" (push b)))
             (('+ ,a b) (list (push a) "+" b))
             (('- a ,b) (list a "-" (push b)))
             (('- ,a b) (list (push a) "-" b))
             (else (error "no match for sql ev-offset" expr))))
    (define (ev expr)
      (match expr
             (`(command (select . ,ss) (from ,f) (where (and (= id 'id) (= sub $sub))) (orderby . ,os) (desc? ,d))
               `("SELECT " ,(ev-select ss) " FROM " ,f " WHERE " ,(ev-where `(and (= id 'id) (= sub $sub))) " ORDER BY " ,(ev-orderby os) ,(ev-desc d)))
             (`(command (select . ,ss) (from ,f) (where (= parentId 'parentId)) (orderby . ,os) (desc? ,d))
               `("SELECT " ,(ev-select ss) " FROM " ,f " WHERE " ,(ev-where `(= parentId 'parentId)) " ORDER BY " ,(ev-orderby os) ,(ev-desc d)))
             (`(command (select . ,ss) (from ,f) (where (= id 'id)) (orderby . ,os) (desc? ,d))
               `("SELECT " ,(ev-select ss) " FROM " ,f " WHERE " ,(ev-where `(= id 'id)) " ORDER BY " ,(ev-orderby os) ,(ev-desc d)))

             (`(command (select . ,ss) (from ,f) (where (= sub 'sub)) (orderby . ,os) (desc? ,d))
               `("SELECT " ,(ev-select ss) " FROM " ,f " WHERE " ,(ev-where `(= sub 'sub)) " ORDER BY " ,(ev-orderby os) ,(ev-desc d)))

             (`(command (select . ,ss) (from ,f) (where (in id 'ids)) (orderby . ,os) (desc? ,d) (limit ,l))
               `("SELECT " ,(ev-select ss) " FROM " ,f " WHERE " ,(ev-where `(in id 'ids)) " ORDER BY " ,(ev-orderby os) ,(ev-desc d) ,(ev-limit l)))

             (`(command (select . ,ss) (from ,f) (where ,w) (orderby . ,os))
              `("SELECT " ,(ev-select ss) " FROM " ,f " WHERE " ,(ev-where w) " ORDER BY " ,(ev-orderby os)))
             (`(command (select . ,ss) (from ,f) (where ,w) (orderby . ,os) (desc? ,d) (limit ,l))
              `("SELECT " ,(ev-select ss) " FROM " ,f " WHERE " ,(ev-where w) " ORDER BY " ,(ev-orderby os) ,(ev-desc d) ,(ev-limit l)))

             (`(command (select . ,ss) (from ,f) (orderby . ,os) (desc? ,d) (limit ,l))
              `("SELECT " ,(ev-select ss) " FROM " ,f " ORDER BY " ,(ev-orderby os) ,(ev-desc d) ,(ev-limit l)))

             (`(command (select . ,ss) (from ,f) (where ,w) (groupby . ,gs) (orderby . ,os) (limit ,l))
               `("SELECT " ,(ev-select ss) " FROM " ,f " WHERE " ,(ev-where w) " GROUP BY " ,(ev-groupby gs) " ORDER BY " ,(ev-orderby os) ,(ev-limit l)))
             (`(command (select . ,ss) (from ,f) (where ,w) (groupby . ,gs) (orderby . ,os) (limit ,l) (offset ,o))
               `("SELECT " ,(ev-select ss) " FROM " ,f " WHERE " ,(ev-where w) " GROUP BY " ,(ev-groupby gs) " ORDER BY " ,(ev-orderby os) ,(ev-limit l) " OFFSET " ,(ev-offset o)))

             (`(command (select . ,ss) (from ,f) (where ,w) (groupby . ,gs) (limit ,l))
               `("SELECT " ,(ev-select ss) " FROM " ,f " WHERE " ,(ev-where w) " GROUP BY " ,(ev-groupby gs) ,(ev-limit l)))
             (`(command (select . ,ss) (from ,f) (where ,w) (groupby . ,gs) (limit ,l) (offset ,o))
               `("SELECT " ,(ev-select ss) " FROM " ,f " WHERE " ,(ev-where w) " GROUP BY " ,(ev-groupby gs) ,(ev-limit l) " OFFSET " ,(ev-offset o)))

             (`(command (select . ,ss) (from ,f) (groupby . ,gs) (limit ,l))
               `("SELECT " ,(ev-select ss) " FROM " ,f " GROUP BY " ,(ev-groupby gs) ,(ev-limit l)))

             (`(command (select . ,ss) (from ,f) (where ,w) (orderby . ,os) (desc? ,d) (limit ,l))
               `("SELECT " ,(ev-select ss) " FROM " ,f " WHERE " ,(ev-where w) " ORDER BY " ,(ev-orderby os) ,(ev-desc d) ,(ev-limit l)))

             (`(command (select . ,ss) (from ,f) (orderby . ,os) (desc? ,d) (limit , l) (offset ,o))
               `("SELECT " ,(ev-select ss) " FROM " ,f " ORDER BY " ,(ev-orderby os) ,(ev-desc d) ,(ev-limit l) " OFFSET " ,(ev-offset o)))
             (`(command (select . ,ss) (from ,f) (where ,w) (orderby . ,os) (desc? ,d) (limit , l) (offset ,o))
               `("SELECT " ,(ev-select ss) " FROM " ,f " WHERE " ,(ev-where w) " ORDER BY " ,(ev-orderby os) ,(ev-desc d) ,(ev-limit l) " OFFSET " ,(ev-offset o)))

             (`(command (insert ,f) (insert . ,vs) (return . ,rs))
               `("INSERT INTO " ,f " (" ,(ev-insert-names vs) ") VALUES (" ,(ev-insert-values vs) ") RETURNING " ,(ev-returns rs)))
             (`(command (insert ,f) (insert . ,vs))
               `("INSERT INTO " ,f " (" ,(ev-insert-names vs) ") VALUES (" ,(ev-insert-values vs) ")"))
             (`(command (insert ,f) (insert . ,vs) (where ,w))
               `("INSERT INTO " ,f " (" ,(ev-insert-names vs) ") VALUES (" ,(ev-insert-values vs) ") WHERE " ,(ev-where w)))

             (`(command (update ,f) (update (= (at ,sn ',i) ,si)) (where ,w))
               `("UPDATE " ,f " SET " ,(ev-update-sets `((= (at ,sn ',i) ,si))) " WHERE " ,(ev-where w) " RETURNING ${" ,i "} AS index, " ,sn "[${" ,i "+1}] AS data"))
             (`(command (update ,f) (update (= ,sn (push ,si))) (where ,w))
               `("UPDATE " ,f " SET " ,(ev-update-sets `((= ,sn (push ,si)))) " WHERE " ,(ev-where w) " RETURNING array_length(" ,sn ", 1)-1 AS index, " ,sn "[array_length(" ,sn ", 1)] AS data"))
             (`(command (update ,f) (update . ,ss) (where ,w) (return . ,rs))
               `("UPDATE " ,f " SET " ,(ev-update-sets ss) " WHERE " ,(ev-where w) " RETURNING " ,(ev-returns rs)))
             (`(command (update ,f) (update . ,ss) (where ,w))
               `("UPDATE " ,f " SET " ,(ev-update-sets ss) " WHERE " ,(ev-where w)))
             (`(command (delete ,f) (where ,w) (return . ,rs))
               `("DELETE FROM " ,f " WHERE " ,(ev-where w) " RETURNING " ,(ev-returns rs)))

             (`(command (insert ,f) (insert . ,vs) (on . ,os) (update . ,ss) (return . ,rs))
               `("INSERT INTO " ,f " (" ,(ev-insert-names vs) ") VALUES (" ,(ev-insert-values vs) ") ON CONFLICT (" ,(ev-insert-names os) ") DO UPDATE SET " ,(ev-update-sets ss) " RETURNING " ,(ev-returns rs)))
             (`(command (insert ,f) (insert . ,vs) (on . ,os)  (return . ,rs))
               `("INSERT INTO " ,f " (" ,(ev-insert-names vs) ") VALUES (" ,(ev-insert-values vs) ") ON CONFLICT (" ,(ev-insert-names os) ") DO UPDATE SET " ,(ev-excluding os) " RETURNING " ,(ev-returns rs)))

             (else (error "(sql.import.ev) no match for make-sql ev" expr))))
    (list
      (smoosh (list "`" (ev cmd) "`"))
      (smoosh (list "[]"))))

  (define (query-command model-name desc? cmd)
    (match cmd
           (`(command (select . ,ss) (where (in id 'ids)))
             `(command (select . ,ss) (from ,model-name) (where (in id 'ids)) (orderby id) (desc? ,desc?) (limit 'count)))
           (`(command (select . ,ss) (where (in sub 'subs)))
             `(command (select . ,ss) (from ,model-name) (where (in sub 'subs)) (orderby id) (desc? ,desc?) (limit 'count)))
           (`(command (select . ,ss) (where (= parentId 'parentId)))
             `(command (select . ,ss) (from ,model-name) (where (= parentId 'parentId)) (orderby id) (desc? ,desc?)))
           (`(command (select . ,ss) (where (= id 'id)))
             `(command (select . ,ss) (from ,model-name) (where (= id 'id)) (orderby id) (desc? ,desc?)))
           (`(command (select . ,ss) (where (and (= id 'id) (= sub $sub))))
             `(command (select . ,ss) (from ,model-name) (where (and (= id 'id) (= sub $sub))) (orderby id) (desc? ,desc?)))
           (`(command (select . ,ss) (where (= sub 'sub)))
             `(command (select . ,ss) (from ,model-name) (where (= sub 'sub)) (orderby id) (desc? ,desc?)))
           (`(command (select . ,ss) (where (= sub $sub)))
             `(command (select . ,ss) (from ,model-name) (where (= sub $sub)) (orderby id) (desc? ,desc?) (limit 'count)))
           (`(command (select . ,ss) (where ,w) (groupby . ,gs) (orderby . ,os))
             `(command (select . ,ss) (from ,model-name) (where ,w) (groupby . ,gs) (orderby . ,os) (limit 'count)))

           (`(command (select . ,ss) (where ,w) (groupby . ,gs))
             `(command (select . ,ss) (from ,model-name) (where ,w) (groupby . ,gs) (limit 'count)))

           (`(command (select . ,ss) (groupby . ,gs))
             `(command (select . ,ss) (from ,model-name) (groupby . ,gs) (limit 'count)))

           (`(command (select (distinct ,s)))
             `(command (select (distinct ,s)) (from ,model-name) (orderby ,s) (desc? ,desc?) (limit 'count)))
           (`(command (select (distinct ,s)) (where ,w))
             `(command (select (distinct ,s)) (from ,model-name) (where ,w) (orderby ,s) (desc? ,desc?) (limit 'count)))

           (`(command (select . ,ss) (where ,w))
             `(command (select . ,ss) (from ,model-name) (where ,w) (orderby id) (desc? ,desc?) (limit 'count)))

           (`(command (select . ,ss) (orderby . ,os))
             `(command (select . ,ss) (from ,model-name) (orderby . ,os) (desc? ,desc?) (limit 'count)))
           (`(command (select . ,ss) (where ,w) (orderby . ,os))
             `(command (select . ,ss) (from ,model-name) (where ,w) (orderby . ,os) (desc? ,desc?) (limit 'count)))

           (`(command (select . ,ss))
             `(command (select . ,ss) (from ,model-name) (orderby id) (desc? ,desc?) (limit 'count)))
           (`(command (insert . ,vs) (where ,w))
             `(command (insert ,model-name) (insert . ,vs) (where ,w)))
           (`(command (insert . ,vs) (return . ,rs))
             `(command (insert ,model-name) (insert . ,vs) (return . ,rs)))
           (`(command (insert . ,vs))
             `(command (insert ,model-name) (insert . ,vs)))
           (`(command (update . ,ss) (where ,w))
             `(command (update ,model-name) (update . ,ss) (where ,w)))
           (`(command (update . ,ss) (where ,w) (return . ,rs))
             `(command (update ,model-name) (update . ,ss) (where ,w) (return . ,rs)))
           (`(command (delete) (where ,w) (return . ,rs))
             `(command (delete ,model-name) (where ,w) (return . ,rs)))

           (`(command (insert . ,vs) (on . ,os) (return . ,rs))
             `(command (insert ,model-name) (insert . ,vs) (on . ,os) (return . ,rs)))
           (`(command (insert . ,vs) (on . ,os) (update . ,ss) (return . ,rs))
             `(command (insert ,model-name) (insert . ,vs) (on . ,os) (update . ,ss) (return . ,rs)))

           (else (error "(sql.import.qc) failed to generate query command from" cmd))))

  (define (paginize-command model-name desc? cmd)
    (match cmd
           (`(command (select (distinct ,s)))
             (if desc?
               `(command (select (distinct ,s)) (from ,model-name) (where (< ,s 'after)) (orderby ,s) (desc? ,desc?) (limit 'count))
               `(command (select (distinct ,s)) (from ,model-name) (where (> ,s 'after)) (orderby ,s) (desc? ,desc?) (limit 'count))))
           (`(command (select (distinct ,s)) (where ,w))
             (if desc?
               `(command (select (distinct ,s)) (from ,model-name) (where (and ,w (< ,s 'after))) (orderby ,s) (desc? ,desc?) (limit 'count))
               `(command (select (distinct ,s)) (from ,model-name) (where (and ,w (> ,s 'after))) (orderby ,s) (desc? ,desc?) (limit 'count))))
           (`(command (select . ,ss) (where (in id 'ids)))
             (if desc?
               `(command (select . ,ss) (from ,model-name) (where (and (in id 'ids) (< id 'after))) (orderby id) (desc? ,desc?) (limit 'count))
               `(command (select . ,ss) (from ,model-name) (where (and (in id 'ids) (> id 'after))) (orderby id) (desc? ,desc?) (limit 'count))))
           (`(command (select . ,ss) (where (in sub 'subs)))
             (if desc?
               `(command (select . ,ss) (from ,model-name) (where (and (in sub 'subs) (< id 'after))) (orderby id) (desc? ,desc?) (limit 'count))
               `(command (select . ,ss) (from ,model-name) (where (and (in sub 'subs) (> id 'after))) (orderby id) (desc? ,desc?) (limit 'count))))
           (`(command (select . ,ss) (where ,w))
             (if desc?
               `(command (select . ,ss) (from ,model-name) (where (and ,w (< id 'after))) (orderby id) (desc? ,desc?) (limit 'count))
               `(command (select . ,ss) (from ,model-name) (where (and ,w (> id 'after))) (orderby id) (desc? ,desc?) (limit 'count))))
           (`(command (select . ,ss) (where ,w) (groupby . ,gs) (orderby . ,os))
             (if desc? 
               `(command (select . ,ss) (from ,model-name) (where ,w) (groupby . ,gs) (orderby . ,os) (limit 'count))
               `(command (select . ,ss) (from ,model-name) (where ,w) (groupby . ,gs) (orderby . ,os) (limit 'count) (offset (+ 'after 1)))))
           (`(command (select . ,ss) (where ,w) (groupby . ,gs))
             (if desc? 
               `(command (select . ,ss) (from ,model-name) (where ,w) (groupby . ,gs) (limit 'count))
               `(command (select . ,ss) (from ,model-name) (where ,w) (groupby . ,gs) (limit 'count) (offset (+ 'after 1)))))
           (`(command (select . ,ss) (groupby . ,gs))
             (if desc? 
               `(command (select . ,ss) (from ,model-name) (groupby . ,gs) (limit 'count))
               `(command (select . ,ss) (from ,model-name) (groupby . ,gs) (limit 'count) (offset (+ 'after 1)))))
           (`(command (select . ,ss))
             (if desc?
               `(command (select . ,ss) (from ,model-name) (where (< id 'after)) (orderby id) (desc? ,desc?) (limit 'count))
               `(command (select . ,ss) (from ,model-name) (where (> id 'after)) (orderby id) (desc? ,desc?) (limit 'count))))

           (`(command (select . ,ss) (orderby (distanceSphere location 'location)))
             `(command (select . ,ss) (from ,model-name) (orderby (distanceSphere location 'location)) (desc? ,desc?) (limit 'count) (offset (+ 'after 1))))
           (`(command (select . ,ss) (where ,w) (orderby (distanceSphere location 'location)))
             `(command (select . ,ss) (from ,model-name) (where ,w) (orderby (distanceSphere location 'location)) (desc? ,desc?) (limit 'count) (offset (+ 'after 1))))

           (`(command (insert . ,vs) (where ,w))
             `(command (insert ,model-name) (insert . ,vs) (where ,w)))
           (`(command  (update . ,ss) (where ,w))
             `(command (update ,model-name) (update . ,ss) (where ,w)))
           (else (error "(sql.import.p) failed to generate paginize command" cmd))))

  (define (cursor-command cmd)
    (match cmd
           (`(command (select . ,ss) (where ,w) (groupby . ,gs) (orderby . ,os))
             "`${(!after ? 0 : JSON.parse(after) + 1) + i}`,")
           (`(command (select . ,ss) (where ,w) (groupby . ,gs))
             "`${(!after ? 0 : JSON.parse(after) + 1) + i}`,")
           (`(command (select . ,ss) (orderby . ,os))
             "`${(!after ? 0 : JSON.parse(after) + 1) + i}`,")
           (`(command (select . ,ss) (where ,w) (orderby . ,os))
             "`${(!after ? 0 : JSON.parse(after) + 1) + i}`,")
           (`(command (select (distinct ,x)))
             (list "row." (lower x)))
           (`(command (select (distinct ,x)) (where ,w))
             (list "row." (lower x)))
           ((or `(command (select . ,ss) (where (in id 'ids)))
                `(command (select . ,ss) (where (in sub 'subs)))
                `(command (select . ,ss) (where (= sub $sub)))
                `(command (select . ,ss) (where (= sub 'sub)))
                `(command (select . ,ss) (where (and (= id 'id) (= sub $sub))))
                `(command (select . ,ss)))
             "row.id,")
           (`(command (select . ,ss) (where ,w))
             "row.id,")
           ((or `(command (select . ,ss) (where (= parentId 'parentId)))
                `(command (select . ,ss) (where (= id 'id))))
            '())
           (`(command (select . ,ss) (groupby . ,g))
             "row.id,")
           (`(command (insert . ,vs) (where ,w)) '())
           (`(command (insert . ,vs) (return . ,rs)) '())
           (`(command (insert . ,vs)) '())
           (`(command (insert . ,vs) (on . ,os) (update . ,ss) (return . ,rs)) '())
           (`(command (insert . ,vs) (on . ,os) (return . ,rs)) '())
           (`(command (update . ,ss) (where ,w) (return . ,rs)) '())
           (`(command (update . ,ss) (where ,w)) '())
           (`(command (delete) (where ,w) (return . ,rs)) '())
           (else (error "(sql.import.c) failed to generate cursor command" cmd))))

  (define (returns-multiple-rows? cmd)
    (match cmd
           (`(command (select . ,ss) (where (= id 'id))) #f)
           (`(command (select . ,ss) (where (= uniqueName 'uniqueName))) #f)
           (`(command (select . ,ss) (where (= parentId 'parentId))) #f)
           (`(command (select . ,ss) (where (= sub 'sub))) #f)
           (`(command (select . ,ss) (where (= chatIdentity 'chatIdentity))) #f)
           (`(command (select . ,ss) (where (and (= id 'id) (= sub $sub)))) #f)
           (`(command (insert . ,vs) . ,rest) #f)
           (`(command (update . ,ss) . ,rest) #f)
           (`(command (delete) . ,rest) #f)
           (else #t)))

  (define (get-command? cmd)
    (match (cons 'command cmd)
           (`(command (select . ,ss) . ,rest) #t)
           (else #f)))

  (define (sql-emit api model method ascending? cmd)
    (let ((desc? (not ascending?))
          (model-name (model->name model)))
      (cond
        ((returns-multiple-rows? cmd)
         (let ((qcmd (query-command model-name desc? cmd))
               (pcmd (paginize-command model-name desc? cmd))
               (cursor (cursor-command cmd)))
           (let ((r1 (make-sql api model method qcmd)) (r2 (make-sql api model method pcmd)))
             (let ((r (zip r1 r2)))
               (let ((q (car r)) (p (cadr r)))
                 (make-sqlresult
                   (smoosh (list "!after ? " (car q) " : " (cadr q)))
                   (smoosh (list "!after ? " (car p) " : " (cadr p)))
                   (smoosh cursor)))))))
        (else
          (let ((qcmd (query-command model-name desc? cmd))
                (cursor (cursor-command cmd)))
            (let ((q (make-sql api model method qcmd)))
              (make-sqlresult
                (smoosh (car q))
                (smoosh (cadr q))
                (smoosh cursor))))))))
  )
