(import configs typedefs resolvers interfaces tools services migrations rdsstore cachestore
        mockstores playbackstores apollobindings2 apollocomponentfixtures schemas serviceapibindings
        version mappers gqlqueries connectors)

(use utils filepath matchable)

(define (ensure-filepath . filepaths)
  (let ((path (apply string-append (map ->string filepaths))))
    (system* (string-append "mkdir -p " (filepath:drop-file-name path)))
    path))

(define (save data . paths)
  (let ((out (open-output-file (apply ensure-filepath paths))))
    (display data out)
    (close-output-port out)))

(define (run-sub entire-api ledger)
  (let ((schemas (generate-schemas #f ledger)))
    (foldr
      (lambda (schema i) 
        (define v (schema->version schema))
        (define vnum (schema->version-number schema))
        (define prev-api (schema->prev-api schema))
        (define delta-api (schema->delta-api schema))
        (define api (schema->api schema)) ; prev-api + delta-api = api
        (print "\n---- [ GENERATING MIGRATION VERSION: v" vnum " ] ----")

        ;(pp (schema->delta-api schema))

        (define (generate-migration timestamp name)
          (let ((z (number->string (inexact->exact (floor (version-name->number name))))))
            (save (migrations-generate-up entire-api api delta-api i) "../infra/postgres/migrations/sqls/" timestamp "-v" z "-up.sql")
            (save (migrations-generate-down api delta-api i) "../infra/postgres/migrations/sqls/" timestamp "-v" z "-down.sql")
            (save (migrations-generate-js timestamp name) "../infra/postgres/migrations/" timestamp "-v" z ".js")))
        (define (gen-timestamp ver)
          (let* ((n (version-name->number v))
                 (z (inexact->exact (floor n)))
                 (r (- n z)))
            (let ((y (+ 1000 z))
                  (s (inexact->exact (floor (+ 10000 (* 10000 r))))))
              (let ((vnum (string->number (string-append (number->string y) "01010" (number->string s)))))
                (number->string vnum)))))
        (generate-migration (gen-timestamp v) v)

        (+ i 1))
      0 (reverse schemas))))

(define (run ledger output-dir)
  (let ((schemas (generate-schemas #t ledger)))
    (let ((i 0) (schema (if (= (length schemas) 1) (car schemas) (error "invalid schema"))))
      (define v (version-name-floor (schema->version schema)))
      (define vnum (schema->version-number schema))
      (define prev-api (schema->prev-api schema))
      (define delta-api (schema->delta-api schema))
      (define api (schema->api schema)) ; prev-api + delta-api = api
      (print "\n---- [ GENERATING API VERSION: v" vnum " ] ----")

      (define (get-path s) (string-append output-dir "/" s))

      (define last? (= (+ i 1) (length schemas)))
      (if last?
        (begin 
          (save (types-generate-client api) (get-path "types/models.ts"))
          ; WIP (save (types-generate api) "../front/webapp/src/types/models.ts")

          (save (apollobindings-generate-index-client api #t) (get-path "models/index.ts"))
          ; WIP (save (apollobindings-generate-index api #f) "../front/webapp/src/components/hocs/models/index.ts")

          ; WIP (map (lambda (name)
                 ;(let ((output (serviceapibindings-generate name api)))
                   ;(save output "../front/mobileapp/src/services/basic/" (lower name) ".ts")))
               ;(map model->name (filter model-serviceapionly? (api->models api))))

          ; WIP (let ((output (apollocomponentfixtures-generate api)))
            ;(save output "../front/webapp/src/__integrations__/tools/componentfixtures.tsx")
            ;(save output "../front/mobileapp/src/__integrations__/tools/componentfixtures.tsx"))

          ;(let ((output (version-generate vnum)))
            ;(save output "../front/styleguide/src/stories/hocs/version.ts")
            ;(save output "../front/mobileapp/src/tools/version.ts")
            ;(save output "../front/webapp/src/tools/version.ts"))

          (define configs '(
                            ;("../back/resize/src/config.ts" resize)
                            ;("../back/postconfirm/src/config.ts" postconfirm)
                            ;("../back/usersync/src/config.ts" usersync)
                            ;("../back/twiliocleanup/src/config.ts" usersync)
                            ;("../front/mobileapp/src/config.ts" mobileapp)
                            ;("../front/webapp/src/config.ts" webapp)
                            ))
          (map (lambda (config) 
                 (save (configs-generate (cadr config)) (car config)))
               configs)))
      api)))

(define (test-applicative)
  ;pure :: a -> f a
  ;(<*>) :: f (a -> b) -> f a -> f b

  (define (some x) `(some ,x))
  (define (none) `(none))
  (define (some? x) (and (pair? x) (eq? (car x) 'some)))
  (define (none? x) (and (pair? x) (eq? (car x) 'none)))
  (define (some->val mb) (if (not (some? mb)) (error "invalid some") (cadr mb)))

  (define pure some)

  (define ((app f) mb)
    (some (if (some? mb)
            (f (some->val mb))
            (none))))

  (define-syntax <$>
    (syntax-rules (<$> <*>)
                  ((<$> p) (some p))
                  ((<$> p <*> f1 <*> f2) ((app f2) ((app f1) (some p))))
                  ((<$> p <*> f) ((app f) (some p)))))

  (print (<$> 5))
  (print (<$> 5 <*> (lambda (x) (* x x))))
  (print (<$> 5 <*> (lambda (x) (* x x)) <*> (lambda (x) (+ x x))))
  )
;(test-applicative)

(define (mb-monads)
  (define (some x) `(some ,x))
  (define (none) `(none))
  (define (some? x) (and (pair? x) (eq? (car x) 'some)))
  (define (some->val mb) (if (not (some? mb)) (error "invalid some") (cadr mb)))
  ; monoid
  (define (mempty) (some '()))
  (define pure none)
  (define (mappend a b) (if (and (some? a) (some? b)) (some (append (some->val a) (some->val b))) (pure)))
  ; monad
  (define (>> m mf2) (if (some? m) (mf2) (none)))
  (define (>>= m mf2) (if (some? m) (mf2 (some->val m)) (none)))
  (define return some)

  (define-syntax doM
    (syntax-rules (doM if <- return let)
                  ((doM (if #t t f)) t)
                  ((doM (if #f t f)) f)
                  ((doM (if m t f)) (>>= m (lambda (p) (if p t f))))
                  ((doM (let b . body)) (let b . body))
                  ((doM (return x)) (return x))
                  ((doM (x <- m) . rest) (>>= m (lambda (x) (doM . rest))))
                  ((doM m . rest) (>> m (lambda () (doM . rest))))
                  ('_ (error "invalid doM syntax"))))
  (define (printM . vs) (return (apply print vs)))
  (define (fmap f ml) (doM (l <- ml) (return (f l))))
  (define (mconcat l) (foldr (lambda (i sum) (mappend (fmap list i) sum)) (mempty) l))
  (define (mapM mf l) (mconcat (map mf l))) ; (a -> m b) -> f a -> m (f a)

  (doM
    (r <- (mapM (lambda (i) (return (+ i i))) (list 1 2 3)))
    (return r))
  )
;(print (mb-monads))

(define (state-monad)
  (define (make-state name value) (list name value))
  (define state->name car)
  (define state->value cadr)
  (define ((set-name n) s) (make-state n (state->value s)))
  (define ((get-name) s) (make-state (state->name s) (state->name s)))
  (define ((set-value v) s) (make-state (state->name s) v))
  ; monoid
  (define ((mempty) s) (pure))
  (define (pure) `(() ()))
  (define ((mappend ma mb) s)
    (let ((sa (ma (pure))) (sb (mb (pure))))
      (make-state (state->name sa)
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
                  ((doM (let b . body)) (let b . body))
                  ((doM (return x)) (return x))
                  ((doM (x <- m) . rest) (>>= m (lambda (x) (doM . rest))))
                  ((doM m . rest) (>> m (lambda () (doM . rest))))
                  ('_ (error "invalid doM syntax"))))
  (define (printM . vs) (return (apply print vs)))
  (define (fmap f ml) (doM (l <- ml) (return (f l))))
  (define (mconcat l) (foldr (lambda (i sum) (mappend (fmap list i) sum)) (mempty) l))
  (define (mapM mf l) (mconcat (map mf l))) ; (a -> m b) -> f a -> m (f a)

  (print
    ((doM
       (x <- (mapM (lambda (i)
                     (doM
                       (set-name "test123")
                       (return (* i i))))
                   (list 1 2 3)))
       (n <- (get-name))
       (printM "name = " n)
       (return x))
     (pure)))
)
;(state-monad)

(define args (command-line-arguments))
(if (not (= (length args) 2))
  (display "usage <path-to-ledger.scm> <output-dir>\n")
  (begin
    (load-relative (car args))
    (define entire-api (run ledger (cadr args)))
    (run-sub entire-api ledger)))

