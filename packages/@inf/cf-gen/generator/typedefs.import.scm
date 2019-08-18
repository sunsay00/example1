(module typedefs (typedefs-generate graphql-params graphql-param)
  (import scheme chicken data-structures tools signatures)
  (require-extension srfi-13 srfi-1)

  (define (make-required x) (list 'Required x))
  (define (required? x) (and (pair? x) (eq? (car x) 'Required)))
  (define (required->type x) (cadr x))

  (define (expand-type x)
    (cond
      ((optional? x)
       (make-optional
         (let ((t (expand-type (optional->type x))))
           (if (required? t) (required->type t) t))))
      ((array? x) (make-required (make-array (expand-type (array->type x)))))
      ((plus-type? x) x)
      ((symbol? x) (make-required x))
      (else (error "unknown param type! (1)" x))))

  (define (type-emit type)
    (define (recur x)
      (cond
        ((optional? x) (recur (optional->type x)))
        ((array? x) (list "[" (recur (array->type x)) "]"))
        ((required? x) (list (recur (required->type x)) "!"))
        ((symbol? x) x)
        (else (error "unknown param type! (2)" x))))
    (recur (expand-type type)))

  (define (paged-type-emit type)
    (define (recur x once? pageit?)
      (cond
        ((optional? x) (recur (optional->type x) #f pageit?))
        ((array? x)
         (let ((y (array->type x)))
           (if once?
             (recur y #f #t)
             (list "[" (recur y #f #f) "]"))))
        ((required? x)
         (let ((y (required->type x)))
           (if (and once? (array? y))
             (recur y #t pageit?)
            (list (recur y #f pageit?) "!"))))
       ((native-type? x)
        (if pageit?  (list x "Page") x))
       ((plus-type? x)
        (let ((t (intersperse (plus-type->types x) "_")))
          (if pageit? (list t "Page") t)))
       ((symbol? x)
        (if pageit? (list x "Page") x))
       (else (error "unknown param type! (3)" x))))
     (recur (expand-type type) #t #f))

  (define (->jsvalue x)
    (cond ((string? x) (list "\"" x "\""))
          ((boolean? x) (if x "true" "false"))
          (else x)))

  (define (graphql-params model method)
    (map (lambda (param) (convert-to-input-param (model->api model) param))
         (remove (lambda (param) (member (param->name param) '(__fields sub $ctx)))
                 (service-signature-params model method))))

  (define (graphql-field api param name-tf-fn)
    (list (name-tf-fn (param->name param)) ": "
          (type-emit (param->type (eval-param api param)))))

  (define (schema-field-emit ind api param) 
    (list (indent ind) (graphql-field api param values)))

  (define (fields-emit ind api input? params)
    (map (lambda (param)
           (list (schema-field-emit ind api param) "\n"))
         (if input?
           (filter (compose not param-copied-field?) params)
           (map (lambda (param) (eval-copied-field (base-type (param->type param)) api param)) params))))

  (define (graphql-param param name-tf-fn)
    (list (name-tf-fn (param->name param)) ": " (type-emit (param->type param))
          (if (param-default? param) (list "=" (->jsvalue (param->default param))) "")))

  (define (schema-param-emit ind param) 
    (list (indent ind) (graphql-param param values)))

  (define (params-emit ind params)
    (map (lambda (param) (list (schema-param-emit ind param) "\n")) params))

  (define (scalar-emit ind scalar) 
    (list "\n" (indent ind) "scalar " (scalar->name scalar)))

  (define (typedef-emit ind api typedef)
    (list "\n" (indent ind) "type " (typedef->name typedef) " {\n"
          (fields-emit (+ ind 1) api #f (remove param-hidden? (typedef->params typedef)))
          ;(list (indent (+ ind 1)) "cursor: String\n")
          (indent ind) "}"))

  (define (typedef-page-emit ind api typedef)
    (list "\n" (indent ind) "type " (typedef->name typedef) "Page {"
          "\n" (indent (+ ind 1)) "cursor: String"
          "\n" (indent (+ ind 1)) "items: [" (typedef->name typedef) "!]!"
          "\n" (indent ind) "}"))

  (define (inputdef-emit ind api inputdef)
    (list "\n" (indent ind) "input " (inputdef->name inputdef) " {\n"
      (fields-emit (+ ind 1) api #t (remove param-readonly? (inputdef->params inputdef)))
      (indent ind) "}"))

  (define (method-emit ind model method)
    (let ((params (let ((params (graphql-params model method)))
                    (if (= (length params) 0) ""
                      (list "(" (intersperse (map (lambda (param) (schema-param-emit 0 param)) params) ", ") ")")))))
      (list "\n" (indent ind) (model->name model) (method->name method) params ": " (paged-type-emit (method->return-type method)))))

  (define (emit-plus-types api ind)
    (define (emit-plus-type pt)
      (let* ((typenames (plus-type->types pt))
             (typedefs (remove (lambda (t) (typedef-disabled? t 'typedefs)) (map (lambda (t) (typedef-assq t api)) typenames))))
        (if (<= (length typedefs) 1) (list)
          (list "\n" (indent ind) "type " (intersperse (map typedef->name typedefs)  "_") " {\n"
                (map (lambda (typedef) (fields-emit (+ ind 1) api #f (remove param-hidden? (typedef->params typedef)))) typedefs)
                (indent ind) "}"))))
    (define (emit-plus-type-page pt)
      (let ((name (intersperse (plus-type->types pt) "_")))
        (list "\n" (indent ind) "type " name "Page {\n"
              (indent (+ 1 ind)) "cursor: String\n"
              (indent (+ 1 ind)) "items: [" name "!]!\n"
              (indent ind) "}")))
    (define (collect map-ex-fn)
      (map (lambda (method)
             (let* ((ret-type (method->return-type method))
                    (t (base-type ret-type)))
               (if (not (plus-type? t)) (list)
                 (list
                   (emit-plus-type t)
                   (if (not (array? ret-type)) (list)
                     (emit-plus-type-page t))))))
           (foldl append '() (map-ex-fn 'typedefs (lambda (method model) method) api))))
    (list (collect map-queries-ex)
          (collect map-mutations-ex)))

  (define (typedefs-emit api) 
    (let*
      ((ind 1))
     (list 
       "// this file has been automatically generated, do not modify\n\n"
       "export default `\n"
       (map (lambda (i) (scalar-emit ind i)) (api->scalars api))
       "\n"
       (map (lambda (i) (typedef-emit ind api i)) (remove (lambda (t) (typedef-disabled? t 'typedefs)) (api->typedefs api)))
       "\n"
       (map (lambda (i) (typedef-page-emit ind api i)) (remove (lambda (t) (typedef-disabled? t 'typedefs)) (api->typedefs api)))
       "\n"
       (map (lambda (i) (inputdef-emit ind api i)) (api->inputdefs api))
       "\n"
       (emit-plus-types api ind)
       "\n"
       "\n" (indent ind) "type TestEmailTypeResponse {"
       "\n" (indent ind) "  email: Email"
       "\n" (indent ind) "}"
       "\n" (indent ind) "type TestDateTimeTypeResponse {"
       "\n" (indent ind) "  dateTime: DateTime"
       "\n" (indent ind) "}"
       "\n"
       "\n" (indent ind) "type StringPage {"
       "\n" (indent ind) "  cursor: String"
       "\n" (indent ind) "  items: [String!]!"
       "\n" (indent ind) "}"
       "\n"
       "\n" (indent ind) "type Queries {"
       "\n" (indent (+ 1 ind)) "# test"
       "\n" (indent (+ 1 ind)) "testEmailType(email: Email): TestEmailTypeResponse"
       "\n" (indent (+ 1 ind)) "testDateTimeType(dateTime: DateTime): TestDateTimeTypeResponse"
       "\n" (indent (+ 1 ind)) "testUnauthorized(arg: String): String"
       "\n" (indent (+ 1 ind)) "testAdminAuthorized(arg: String): String"
       "\n" (indent (+ 1 ind)) "testAuthorized(arg: String): String"
       "\n"
       (map-queries-ex 'typedefs (lambda (method model) (method-emit (+ 1 ind) model method)) api)
       "\n" (indent ind) "}\n\n" (indent ind) "type Mutations {"
       (map-mutations-ex 'typedefs (lambda (method model) (method-emit (+ 1 ind) model method)) api)
       "\n" (indent ind) "}\n\n" (indent ind) "schema {\n" (indent (+ ind 1)) "query: Queries\n" (indent (+ ind 1)) "mutation: Mutations\n" (indent ind) "}\n"
       "`;\n")))

  (define (typedefs-generate api) 
    ;(->string (typedefs-emit api))
    (smoosh (typedefs-emit api))
    )
)
