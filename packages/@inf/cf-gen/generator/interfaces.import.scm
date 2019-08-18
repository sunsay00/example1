(module interfaces (service-interfaces-generate store-interfaces-generate types-generate)
  (import scheme chicken data-structures tools signatures)
  (require-extension srfi-13 srfi-1)

  (define (string-transform-first fn str)
    (if (null? str) ""
      (let ((l (string->list str)))
        (list->string (cons (fn (car l)) (cdr l))))))

  (define (type-emit ind api input? type)
    (list "\n\ntype " (type->name type) " = {"
          (map (lambda (field) 
                 (list "\n" (indent ind) (field-emit (eval-param api field))))
               (remove param-hidden?
                       (if input?
                         (remove (lambda (field)
                                   (or (param-readonly? field) (param-copied-field? field)))
                                 (type->fields type))
                         (map (lambda (field)
                                (if (param-copied-field? field)
                                  (eval-copied-field type api field)
                                  field))
                              (type->fields type)))))
          "\n};"))

  (define (datatype-emit ind api type)
    (list
      (if (native-type? type)
        (list "\n\ntype Cusored<" (type->name type) "> = {")
        (list "\n\ntype " (type->name type) "Data = {"))
          (map (lambda (field) 
                 (if (param-copied-field? field)
                   (list "\n" (indent ind) (field-emit (eval-param api (eval-copied-field type api field))))
                   (list "\n" (indent ind) (field-emit (eval-foreign-field type api field)))))
               (remove (lambda (field) (foreign-field+user? field)) (type->fields type)))
          "\n};"))

  (define (service-interface-method-emit ind model method)
    (list "\n" (indent ind) (service-sig-emit model method) ";"))

	(define (service-interface-emit ind model)
    (let* ((all-methods (remove (lambda (m) (method-disabled? m 'service-interfaces)) (model->methods model)))
           (service-methods (filter method-serviceonly? all-methods))
           (methods (remove method-serviceonly? all-methods)))
      (if (zero? (length service-methods))
        (list "\n\ninterface I" (first-up (model->name model)) "Service {" 
              (map (lambda (method) (service-interface-method-emit ind model method))
                   all-methods)
              "\n}")
        (list "\n\ninterface I" (first-up (model->name model)) "PartialService {" 
              (map (lambda (method) (service-interface-method-emit ind model method))
                   service-methods)
              "\n}"
              "\ninterface I" (first-up (model->name model)) "Service extends I" (first-up (model->name model)) "PartialService {" 
              (map (lambda (method) (service-interface-method-emit ind model method))
                   methods)
              "\n}"))))

  (define (store-interface-method-emit ind model method)
    (list "\n" (indent ind) (store-sig-emit model method) ";"))

	(define (store-interface-emit ind model)
    (list "\n\ninterface I" (first-up (model->name model)) "Store {" 
          (map (lambda (method) (store-interface-method-emit ind model method)) 
               (remove (lambda (method) (method-disabled? method 'store-interfaces))
                       (model->all-methods model)))
          "\n}"))

  (define (export-type-emit ind type)
    (list "\nexport const " (type->name type) ";"))

  (define field->name car)
  (define field->type cadr)
  (define field-emit param-emit)

  (define (types-generate api)
    (let*
      ((ind 1)
       (expr (list
               "// this file has been automatically generated, do not modify"
               (map (lambda (type) (list (datatype-emit ind api type))) (remove (lambda (t) (typedef-disabled? t 'frontend-interfaces)) (api->typedefs api)))
               (map (lambda (type) (list (type-emit ind api #f type))) (remove (lambda (t) (typedef-disabled? t 'frontend-interfaces)) (api->typedefs api)))
               (map (lambda (type) (list (type-emit ind api #t type))) (api->inputdefs api)))))
      (smoosh expr)))

  (define (store-interfaces-generate api) 
    (let*
      ((ind 1)
       (expr 
         (let ((models (remove (lambda (m) (model-disabled? m 'store-interfaces)) (api->models api))))
           (list
             "// this file has been automatically generated, do not modify"
             (map (lambda (model) (store-interface-emit ind model)) models)
             "\n\ninterface IStore extends " 
             (intersperse 
               (map (lambda (model)
                      (list "I" (first-up (model->name model)) "Store")) 
                    models) ", ")
             " {"
             "\n  runSmokeTests(): Promise<string>;"
             "\n"
             "\n  beginTransaction(): Promise<void>;"
             "\n  commitTransaction(): Promise<void>;"
             "\n  rollbackTransaction(): Promise<void>;"
             "\n};"))))
      (smoosh expr)))

  (define (service-interfaces-generate api) 
    (let*
      ((ind 1)
       (expr 
         (let ((typedefs (remove (lambda (t) (typedef-disabled? t 'service-interfaces)) (api->typedefs api)))
               (models (remove (lambda (m) (model-disabled? m 'service-interfaces)) (api->models api))))
           (list
             "// this file has been automatically generated, do not modify"
             (map (lambda (type) (datatype-emit ind api type)) typedefs)
             (map (lambda (type) (type-emit ind api #f type)) typedefs)
             (map (lambda (type) (type-emit ind api #t type)) (api->inputdefs api))
             "\n"
             (map (lambda (model) (service-interface-emit ind model)) models)))))
      (smoosh expr)))
)
