(module interfaces (service-interfaces-generate store-interfaces-generate types-generate types-generate-client interface-imports)
  (import scheme chicken data-structures tools signatures)
  (require-extension srfi-13 srfi-1)

  (define (string-transform-first fn str)
    (if (null? str) ""
      (let ((l (string->list str)))
        (list->string (cons (fn (car l)) (cdr l))))))

  (define (type-export ind api input? type) (type->name type))

  (define (datatype-export ind api type)
    (if (native-type? type) (list) (list "" (type->name type) "Data")))

  (define (type-emit ind api input? type scoped?)
    (list "\n\nexport type " (type->name type) " = {"
          (map (lambda (field) 
                 (list "\n" (indent ind) (field-emit/scope (eval-param api field) scoped?)))
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

  (define (datatype-emit ind api type scoped?)
    (list
      (if (native-type? type)
        (list "\n\nexport type Cusored<" (type->name type) "> = {")
        (list "\n\nexport type " (type->name type) "Data = {"))
          (map (lambda (field) 
                 (if (param-copied-field? field)
                   (list "\n" (indent ind) (field-emit/scope (eval-param api (eval-copied-field type api field)) scoped?))
                   (list "\n" (indent ind) (field-emit/scope (eval-foreign-field type api field) scoped?))))
               (remove (lambda (field) (foreign-field+user? field)) (type->fields type)))
          "\n};"))

  (define (service-interface-method-emit ind model method)
    (list "\n" (indent ind) (service-sig-emit model method) ";"))

	(define (service-interface-emit ind model)
    (let* ((all-methods (remove (lambda (m) (method-disabled? m 'service-interfaces)) (model->methods model)))
           (service-methods (filter method-serviceonly? all-methods))
           (methods (remove method-serviceonly? all-methods)))
      (if (zero? (length service-methods))
        (list "\n\nexport interface I" (first-up (model->name model)) "Service<C extends IUserContext> {" 
              (map (lambda (method) (service-interface-method-emit ind model method))
                   all-methods)
              "\n}")
        (list "\n\nexport interface I" (first-up (model->name model)) "PartialService<C extends IUserContext> {" 
              (map (lambda (method) (service-interface-method-emit ind model method))
                   service-methods)
              "\n}"
              "\nexport interface I" (first-up (model->name model)) "Service<C extends IUserContext> extends I" (first-up (model->name model)) "PartialService<C> {" 
              (map (lambda (method) (service-interface-method-emit ind model method))
                   methods)
              "\n}"))))

  (define (store-interface-method-emit ind model method)
    (list "\n" (indent ind) (store-sig-emit model method) ";"))

	(define (store-interface-emit ind model)
    (list "\n\nexport interface I" (first-up (model->name model)) "Store<C extends IUserContext> {" 
          (map (lambda (method) (store-interface-method-emit ind model method)) 
               (remove (lambda (method) (method-disabled? method 'store-interfaces))
                       (model->all-methods model)))
          "\n}"))

  (define (export-type-emit ind type)
    (list "\nconst " (type->name type) ";"))

  (define field->name car)
  (define field->type cadr) (define (field-emit/scope p scoped?) (param-emit/scope p scoped?))
  (define (field-emit p) (param-emit/scope p #t))

  (define (interface-imports api)
    (append
      (map (lambda (type) (type->name type))
           (remove (lambda (t) (typedef-disabled? t 'frontend-interfaces)) (api->typedefs api)))
      (map (lambda (type)
             (if (native-type? type) (list)
               (list (type->name type) "Data")))
           (remove (lambda (t) (typedef-disabled? t 'frontend-interfaces)) (api->typedefs api)))
      (map (lambda (type) (type->name type)) (api->inputdefs api))))

  (define (types-generate-client api)
    (let*
      ((ind 1)
       (expr (list
               "// this file has been automatically generated, do not modify"
               "\n"
               "\nimport { IUserContext, Cursored, Cursorize, Point } from '@inf/cf-gen/types';"
               (map (lambda (type) (list (datatype-emit ind api type #f))) (remove (lambda (t) (typedef-disabled? t 'frontend-interfaces)) (api->typedefs api)))
               (map (lambda (type) (list (type-emit ind api #f type #f))) (remove (lambda (t) (typedef-disabled? t 'frontend-interfaces)) (api->typedefs api)))
               (map (lambda (type) (list (type-emit ind api #t type #f))) (api->inputdefs api)))))
      (smoosh expr)))

  (define (types-generate api)
    (let*
      ((ind 1)
       (expr (list
               "// this file has been automatically generated, do not modify"
               "\n"
               "\nimport { IUserContext, Cursored, Cursorize, Point } from '../../../../types';"
               (map (lambda (type) (list (datatype-emit ind api type #f))) (remove (lambda (t) (typedef-disabled? t 'frontend-interfaces)) (api->typedefs api)))
               (map (lambda (type) (list (type-emit ind api #f type #f))) (remove (lambda (t) (typedef-disabled? t 'frontend-interfaces)) (api->typedefs api)))
               (map (lambda (type) (list (type-emit ind api #t type #f))) (api->inputdefs api)))))
      (smoosh expr)))

  (define (store-interfaces-generate api) 
    (let*
      ((ind 1)
       (expr 
         (let ((typedefs (remove (lambda (t) (typedef-disabled? t 'service-interfaces)) (api->typedefs api)))
               (models (remove (lambda (m) (model-disabled? m 'store-interfaces)) (api->models api))))
           (list
             "// this file has been automatically generated, do not modify"
             "\n"
             "\nimport { IUserContext, Cursored, Cursorize, Point } from '../../../../types';"
             "\nimport * as M from './serviceinterfaces';"
             (map (lambda (model) (store-interface-emit ind model)) models)
             "\n\nexport interface IStore<C extends IUserContext> extends " 
             (intersperse 
               (map (lambda (model)
                      (list "I" (first-up (model->name model)) "Store<C>")) 
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
             "\n"
             "\nimport { IUserContext, Cursored, Cursorize, Point } from '../../../../types';"
             "\nimport * as M from '../types/models';"
             (map (lambda (type) (datatype-emit ind api type #t)) typedefs)
             (map (lambda (type) (type-emit ind api #f type #t)) typedefs)
             (map (lambda (type) (type-emit ind api #t type #t)) (api->inputdefs api))
             "\n"
             (map (lambda (model) (service-interface-emit ind model)) models)))))
      (smoosh expr)))
)
