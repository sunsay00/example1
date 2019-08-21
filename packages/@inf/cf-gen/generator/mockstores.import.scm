(module mockstores (mockstores-generate)
  (import scheme chicken data-structures tools signatures)
  (require-extension srfi-13 srfi-1)

  (define (string-transform-first fn str)
    (if (null? str) ""
      (let ((l (string->list str)))
        (list->string (cons (fn (car l)) (cdr l))))))

  (define (type-emit ind type)
    (list "\n\ntype " (type->name type) " = {"
          (map (lambda (field) 
                 (list "\n" (indent ind) (field-emit field)))
               (type->fields type))
          "\n}"))

  (define (store-interface-method-emit ind model method)
    (list "\n" (indent ind) "async " (store-sig-emit model method) " { throw new Error('" (model->name model) (method->name method) " should not be invoked') }"))

	(define (store-interface-emit ind model)
    (list "\n" (indent ind) "// I" (first-up (model->name model)) "Store"
          (map (lambda (method) (store-interface-method-emit ind model method))
               (remove (lambda (method) (method-disabled? method 'mockstores))
                       (model->all-methods model)))))

  (define field->name car)
  (define field->type cadr)
  (define field-emit param-emit)

  (define (mockstores-generate api) 
    (let*
      ((ind 1)
       (expr 
         (let ((models (api->models api)))
           (list
             "// this file has been automatically generated, do not modify"
             "\n"
             "\nimport { IStore } from '../../../types/storeinterfaces';"
             "\nimport { IUserContext, Cursorize, Point, Cursored } from '../../../../../../types';"
             "\nimport * as M from '../../../types/models';"
             "\n"
             "\nexport default class MockStore<C extends IUserContext> implements IStore<IUserContext> {"
             "\n"
             "\n  async runSmokeTests(): Promise<string> { throw new Error('runSmokeTests should not be invoked'); }"
             "\n"
             "\n  async beginTransaction(): Promise<void> {}"
             "\n  async commitTransaction(): Promise<void> {}"
             "\n  async rollbackTransaction(): Promise<void> {}"
             (map (lambda (model) (store-interface-emit ind model))
                  (api->models api))
             "\n}"))))
      (smoosh expr)))
)
