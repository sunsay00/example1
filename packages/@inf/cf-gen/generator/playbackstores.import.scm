(module playbackstores (playbackstores-generate)
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
    (list "\n" (indent ind) "async " (store-sig-emit model method) " { "
          "return mockshot.record(() => this._store." (model->name model) (method->name method) "("
          (intersperse (cons "$ctx" (map param->name (store-signature-params model method))) ", ")
          ")) }"))

	(define (store-interface-emit ind model)
    (list "\n" (indent ind) "// I" (first-up (model->name model)) "Store"
          (map (lambda (method) (store-interface-method-emit ind model method))
               (remove (lambda (method) (method-disabled? method 'playbackstores))
                       (model->all-methods model)))))

  (define field->name car)
  (define field->type cadr)
  (define field-emit param-emit)

  (define (playbackstores-generate api) 
    (let*
      ((ind 1)
       (expr 
         (let ((models (remove (lambda (m) (model-disabled? m 'playbackstores)) (api->models api))))
           (list
             "// this file has been automatically generated playbackstores, do not modify"
             "\n"
             "\nconst mockshot = require('mockshot');"
             ;"\nimport MockStore from './mockstore';"
             "\n"
             "\nexport default class PlaybackStore implements IStore {"
             "\n  private _store: IStore;"
             "\n  constructor(store: IStore) {"
             "\n    this._store = store;"
             "\n  }"
             "\n"
             "\n  runSmokeTests = async () => 'success';"
             "\n"
             "\n  enablePlayback(value: boolean) {"
             "\n    mockshot.replay();"
             "\n  }"
             "\n"
             "\n  async beginTransaction(): Promise<void> { await this._store.beginTransaction(); }"
             "\n  async commitTransaction(): Promise<void> { await this._store.commitTransaction(); }"
             "\n  async rollbackTransaction(): Promise<void> { await this._store.rollbackTransaction(); }"
             (map (lambda (model) (store-interface-emit ind model))
                  models)
             "\n}"))))
      (smoosh expr)))
)
