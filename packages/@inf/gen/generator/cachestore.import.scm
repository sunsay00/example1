(module cachestore (cachestore-generate)
  (import scheme chicken data-structures tools signatures matchable migrations redis)
  (require-extension srfi-13 srfi-1)

  (define (dynamic-command-emit ind api model method)
    (let ((model-name (model->name model))
          (typedef (model->typedef model))
          (ret-type (method->return-type method))
          (redisresult (redis-emit api model method (method-ascending? method) (cons 'command (method->command method)))))
      (list "\n  async " (store-sig-emit model method) " {"
            redisresult
            "\n  }")))

  (define (params->input-params params) (filter param-primary-key? params))

  (define (disabled? param) (param-disabled? param 'rdsstore))

  (define (store-method-emit ind api model method)
    (if (not (method-command? method))
      (error "method command not found")
      (dynamic-command-emit (+ ind 1) api model method)))

  (define (store-emit ind api model)
    (list "\n\n" (indent ind) "// interface I" (first-up (model->name model)) "Store"
          (map (lambda (method) (store-method-emit ind api model method)) 
               (remove (lambda (method) (method-disabled? method 'rdsstore))
                       (model->all-methods model)))))

  (define (cachestore-generate api) 
    (let*
      ((ind 1)
       (expr 
         (let ((models (remove (lambda (m) (model-disabled? m 'rdsstore)) (api->models api))))
           (list
             "// this file has been automatically generated, do not modify"
             "\n"
             "\nimport * as _ from 'lodash';"
             "\nimport * as SmokeTester from '../../tools/smoketester';"
             "\n"
             "\nexport default class CachedStore implements IStore {"
             "\n  private _store: IStore;"
             "\n  private _client: ICacheClient;"
             "\n"
             "\n  constructor(store: IStore, client: ICacheClient) { this._store = store; this._client = client; }"
             "\n"
             "\n  runSmokeTests = () => SmokeTester.runCacheStoreSmokeTests(this._store, this._client);"
             "\n"
             "\n  async beginTransaction(): Promise<void> { await this._store.beginTransaction(); }"
             "\n  async commitTransaction(): Promise<void> { await this._store.commitTransaction(); }"
             "\n  async rollbackTransaction(): Promise<void> { await this._store.rollbackTransaction(); }"
             (map (lambda (model) 
                    (list (store-emit ind api model)))
                  models)
             "\n}"))))
      (smoosh expr)))
  )
