(module connectors (connectors-generate)
  (import scheme chicken data-structures tools signatures typedefs gqlqueries sql)
  (require-extension srfi-13 srfi-1)

  (define (emit-ff-names ff-param)
    (let ((metadata (param->metadata ff-param 'foreign-field)))
      (foreign-field->key metadata)))

  (define (emit-ff-find-by-x model ff-param)
    (let* ((metadata (param->metadata ff-param 'foreign-field))
           (key (foreign-field->key metadata))
           (name (foreign-field->name metadata))
           (fkey (foreign-field->fkey metadata))
           (fmodel (foreign-field->fmodel (model->api model) metadata)))
      (list "const " name " = await this._store." fmodel "FindBy" (first-up fkey) "($ctx, param." key ");")))

  (define (emit-ff-find-by-x-in model ff-param)
    (let* ((metadata (param->metadata ff-param 'foreign-field))
           (key (foreign-field->key metadata))
           (name (foreign-field->name metadata))
           (fkey (foreign-field->fkey metadata))
           (fmodel (foreign-field->fmodel (model->api model) metadata)))
      (list "const " name "List = await this._store." fmodel "FindBy" (first-up fkey) "In($ctx, " key "List);")))

  (define (emit-ff-arg model param)
    (let* ((metadata (param->metadata param 'foreign-field))
           (name (foreign-field->name metadata))
           (sel (foreign-field->selector metadata)))
      (if (null? sel)
        (list name ",")
        (list name ": " name " == undefined ? undefined : " name (list "." sel) ","))))

  (define (emit-ff-mapped-arg model param)
    (let* ((metadata (param->metadata param 'foreign-field))
           (key (foreign-field->key metadata))
           (name (foreign-field->name metadata))
           (sel (foreign-field->selector metadata)))
      (if (null? sel)
        (list name ": " name "Map[p." key "],")
        (list name ": " name "Map[p." key "] == undefined ? undefined : " name "Map[p. " key "]" (list "." sel) ","))))

  (define (emit-ff-unique-ids ff-param)
    (let* ((metadata (param->metadata ff-param 'foreign-field))
           (key (foreign-field->key metadata)))
      (list "const " key "List = _.uniq(param.map(p => p." key "));")))

  (define (emit-ff-mapping ff-param)
    (let* ((metadata (param->metadata ff-param 'foreign-field))
           (key (foreign-field->key metadata))
           (name (foreign-field->name metadata))
           (type (foreign-field->type metadata))
           (fkey (foreign-field->fkey metadata)))
      (list "const " name "Map = " name "List.reduce((sum, " name ") => ({...sum, [" name "." fkey "]: " name "}), {} as Hash<" type ">);")))

  (define (emit-hidden-props model)
    (map (lambda (p) (list (param->name p) ", "))
         (model->hidden-props model)))

  (define (required-set-connector name model db-method api-method)
    (let* ((typedef (model->typedef model))
           (special-props (method->special-props model api-method))
           (params (append special-props (typedef->params typedef))) ; does this need to be unique'd?
           (foreign-params (filter param-foreign-field? params)))
      (list "\n\n  async " (model->name model) name "($ctx: IUserContext, param: " (return-datatype-emit (method->return-type db-method)) "): Promise<" (return-type-emit (method->return-type api-method)) "> {"
            (let ((ff-params (remove (lambda (name) (eq? name 'sub)) (map emit-ff-names foreign-params))))
              (if (zero? (length ff-params))
                (list "\n    const { " (emit-hidden-props model) " ...rest } = param;")
                (list "\n    const { " (emit-hidden-props model) (intersperse ff-params ", ") ", ...rest } = param;")))
            (map (lambda (ff-param) (list "\n    " (emit-ff-find-by-x model ff-param))) foreign-params)
            "\n    return {"
            "\n      ...rest,"
            (map (lambda (ff-param) (list "\n      " (emit-ff-arg model ff-param))) foreign-params)
            "\n    };"
            "\n  }")))

  (define (optional-set-connector name model db-method api-method)
    (let* ((typedef (model->typedef model))
           (params (typedef->params typedef))
           (foreign-params (filter param-foreign-field? params)))
      (list "\n\n  async " (model->name model) name "($ctx: IUserContext, param: " (return-datatype-emit (method->return-type db-method)) "): Promise<" (return-type-emit (method->return-type api-method)) "> {"
            "\n    if (param == undefined) return undefined;"
            (let ((ff-params (remove (lambda (name) (eq? name 'sub)) (map emit-ff-names foreign-params))))
              (if (zero? (length ff-params))
                (list "\n    const { " (emit-hidden-props model) " ...rest } = param;")
                (list "\n    const { " (emit-hidden-props model) (intersperse ff-params ", ") ", ...rest } = param;")))
            (map (lambda (ff-param) (list "\n    " (emit-ff-find-by-x model ff-param))) foreign-params)
            "\n    return {"
            "\n      ...rest,"
            (map (lambda (ff-param) (list "\n      " (emit-ff-arg model ff-param))) foreign-params)
            "\n    };"
            "\n  }")))

  (define (foreign-keyed? model api-method)
    (let* ((typedef (model->typedef model))
           (params (typedef->params typedef))
           (foreign-params (filter param-foreign-field? params))
           (return-type (method->return-type api-method)))
      (let ((basetype (base-type return-type)))
        (if (native-type? basetype) #f
          (let ((field-names (map param->name (type->fields (typedef-assq! (base-type return-type) (model->api model)))))
                (param-names (map param->name foreign-params)))
            (not (not (any (lambda (x) (member x field-names)) param-names))))))))

  (define (multi-get-connector name model db-method api-method)
    (let* ((model-name (model->type model))
           (typedef (model->typedef model))
           (params (typedef->params typedef))
           (foreign-params (filter param-foreign-field? params))
           (return-type (method->return-type api-method)))
      (list "\n\n  async " (model->name model) name "($ctx: IUserContext, param: " (return-datatype-emit (method->return-type db-method)) "): Promise<" (return-type-emit return-type) "> {"
            (if (foreign-keyed? model api-method)
              (list
                "\n    if (param.length == 0) return [];"
                (map (lambda (ff-param) (list "\n    " (emit-ff-unique-ids ff-param))) foreign-params)
                (map (lambda (ff-param) (list "\n    " (emit-ff-find-by-x-in model ff-param))) foreign-params)
                (map (lambda (ff-param) (list "\n    " (emit-ff-mapping ff-param))) foreign-params)
                (if (native-type? model-name)
                  (list "\n    return param.map((p: Cursored<" model-name ">) => ({")
                  (list "\n    return param.map((p: " model-name "Data) => ({"))
                "\n      ...p,"
                (map (lambda (ff-param) (list "\n      " (emit-ff-mapped-arg model ff-param))) foreign-params)
                "\n    }));")
              "\n    return param;")
            "\n  }"
            )))

  (define (optional-get-connector name model db-method api-method)
    (let* ((typedef (model->typedef model))
           (params (typedef->params typedef))
           (foreign-params (filter param-foreign-field? params)))
      (list "\n\n  async " (model->name model) name "($ctx: IUserContext, param: " (return-datatype-emit (method->return-type db-method)) "): Promise<" (return-type-emit (method->return-type api-method)) "> {"
            "\n    if (param == undefined) return undefined;"
            (let ((ff-params (remove (lambda (name) (eq? name 'sub)) (map emit-ff-names foreign-params))))
              (if (zero? (length ff-params))
                (list "\n    const { " (emit-hidden-props model) " ...rest } = param;")
                (list "\n    const { " (emit-hidden-props model) (intersperse ff-params ", ") ", ...rest } = param;")))
            (map (lambda (ff-param) (list "\n    " (emit-ff-find-by-x model ff-param))) foreign-params)
            "\n    return {"
            "\n      ...rest,"
            (map (lambda (ff-param) (list "\n      " (emit-ff-arg model ff-param))) foreign-params)
            "\n    };"
            "\n  }")))

  (define (lookup-connector model api-method db-method)
    (let* ((api-method-name (symbol->string (method->name api-method)))
           (db-method-name (symbol->string (method->name db-method)))
           (name (string-append db-method-name "_to_" api-method-name)))
      (let ((type (method->return-type api-method))
            (get? (get-command? (method->command api-method))))
        (let ((connector (cond
                           ((array? type)
                            (if get?
                              multi-get-connector
                              (error "multi set connector is undefined")))
                           ((optional? type)
                            (if get?
                              optional-get-connector
                              optional-set-connector))
                           (else
                             (if get?
                               (error "required get connector is undefined")
                               required-set-connector)))))
          (connector name model db-method api-method)))))


  (define (emit-permutations api)
    (let ((models (api->models api)))
      (map (lambda (model)
             (let* ((methods (model->all-methods model))
                    (api-methods (filter method-apionly? methods))
                    (db-methods (filter method-dbonly? methods)))
               (map (lambda (api-method)
                      (map (lambda (db-method)
                             (let ((api-method-name (method->name api-method))
                                   (db-method-name (method->name db-method)))
                               (if (eq? api-method-name db-method-name)
                                 (lookup-connector model api-method db-method) "")))
                           db-methods))
                    api-methods)))
             models)))

  (define (connectors-generate api)
		(let*
			((ind 1)
			 (expr 
         (list
           "// this file has been automatically generated, do not modify\n"
           "\n"
           "import * as _ from 'lodash';"
           "\n"
           "\nexport default class Connector {"
           "\n  private _store: IStore;"
           "\n  constructor(store: IStore) {"
           "\n    this._store = store;"
           "\n  }"
           (emit-permutations api)
           "\n}")))
      (smoosh expr)))
)

