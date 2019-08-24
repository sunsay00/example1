(module rdsstore (rdsstore-generate)
  (import scheme chicken data-structures tools signatures matchable migrations sql interfaces)
  (require-extension srfi-13 srfi-1)

  (define (set-update-element model method query sql-params)
    (list
      "\n    const query = " query ";"
      "\n    if (process.env.NODE_ENV != 'test' && process.env.NODE_ENV != 'production') console.log(`${query}`);"
      "\n    const result = await this._client.query(query, " sql-params ");"
      "\n    if (!result.rows) {"
      "\n      throw new Error('failed to find " (model->name model) " for " (first-down (method->name method)) "');"
      "\n    } else if (result.rows.length == 0) {"
      "\n      return undefined;"
      "\n    } else {"
      "\n      return {"
      "\n        ...result.rows[0].data,"
      (map (lambda (param)
             (list "\n        " (param->name param) ": `${result.rows[0].index}`,"))
           (type->sequential-params (model->api model) (method->return-type method)))
      "\n      };"
      "\n    }"
      "\n  }"))

  (define (get-select-one model ind query params)
    (list
      "\n    const query = " query ";"
      "\n    if (process.env.NODE_ENV != 'test' && process.env.NODE_ENV != 'production') console.log(`${query}`);"
      "\n    const result = await this._client.query(query, " params ");"
      "\n    if (!result.rows) {"
      "\n      throw new Error('failed to find " (model->name model) "');"
      "\n    } else if (result.rows.length == 0) {"
      "\n      return undefined;"
      "\n    } else {"
      "\n      const row = result.rows[0] as Dict<any>;"
      "\n      return {"
      (emit-return-args (model->api model) (model->typedef model) (+ ind 2))
      "\n      };"
      "\n    }"
      "\n  }"))

  (define (get-select-in model method ind query params cursor xs)
    (list
      "\n    if (" xs ".length == 0) {"
      "\n      return [];"
      "\n    } else {"
      "\n      const query = " query ";"
      "\n      if (process.env.NODE_ENV != 'test' && process.env.NODE_ENV != 'production') console.log(`${query}`);"
      "\n      const result = await this._client.query(query, " params ");"
      "\n      if (!result.rows) {"
      "\n        throw new Error('failed to find " (model->name model) " for " (first-down (method->name method)) "');"
      "\n      } else {"
      "\n        const hasMore = result.rows.length > (count || 50);"
      "\n        if (result.rows.length > (count || 50)) result.rows.length = (count || 50);"
      "\n        return result.rows.map((row, i) => ({"
      (emit-dynamic-return-fields (+ ind 3) model method)
      (list "\n" (indent (+ ind 2)) "cursor: !hasMore ? undefined : " cursor)
      "\n        }));"
      "\n      }"
      "\n    }"
      "\n  }"))

  (define (get-select model method ind query params cursor)
    (let ((ret-type (method->return-type method)))
      (if (not (array? ret-type)) (error "get-select expects an array return type")
        (list
          "\n    const query = " query ";"
          "\n    if (process.env.NODE_ENV != 'test' && process.env.NODE_ENV != 'production') console.log(`${query}`);"
          "\n    const result = await this._client.query(query, " params ");"
          "\n    if (!result.rows) {"
          "\n      throw new Error('failed to find " (model->name model) " for " (first-down (method->name method)) "');"
          "\n    } else {"
          "\n      const hasMore = result.rows.length > (count || 50);"
          "\n      if (result.rows.length > (count || 50)) result.rows.length = (count || 50);"
          "\n      return result.rows.map((row, i) => ({"
          (emit-dynamic-return-fields (+ ind 2) model method)
          (list "\n" (indent (+ ind 2)) "cursor: !hasMore ? undefined : " cursor)
          "\n      }));"
          "\n    }"
          "\n  }"))))

  (define (type->sequential-params api type)
    (let ((basetype (base-type type)))
      (define (param-sequential? param)
        (let ((default (param->default-or param "")))
          (match default ('$sequential #t) (_ #f))))
      (let ((typedef (typedef-assq basetype api)))
        (if (not typedef) '()
            (filter param-sequential? (typedef->params typedef))))))

  (define (dynamic-command-emit ind api model method)
    (let* ((model-name (model->name model))
           (typedef (model->typedef model))
           (ret-type (method->return-type method))
           (cmd (cons 'command (method->command method)))
           (sqlresult (sql-emit api model method (method-ascending? method) cmd )))
      (let* ((query (sqlresult->query sqlresult))
             (params (sqlresult->params sqlresult))
             (cursor (sqlresult->cursor sqlresult)))
        (list "\n  async " (store-sig-emit model method) " {"
              (match cmd
                     (`(command (update (= (at ,elements ',elementIndex) ,element)) (where (= id 'id)))
                       (set-update-element model method query params))
                     (`(command (update (= ,elements (push ,element))) (where (= id 'id)))
                       (set-update-element model method query params))
                     (`(command (update . ,ss) (where ,w) (return . ,rs))
                       (list
                         "\n    const query = " query ";"
                         "\n    if (process.env.NODE_ENV != 'test' && process.env.NODE_ENV != 'production') console.log(`${query}`);"
                         "\n    const result = await this._client.query(query, " params ");"
                         "\n    if (result.rows.length != 1) {"
                         "\n      return undefined;"
                         "\n    } else {"
                         "\n      const row = result.rows[0] as Dict<any>;"
                         "\n      return {"
                         (emit-dynamic-return-fields (+ ind 2) model method)
                         "\n      };"
                         "\n    }"
                         "\n  }"))
                     (`(command (delete) . ,rest)
                       (list
                         "\n    const query = " query ";"
                         "\n    if (process.env.NODE_ENV != 'test' && process.env.NODE_ENV != 'production') console.log(`${query}`);"
                         "\n    const result = await this._client.query(query, " params ");"
                         "\n    if (result.rows.length != 1) {"
                         "\n      return undefined;"
                         "\n    } else {"
                         "\n      const row = result.rows[0] as Dict<any>;"
                         "\n      return {"
                         (emit-dynamic-return-fields (+ ind 2) model method)
                         "\n      };"
                         "\n    }"
                         "\n  }"))
                     (`(command (insert . ,vs) (on . ,os) (return . ,rs))
                       (list
                         "\n    // @ts-ignore"
                         "\n    const $now = this.now();"
                         "\n    const query = " query ";"
                         "\n    if (process.env.NODE_ENV != 'test' && process.env.NODE_ENV != 'production') console.log(`${query}`);"
                         "\n    const result = await this._client.query(query, " params ");"
                         "\n    if (result.rows.length != 1) {"
                         "\n      throw new Error('failed to create " model-name "');"
                         "\n    } else {"
                         "\n      const row = result.rows[0] as Dict<any>;"
                         "\n      return {"
                         (emit-dynamic-return-fields (+ ind 2) model method)
                         "\n      };"
                         "\n    }"
                         "\n  }"))
                     (`(command (insert . ,vs) (on . ,os) (update . ,ss) (return . ,rs))
                       (list
                         "\n    // @ts-ignore"
                         "\n    const $now = this.now();"
                         "\n    const query = " query ";"
                         "\n    if (process.env.NODE_ENV != 'test' && process.env.NODE_ENV != 'production') console.log(`${query}`);"
                         "\n    const result = await this._client.query(query, " params ");"
                         "\n    if (result.rows.length != 1) {"
                         "\n      throw new Error('failed to create " model-name "');"
                         "\n    } else {"
                         "\n      const row = result.rows[0] as Dict<any>;"
                         "\n      return {"
                         (emit-dynamic-return-fields (+ ind 2) model method)
                         "\n      };"
                         "\n    }"
                         "\n  }"))
                     (`(command (insert . ,vs) (return . ,rs))
                       (list
                         "\n    // @ts-ignore"
                         "\n    const $now = this.now();"
                         "\n    const query = " query ";"
                         "\n    if (process.env.NODE_ENV != 'test' && process.env.NODE_ENV != 'production') console.log(`${query}`);"
                         "\n    const result = await this._client.query(query, " params ");"
                         "\n    if (result.rows.length != 1) {"
                         "\n      throw new Error('failed to create " model-name "');"
                         "\n    } else {"
                         "\n      const row = result.rows[0] as Dict<any>;"
                         "\n      return {"
                         (emit-dynamic-return-fields (+ ind 2) model method)
                         "\n      };"
                         "\n    }"
                         "\n  }"))
                     ((or `(command (select . ,ss) (where (= id 'id)))
                          `(command (select . ,ss) (where (= uniqueName 'uniqueName)))
                          `(command (select . ,ss) (where (= chatIdentity 'chatIdentity)))
                          `(command (select . ,ss) (where (= parentId 'parentId)))
                          `(command (select . ,ss) (where (and (= id 'id) (= sub $sub))))
                          `(command (select . ,ss) (where (= sub 'sub))))
                      (get-select-one model ind query params))
                     (`(command (select . ,ss) (where (in ,x ',xs)))
                       (get-select-in model method ind query params cursor xs))
                     ((or `(command (select . ,ss))
                          `(command (select . ,ss) (where (= sub $sub))))
                      (get-select model method ind query params cursor))
                     ((or `(command (select . ,ss) (where ,?))
                          `(command (select . ,ss) (groupby . ,?))
                          `(command (select . ,ss) (orderby . ,?)))
                      (get-select model method ind query params cursor))
                     (`(command (select . ,ss) (where ,?) (orderby . ,??))
                       (get-select model method ind query params cursor))
                     (`(command (select . ,ss) (where ,?w) (groupby . ,?g))
                      (get-select model method ind query params cursor))
                     (_ (error "(rdsstore.import.d) no rds match for" cmd)))))))

  (define (map-type-fields api type fn)
    (map (lambda (t)
           (let ((result (model-assq t api)))
             (if (pair? result)
               (let ((model (cadr result)))
                 (map fn (model->props+foreign model)))
               (let ((result (typedef-assq t api)))
                 (if (not result) '()
                   (map fn (typedef->params result)))))))
         (if (plus-type? type)
           (plus-type->types type)
           (list type))))

  (define (emit-dynamic-return-fields ind model method)
    (let ((cmd (cons 'command (method->command method))))
      (match cmd
             (`(command (select (distinct ,x)) (where ,w))
               (list "\n        data: row." (lower x) ","))
             (`(command (select (distinct ,x)))
               (list "\n        data: row." (lower x) ","))
             (_ (let ((type (base-type (method->return-type method))))
                  (map-type-fields (model->api model) type
                                   (lambda (field) (emit-return-field (model->api model) ind field))))))))

  (define (emit-create-values params)
    (define (type->sqlvalue type value)
      (cond
        ((eq? 'String type) (list "'" value "'"))
        ((optional? type) (type->sqlvalue (optional->type type) value))
        (else value)))
    (define idxparams->value car)
    (define idxparams->index cdr)
    (define (make-empty-idxparams) (cons '() 1))
    (define (make-idxparams prev n result) (cons (append (idxparams->value prev) (list result)) (+ n (idxparams->index prev))))
    (intersperse
      (idxparams->value
        (foldl (lambda (prev param)
                 (let ((default (param->specs-assq 'default param)))
                   (if (or (not default))
                     (make-idxparams prev 1 (list "$" (idxparams->index prev)))
                     (make-idxparams prev 0 (type->sqlvalue (param->type param) default)))))
               (make-empty-idxparams)
               params)) ", "))

  (define (emit-lowered-return-fields api typedef ind)
    (let* ((params (typedef->params typedef))
           (ff-params (filter param-foreign-field? params)))
      (list (map (lambda (p)
                   (if (param-foreign-field? p)
                     (let* ((name (param->name p))
                            (type (param->type p))
                            (metadata (param->metadata p 'foreign-field))
                            (fmodel (foreign-field->fmodel api metadata))
                            (sel (foreign-field->selector metadata))
                            (fkey (foreign-field->fkey metadata))
                            (ffnames (map param->name (typedef->params (typedef-assq! (foreign-field->type metadata) api)))))
                       (list "\n" (indent ind) name ": row['" fmodel "." fkey "'] == undefined ? undefined : "
                             (if (null? sel)
                               (list "{"
                                     (map (lambda (ffname)
                                            (list "\n" (indent (+ ind 1)) ffname ": "
                                                  "row['" fmodel "." ffname "'],"
                                                  )) ffnames)
                                     "\n" (indent ind) "},")
                               (list "row['" fmodel "." sel "'],"))))
                     (emit-return-field api ind p))) params))))

  (define (emit-return-field api ind param)
    (let ((name (param->name param))
          (type (param->type param)))
      (let ((sequential-params (type->sequential-params api type)))
        (list "\n" (indent ind) name ": "
              (if (not (null? sequential-params))
                (list "row." (lower name) ".map((x: any, i: any) => ({...x, " (map (lambda (name) (list name ": i")) (map param->name sequential-params)) "}))")
                (match type
                       ('(Optional Point)
                        (list "!row." (lower name) " ? undefined : parsePoint(row." (lower name) ")"))
                       ('Point
                        (list "parsePoint(row." (lower name) ")"))
                       ('DateTime
                        (list "row." (lower name) " instanceof Date ? row." (lower name) " : new Date(row." (lower name) ")" ""))
                       (else (list "row." (lower name)))))
              ","))))

  (define (emit-return-args api typedef ind)
    (let* ((params (typedef->params typedef))
           (ff-params (filter param-foreign-field? params)))
      (list (map (lambda (p)
                   (let* ((name (param->name p))
                          (type (param->type p))
                          (sequential-params (type->sequential-params api type)))
                     (if (param-foreign-field? p)
                       (if (eq? name 'user) ""
                         (let* ((metadata (param->metadata p 'foreign-field))
                                (key (foreign-field->key metadata)))
                           (list "\n" (indent ind) key ": row." (lower key) ",")))
                       (list "\n" (indent ind) name ": "
                             (match type
                                    ('(Optional Point)
                                     (list "!row." (lower name) " ? undefined : parsePoint(row." (lower name) ")"))
                                    ('Point
                                     (list "parsePoint(row." (lower name) ")"))
                                    ('DateTime
                                     (list "row." (lower name ) " instanceof Date ? row." (lower name) " : new Date(row." (lower name) ")" ""))
                                    (else
                                      (if (null? sequential-params)
                                        (list "row." (lower name))
                                        (list "row." (lower name) ".map((x: any, i: any) => ({...x, " (map (lambda (name) (list name ": i")) (map param->name sequential-params)) "}))"))))
                             ","))))
                 params))))

  (define (emit-return-fields api typedef ind)
    (let* ((params (typedef->params typedef))
           (ff-params (filter param-foreign-field? params)))
      (list (map (lambda (p)
                   (let ((name (param->name p))
                         (type (param->type p)))
                     (if (param-foreign-field? p)
                       (let* ((metadata (param->metadata p 'foreign-field))
                              (fmodel (foreign-field->fmodel api metadata))
                              (sel (foreign-field->selector metadata))
                              (fkey (foreign-field->fkey metadata))
                              (ffnames (map param->name (typedef->params (typedef-assq! (foreign-field->type metadata) api)))))
                         (list "\n" (indent ind) name ": row['" fmodel "." fkey "'] == undefined ? undefined : "
                               (if (null? sel)
                                 (list "{"
                                       (map (lambda (ffname)
                                              (list "\n" (indent (+ ind 1)) ffname ": "
                                                    "row['" fmodel "." ffname "'],"
                                                    )) ffnames)
                                       "\n" (indent ind) "},")
                                 (list "row['" fmodel "." sel "'],"))))
                       (list "\n" (indent ind) name ": row." name
                             (if (not (eq? type 'DateTime)) ""
                               (list " instanceof Date ? row." name " : new Date(row." name ")" ""))
                             ","))))
                 params))))

  (define (emit-fully-qualified-selected-columns api typedef)
    (let* ((params (typedef->params typedef))
           (key-params (typedef->key-params api typedef))
           (ff-params (filter param-foreign-field? params)))
      (list "_.uniq(_.concat(["
            (emit-ret-params key-params values)
            "], __fields.filter(f => "
            (if (zero? (length ff-params)) ""
              (list "[" (intersperse (map (lambda (p) (list "'" (param->name p) "'")) ff-params) ", ") "].indexOf(f) == -1 && "))
            "f !== '__typename'))).map(p => `result.${p} AS \"${p}\"`).join(', ');")))

  (define (emit-fully-qualified-columns api typedef)
    (let ((f-params (typedef->params+foreign api api typedef)))
          (list "[" (emit-ret-params (remove disabled? f-params) (lambda (p) (list "result." p " AS \"" p "\""))) "].join(', ');")))

  (define (emit-selected-columns api typedef find?)
    (let* ((name (typedef->name typedef))
           (bad-type? (or (eq? name 'Deck)
                          (eq? name 'Building)
                          ))) ; hack until i simplified the joins
      (let* ((params (typedef->params typedef))
             (key-params (typedef->key-params api typedef))
             (ff-params (filter param-foreign-field? params)))
        (list "_.uniq(_.concat(["
              (emit-ret-params (remove param-foreign-field? key-params) values)
              "], __fields.filter(f => "
              (if (zero? (length ff-params)) ""
                (list "[" (intersperse (map (lambda (p) (list "'" (param->name p) "'")) ff-params) ", ") "].indexOf(f) == -1 && "))
              "f != '__typename')))"
              (if (and find? (not bad-type?)) ".map(n => `${n} AS \"${n}\"`)" "")
              ".join(', ');"))))

  (define (emit-columns api typedef)
    (let ((f-params (typedef->params+foreign api api typedef)))
      (list "[" (emit-ret-params (remove disabled? f-params) values) "].join(', ');")))
  
  (define (emit-ret-params params tf-fn)
    (intersperse (map-names (lambda (name) (list "'" (tf-fn name) "'")) params) ", "))

  (define (typedef->key-params api typedef)
    (let ((params (typedef->params typedef)))
      (let ((ff-names (map (compose foreign-field->key param->foreign-field) (filter param-foreign-field? params))))
        (filter (lambda (p) (or (param-primary-key? p) (member (param->name p) ff-names))) (typedef->params+foreign api api typedef)))))

  (define (params->input-params params) (filter param-primary-key? params))

  (define (query->foreign-query api inner-query params keyprefix)
    (let ((foreign-params (filter param-foreign-field? params)))
      (list
        "WITH result AS (" inner-query ")"
        "\n      SELECT ${retFQFields}"
        (map (lambda (param)
               (let*
                 ((metadata (param->metadata param 'foreign-field))
                  (fmodel (foreign-field->fmodel api metadata))
                  (params (map (lambda (n) (list ", " (apply string-append (map ->string (list "" fmodel "." (param->name n) " AS \"" fmodel "." (param->name n) "\"")))))
                               (typedef->params (typedef-assq! (foreign-field->type metadata) api)))))
                 params))
             foreign-params)
          " FROM result"
        (intersperse 
          (map (lambda (param)
                 (let*
                   ((metadata (param->metadata param 'foreign-field))
                    (key (foreign-field->key metadata))
                    (fkey (foreign-field->fkey metadata))
                    (fmodel (foreign-field->fmodel api metadata))
                    (name (foreign-field->name metadata))
                    (params (map param->name (typedef->params (typedef-assq! (foreign-field->type metadata) api)))))
                   (list "\n      LEFT OUTER JOIN " fmodel " ON " (apply string-append (map ->string (list fmodel "." fkey "=result." keyprefix key))))))
               foreign-params) " "))))

  (define (sequence n) (define (recur i) (if (= i n) '() (cons (+ i 1) (recur (+ i 1))))) (recur 0))

  (define (disabled? param) (param-disabled? param 'rdsstore))

  (define (set-emit api method pair)
    (let ((name (param->name pair)))
      (let ((result (method->params-assq name method)))
        (if (not result)
          (list (car pair) "=$" (cadr pair))
          (let ((type (cadr result)))
            (if (array? type)
              (let ((any-readonly? (foldr (lambda (a b) (or a b)) #f (map param-readonly? (typedef->params (typedef-assq! (base-type type) api))))))
                (if any-readonly?
                  (list (car pair) "=(SELECT array(SELECT a||b FROM (SELECT unnest(u." name ") AS a, unnest($1) AS b) AS t LIMIT ${" name ".length}))")
                  (list (car pair) "=$" (cadr pair))))
              (list (car pair) "=$" (cadr pair))))))))

  (define (remove-name name params) (remove (lambda (param) (eq? (param->name param) name)) params))

  (define (map-names fn params) 
    (map fn (map param->name params)))

  (define (map-sql-args fn params) 
    (map fn (map (lambda (param) 
                   (let ((name (param->name param)))
                     (if (eq? (base-type (param->type param)) 'Boolean)
                       (list name "?1:0")
                       name))) params)))

  (define (map-indexed-names fn offs params)
    (map fn (zip (map param->name params) (map (lambda (n) (+ n offs)) (sequence (length params))))))

  (define (insert-values-emit api param index)
    (define (recur type)
      (cond
        ((array? type) (list "array[" (recur (array->type type)) "]"))
        ((optional? type) (recur (optional->type type)))
        (else
          (let ((typedef (typedef-assq type api)))
            (if (not typedef) (list "$" index)
              (list "$" index "::jsonb"))))))
    (recur (param->type param)))

  (define (insert-values-replacements-emit api param)
    (define (recur type)
      (cond
        ((array? type) (recur (array->type type)))
        ((optional? type) (recur (optional->type type)))
        (else
          (let ((typedef (typedef-assq type api)))
            (if (not typedef)
              (param->name param)
              (list "JSON.stringify(" (param->name param) ")"))))))
    (recur (param->type param)))

  (define (store-method-emit ind api model method)
    (if (not (method-command? method))
      (error "method command not found")
      (dynamic-command-emit (+ ind 1) api model method)))

  (define (store-emit ind api model)
    (list "\n\n" (indent ind) "// interface I" (first-up (model->name model)) "Store"
          (map (lambda (method) (store-method-emit ind api model method)) 
               (remove (lambda (method) (method-disabled? method 'rdsstore))
                       (model->all-methods model)))))

  (define (rdsstore-generate api) 
    (let*
      ((ind 1)
       (expr 
         (let ((models (remove (lambda (m) (model-disabled? m 'rdsstore)) (api->models api))))
           (list
             "// this file has been automatically generated, do not modify"
             "\n"
             "\nimport * as SmokeTester from '../../../../../tools/smoketester';"
             "\nimport { Point, Dict, IUserContext, Cursorize, Cursored } from '../../../../../types';"
             "\nimport { IStore } from '../../types/storeinterfaces';"
             "\nimport * as M from '../../types/models';"
             "\nimport { IDBClient } from '../../../../../types';"
             "\n"
             "\nconst parsePoint = (str: string): Point => {"
             "\n  const matches = /^POINT\\(([^ ]+) ([^)]+)\\)$/.exec(str);"
             "\n  if (matches == null || matches.length != 3) throw new Error(`failed to parse point ${str}`);"
             "\n  const lon = parseFloat(matches[1]);"
             "\n  if (isNaN(lon)) throw new Error(`failed to parse point (1) ${str}`);"
             "\n  const lat = parseFloat(matches[2]);"
             "\n  if (isNaN(lat)) throw new Error(`failed to parse point (2) ${str}`);"
             "\n  return {lon, lat};"
             "\n};"
             "\n"
             "\n"
             "\nexport default class RDSDBStore<C extends IUserContext> implements IStore<C> {"
             "\n  private _client: IDBClient;"
             "\n"
             "\n  now() { return new Date(Date.now()); }"
             "\n"
             "\n  S = (value: unknown) => this._client.prepareString(value);"
             "\n"
             "\n  constructor(client: IDBClient) { this._client = client; }"
             "\n"
             "\n  runSmokeTests = () => SmokeTester.runRDSDBStoreSmokeTests(this._client);"
             "\n"
             "\n  async beginTransaction(): Promise<void> { await this._client.beginTransaction(); }"
             "\n  async commitTransaction(): Promise<void> { await this._client.commitTransaction(); }"
             "\n  async rollbackTransaction(): Promise<void> { await this._client.rollbackTransaction(); }"
             (map (lambda (model) 
                    (list (store-emit ind api model)))
                  models)
             "\n}"))))
      (smoosh expr)))
  )
