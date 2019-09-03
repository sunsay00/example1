(module gqlqueries (gql-proxy-query-emit gql-query-emit gql-mutation-emit gqlqueries-generate-fixtures)
  (import scheme chicken data-structures tools signatures typedefs)
  (require-extension srfi-13 srfi-1)

  (define (type-fields api type)
    (if (plus-type? type)
      (foldl append '()
             (map (lambda (t)
                    (typedef->params (typedef-assq! t api)))
                  (plus-type->types type)))
      (typedef->params
        (typedef-assq! type api))))

  (define (methoddisabled? m) (method-disabled? m 'gqlqueriesfixtures))

  (define (surround params fn)
    (if (zero? (length params)) "" (list "(" (fn) ")")))

  (define (params-emit params)
    (surround
      params
      (lambda ()
        (intersperse 
          (map (lambda (param) 
                 (graphql-param param (lambda (name) (list "$" name)))) params) ", "))))

  (define (inner-params-emit params)
    (surround
      params
      (lambda ()
        (intersperse (map (lambda (param) (list (param->name param) ": $" (param->name param))) params) ", "))))

  (define (recur-param->name model param)
    (define (recur param)
      (let ((type (base-type (param->type param))))
        (list (param->name param)
              (if (native-type? type) '()
                (list " { "
                      (intersperse
                        (cond
                          ((param-foreign-field? param)
                           (let* ((ff (param->metadata param 'foreign-field))
                                  (selector (foreign-field->selector ff)))
                             (if (not (null? selector))
                               (let ((fftype (foreign-field->type ff)))
                                 (map param->name (eval-selector-params (model->api model) selector fftype)))
                               (map recur (eval-selector-params (model->api model) '() type)))))
                          ((param-copied-field? param)
                           (let* ((cf (param->metadata param 'copied-field))
                                  (selector (copied-field->selector cf)))
                             (if (not (null? selector))
                               (let ((cftype (base-type (param->type param))))
                                 (map param->name (eval-selector-params (model->api model) selector cftype)))
                               (map recur (eval-selector-params (model->api model) '() type)))))
                          (else (map recur (eval-selector-params (model->api model) '() type)))) " ")
                      " }")))))
    (recur param))

  (define (gql-proxy-query-emit model method)
    (let ((name (method->name method))
          (params (graphql-params model method)))
      (list
        "\n                " (model->name model) " {"
        "\n                  " (intersperse
                     (map (lambda (param) (recur-param->name model param))
                          (remove param-hidden? (typedef->params (model->typedef model)))) " ")
        "\n              }")))

  (define (gql-query-emit model method)
    (let ((name (method->name method))
          (params (graphql-params model method))
          (rettype (method->return-type method)))
      (list
        "\n  query " (params-emit params) " {"
        "\n    " (model->name model) name " " (inner-params-emit params)
        (if (native-type? (base-type rettype))
          (if (array? rettype)
            (let ((child-type (array->type rettype)))
              (if (native-type? child-type)
                (list " {"
                  "\n      cursor"
                  "\n      items"
                  "\n    }")
                ""))
            "")
          (let ((result-params
                  (intersperse
                    (map (lambda (param) (recur-param->name model param))
                         (remove param-hidden?
                                 (type-fields (model->api model)
                                             (base-type rettype))
                                 )) " ")))
            (list " {"
                  (if (array? rettype)
                    (list
                      "\n      cursor"
                      "\n      items { " result-params " }")
                    (list "\n      " result-params))
                  "\n    }")))
        "\n  }")))

  (define (gql-mutation-emit model method)
    (let ((name (method->name method))
          (params (graphql-params model method))
          (typedef (model->typedef model))
          (rettype (base-type (method->return-type method))))
      (list
        "\n  mutation " (params-emit params) " {"
        "\n    " (model->name model) name " " (inner-params-emit params)
        (if (native-type? (base-type rettype)) ""
          (list " {"
                "\n      " (intersperse
                             (map (lambda (param) (recur-param->name model param))
                                  (remove param-hidden? (typedef->params (typedef-assq! rettype (model->api model))))) " ")
                "\n    }"))
        "\n  }")))

  (define (get-method-emit model method)
    (let* ((name (list (model->name model) (method->name method)))
           (cname (list (first-up (model->name model)) (method->name method)))
           (params (graphql-params model method))
           (ret-type (method->return-type method)))
      (list 
        "\nexport const " name " = async <C extends IUserContext>(resolver: Resolver<C>, $ctx: C, " (intersperse (map param-emit params) ", ") "): Promise<" (paginated-type return-type-emit ret-type) "> => {"
        "\n  const query = `" (gql-query-emit model method) "`;"
        "\n  const json = await resolver.resolve({}, query, {"
        (intersperse (map (lambda (n) (list "\n    " n)) (map param->name params)) ",")
        "\n  }, $ctx);"
        (cond
          ((array? ret-type)
           (list
             "\n  if (json != undefined && json.errors != undefined) {"
             "\n    throw new Error(`resolver-error (" name "): ${json.errors.map(e => e.message).join(\", \")}`);"
             "\n  } else if (json != undefined && json.data != undefined && !json.data." name ") {"
             "\n    return { items: [] };"
             "\n  } else {"
             "\n    expect(json).toMatchObject({ data: { " name ": expect.anything() } });"
             "\n    if (!json || !json.data) throw new Error('failed to create " name "');"
             "\n    return json.data." name ";"
             "\n  }"
             "\n};"))
          ((optional? ret-type)
           (list
             "\n  if (json != undefined && json.errors != undefined) {"
             "\n    throw new Error(json.errors.map(e => e.message).join(\", \"));"
             "\n  } else if (json != undefined && json.data != undefined && !json.data." name ") {"
             "\n    return undefined;"
             "\n  } else {"
             "\n    expect(json).toMatchObject({ data: { " name ": expect.anything() } });"
             "\n    if (!json || !json.data) throw new Error('failed to create " name "');"
             "\n    return json.data." name ";"
             "\n  }"
             "\n};"))
          (else
            (list
              "\n  if (json != undefined && json.errors != undefined) {"
              "\n    throw new Error(json.errors.map(e => e.message).join(\", \"));"
              "\n  } else if (json != undefined && json.data != undefined && !json.data." name ") {"
              "\n    throw new Error(\"invalid gql response\");"
              "\n  } else {"
              "\n    expect(json).toMatchObject({ data: { " name ": expect.anything() } });"
              "\n    if (!json || !json.data) throw new Error('failed to create " name "');"
              "\n    return json.data." name ";"
              "\n  }"
              "\n};"))))))

  (define (set-method-emit model method)
    (let* ((name (list (model->name model) (method->name method)))
           (cname (list (first-up (model->name model)) (method->name method)))
           (params (graphql-params model method))
           (ret-type (method->return-type method)))
      (list 
       "\nexport const " name " = async <C extends IUserContext>(resolver: Resolver<C>, $ctx: C, " (intersperse (map param-emit params) ", ") "): Promise<" (cursorize return-type-emit ret-type) "> => {"
       "\n  const query = `" (gql-mutation-emit model method) "`;"
       "\n  const json = await resolver.resolve({}, query, {"
       (intersperse (map (lambda (n) (list "\n    " n ": " n " as any")) (map param->name params)) ",")
       "\n  }, $ctx);"
       (cond
         ((optional? ret-type)
          (list
            "\n  if (json != undefined && json.errors != undefined) {"
            "\n    throw new Error(json.errors.map(e => e.message).join(\", \"));"
            "\n  } else if (json != undefined && json.data != undefined && !json.data." name ") {"
            "\n    return undefined;"
            "\n  } else {"
            "\n    expect(json).toMatchObject({ data: { " name ": expect.anything() } });"
            "\n    if (!json || !json.data) throw new Error('failed to create " name "');"
            "\n    return json.data." name ";"
            "\n  }"
            "\n};"))
         (else
           (list
             "\n  if (json != undefined && json.errors != undefined) {"
             "\n    throw new Error(json.errors.map(e => e.message).join(\", \"));"
             "\n  } else {"
             "\n    expect(json).toMatchObject({ data: { " name ": expect.anything() } });"
             "\n    if (!json || !json.data) throw new Error('failed to create " name "');"
             "\n    return json.data." name ";"
             "\n  }"
             "\n};"))))))

  (define (model-emit model)
    (let ((queries (remove methoddisabled? (filter get? (model->methods model))))
          (mutations (remove methoddisabled? (filter set? (model->methods model)))))
      (list 
        (map (lambda (method) (get-method-emit model method)) queries)
        (map (lambda (method) (set-method-emit model method)) mutations)
        "\n")))

  (define (gqlqueries-generate-fixtures api)
    (let* ((z 1)
           (models (remove (lambda (m) (model-disabled? m 'gqlqueries)) (api->models api)))
           (expr
             (list
               "// this file has been automatically generated by gqlqueries, do not modify"
               "\n"
               "\nimport { IStore } from '../../../types/storeinterfaces';"
               "\nimport { IUserContext, Paginated, Point } from '../../../../../../types';"
               "\nimport * as M from '../../../types/models';"
               "\nimport { Resolver } from '../../../../../../tools/resolver';"
               "\n"
               (map model-emit models)
               "\n"
               "\nexport const createDeck = async <C extends IUserContext>(resolver: Resolver<C>, $ctx: C, factoryId: string, title?: string, description?: string): Promise<M.Deck> => {"
               "\n  const createQuery = `mutation ($factoryId: String!, $expiresAt: DateTime, $cards: [DeckCardInput!]!, $title: String = null, $description: String = null) {"
               "\n      decksCreate (factoryId: $factoryId, expiresAt: $expiresAt, cards: $cards, title: $title, description: $description) {"
               "\n        sub id createdAt updatedAt factory { id } expiresAt user { name, avatarUri } cards { name value } title description"
               "\n      }"
               "\n    }`;"
               "\n  const createJson = await resolver.resolve({}, createQuery, {"
               "\n    factoryId,"
               "\n    expiresAt: new Date(54321).toJSON(),"
               "\n    cards: [{ name: 'card1', value: 1 }],"
               "\n    title,"
               "\n    description"
               "\n  }, $ctx);"
               "\n  expect(createJson).toMatchObject({ data: { decksCreate: expect.anything() } });"
               "\n  if (!createJson || !createJson.data) throw new Error('failed to create deck');"
               "\n  return createJson.data.decksCreate;"
               "\n};"
               "\n"
               "\nexport const removeDeck = async <C extends IUserContext>(resolver: Resolver<C>, $ctx: C, deck: M.Deck): Promise<M.Deck> => {"
               "\n  const removeQuery = `mutation ($id: String!) {"
               "\n      decksRemove (id: $id) {"
               "\n        sub id user { name } title description"
               "\n      }"
               "\n    }`;"
               "\n  const removeJson = await resolver.resolve({}, removeQuery, { id: deck.id }, $ctx);"
               "\n  if (removeJson.data == undefined) throw new Error('failed to remove deck');"
               "\n  return removeJson.data.decksRemove;"
               "\n};"
               "\n"
               "\nexport const findDeckById = async <C extends IUserContext>(resolver: Resolver<C>, $ctx: C, deckId: string): Promise<M.Deck | undefined> => {"
               "\n  const deckQuery = `query ($id: String!) {"
               "\n      decksFindById (id: $id) {"
               "\n        sub id user { name avatarUri }"
               "\n      }"
               "\n    }`;"
               "\n  const result = await resolver.resolve({}, deckQuery, { id: deckId }, $ctx);"
               "\n  if (result == undefined || result.data == undefined) return undefined;"
               "\n  return result.data.decksFindById as M.Deck;"
               "\n};"
               "\n"
               "\nexport const backendCreateUser = async <C extends IUserContext>(store: IStore<C>, $ctx: C, name: string, avatarUri: string, locale: string, region: string): Promise<M.User> => {"
               "\n  return store.usersCreate($ctx, $ctx.sub, { name: name, avatarUri: avatarUri }, locale, region);"
               "\n};"
               "\n"
               "\nexport const backendRemoveUser = async <C extends IUserContext>(store: IStore<C>, $ctx: C, id: string): Promise<M.User> => {"
               "\n  const result = await store.usersRemove($ctx, id);"
               "\n  if (result == undefined) throw new Error('user should not be undefined');"
               "\n  return result;"
               "\n}"
               "\n"
               "\nexport const backendFindUserById = async <C extends IUserContext>(store: IStore<C>, $ctx: C, sub: string, id: string): Promise<M.User | undefined> => {"
               "\n  return store.usersFindById($ctx, id.toString());"
               "\n}")))
      (smoosh expr)))

)
