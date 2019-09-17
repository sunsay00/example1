(module apollobindings2 (apollobindings-generate-index apollobindings-generate-index-client)
  (import scheme chicken data-structures tools signatures typedefs gqlqueries matchable functional)
  (require-extension srfi-13 srfi-1)

  (define (gen-update-code model method method-ref)
    (let ((model-name (model->name model)))
      (cond
        (#t
         (if (not (method-command? method)) ""
           (let ((cmd (cons 'command (method->command method))))
             (if (not (method-command? method-ref)) ""
               (let ((name (method->name method-ref))
                     (cmd-ref (cons 'command (method->command method-ref))))

                 (define (gen-unset)
                   (let ((prop-name (list (model->name model) name)))
                     (list "\n        delete readData." prop-name ";")))

                 (define (gen-set)
                   (let ((prop-name (list (model->name model) name)))
                     (list "\n        readData." prop-name " = result;")))

                 (match cmd
                        (`(command (insert . ,is) (on . ,os) (return . ,ss))
                          (list))
                        (`(command (update (= (at ,elements ',index) ,element)) (where (= ,id ',id)))
                          (list))
                        (`(command (update (= ,elements (push ,element))) (where (= ,id ',id)))
                          (list))
                        (`(command (insert . ,is) (return . ,rs))
                          (match cmd-ref
                                 (`(command (select . ,ss) (where ,w)) (gen-set))
                                 (_ (error (smoosh "(apollobindings.import.ref) no match for gen-update-code " (->string cmd-ref))))))
                        (`(command (insert . ,is) (on . ,os) (update . ,us) (return . ,rs))
                          (match cmd-ref
                                 (`(command (select . ,ss) (where ,w)) (gen-set))
                                 (_ (error (smoosh "(apollobindings.import.ref) no match for gen-update-code " (->string cmd-ref))))))
                        (`(command (update . ,us) (where ,w) (return . ,rs))
                          (match cmd-ref
                                 (`(command (select . ,ss) (where ,w)) (gen-set))
                                 (_ (error (smoosh "(apollobindings.import.ref) no match for gen-update-code " (->string cmd-ref))))))
                        (`(command (delete) (where ,w) (return . ,rs))
                          (match cmd-ref
                                 (`(command (select . ,ss) (where ,w)) (gen-unset))
                                 (_ (error (smoosh "(apollobindings.import.ref) no match for gen-update-code " (->string cmd-ref))))))
                        (_ (error (smoosh "(apollobindings.import) no match for gen-update-code " (->string cmd)))))))))))))

  (define (gen-result-variables params)
    (list "{ " 
          (intersperse
            (map (lambda (param) 
                   (list (param->name param) ": result." (param->name param) " == undefined ? null : result." (param->name param))) params) ", ") " }"))

  (define (gen-variables params)
    (list "{ " 
          (intersperse
            (map (lambda (param) 
                   (list "\n      " (param->name param) ": opts." (param->name param) " == undefined ? null : opts." (param->name param))) params) ", ") "\n    }"))

  (define (gen-update-variables model name)
    (cond
      ((eq? name 'FindByBuildingIdAll) 
       (list "{ buildingId: result.buildingId, after: null, count: null }"))
      ((eq? name 'FindByBuildingId) 
       (list "{ buildingId: result.buildingId, after: null, count: null }"))
      (else
        (list "{ after: null, count: null }"))))

  (define (member/M vars)
    (doM
      (s <- (state-get))
      (return (member (->string (sort (map symbol->string vars) string<?)) s))))

  (define (append/M vars)
    (doM
      (prev <- (state-get))
      (if/m (member/M vars)
            (return)
            (doM
              (state-set (cons (->string (sort (map symbol->string vars) string<?)) prev))
              (return)))))

  (define (gen-update-vars/M model method)
    (doM
      (if (eq? (method->name method) 'FindMine)
        (doM
          (append/M '(sub))
          (return (list ", variables: { sub: result.sub  }")))
        (let ((ffs (model->foreign model)))
          (if (null? ffs)
            (doM
              (if/m (member/M '())
                    (return '())
                    (doM (append/M '()) (return ""))))
            (let ((fffs (filter (lambda (ff) (member (foreign-field->key ff) (map param->name (method->params method)))) ffs)))
              (if (null? fffs)
                (doM
                  (if/m (member/M '())
                        (return '())
                        (doM (append/M '()) (return ""))))
                (let ((vars (map foreign-field->key fffs)))
                  (doM
                    (append/M vars)
                    (return (list ", variables: { " (intersperse (map (lambda (ff) (list (foreign-field->key ff) ": result." (foreign-field->name ff) "." (foreign-field->fkey ff))) fffs) ", ") " }")))))))))))

  (define (gen-update mode model method storybook?)

    (let* ((methods (filter (lambda (method)
                              (and (get? method)
                                   (or (method-serviceonly? method) (method-apionly? method))))
                            (model->all-methods model)))
           (model-name (model->name model))
           (methodnames-to-update (delete-duplicates
                                    (map method->name (filter (lambda (m) (not (array? (method->return-type m)))) methods))))
           (methodnames-to-update/array (delete-duplicates
                                          (map method->name (filter (lambda (m) (array? (method->return-type m))) methods))))
           (method-lookup (map (lambda (m) (cons (method->name m) m)) methods)))
      (list
        (if (and (zero? (length methodnames-to-update)) (zero? (length methodnames-to-update/array))) (list)
          (list "\n    update: (proxy: any, mutationResult: any) => {"
                "\n      const result = mutationResult.data." model-name mode ";"
                "\n      if (!result) return;"

                ;(if (zero? (length methodnames-to-update)) (list)
                ;  (intersperse
                ;    (map (lambda (name)
                ;           (let ((query-name (list (model->name model) name "Query"))
                ;                 (method-ref (cdr (assq name method-lookup))))
                ;             (list
                ;               "\n      try {"
                ;               "\n        let readData = proxy.readQuery({ query: " query-name ", variables: " (gen-result-variables (method->params method-ref)) " });"
                ;               (gen-update-code model method method-ref)
                ;               "\n        proxy.writeQuery({ query: " query-name ", data: readData, variables: " (gen-result-variables (method->params method-ref)) " });"
                ;               "\n      } catch (err) {"
                ;               (if storybook?
                ;                 (list "\n        console.log('cache not updated " model-name "." name "');")
                ;                 (list "\n        if (process.env.NODE_ENV != 'test') console.log('cache not updated " model-name "." name "');"))
                ;               "\n      }"
                ;               )))
                ;         methodnames-to-update) "\n"))

                (if (zero? (length methodnames-to-update/array)) (list)
                    (state-run
                      (mapM (lambda (name)
                              (doM
                                (query-name <- (return (list (model->name model) name "Query")))
                                (variables <- (gen-update-vars/M model (cdr (assq name method-lookup))))
                                (return (if (null? variables) (list) (list
                                          "\n      try {"
                                          "\n        const data = proxy.readQuery({ query: " query-name variables " });"
                                          (cond
                                            ((eq? (command->cmdtype model method mode) 'update)
                                             (list
                                               "\n        data." model-name name ".items[result.id] = { ...data." model-name name ".items[result.id], ...result };"))
                                            ((eq? (command->cmdtype model method mode) 'delete)
                                             (list
                                               "\n        data." model-name name ".items = data." model-name name ".items.filter((i: any) => i.id != result.id);"))
                                            ((eq? (command->cmdtype model method mode) 'create)
                                             (list
                                               (if (method-ascending? method)
                                                 (list "\n        data." model-name name ".items.unshift(result);")
                                                 (list "\n        data." model-name name ".items.push(result);")) 
                                               ))
                                            (else (error "unknown gen-update mode in apollobindings2 " mode)))
                                          "\n        proxy.writeQuery({ query: " query-name ", data" variables " });"
                                          "\n      } catch (err) {"
                                          (if storybook?
                                            (list "\n        console.log('cache not updated " model-name "." name " ', err);")
                                            (list "\n        if (process.env.NODE_ENV != 'test') console.log('cache not updated " model-name "." name " ', err);"))
                                          "\n      }"
                                          )))))
                            methodnames-to-update/array)))
                "\n    },")
          ))))

  (define (strip-typename params)
    (map (lambda (param) (list "\n        if (" (param->name param) " != undefined) delete (" (param->name param) " as any).__typename;"))
         (remove (compose native-type? base-type param->type) params)))

  (define (option-params params storybook?)
    (if (zero? (length params)) ""
    (list "{ " (intersperse (map param->name params) ", ") " }" (if storybook? ": any" ""))))

  (define (default-query ind model method storybook?)
    (let ((name (method->name method))
          (params (graphql-params model method))
          (lname (list (model->name model) (method->name method)))
          (cname (list (first-up (model->name model)) (method->name method))))
      ;(list "\nexport type " name "Result = " (paginated-type return-type-emit (method->return-type method)) ";")))
      (list
        "\nexport const use" cname " = (client: ApolloClient<object>, opts: " cname "Props):"
        "\n  QueryResult<" cname "Result, Record<string, any>> => {"
        "\n  const result = useQuery<" cname "Result>(" lname "Query, {"
        "\n    client,"
        "\n    variables: " (gen-variables params)
        "\n  });"
        "\n  return { ...result,"
        (if (not (array? (method->return-type method))) ""
          (list
            "\n    // @ts-ignore"
            "\n    fetchMore: async (props: {"
            "\n      variables: " cname "Props,"
            "\n      updateQuery?: (prev: " cname "Result, opts: { fetchMoreResult?: " cname "Result }) => void"
            "\n    }) => {"
            "\n      return result.fetchMore({"
            "\n        variables: props.variables,"
            "\n        updateQuery: ("
            "\n          prev: { " lname ": " cname "Result },"
            "\n          opts: { fetchMoreResult?: { " lname ": " cname "Result } }) => {"
            "\n"
            "\n          props.updateQuery && props.updateQuery(prev." lname ", { fetchMoreResult: opts.fetchMoreResult && opts.fetchMoreResult." lname " });"
            "\n"
            "\n          if (!opts.fetchMoreResult || !opts.fetchMoreResult." lname ") return prev;"
            "\n"
            "\n          return {"
            "\n            ...opts.fetchMoreResult,"
            "\n            " lname ": {"
            "\n              ...opts.fetchMoreResult." lname ","
            (if (method-ascending? method)
              (list "\n              items: [...opts.fetchMoreResult." lname ".items, ...prev." lname ".items]")
              (list "\n              items: [...prev." lname ".items, ...opts.fetchMoreResult." lname ".items]"))
            ;"\n              ...(props.updateQuery && props.updateQuery(prev." lname ", { fetchMoreResult: opts.fetchMoreResult && opts.fetchMoreResult." lname " }) || prev." cname ")"
            "\n            }"
            "\n          };"
            "\n        }"
            "\n      });"
            "\n    },"))
        "\n    // @ts-ignore"
        "\n    data: result.data && result.data." lname
        "\n  };"
        ;"\n  try {"
        ;"\n    const subscription = await client.watchQuery<{ " lname ": " cname "Result }>({"
        ;"\n      query: " lname "Query,"
        ;"\n      variables: " (gen-variables params)
        ;"\n    });"
        ;"\n    return {"
        ;"\n      subscription,"
        ;(if (not (array? (method->return-type method))) ""
        ;  (list
        ;    "\n      hasMore: () => !!(subscription.currentResult().data as { " lname ": " cname "Result })." lname ".cursor,"
        ;    "\n      fetchMore: (variables?: {}) => {"
        ;    "\n        const cursor = (subscription.currentResult().data as { " lname ": " cname "Result })." lname ".cursor;"
        ;    "\n        return subscription.fetchMore({"
        ;    "\n          query: subscription.options.query,"
        ;    "\n          variables: {"
        ;    "\n            ...subscription.variables,"
        ;    "\n            after: cursor,"
        ;    "\n            ...variables,"
        ;    "\n          },"
        ;    "\n          updateQuery: (prevResult, { fetchMoreResult }) => {"
        ;    "\n            try {"
        ;    "\n              const latestCursor = (subscription.currentResult().data as { " lname ": " cname "Result })." lname ".cursor;"
        ;    "\n              const prev = prevResult as { " lname ": " cname "Result };"
        ;    "\n              const next = fetchMoreResult as { " lname ": " cname "Result };"
        ;    "\n              const isNewPage = latestCursor == cursor;"
        ;    "\n              return {"
        ;    "\n                " lname ": {"
        ;    "\n                  ...prev." lname ","
        ;    "\n                  cursor: next." lname ".cursor,"
        ;    (if (method-ascending? method)
        ;      (list "\n                  items: isNewPage ? [...prev." lname ".items, ...next." lname ".items] : prev." lname ".items,")
        ;      (list "\n                  items: isNewPage ? [...next." lname ".items, ...prev." lname ".items] : prev." lname ".items,"))
        ;    "\n                },"
        ;    "\n              };"
        ;    "\n            } catch (err) {"
        ;    "\n              console.error(err);"
        ;    "\n              throw err;"
        ;    "\n            }"
        ;    "\n          }"
        ;    "\n        });"
        ;    "\n      },"))
        ;"\n    };"
        ;"\n  } catch (err) {"
        ;"\n    console.error(err);"
        ;"\n    throw err;"
        ;"\n  }"
        "\n}"
        )
      ))

  (define (default-mutate ind model method storybook?)
    (let ((name (method->name method))
          (params (graphql-params model method))
          (lname (list (model->name model) (method->name method)))
          (cname (list (first-up (model->name model)) (method->name method))))
      (list
          "\nexport const use" cname " = (client: ApolloClient<object>):"
          "\n  [(opts: " cname "Props) => Promise<ExecutionResult<{ " lname ": " cname "Result }>>,"
          "\n    { data: " cname "Result | undefined; error?: ApolloError | undefined; loading: boolean; called: boolean; client?: ApolloClient<object> | undefined; }] => {"
          "\n  const [fn, result] = useMutation<{ " lname ": " cname "Result }>(" lname "Query, {"
          "\n    client,"
          (gen-update (method->name method) model method storybook?)
          "\n  });"
          "\n  return ["
          "\n    (opts: " cname "Props) => fn({"
          "\n      variables: " (gen-variables params) ","
          "\n    }),"
          "\n    { ...result, data: result.data && result.data." lname " }"
          "\n  ];"
          "\n}"
          ;"\nexport const " lname " = async (client: ApolloClient<object>, opts: " cname "Props): Promise<{ data?: " cname "Result }> => {"
          ;"\n  try {"
          ;"\n    const result = await client.mutate<{ " lname ": " cname "Result }>({"
          ;"\n      mutation: " lname "Query,"
          ;"\n      variables: " (gen-variables params) ","
          ;(gen-update (method->name method) model method storybook?)
          ;"\n    });"
          ;"\n    return {"
          ;"\n      data: result.data == undefined ? undefined : result.data." lname ","
          ;"\n    };"
          ;"\n  } catch (err) {"
          ;"\n    console.error(err);"
          ;"\n    throw err;"
          ;"\n  }"
          ;"\n}"
        )
      ))

  (define (loaders-emit ind model method)
    (let ((name (method->name method))
          (params (graphql-params model method))
          (lname (list (model->name model) (method->name method)))
          (cname (list (first-up (model->name model)) (method->name method))))
      (list "\nexport class " cname "Loader extends Loader<{ " lname ": " cname "Result }>{ };")))

  (define (methoddisabled? m) (method-disabled? m 'apollobindings))

  (define (model-has-method? name model)
    (not (not (member name (map method->name (remove methoddisabled? (model->methods model)))))))

  (define (query-emit ind model method)
    (let ((name (method->name method))
          (params (graphql-params model method)))
      (list
        "\nexport const " (model->name model) name "Query = gql`"
        (gql-query-emit model method)
        "\n`;")))

  (define (mutation-emit ind model method)
    (let ((name (method->name method))
          (params (graphql-params model method)))
      (list
        "\nexport const " (model->name model) (method->name method) "Query = gql`"
        (gql-mutation-emit model method)
        "\n`;")))

  (define (props-type-emit ind model method)
    (let ((name (list (first-up (model->name model)) (method->name method)))
          (params (graphql-params model method)))
      (list 
        "\nexport type " name "Props = {"
        (map (lambda (param) (list "\n  " (param-emit param) ";")) params)
        "\n}")))

  (define (result-type-emit ind model method)
    (let ((name (list (first-up (model->name model)) (method->name method))))
      (list "\nexport type " name "Result = " (paginated-type return-type-emit (method->return-type method)) ";")))

  (define (mutation-props-type-emit ind model method)
    (let ((name (list (first-up (model->name model)) (method->name method)))
          (params (graphql-params model method)))
      (list 
        "\nexport type " name "Props = {"
        (map (lambda (param) (list "\n  " (param-emit param) ";")) params)
        "\n};")))

  (define (apollobindings-generate-index-model api ind model storybook?)
    (let ((methods (remove methoddisabled? (model->methods model)))
          (queries (remove methoddisabled? (filter get? (model->methods model))))
          (mutations (remove methoddisabled? (filter set? (model->methods model)))))
      (list
        (map (lambda (method) (result-type-emit ind model method)) methods)
        "\n"
        (map (lambda (method) (props-type-emit ind model method)) queries)
        "\n"
        (map (lambda (method) (mutation-props-type-emit ind model method)) mutations)
        "\n"
        (map (lambda (method) (query-emit ind model method)) queries)
        "\n"
        (map (lambda (method) (mutation-emit ind model method)) mutations)
        "\n"
        (map (lambda (method)
               (let* ((name (method->name method)))
                 (if (set? method)
                   (list "\n" (default-mutate ind model method storybook?))
                   (list "\n" (default-query ind model method storybook?)))))
             methods)
        "\n"
        ;(map (lambda (method) (loaders-emit ind model method)) queries)
        "\n"
        )))

  (define (apollobindings-generate-index-client api storybook?)
    (let ((expr
            (list
              "// this file has been automatically generated by apollobindings2, do not modify"
              "\n"
              "\nimport { ObservableQuery, ApolloQueryResult, ApolloClient, ApolloError } from 'apollo-client';"
              "\nimport gql from 'graphql-tag';"
              "\nimport * as M from './types';"
              "\nimport { useQuery, useMutation } from '@apollo/react-hooks';"
              "\nimport { QueryResult } from '@apollo/react-common';"
              "\nimport { ExecutionResult } from 'graphql';"
              "\nimport { Paginated, Point } from '@inf/cf-gen';"
              "\n"
              "\nexport type LoadResult<R> = {"
              "\n  subscription: ObservableQuery<R>,"
              "\n  hasMore?: () => boolean,"
              "\n  fetchMore?: (variables?: {}) => Promise<ApolloQueryResult<R>>,"
              "\n};"
              "\n"
              (map (lambda (model) (apollobindings-generate-index-model api 0 model storybook?))
                   (remove (lambda (m) (model-disabled? m 'apollobindings)) (api->models api)))
              )))
      (smoosh expr)))

  (define (apollobindings-generate-index api storybook?)
    (let ((expr
            (list
              "// this file has been automatically generated by apollobindings2, do not modify"
              "\n"
              "\nimport { ObservableQuery, ApolloQueryResult } from 'apollo-client';"
              "\nimport gql from 'graphql-tag';"
              "\nimport Loader from '../../loader';"
              "\nimport Root from '../../../root';"
              "\n"
              "\nexport type LoadResult<R> = {"
              "\n  subscription: ObservableQuery<R>,"
              "\n  hasMore?: () => boolean,"
              "\n  fetchMore?: (variables?: {}) => Promise<ApolloQueryResult<R>>,"
              "\n};"
              "\n"
              (map (lambda (model) (apollobindings-generate-index-model api 0 model storybook?))
                   (remove (lambda (m) (model-disabled? m 'apollobindings)) (api->models api)))
              )))
      (smoosh expr)))

  )
