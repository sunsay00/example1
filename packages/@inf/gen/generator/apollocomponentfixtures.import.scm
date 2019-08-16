(module apollocomponentfixtures (apollocomponentfixtures-generate)
  (import scheme chicken data-structures tools signatures typedefs)
  (require-extension srfi-13 srfi-1)

  (define (methoddisabled? m) (method-disabled? m 'componentfixtures))

  (define (model-include-emit model)
    (let ((methods (remove methoddisabled? (model->methods model))))
      (if (zero? (length methods)) (list)
        (list
          "\n// @ts-ignore"
          "\nimport {"
          (intersperse
            (list (intersperse 
                    (map (lambda (method)
                           (list "with" (first-up (model->name model)) (method->name method)))
                         methods) ", ")
                  (intersperse
                    (map (lambda (method)
                           (list (first-up (model->name model)) (method->name method) "Result"))
                         methods) ", ")
                  (intersperse
                    (map (lambda (method)
                           (list (first-up (model->name model)) (method->name method) "PubProps"))
                         methods) ", ")
                  (intersperse
                    (map (lambda (method)
                           (list (first-up (model->name model)) (method->name method) "BaseProps"))
                         methods) ", "))
            ", ")
          "} from '../../components/hocs/models/" (lower (model->name model)) "';"))))

  (define (mutabletest-props-emit model method)
    (let ((name (list (first-up (model->name model)) (method->name method)))
          (params (graphql-params model method)))
      (list
        "\ntype Test" name "PubProps = {"
        (map (lambda (param) (list "\n  " (param-emit param) ";")) params)
        "\n  onResolve: (result: " name "Result) => void,"
        "\n  onReject: (err: Error) => void,"
        "\n};"
        "\ntype Test" name "Props = " name "BaseProps & Test" name "PubProps;")))

  (define (querytest-props-emit model method)
    (let ((name (list (first-up (model->name model)) (method->name method)))
          (params (graphql-params model method)))
      (list
        "\ntype Test" name "PubProps = {"
        (map (lambda (param) (list "\n  " (param-emit param) ";")) params)
        "\n  onResolve: (result: " name "BaseProps) => void,"
        "\n  onReject: (err: Error) => void,"
        "\n};"
        "\ntype Test" name "Props = " name "BaseProps & Test" name "PubProps;")))

  (define (mutabletest-component-emit model method)
    (let ((name (list (first-up (model->name model)) (method->name method)))
          (params (graphql-params model method)))
          (list
            "\nclass Test" name " extends React.Component<Test" name "Props, {}> {"
            "\n  async componentDidMount() {"
            "\n    try {"
            "\n      this.props.onResolve(await this.props.on" name "("
            (intersperse (map (lambda (param) (list "this.props." (param->name param))) params) ", ")
            "));"
            "\n    } catch (err) {"
            "\n      this.props.onReject(err);"
            "\n    }"
            "\n  }"
            "\n  render() { return null; }"
            "\n};"
            "\nconst Test" name "Wrapped = with" name"<Test" name "PubProps>(Test" name ");"
            "\n")))

  (define (querytest-component-emit model method)
    (let ((name (list (first-up (model->name model)) (method->name method)))
          (params (graphql-params model method)))
      (list 
        "\nclass Test" name " extends React.Component<Test" name "Props, {}> {"
        "\n  render() {"
        "\n    if (!this.props.data.loading) {"
        "\n      if (this.props.data.error) {"
        "\n        this.props.onReject(this.props.data.error);"
        "\n      } else {"
        "\n        this.props.onResolve(this.props);"
        "\n      }"
        "\n    }"
        "\n    return null;"
        "\n  }"
        "\n};"
        "\nconst Test" name "Wrapped = with" name "<Test" name "PubProps>(Test" name ");"
        "\n")))

  (define (mutable-component-wrapper-emit model method)
    (let* ((name (list (model->name model) (method->name method)))
           (cname (list (first-up (model->name model)) (method->name method)))
           (params (graphql-params model method)))
      (list 
        "\nexport const " name " = async (client: ApolloClient, " (intersperse (map param-emit params) ", ") ") => {"
        "\n  const { data: { " name ": result } } = await new Promise<" cname "Result>((resolve, reject) => {"
        "\n    ReactTestRenderer.create("
        "\n      <ApolloProvider client={client}>"
        "\n        <Test" cname "Wrapped "
        (intersperse (map (lambda (param) (list (param->name param) "={" (param->name param) "}")) params) " ")
        " onResolve={resolve} onReject={reject} />"
        "\n      </ApolloProvider>);"
        "\n  });"
        "\n  return result;"
        "\n};")))

  (define (query-component-wrapper-emit model method)
    (let* ((name (list (model->name model) (method->name method)))
           (cname (list (first-up (model->name model)) (method->name method)))
           (params (graphql-params model method)))
      (list 
        "\nexport const " name " = async (client: ApolloClient, " (intersperse (map param-emit params) ", ") ") => {"
        "\n  const { data: { " name ": result } } = await new Promise<" cname "BaseProps>((resolve, reject) => {"
        "\n    ReactTestRenderer.create("
        "\n      <ApolloProvider client={client}>"
        "\n        <Test" cname "Wrapped "
        (intersperse (map (lambda (param) (list (param->name param) "={" (param->name param) "}")) params) " ")
        " onResolve={resolve} onReject={reject} />"
        "\n      </ApolloProvider>);"
        "\n  });"
        "\n  if (result == undefined) throw new Error('failed to retreive " name "');"
        "\n  return result;"
        "\n};")))

  (define (model-emit model)
    (let ((queries (remove methoddisabled? (filter get? (model->methods model))))
          (mutations (remove methoddisabled? (filter set? (model->methods model)))))
      (list 
        (map (lambda (method) (querytest-props-emit model method)) queries)
        (map (lambda (method) (mutabletest-props-emit model method)) mutations)
        "\n"
        (map (lambda (method) (querytest-component-emit model method)) queries)
        (map (lambda (method) (mutabletest-component-emit model method)) mutations)
        "\n"
        (map (lambda (method) (query-component-wrapper-emit model method)) queries)
        (map (lambda (method) (mutable-component-wrapper-emit model method)) mutations)
        "\n")))

	(define (apollocomponentfixtures-generate api) 
    (let*
      ((ind 1)
       (models (remove (lambda (m) (model-disabled? m 'apollocomponentfixtures)) (api->models api)))
       (expr 
         (list
           "// this file has been automatically generated by apollocomponentfixtures, do not modify"
           "\n"
           "\nimport * as React from 'react';"
           "\nimport * as ReactTestRenderer from 'react-test-renderer';"
           "\nimport ApolloClient from 'apollo-client';"
           "\nimport { ApolloProvider } from 'react-apollo';"
           (map model-include-emit models)
           "\n"
           (map model-emit models))))
      (smoosh expr)))
  )
