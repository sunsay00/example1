(module resolvers (resolvers-generate)
  (import scheme chicken data-structures tools signatures)
  (require-extension srfi-13 srfi-1)

	(define (scalar-emit ind scalar) 
    (let ((name (scalar->name scalar)))
      (list (indent ind) name ": GraphQL" name ",\n")))

	(define (string-transform-first fn str)
		(if (null? str) ""
			(let ((l (string->list str)))
				(list->string (cons (fn (car l)) (cdr l))))))

  (define (param->null-checked-str param)
    (let ((type (param->type param)))
      (if (optional? type)
        (let ((param-str (symbol->string (param->name param))))
          (string-append param-str " == null ? undefined : " param-str))
        (param->name param))))

	(define (method-emit ind model method mut?)
    (let* ((params (service-signature-params model method))
           (param-names (map param->name params))
           (name (string->symbol (string-transform-first char-downcase (symbol->string (method->name method)))))
           (rettype (method->return-type method)))
			(list "\n" (indent ind) (model->name model)
						(method->name method) ": async (obj: any, {" 
						(intersperse (remove (lambda (name) (member name `(__fields $ctx))) param-names) ", ") "}: any, $ctx: IUserContext, info: any) => {"
            "\n" (indent (+ ind 1)) "try {"
            (if mut? ""
              (list "\n" (indent (+ ind 1)) "  const __fields = getFieldNames(info);"))
            (let ((null-checked-param-strs (map param->null-checked-str params)))
              (if (array? rettype)
                (let ((child-type (array->type rettype)))
                  (list 
                    "\n" (indent (+ ind 1)) "  const result = await mapper." (model->name model) (first-up name) "_v" (method->version method) "(" (intersperse null-checked-param-strs ", ") ");"
                    "\n" (indent (+ ind 1)) "  if (result.length == 0) return {cursor: undefined, items: []};"
                    (if (native-type? child-type)
                      (list "\n" (indent (+ ind 1)) "  return {cursor: result[result.length-1].cursor, items: result.map(r => r.data)};")
                      (list "\n" (indent (+ ind 1)) "  return {cursor: result[result.length-1].cursor, items: result};"))))
                (list "\n" (indent (+ ind 1)) "  return await mapper." (model->name model) (first-up name) "_v" (method->version method) "(" (intersperse null-checked-param-strs ", ") ");")))
            "\n" (indent (+ ind 1)) "} catch (err) {"
            "\n" (indent (+ ind 1)) "  if (config('CFG::Stage') != 'test') console.error(err);"
            "\n" (indent (+ ind 1)) "  throw err;"
            "\n" (indent (+ ind 1)) "}"
            "\n" (indent ind) "},")))

	(define (dependencies ind api)
    (intersperse 
      (map (lambda (p) (list "\n" (indent ind) p ": I" (first-up p) "Service")) 
           (map model->name (api->models api))) ", "))

  (define (resolvers-generate api) 
    (let*
      ((ind 1)
       (expr 
         (list
           "// this file has been automatically generated by resolvers, do not modify"
					 "\n"
           "\n// @ts-ignore"
					 "\nimport { grant } from 'tools/decorators';"
					 "\nimport { GraphQLDateTime, GraphQLEmail, GraphQLPoint } from '../../customtypes';"
					 "\nconst getFieldNames = require('graphql-list-fields');"
           "\nimport Mapper from '../../mapper';"
					 "\nimport TestingService from '../../services/testingservice';"
           "\nimport config from '../../config';"
					 "\n"
					 "\nexport default (mapper: Mapper, testing: TestingService) => ({\n"
           (map (lambda (scalar) (scalar-emit ind scalar)) (api->scalars api))
					 "\n  Queries: {"
					 "\n    // test"
					 "\n    testEmailType: async (obj: any, { email }: any, $ctx: IUserContext, info: any) => { return { email }; },"
					 "\n    testDateTimeType: async (obj: any, { dateTime }: any, $ctx: IUserContext, info: any) => { return { dateTime }; },"
					 "\n    testUnauthorized: async (obj: any, { arg }: any, $ctx: IUserContext, info: any) => { return testing.publicUpperCase($ctx, arg); },"
					 "\n    testAdminAuthorized: async (obj: any, { arg }: any, $ctx: IUserContext, info: any) => { return testing.privateUpperCase($ctx, arg); },"
					 "\n    testAuthorized: (obj: any, { arg }: any, $ctx: IUserContext, info: any) => { if (arg !== 'pong') { throw new Error('invalid test argument'); } return testing.protectedUpperCase($ctx, arg); },"
					 "\n"
           (map-queries-ex 'resolvers (lambda (method model) (method-emit (+ 1 ind) model method #f)) api)
					 "\n  },"
					 "\n"
					 "\n  Mutations: {"
           (map-mutations-ex 'resolvers (lambda (method model) (method-emit (+ 1 ind) model method #t)) api)
					 "\n  }"
					 "\n});")))
			(smoosh expr)))
)