(module services (services-generate)
  (import scheme chicken data-structures tools signatures matchable interfaces)
  (require-extension srfi-13 srfi-1)

  (define (arbitrate-emit ind model method)
    (let ((typedef (typedef-assq (base-type (method->return-type method)) (model->api model))))
      (if (not typedef) ""
        (let ((keys (map param->name (filter param-primary-key? (typedef->params typedef)))))
          (if (zero? (length keys)) ""
            (if (set? method)
              (list "\n" (indent (+ ind 1))
                    "if (ret != undefined) { await this._notifications.dispatch" (first-up (model->name model)) "(this._store, $ctx.sub, '" (first-down (method->name method)) "', ret); }")
              ""))))))

  ; method/output monad
  (define ((bind-map mfn l) state)
    (foldr (lambda (x sum)
             (let ((count (car sum))
                   (method (cadr sum)))
               ((mfn x count method) sum))) state l))
  (define ((return count method output) state)
    (let ((prev-output (caddr state)))
      (list count method (list prev-output output))))

  (define (default-field-emit name default)
    (let ((result (match default
                         ('$sequential "$i.toString()")
                         ('$now "$now")
                         ('$sub "$ctx.sub")
                         (#t "true")
                         (#f "false")
                         (x x))))
      (list name ": " result)))

  (define (initial-field-emit name type)
    (list name ": "
          (cond
            ((eq? type 'Int) 0)
            ((eq? type 'String) "''")
            ((optional? type) "undefined")
            ((array? type) "[]")
            ((eq? type 'DateTime) "new Date()")
            (else (error "unhandled initial field")))))

  (define (has-defaults? method typedef)
    (any fieldvalue-default? (method+typedef->fieldvalues method typedef)))

  (define (defaults-emit ind method typedef)
    (map (lambda (fv)
           (if (fieldvalue-default? fv)
             (list "\n" (indent (+ ind 2)) "  " (default-field-emit (fieldvalue->name fv) (fieldvalue->value fv)) ",")
             (list "\n" (indent (+ ind 2)) "  " (initial-field-emit (fieldvalue->name fv) (fieldvalue->value fv)) ", // ignored")))
         (method+typedef->fieldvalues method typedef)))

	(define (method-emit ind api model method) 
		(let ((name (first-down (method->name method)))
          (mode (method->mode method))
          (apionly? (method-apionly? method))
          (serviceonly? (or (model-serviceonly? model) (method-serviceonly? method)))
          (access (method->access method)))

			(list
        (if (not access) ""
          (list "\n" (indent ind) "@grant(["
                (intersperse
                  (map (lambda (a) (string-append "\"" (symbol->string a) "\"")) access) ", ") "])"))
				"\n" (indent ind) "async " (service-sig-emit model method) " {"
        (if serviceonly? 
          (list "\n" (indent (+ ind 1)) "return this._service." (first-down (method->name method)) "(" (intersperse (map param->name (service-signature-params model method)) ", ") ");")

          (if (set? method)
            (list
              "\n" (indent (+ ind 1)) "try {"
              "\n" (indent (+ ind 2)) "this._store.beginTransaction();"
              "\n"

              ; emit copied type fields"
              (let* ((ret ((bind-map
                             (lambda (param count method)
                               (let ((param-type (base-type (param->type param)))
                                     (param-name (param->name param)))
                                 (let ((typedef (typedef-assq param-type (model->api model))))
                                   (if (not typedef) (return count method "")
                                     (let ((copied-fields (filter param-copied-field? (typedef->params typedef))))
                                       (if (null? copied-fields)
                                         (if (has-defaults? method typedef)
                                           (let ((new-param-name (string->symbol (string-append (symbol->string param-name) (number->string count)))))
                                             (return count
                                                     (method-param-rename method param-name new-param-name)
                                                     (list
                                                       "\n" (indent (+ ind 2)) "const $now = new Date();"
                                                       "\n" (indent (+ ind 2)) "const " new-param-name " = " param-name ".map((c, $i) => ({"
                                                       "\n" (indent (+ ind 2)) "  ...c,"
                                                       (defaults-emit ind
                                                                      method
                                                                      typedef)
                                                       "\n" (indent (+ ind 2)) "}));"
                                                       "\n")))
                                           (return count method (list)))
                                         (bind-map
                                           (lambda (field count method)
                                             (let ((name (param->name field))
                                                   (type (param->type field))
                                                   (metadata (param->metadata field 'copied-field)))
                                               (let ((key (copied-field->key metadata))
                                                     (fkey (copied-field->fkey metadata))
                                                     (fmodel (copied-field->fmodel metadata))
                                                     (selector (copied-field->selector metadata))
                                                     (evaluated-type (param->type (eval-copied-field param-type api field)))
                                                     (new-param-name (string->symbol (string-append (symbol->string param-name) (number->string count)))))
                                                 (let ((use-context? (not (null? (filter (lambda (p) (and (param-readonly? p) (eq? (param->name p) key) (eq? key 'sub))) (typedef->params typedef))))))
                                                   (return
                                                     (+ count)
                                                     (method-param-rename method param-name new-param-name)
                                                     (if use-context?
                                                       (if (array? (param->type param))
                                                         (list
                                                           "\n" (indent (+ ind 2)) "const $now = new Date();"
                                                           "\n" (indent (+ ind 2)) "const " fmodel "Item = await this._store." (typename->modelname api type) "FindBy" (first-up key) "($ctx, $ctx." key ");"
                                                           "\n" (indent (+ ind 2)) "const " new-param-name " = " param-name ".map((c, $i) => ({"
                                                           "\n" (indent (+ ind 2)) "  ...c,"
                                                           (defaults-emit ind method typedef)
                                                           "\n" (indent (+ ind 2)) "  " fmodel ": " fmodel "Item && " fmodel "Item." selector ","
                                                           "\n" (indent (+ ind 2)) "}));"
                                                           "\n")
                                                         (list
                                                           "\n" (indent (+ ind 2)) "const $now = new Date();"
                                                           "\n" (indent (+ ind 2)) "const $i = 0;"
                                                           "\n" (indent (+ ind 2)) "const " fmodel "Item = await this._store." (typename->modelname api type) "FindBy" (first-up key) "($ctx, $ctx." key ");"
                                                           "\n" (indent (+ ind 2)) "const " new-param-name " = {"
                                                           "\n" (indent (+ ind 2)) "  ..." param-name ","
                                                           (defaults-emit ind method typedef)
                                                           "\n" (indent (+ ind 2)) "  " fmodel ": " fmodel "Item && " fmodel "Item." selector ","
                                                           "\n" (indent (+ ind 2)) "};"))
                                                       (if (array? (param->type param))
                                                         (list
                                                           "\n" (indent (+ ind 2)) "const " key "List = " param-name ".map(c => c." key ");"
                                                           "\n" (indent (+ ind 2)) "const " selector " = (await this._store." (typename->modelname api type) "FindBy" (first-up key) "In(" key "List, undefined, " param-name ".length)).reduce((sum, u) => ({...sum, [u." fkey "]: u." selector "}), {} as Dict<" (return-datatype-emit evaluated-type) ">);"
                                                           "\n" (indent (+ ind 2)) "const " new-param-name " = " param-name ".map(c => ({...c, " fmodel ": " selector "[c." key "]}));"
                                                           "\n")
                                                         (list
                                                           "\n" (indent (+ ind 2)) "const " new-param-name ": " param-type " = " param-name ";"
                                                           "\n" (indent (+ ind 2)) "const " param-name type " = await this._store." (typename->modelname api type) "FindBy" (first-up key) "(" param-name "." key ");"
                                                           "\n" (indent (+ ind 2)) "if (" param-name type " != undefined) {"
                                                           "\n" (indent (+ ind 2)) "  " new-param-name "." fmodel " = " param-name type "." selector ";"
                                                           "\n" (indent (+ ind 2)) "}"))))))))
                                           copied-fields)))))))
                             (method->params method))
                           (list 1 method (list))))
                     (method (cadr ret))
                     (output (caddr ret)))
                (list
                  output
                  (if apionly?
                    (list 
                      (list "\n" (indent (+ ind 2)) "const ret = await this._connectors." (model->name model) (method->name method) "_to_" (method->name method) "($ctx, await this._store." (model->name model) (method->name method) "(" (intersperse (store-signature-args model method) ", ") "));")
                      (arbitrate-emit (+ ind 1) model method)
                      (list "\n" (indent (+ ind 2)) "this._store.commitTransaction();")
                      (list "\n" (indent (+ ind 2)) "return ret;"))
                    (list 
                      (list "\n" (indent (+ ind 2)) "const ret = await this._store." (model->name model) (method->name method) "(" (intersperse (store-signature-args model method) ", ") ");")
                      (arbitrate-emit (+ ind 1) model method)
                      (list "\n" (indent (+ ind 2)) "this._store.commitTransaction();")
                      (list "\n" (indent (+ ind 2)) "return ret;")))
                  "\n" (indent (+ ind 1)) "} catch (ex) {"
                  "\n" (indent (+ ind 2)) "this._store.rollbackTransaction();"
                  "\n" (indent (+ ind 2)) "throw ex;"
                  "\n" (indent (+ ind 1)) "}")))
            (list
              (if apionly?
                (list 
                  (list "\n" (indent (+ ind 1)) "const ret = await this._store." (model->name model) (method->name method) "(" (intersperse (store-signature-args model method) ", ") ");")
                  (list "\n" (indent (+ ind 1)) "return this._connectors." (model->name model) (method->name method) "_to_" (method->name method) "($ctx, ret);"))
                (list 
                  (list "\n" (indent (+ ind 1)) "return this._store." (model->name model) (method->name method) "(" (intersperse (store-signature-args model method) ", ") ");"))))))
        "\n" (indent ind) "}")))

	(define (services-generate model-name api) 
		(let*
			((ind 1)
			 (expr 
				 (let* ((model (models-assq model-name api))
                (model-name (model->name model))
                (typedef (model->typedef model))
                (all-methods (remove (lambda (m) (method-disabled? m 'service-interfaces)) (model->methods model)))
                (service-methods (filter method-serviceonly? all-methods))
                (partial? (not (zero? (length service-methods)))))
					 (list
						 "// this file has been automatically generated by services, do not modify"
						 "\n"
             "\nimport { grant } from '../../../grant';"
             "\nimport Connectors from '../../connectors';"
             "\nimport { Point, Cursored, Cursorize } from '@inf/cf-gen';"
             "\nimport { IUserContext } from '../../../';"
             "\nimport { INotificationManager } from '../../..';"
             "\nimport { " (if partial? (list "I" (first-up model-name) "PartialService, ") "") "I" (first-up model-name) "Service } from '../../../types/serviceinterfaces';"
             "\nimport { IStore } from '../../../types/storeinterfaces';"
             ;"\nimport { " (typedef->name typedef) ", " (typedef->name typedef) "Input } from '../../../types/serviceinterfaces';"
             "\nimport * as M from '../../../types/serviceinterfaces';"
						 "\n"
						 "\nexport default class " (first-up model-name) "Service<C extends IUserContext> implements I" (first-up model-name) "Service<C> {"
             (cond
               ((model-serviceonly? model)
                (list "\n  _connectors: Connectors<C>;"
                      "\n  _notifications: INotificationManager<C>;"
                      "\n  _store: IStore<C>;"
                      "\n  private _service: I" (first-up model-name) "Service<C>;"
                      "\n  constructor(connectors: Connectors<C>, notifications: INotificationManager<C>, store: IStore<C>, service: I" (first-up model-name) "Service<C>) {"
                      "\n    this._connectors = connectors;"
                      "\n    this._notifications = notifications;"
                      "\n    this._store = store;"
                      "\n    this._service = service;"
                      "\n  }"))
               ((any method-serviceonly? (model->all-methods model))
                (list "\n  _connectors: Connectors<C>;"
                      "\n  _notifications: INotificationManager<C>;"
                      "\n  _store: IStore<C>;"
                      "\n  private _service: I" (first-up model-name) "PartialService<C>;"
                      "\n  constructor(connectors: Connectors<C>, notifications: INotificationManager<C>, store: IStore<C>, service: I" (first-up model-name) "PartialService<C>) {"
                      "\n    this._connectors = connectors;"
                      "\n    this._notifications = notifications;"
                      "\n    this._store = store;"
                      "\n    this._service = service;"
                      "\n  }"))
               (else
                 (list "\n  _connectors: Connectors<C>;"
                       "\n  _notifications: INotificationManager<C>;"
                       "\n  _store: IStore<C>;"
                       "\n  constructor(connectors: Connectors<C>, notifications: INotificationManager<C>, store: IStore<C>) {"
                       "\n    this._connectors = connectors;"
                       "\n    this._notifications = notifications;"
                       "\n    this._store = store;"
                       "\n  }")))
						 (map (lambda (method) (method-emit ind api model method)) (remove method-dbonly? (model->all-methods model)))
						 "\n}"))))
			(smoosh expr)))
)
