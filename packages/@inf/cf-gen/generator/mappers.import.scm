(module mappers (mappers-generate mappers-generate-index)
  (import scheme chicken data-structures tools signatures)
  (require-extension srfi-13 srfi-1)

  (define (method-emit ind model method) 
    (let ((name (first-down (method->name method)))
          (mode (method->mode method)))
      (list "\n" (indent ind) (model->name model) (method->name method) "_v" (inexact->exact (floor (method->version method))) " = async " (service-signature-emit model method) " => {"
            "\n" (indent (+ ind 1)) 
            (list "return this._" (model->name model) "." (first-down (method->name method)) "("
                  (intersperse (map param->name (service-signature-params model method)) ", ")
                  ");")
            "\n" (indent ind) "}")))

  (define (partial? all-methods)
    (let ((service-methods (filter method-serviceonly? all-methods)))
      (not (zero? (length service-methods)))))

  (define (dependencies fn delimit api)
    (intersperse 
      (map (lambda (model)
             (let ((all-methods (model->all-methods model)))
               (fn (model->name model)
                   (or (model-serviceonly? model)
                       (any method-serviceonly? all-methods))
                   (partial? all-methods))))
           (api->models api)) delimit))

  (define (mappers-generate prev-api delta-api vnum) 
    (let*
      ((ind 1)
       (prev-vnum (- vnum 1))
       (expr 
         (list
           "// this file has been automatically generated, do not modify"
           "\n"
           "\nimport { IUserContext, Point, Cursorize, Cursored } from '../../../../../types';"
           "\nimport * as M from '../../types/serviceinterfaces';"
           "\n"
           "\nexport default class Mapper_v" vnum "<C extends IUserContext>" (if (null? prev-api) "" (list " extends Mapper_v<C>" prev-vnum)) " {"
           (dependencies (lambda (p s? p?) (list "\n" (indent ind) "_" p ": M.I" (first-up p) "Service<C>;")) "" delta-api)
           "\n  constructor(params: {"
           (dependencies (lambda (p s? p?) (list "\n" (indent (+ 1 ind)) p ": M.I" (first-up p) "Service<C>")) ", " prev-api)
           (if (null? prev-api) "" ",")
           (dependencies (lambda (p s? p?) (list "\n" (indent (+ 1 ind)) p ": M.I" (first-up p) "Service<C>")) ", " delta-api)
           "\n  }) {"
           (if (null? prev-api) ""
             (list "\n    super(" (dependencies (lambda (p s? p?) p) ", " prev-api) ");"))
           (dependencies (lambda (p s? p?) (list "\n" (indent (+ 1 ind)) "this._" p " = params." p ";")) "" delta-api)
           "\n  };"
           "\n"
           (map-queries-ex 'mappers (lambda (method model) (method-emit ind model method)) delta-api)
           "\n"
           (map-mutations-ex 'mappers (lambda (method model) (method-emit ind model method)) delta-api)
           "\n};")))
      (smoosh expr)))

  (define (mappers-generate-index api vnum)
    (let* ((ind 1)
           (expr (list
                   "// this file has been automatically generated, do not modify"
                   "\n"
                   "\nimport { IStore } from '../../types/storeinterfaces';"
                   "\nimport * as Services from '../../types/serviceinterfaces';"
                   "\nimport { IUserContext, INotificationManager } from '../../../../../types';"
                   (dependencies (lambda (p serviceonly? partial?)
                                   (list "\nimport " (first-up p) "BasicService from '../services/basic/" (lower p) "service';")) "" api)
                   "\n"
                   "\nimport Connectors from '../connectors';"
                   "\n"
                   "\nimport Mapper from './mapper_v" (inexact->exact (floor vnum)) "';"
                   "\n"
                   "\nexport type MappedServices<C extends IUserContext> = {"
                   (dependencies (lambda (p serviceonly? partial?)
                                   (if serviceonly?
                                     (list "\n  " p ": (store: IStore<C>) => Services.I" (first-up p) (if partial? "Partial" "") "Service<C>,") "")) "" api)
                   "\n}"
                   "\n"
                   "\nexport const createServiceMapper = <C extends IUserContext>(notifications: INotificationManager<C>, store: IStore<C>, services: MappedServices<C>) => {"
                   "\n  const connectors = new Connectors<C>(store);"
                   "\n  return new Mapper({"
                   (dependencies (lambda (p needsservice? partial?)
                                   (if needsservice?
                                     (list "\n    " p ": new " (first-up p) "BasicService(connectors, notifications, store, services." p "(store))")
                                     (list "\n    " p ": new " (first-up p) "BasicService(connectors, notifications, store)"))) "," api)
                   "\n  });"
                   "\n}"
                   "\nexport default Mapper;")))
      (smoosh expr)))

  )

