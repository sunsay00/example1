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

	(define (dependencies fn delimit api)
    (intersperse 
      (map (lambda (model)
             (fn (model->name model)
                 (or (model-serviceonly? model)
                     (any method-serviceonly? (model->all-methods model)))))
           (api->models api)) delimit))

  (define (mappers-generate prev-api delta-api vnum) 
    (let*
      ((ind 1)
       (prev-vnum (- vnum 1))
       (expr 
         (list
           "// this file has been automatically generated, do not modify\n"
           (if (null? prev-api) ""
             (list "\nimport Mapper_v" prev-vnum " from './mapper_v" prev-vnum "';\n"))
           "\nexport default class Mapper_v" vnum (if (null? prev-api) "" (list " extends Mapper_v" prev-vnum)) " {"
					 (dependencies (lambda (p s?) (list "\n" (indent ind) "_" p ": I" (first-up p) "Service;")) "" delta-api)
           "\n  constructor("
					 (dependencies (lambda (p s?) (list "\n" (indent (+ 1 ind)) p ": I" (first-up p) "Service")) ", " prev-api)
           (if (null? prev-api) "" ",")
					 (dependencies (lambda (p s?) (list "\n" (indent (+ 1 ind)) p ": I" (first-up p) "Service")) ", " delta-api)
           "\n  ) {"
           (if (null? prev-api) ""
             (list "\n    super(" (dependencies (lambda (p s?) p) ", " prev-api) ");"))
					 (dependencies (lambda (p s?) (list "\n" (indent (+ 1 ind)) "this._" p " = " p ";")) "" delta-api)
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
                   "// this file has been automatically generated, do not modify\n"
                   (dependencies (lambda (p serviceonly?)
                                   (list 
                                     "\nimport " (first-up p) "BasicService from '../services/basic/" (lower p) "service';"
                                     (if serviceonly?
                                       (list "\nimport " (first-up p) "Service from '../services/" (lower p) "service';")
                                       ""))) "" api)
                   "\n"
                   "\nimport Connectors from '../connectors';"
                   "\nimport NotificationManager from '../../tools/notificationmanager';"
                   "\n"
                   "\nimport Mapper from './mapper_v" (inexact->exact (floor vnum)) "';"
                   "\n"
                   "\nexport const createDefaultMapper = (notifications: NotificationManager, store: IStore) => {"
                   "\n  const connectors = new Connectors(store);"
                   "\n  return new Mapper("
                   (dependencies (lambda (p needsservice?)
                                   (if needsservice?
                                     (list "\n    new " (first-up p) "BasicService(connectors, notifications, store, new " (first-up p) "Service(notifications, store))")
                                     (list "\n    new " (first-up p) "BasicService(connectors, notifications, store)"))) "," api)
                   "\n  );"
                   "\n}"
                   "\nexport default Mapper;")))
      (smoosh expr)))

)

