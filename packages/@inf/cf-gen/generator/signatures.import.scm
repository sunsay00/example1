(module signatures (
                    service-signature-params store-signature-params store-signature-args return-datatype-emit
                    param-emit return-type-emit service-sig-emit store-sig-emit service-signature-emit
                    convert-to-input-param store-call-emit serviceapi-sig-emit)
  (import scheme chicken data-structures tools)
  (require-extension srfi-13 srfi-1)

  (define (method->implicit-params model method)
    (let ((implicit-paramnames (if (not (model-typedef? model)) #f (method->config-assq 'implicit method))))
      (if (not implicit-paramnames) (list)
        (let ((typedef (model->typedef model)))
          (map (lambda (name)
                 (let ((result (typedef->params-assq typedef name)))
                   (if (not result) (error "implicit parameter" name "not found in type" (model->type model)))
                   (cadr result))) implicit-paramnames)))))

  (define (signature-params model method)
    (let ((params (append (method->implicit-params model method)
                          (method->params method))))
      (if (get? method)
        (cons (make-param '__fields '(Array String)) params)
        params)))

  (define (serviceapi-signature-params model method)
    (let ((params (method->params method)))
      (map (lambda (param) (convert-to-input-param (model->api model) param)) params)))

  (define (service-signature-params model method)
    (cons (make-param '$ctx 'IUserContext)
          (let ((params (method->params method)))
            (if (get? method)
              (cons (make-param '__fields '(Array String)) params)
              (map (lambda (param) (convert-to-input-param (model->api model) param)) params)))))

  (define (store-signature-args model method)
    ;(append (map (lambda (param) (list "$ctx." (param->name param))) (method->implicit-params model method))
            ;(map param->name (method->params method))))
            (cons "$ctx" (map param->name (method->params method))))

  (define (store-signature-params model method)
      (append (method->implicit-params model method) (method->params method)))

  (define (serviceapi-signature-emit model method)
    (let ((params (serviceapi-signature-params model method)))
      (list "(" (intersperse (map param-emit params) ", ") "): "
            (let ((ret-type (method->return-type method)))
              (list "Promise<" (cursorize return-type-emit ret-type) ">")))))

  (define (service-signature-emit model method)
    (let ((params (service-signature-params model method)))
      (list "(" (intersperse (map param-emit params) ", ") "): "
            (let ((ret-type (method->return-type method)))
              (list "Promise<" (cursorize return-type-emit ret-type) ">")))))

  (define (store-call-emit model method)
    (list (model->name model)
          (method->name method)
          (let ((params (store-signature-params model method)))
            (list "(" (intersperse
                        (cons "$ctx" (map param->name params)) ", ") ")"))))

  (define (store-signature-emit model method)
    (let ((params (store-signature-params model method)))
      (list "(" (intersperse (cons "$ctx: IUserContext" (map param-emit params)) ", ") "): "
            (if (method-dbonly? method)
              (list "Promise<" (cursorize return-datatype-emit (method->return-type method)) ">")
              (list "Promise<" (cursorize return-type-emit (method->return-type method)) ">")))))

  (define (store-sig-emit model method) 
     (list (model->name model)
           (method->name method)
           (store-signature-emit model method)))

  (define (service-sig-emit model method) 
     (list (first-down (method->name method))
           (service-signature-emit model method)))

  (define (serviceapi-sig-emit model method) 
     (list (first-down (method->name method)) " = async " (serviceapi-signature-emit model method)))

  (define (type->jstype type)
    (cond
      ((eq? type 'String) 'string)
      ((eq? type 'Int) 'number)
      ((eq? type 'Float) 'number)
      ((eq? type 'DateTime) 'Date)
      ((eq? type 'Boolean) 'boolean)
      (else type)))

  (define (return-datatype-emit type) 
    (cond
      ((optional? type) (list (return-datatype-emit (optional->type type)) " | undefined"))
      ((array? type)
       (let ((child-type (array->type type)))
         (if (native-type? child-type)
           (list "Cursored<" (return-datatype-emit child-type) ">[]")
           (list (return-datatype-emit child-type) "[]"))))
      ((plus-type? type) (intersperse (map type->jstype (plus-type->types type)) " & "))
      ((native-type? type) (list (type->jstype type)))
      (else (list (type->jstype type) "Data"))))

  (define (convert-to-input-param api param)
    (define (recur type)
      (cond
        ((array? type) (make-array (recur (array->type type))))
        ((optional? type) (make-optional (recur (optional->type type))))
        (else
          (let ((typedef (typedef-assq type api)))
            (if (not typedef) type
              (string->symbol (string-append (symbol->string type) "Input")))))))
    (make-param (param->name param)
                (recur (param->type param))
                (param->specs param)))

  (define (return-type-emit type) 
    (cond
      ((optional? type) (list (return-type-emit (optional->type type)) " | undefined"))
      ((array? type)
       (let ((child-type (array->type type)))
         (if (native-type? child-type)
           (list "Cursored<" (return-type-emit child-type) ">[]")
           (list (return-type-emit child-type) "[]"))))
      ((plus-type? type) (intersperse (map type->jstype (plus-type->types type)) " & "))
      (else (type->jstype type))))

  (define (param-emit param)
    (let ((name (param->name param))
          (type (param->type param)))
      (cond
        ((optional? type) (list name "?: " (return-type-emit (optional->type type))))
        ((array? type) (list name ": " (return-type-emit (array->type type)) "[]"))
        (else (list name ": " (type->jstype type))))))
  )
