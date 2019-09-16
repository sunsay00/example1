(module functional (mapM doM return >>= >> state-set state-get state-run print/M)
  (import scheme chicken data-structures)

  (define (test-applicative)
    ;pure :: a -> f a
    ;(<*>) :: f (a -> b) -> f a -> f b

    (define (some x) `(some ,x))
    (define (none) `(none))
    (define (some? x) (and (pair? x) (eq? (car x) 'some)))
    (define (none? x) (and (pair? x) (eq? (car x) 'none)))
    (define (some->val mb) (if (not (some? mb)) (error "invalid some") (cadr mb)))

    (define pure some)

    (define ((app f) mb)
      (some (if (some? mb)
              (f (some->val mb))
              (none))))

    (define-syntax <$>
      (syntax-rules (<$> <*>)
                    ((<$> p) (some p))
                    ((<$> p <*> f1 <*> f2) ((app f2) ((app f1) (some p))))
                    ((<$> p <*> f) ((app f) (some p)))))

    (print (<$> 5))
    (print (<$> 5 <*> (lambda (x) (* x x))))
    (print (<$> 5 <*> (lambda (x) (* x x)) <*> (lambda (x) (+ x x))))
    )
  ;(test-applicative)

  (define (mb-monads)
    (define (some x) `(some ,x))
    (define (none) `(none))
    (define (some? x) (and (pair? x) (eq? (car x) 'some)))
    (define (some->val mb) (if (not (some? mb)) (error "invalid some") (cadr mb)))
    ; monoid
    (define (mempty) (some '()))
    (define pure none)
    (define (mappend a b) (if (and (some? a) (some? b)) (some (append (some->val a) (some->val b))) (pure)))
    ; monad
    (define (>> m mf2) (if (some? m) (mf2) (none)))
    (define (>>= m mf2) (if (some? m) (mf2 (some->val m)) (none)))
    (define return some)

    (define-syntax doM
      (syntax-rules (doM if <- return let)
                    ((doM (if #t t f)) t)
                    ((doM (if #f t f)) f)
                    ((doM (if m t f)) (>>= m (lambda (p) (if p t f))))
                    ((doM (let b . body)) (let b . body))
                    ((doM (return x)) (return x))
                    ((doM (x <- m) . rest) (>>= m (lambda (x) (doM . rest))))
                    ((doM m . rest) (>> m (lambda () (doM . rest))))
                    ('_ (error "invalid doM syntax"))))
    (define (print/M . vs) (return (apply print vs)))
    (define (fmap f ml) (doM (l <- ml) (return (f l))))
    (define (mconcat l) (foldr (lambda (i sum) (mappend (fmap list i) sum)) (mempty) l))
    (define (mapM mf l) (mconcat (map mf l))) ; (a -> m b) -> f a -> m (f a)

    (doM
      (r <- (mapM (lambda (i) (return (+ i i))) (list 1 2 3)))
      (return r))
    )
  ;(print (mb-monads))

  (define (print/M . vs) (return (apply print vs)))

  ;(define (state-monad)
  (define (make-state name value) (list name value))
  (define state->name car)
  (define state->value cadr)
  (define ((set-name n) s) (make-state n (state->value s)))
  (define ((get-name) s) (make-state (state->name s) (state->name s)))
  (define ((set-value v) s) (make-state (state->name s) v))
  (define state-set set-name)
  (define state-get get-name)
  (define (state-run mf) (cadr (mf (pure))))
  ; monoid
  (define (mempty) (return '()))
  (define (pure) `(() ()))
  (define ((mappend ma mb) s)
    (let* ((sa (ma s))
           (sb (mb sa)))
      (make-state (state->name sb)
                  (append (state->value sa) (state->value sb)))))
  ; monad
  (define ((>>= m mf) s) (let ((s1 (m s))) ((mf (state->value s1)) s1)))
  (define ((>> m mf) s) (let ((s1 (m s))) ((mf) s1)))
  (define (return . r) (if (null? r) (set-value '()) (apply set-value r)))

  (define-syntax doM
    (syntax-rules (doM if <- return let)
                  ((doM (if p t f)) (if p t f))
                  ((doM (if/m m t f)) (>>= m (lambda (p) (if p t f))))
                  ((doM (let b . body)) (let b . body))
                  ((doM (return)) (return '()))
                  ((doM (return x)) (return x))
                  ((doM (x <- m) . rest) (>>= m (lambda (x) (doM . rest))))
                  ((doM m . rest) (>> m (lambda () (doM . rest))))
                  ('_ (error "invalid doM syntax"))))
  (define (fmap f ml) (doM (l <- ml) (return (f l))))
  (define (mconcat l) (foldr (lambda (i sum) (mappend (fmap list i) sum)) (mempty) l))
  (define (mapM mf l) (mconcat (map mf l))) ; (a -> m b) -> f a -> m (f a)

  ; (print (state-run
  ;          (doM
  ;            (x <- (mapM (lambda (i)
  ;                          (doM
  ;                            (state-set "hello-world")
  ;                            (return (* i i))) )
  ;                        (list 1 2 3)))
  ;            (val <- (state-get))
  ;            (return 1))))
  ;)
  ;(state-monad)
  )
