#lang plai-typed

;; ----------------------------------------
;; #10 (moved from typed-class)
(define-type Type
  [numT]
  [objT (class-name : symbol)]
  [arrayT (t : Type)]

  ;; #7
  [nullT])

(define-type ExprC
  [numC (n : number)]
  [plusC (lhs : ExprC)
         (rhs : ExprC)]
  [multC (lhs : ExprC)
         (rhs : ExprC)]
  [argC]
  [thisC]
  [newC (class-name : symbol)
        (args : (listof ExprC))]
  [getC (obj-expr : ExprC)
        (field-name : symbol)]
  [sendC (obj-expr : ExprC)
         (method-name : symbol)
         (arg-expr : ExprC)]
  [ssendC (obj-expr : ExprC)
          (class-name : symbol)
          (method-name : symbol)
          (arg-expr : ExprC)]
  
  ;; #2
  [instanceofC (obj : ExprC)
               (class-name : symbol)]

  ;; #3
  [if0C (t : ExprC)
        (thn : ExprC)
        (els : ExprC)]

  ;; #5
  [castC (class-name : symbol)
         (obj : ExprC)]

  ;; #6
  [setC (obj-expr : ExprC)
        (field-name : symbol)
        (arg : ExprC)]

  ;; #7
  [nullC]

  ;; #9
  [newarrayC (t : Type)
             (len : ExprC)
             (init : ExprC)]
  [arrayrefC (arr : ExprC)
             (ind : ExprC)]
  [arraysetC (arr : ExprC)
            (ind : ExprC)
            (add : ExprC)])

(define-type ClassC
  [classC (name : symbol)
          ;; #2
          (syper-name : symbol) 
          (field-names : (listof symbol))
          (methods : (listof MethodC))])


(define-type MethodC
  [methodC (name : symbol)
           (body-expr : ExprC)])

(define-type Value
  [numV (n : number)]

  ;; #6
  [objV (class-name : symbol)
        (field-values : (listof (boxof Value)))]

  ;; #7
  [nullV]
  
  [arrayV (t : Type)
          (len : number)
          (arr : (listof (boxof Value)))])

(module+ test
  (print-only-errors true))

;; ----------------------------------------

(define (make-find [name-of : ('a -> symbol)])
  (lambda ([name : symbol] [vals : (listof 'a)]) : 'a
    (cond
     [(empty? vals)
      (error 'find "not found")]
     [else (if (equal? name (name-of (first vals)))
               (first vals)
               ((make-find name-of) name (rest vals)))])))

(define find-class : (symbol (listof ClassC) -> ClassC)
  (make-find classC-name))

(define find-method : (symbol (listof MethodC) -> MethodC)
  (make-find methodC-name))

;; A non-list pair:
(define-type (Pair 'a 'b)
  [kons (first : 'a) (rest : 'b)])

;; #6
(define (get-field [name : symbol] 
                   [field-names : (listof symbol)] 
                   [vals : (listof (boxof Value))])
  ;; Pair fields and values, find by field name,
  ;; then extract value from pair
  (unbox (kons-rest ((make-find kons-first)
              name
              (map2 kons field-names vals)))))

;; #6
(define (set-field! [name : symbol] 
                    [field-names : (listof symbol)] 
                    [vals : (listof (boxof Value))]
                    [val : Value])
  (set-box! (kons-rest ((make-find kons-first)
                        name
                        (map2 kons field-names vals)))
            val))

(module+ test
  (test/exn (find-class 'a empty)
            "not found")
  (test (find-class 'a (list (classC 'a 'object empty empty)))
        (classC 'a 'object empty empty))
  (test (find-class 'b (list (classC 'a 'object empty empty)
                             (classC 'b 'object empty empty)))
        (classC 'b 'object empty empty))
  (test (get-field 'a 
                   (list 'a 'b)
                   (list (box (numV 0)) (box (numV 1))))
        (numV 0))
  (test (local [(define l (list (box (numV 2)) (box (numV 2))))]
          (begin
            (set-field! 'a
                        (list 'a 'b)
                        l
                        (numV 3))
             (get-field 'a 
                   (list 'a 'b)
                   l)))
        (numV 3)))

;; ----------------------------------------
;; #2 #10 
(define (subclass? obj-class-name class-name classes)
  (cond
    [(equal? obj-class-name class-name) #t]
    [(equal? obj-class-name 'object) #f]
    [else
     (type-case ClassC (find-class obj-class-name classes)
       [classC (name super-name field-names methods)
               (subclass? super-name class-name classes)])]))

(module+ test
  (test (subclass? 'object 'object empty)
        #t)
  (test (subclass? 'a 'object (list (classC 'a 'object empty empty)))
        #t)
  (test (subclass? 'object 'a (list (classC 'a 'object empty empty)))
        #f)
  (test (subclass? 'b 'a (list (classC 'a 'object empty empty)
                               (classC 'b 'a empty empty)))
        #t)
  (test (subclass? 'b 'object (list (classC 'a 'object empty empty)
                                    (classC 'b 'a empty empty)))
        #t))

;; list set -------------------------------
(define list-set! : ((listof (boxof Value)) number Value -> void)
  (lambda (l ind elem)
    (cond
      [(empty? l) (void)]
      [(= ind 0) (set-box! (first l) elem)]
      [else (list-set! (rest l) (- 1 ind) elem)])))

(module+ test
  (test (list (box (numV 2)) (box (numV 2)))
        (list (box (numV 2)) (box (numV 2))))
  (test (local [(define l (list (box (numV 2)) (box (numV 2))))]
          (begin
            (list-set! l 0 (numV 3))
            l))
        (list (box (numV 3)) (box (numV 2))))
  (test (local [(define l (list (box (numV 2)) (box (numV 2))))]
          (begin
            (list-set! l 3 (numV 3))
            l))
        (list (box (numV 2)) (box (numV 2)))))
          

;; ----------------------------------------

(define interp : (ExprC (listof ClassC) Value Value -> Value)
  (lambda (a classes this-val arg-val)
    (local [(define (recur expr)
              (interp expr classes this-val arg-val))]
      (type-case ExprC a
        [numC (n) (numV n)]
        [plusC (l r) (num+ (recur l) (recur r))]
        [multC (l r) (num* (recur l) (recur r))]
        [thisC () this-val]
        [argC () arg-val]
        [newC (class-name field-exprs)
              (local [(define c (find-class class-name classes))
                      (define vals (map box (map recur field-exprs)))]
                (if (= (length vals) (length (classC-field-names c)))
                    (objV class-name vals)
                    (error 'interp "wrong field count")))]
        [getC (obj-expr field-name)
              (type-case Value (recur obj-expr)
                [objV (class-name field-vals)
                      (type-case ClassC (find-class class-name classes)
                        [classC (name class-name field-names methods)
                                (get-field field-name field-names 
                                           field-vals)])]
                ;; #7
                [nullV ()
                       (error 'interp "not an object")]
                [else (error 'interp "not an object")])]
        
        ;; #6
        [setC (obj-expr field-name expr)
              (type-case Value (recur obj-expr)
                [objV (obj-class-name field-vals)
                      (type-case ClassC (find-class obj-class-name classes)
                        [classC (name class-name field-names methods)
                                (begin
                                  (set-field! field-name field-names field-vals (recur expr))
                                  (numV 0))])]
                ;; #7
                [nullV ()
                       (error 'interp "not an object")]
                [else (error 'interp "not an object")])]
        [sendC (obj-expr method-name arg-expr)
               (local [(define obj (recur obj-expr))
                       (define arg-val (recur arg-expr))]
                 (type-case Value obj
                   [objV (class-name field-vals)
                         (call-method class-name method-name classes
                                      obj arg-val)]
                   ;; #7
                   [nullV ()
                          (error 'interp "not an object")]
                   [else (error 'interp "not an object")]))]
        [ssendC (obj-expr class-name method-name arg-expr)
                (local [(define obj (recur obj-expr))
                        (define arg-val (recur arg-expr))]
                  (call-method class-name method-name classes
                               obj arg-val))]
        
        ;; #2
        [instanceofC (obj-expr class-name)
                     (type-case Value (recur obj-expr)
                       [objV (obj-class-name field-vals)
                             (if (subclass? obj-class-name class-name classes)
                                 (numV 0)
                                 (begin
                                   (find-class class-name classes)
                                   (numV 1)))]
                       ;; #7
                       [nullV ()
                              ;; not sure on this, seems like null is never instanceof a class
                              ;; but shouldn't throw error
                              (numV 1)]
                       [else (error 'interp "not an object")])]
        
        ;; #3 
        [if0C (t thn els)
              (if (= (numC-n t) 0)
                  (recur thn)
                  (recur els))]

        ;; #5
        [castC (cast-class-name obj-expr)
               (local [(define obj-val (recur obj-expr))]
                 (type-case Value obj-val
                   [objV (obj-class-name field-vals)
                         (if (subclass? obj-class-name cast-class-name classes)
                             obj-val
                             (error 'interp "not a subclass"))]
                   ;; #7
                   [nullV ()
                          obj-val]
                   [else (error 'interp "not an object")]))]

        ;; #7
        [nullC ()
               (nullV)]

        ;; #9
        [newarrayC (t len-expr arr-expr)
                   (local [(define len (recur len-expr))
                           (define arr (recur arr-expr))]
                     (arrayV t (numV-n len) (build-list (numV-n len) (λ (x) (box arr)))))]
        [arrayrefC (arr-expr ind-expr)
                   (local [(define arr (recur arr-expr))
                           (define ind (recur ind-expr))]
                     (if (< (numV-n ind) (arrayV-len arr))
                         (unbox (list-ref (arrayV-arr arr) (numV-n ind)))
                         (error 'interp "index out of bounds")))]
        [arraysetC (arr-expr ind-expr elem-expr)
                   (local [(define arr (recur arr-expr))
                           (define ind (recur ind-expr))
                           (define elem (recur elem-expr))]
                     ;; #10
                     (type-case Type (arrayV-t arr)
                       [objT (arr-class-name)
                             (type-case Value elem
                               [objV (elem-class-name elem-field-vals)         
                                     (if (subclass? elem-class-name arr-class-name classes)
                                         (if (< (numV-n ind) (arrayV-len arr))
                                             (begin (list-set! (arrayV-arr arr) (numV-n ind) elem)
                                                    (numV 0))
                                             (error 'interp "index out of bounds"))
                                         (error 'interp "not a subclass"))]
                               ;; #7
                               [nullV ()
                                      (if (< (numV-n ind) (arrayV-len arr))
                                             (begin (list-set! (arrayV-arr arr) (numV-n ind) elem)
                                                    (numV 0))
                                             (error 'interp "index out of bounds"))]
                               [else (error 'interp "not an object")])]
                       [else
                        (if (< (numV-n ind) (arrayV-len arr))
                                             (begin (list-set! (arrayV-arr arr) (numV-n ind) elem)
                                                    (numV 0))
                                             (error 'interp "index out of bounds"))]))]))))

(define (call-method class-name method-name classes
                     obj arg-val)
  (type-case ClassC (find-class class-name classes)
    [classC (name class-name field-names methods)
            (type-case MethodC (find-method method-name methods)
              [methodC (name body-expr)
                       (interp body-expr
                               classes
                               obj
                               arg-val)])]))

(define (num-op [op : (number number -> number)]
                [op-name : symbol] 
                [x : Value]
                [y : Value]) : Value
  (cond
    [(and (numV? x) (numV? y))
     (numV (op (numV-n x) (numV-n y)))]
    [else (error 'interp "not a number")]))

(define (num+ x y) (num-op + '+ x y))
(define (num* x y) (num-op * '* x y))

;; ----------------------------------------
;; Examples

(module+ test
  (define posn-class
    (classC 
     'posn
     'object
     (list 'x 'y)
     (list (methodC 'mdist
                    (plusC (getC (thisC) 'x) (getC (thisC) 'y)))
           (methodC 'addDist
                    (plusC (sendC (thisC) 'mdist (numC 0))
                           (sendC (argC) 'mdist (numC 0))))
           (methodC 'addX
                    (plusC (getC (thisC) 'x) (argC)))
           (methodC 'multY (multC (argC) (getC (thisC) 'y)))
           (methodC 'factory12 (newC 'posn (list (numC 1) (numC 2)))))))

  (define test-class
    (classC 
     'test
     'object
     (list 'x)
     (list (methodC 'set
                    (arraysetC (getC (thisC) 'x) (argC) (numC 0)))
           (methodC 'ref
                    (arrayrefC (getC (thisC) 'x) (argC))))))

  (define posn3D-class
    (classC 
     'posn3D
     'posn
     (list 'x 'y 'z)
     (list (methodC 'mdist (plusC (getC (thisC) 'z)
                                  (ssendC (thisC) 'posn 'mdist (argC))))
           (methodC 'addDist (ssendC (thisC) 'posn 'addDist (argC))))))

  (define posn27 (newC 'posn (list (numC 2) (numC 7))))
  (define posn531 (newC 'posn3D (list (numC 5) (numC 3) (numC 1))))
  (define array-posn27 (newC 'test (list (newarrayC (objT 'posn) (numC 1) posn27))))

  (define (interp-posn a)
    (interp a (list posn-class posn3D-class) (numV -1) (numV -1))))

;; ----------------------------------------

(module+ test
  (test (interp (numC 10) 
                empty (numV -1) (numV -1))
        (numV 10))
  (test (interp (plusC (numC 10) (numC 17))
                empty (numV -1) (numV -1))
        (numV 27))
  (test (interp (multC (numC 10) (numC 7))
                empty (numV -1) (numV -1))
        (numV 70))

  (test (interp-posn (newC 'posn (list (numC 2) (numC 7))))
        (objV 'posn (list (box (numV 2)) (box (numV 7)))))

  (test (interp-posn (sendC posn27 'mdist (numC 0)))
        (numV 9))
  
  (test (interp-posn (sendC posn27 'addX (numC 10)))
        (numV 12))

  (test (interp-posn (sendC (ssendC posn27 'posn 'factory12 (numC 0))
                            'multY
                            (numC 15)))
        (numV 30))

  (test (interp-posn (sendC posn531 'addDist posn27))
        (numV 18))
  
  (test/exn (interp-posn (plusC (numC 1) posn27))
            "not a number")
  (test/exn (interp-posn (getC (numC 1) 'x))
            "not an object")
  (test/exn (interp-posn (sendC (numC 1) 'mdist (numC 0)))
            "not an object")
  (test/exn (interp-posn (ssendC (numC 1) 'posn 'mdist (numC 0)))
            "not an object")
  (test/exn (interp-posn (newC 'posn (list (numC 0))))
            "wrong field count")
  ;; #2
  (test (interp-posn (instanceofC posn27 'posn))
        (numV 0))
  (test (interp-posn (instanceofC posn531 'posn))
        (numV 0))
  (test (interp-posn (instanceofC posn27 'object))
        (numV 0))
  (test (interp-posn (instanceofC posn27 'posn3D))
        (numV 1))
  (test/exn (interp-posn (instanceofC (numC 1) 'posn))
            "not an object")
  (test/exn (interp-posn (instanceofC posn27 'fish))
            "not found")
  
  ;; #3
  (test (interp (if0C (numC 0) (numC 1) (numC 2))
                empty (numV -1) (numV -1))
        (numV 1))
  (test (interp (if0C (numC 1) (numC 0) (numC 2))
                empty (numV -1) (numV -1))
        (numV 2))

  ;; #5
  (test (interp-posn (castC 'posn posn531))
        (objV 'posn3D (list (box (numV 5)) (box (numV 3)) (box (numV 1)))))
  (test/exn (interp-posn (castC 'posn3D posn27))
            "not a subclass")
  (test/exn (interp-posn (castC 'posn3D (numC 1)))
            "not an object")

  ;; #6
  (test (interp-posn (getC posn531 'x))
        (numV 5))
  (test (interp-posn (setC posn531 'x (numC 0)))
        (numV 0))
  (test/exn (interp-posn (setC (numC 1) 'x (numC 0)))
        "not an object")

  ;; #7
  (test/exn (interp-posn (getC (nullC) 'x))
            "not an object")
  (test/exn (interp-posn (setC (nullC) 'x (numC 1)))
            "not an object")
  (test/exn (interp-posn (sendC (nullC) 'x (numC 1)))
            "not an object")
  (test (interp-posn (castC 'posn (nullC)))
        (nullV))
  (test (interp-posn (instanceofC (nullC) 'posn))
            (numV 1))
  (test (interp-posn (arraysetC (newarrayC (objT 'posn) (numC 1) posn27)
                                (numC 0)
                                (nullC)))
            (numV 0))
  (test/exn (interp-posn (arraysetC (newarrayC (objT 'posn) (numC 1) posn27)
                                (numC 3)
                                (nullC)))
            "index out of bounds")
  
  
  ;; #9
  (test (interp (newarrayC (numT) (numC 3) (numC 3))
                empty (numV -1) (numV -1))
        (arrayV (numT) 3 (list (box (numV 3)) (box (numV 3)) (box (numV 3)))))
  
  (test (interp (arrayrefC (newarrayC (numT) (numC 3) (numC 2))
                           (numC 1))
                empty (numV -1) (numV -1))
        (numV 2))
  (test/exn (interp (arrayrefC (newarrayC (numT) (numC 3) (numC 2))
                           (numC 3))
                empty (numV -1) (numV -1))
        "index out of bounds")
  (test (interp (arraysetC (newarrayC (numT) (numC 3) (numC 2))
                           (numC 0)
                           (numC 1))
                empty (numV -1) (numV -1))
        (numV 0))
  (test (interp-posn (arraysetC (newarrayC (objT 'posn) (numC 3) posn27)
                           (numC 1)
                           posn531))
        (numV 0))
  

  ;; #10
  (test (interp-posn (arraysetC (newarrayC (objT 'posn) (numC 1) posn27)
                           (numC 0)
                           posn531))
            (numV 0))
  (test/exn (interp-posn (arraysetC (newarrayC (objT 'posn3D) (numC 1) posn531)
                           (numC 2)
                           posn27))
            "not a subclass")
  (test/exn (interp-posn (arraysetC (newarrayC (objT 'posn) (numC 1) posn27)
                           (numC 3)
                           posn27))
            "index out of bounds")
  (test/exn (interp-posn (arraysetC (newarrayC (numT) (numC 1) (numC 1))
                           (numC 2)
                           (numC 2)))
            "index out of bounds")
  (test/exn (interp-posn (arraysetC (newarrayC (objT 'posn) (numC 1) posn27)
                           (numC 2)
                           (numC 2)))
            "not an object")
        
  #;(test (interp (arrayrefC tstarr (numC 0))
                empty (numV -1) (numV -1))
        (numV 0))
  #;(test (interp (arrayrefC tstarr (numC 1))
                empty (numV -1) (numV -1))
        (numV 0)))
