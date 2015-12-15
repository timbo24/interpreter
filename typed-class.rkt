#lang plai-typed

(require "class.rkt"
         "inherit.rkt")


(define-type ClassT
  [classT (name : symbol)
          (super-name : symbol)
          (fields : (listof FieldT))
          (methods : (listof MethodT))])

(define-type FieldT
  [fieldT (name : symbol)
          (type : Type)])

(define-type MethodT
  [methodT (name : symbol)
           (arg-type : Type)
           (result-type : Type)
           (body-expr : ExprI)])

(module+ test
  (print-only-errors true))

;; ----------------------------------------

(define find-classT
  (make-find classT-name))

(define find-fieldT
  (make-find fieldT-name))

(define find-methodT
  (make-find methodT-name))

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define (get-all-field-types class-name t-classes)
  (if (equal? class-name 'object)
      empty        
      (type-case ClassT (find-classT class-name t-classes)
        [classT (name super-name fields methods)
                (append 
                 (get-all-field-types super-name t-classes)
                 (map fieldT-type fields))])))

;; ----------------------------------------

(define (make-find-in-tree find-in-list extract)
  (lambda (name t-class t-classes)
    (local [(define items (extract t-class))
            (define super-name 
              (classT-super-name t-class))]
      (if (equal? super-name 'object)
          (find-in-list name items)
          (try (find-in-list name items)
               (lambda ()
                 ((make-find-in-tree find-in-list extract)
                  name 
                  (find-classT super-name t-classes)
                  t-classes)))))))

(define find-field-in-tree
  (make-find-in-tree find-fieldT classT-fields))

(define find-method-in-tree
  (make-find-in-tree find-methodT classT-methods))

;; ----------------------------------------
(define (is-subclass? name1 name2 t-classes)
  (cond
    [(equal? name1 name2) true]
    [(equal? name1 'object) false]
    [else
     (type-case ClassT (find-classT name1 t-classes)
       [classT (name super-name fields methods)
               (is-subclass? super-name name2 t-classes)])]))

;; #4
(define (has-least-upper-bound-class? name1 name2 t-classes)
  (cond
    [(is-subclass? name1 name2 t-classes) true]
    [else
     (type-case ClassT (find-classT name2 t-classes)
       [classT (name super-name fields methods)
               (has-least-upper-bound-class? name1 super-name t-classes)])]))
;; #4
(define (least-upper-bound-class name1 name2 t-classes)
  (cond
    [(is-subclass? name1 name2 t-classes) name2]
    [else
     (type-case ClassT (find-classT name2 t-classes)
       [classT (name super-name fields methods)
               (least-upper-bound-class name1 super-name t-classes)])]))

;; #4
;; only called if has-least-upper-bound is true
(define (least-upper-bound-type t1 t2 t-classes)
  (type-case Type t1
    [objT (name1)
          (type-case Type t2 
            [objT (name2)
                  (objT (least-upper-bound-class name1 name2 t-classes))]
            [else (error 'type "no least upper bound")])]
    ;; #9 #10
    [arrayT (array1-t)
          (type-case Type t2 
            [arrayT (array2-t) (arrayT (least-upper-bound-type array1-t array2-t t-classes))]
            [else (error 'type "no least upper bound")])]

    ;; #7
    [nullT ()
           (type-case Type t2
             [nullT () (nullT)]
             [objT (name2) (objT name2)]
             [else (error 'type "no least upper bound")])]
    [else
     (if (equal? t1 t2)
         t1
         (error 'type "no least upper bound"))]))

;; #4
(define (has-least-upper-bound-type? t1 t2 t-classes)
  (type-case Type t1
    [objT (name1)
          (type-case Type t2 
            [objT (name2)
                  (has-least-upper-bound-class? name1 name2 t-classes)]
            [else false])]
    ;; #9 #10
    [arrayT (array1-t)
            (type-case Type t2
              [arrayT (array2-t)
                      (has-least-upper-bound-type? array1-t array2-t t-classes)]
              [else false])]
    
    ;; #7
    [nullT ()
           (type-case Type t2
             [nullT () true]
             [objT (name2) true]
             [else false])]
    [else (equal? t1 t2)]))
     

(define (is-subtype? t1 t2 t-classes)
  (type-case Type t1
    [objT (name1)
          (type-case Type t2 
            [objT (name2)
                  (is-subclass? name1 name2 t-classes)]
            [else false])]

    ;; #9 #10
    [arrayT (array1-t)
            (type-case Type t2
              [arrayT (array2-t)
                      (is-subtype? array1-t array2-t t-classes)]
              [else false])]

    ;; #7
    [nullT ()
           (type-case Type t2
             [nullT () true]
             [objT (name2) true]
             [else false])]
    [else (equal? t1 t2)]))

(module+ test
  (define a-t-class (classT 'a 'object empty empty))
  (define b-t-class (classT 'b 'a empty empty))
  (define c-t-class (classT 'c 'object empty empty))
  (define d-t-class (classT 'd 'e empty empty))
  (define f-t-class (classT 'f 'g empty empty))

  (test (is-subclass? 'object 'object empty)
        true)
  (test (is-subclass? 'a 'b (list a-t-class b-t-class))
        false)
  (test (is-subclass? 'b 'a (list a-t-class b-t-class))
        true)

  (test (is-subtype? (numT) (numT) empty)
        true)
  (test (is-subtype? (numT) (objT 'object) empty)
        false)
  (test (is-subtype? (objT 'object) (numT) empty)
        false)
  (test (is-subtype? (objT 'a) (objT 'b) (list a-t-class b-t-class))
        false)
  (test (is-subtype? (objT 'b) (objT 'a) (list a-t-class b-t-class))
        true)

  ;; #4
  (test (has-least-upper-bound-type? (objT 'a) (objT 'b) (list a-t-class b-t-class))
        true)
  (test (has-least-upper-bound-type? (objT 'b) (objT 'a) (list a-t-class b-t-class))
        true)
  (test (has-least-upper-bound-type? (objT 'c) (objT 'b) (list a-t-class b-t-class c-t-class))
        true)
  (test/exn (least-upper-bound-type (numT) (arrayT (objT 'c)) (list a-t-class b-t-class c-t-class))
            "no least upper bound")
  (test/exn (least-upper-bound-type (objT 'c) (numT) (list a-t-class b-t-class c-t-class))
            "no least upper bound")
  (test/exn (least-upper-bound-type (arrayT (objT 'c)) (numT) (list a-t-class b-t-class c-t-class))
            "no least upper bound")
  (test (has-least-upper-bound-type? (objT 'c) (numT) (list a-t-class b-t-class c-t-class))
            false)
  (test (has-least-upper-bound-type? (arrayT (objT 'c)) (numT) (list a-t-class b-t-class c-t-class))
            false)

  ;; #7
  (test (is-subtype? (nullT) (objT 'b) (list a-t-class b-t-class))
        true)
  (test (is-subtype? (nullT) (nullT) (list a-t-class b-t-class))
        true)
  (test (is-subtype? (nullT) (numT) (list a-t-class b-t-class))
        false)
  (test (has-least-upper-bound-type? (nullT) (objT 'b) (list a-t-class b-t-class))
        true)
  (test (has-least-upper-bound-type? (nullT) (nullT) (list a-t-class b-t-class))
        true)
  (test (has-least-upper-bound-type? (nullT) (numT) (list a-t-class b-t-class))
        false)
  (test (least-upper-bound-type (nullT) (nullT) (list a-t-class b-t-class c-t-class))
            (nullT))
  (test (least-upper-bound-type (nullT) (objT 'b) (list a-t-class b-t-class c-t-class))
            (objT 'b))
  (test/exn (least-upper-bound-type (nullT) (numT) (list a-t-class b-t-class c-t-class))
            "no least upper bound")

  ;; #9 #10
  (test (has-least-upper-bound-type? (objT 'c) (objT 'b) (list a-t-class b-t-class c-t-class))
        true)
  (test (is-subtype? (arrayT (objT 'b)) (arrayT (objT 'a)) (list a-t-class b-t-class))
        true)
  (test (is-subtype? (arrayT (objT 'a)) (arrayT (objT 'b)) (list a-t-class b-t-class))
        false)
  (test (is-subtype? (arrayT (objT 'a)) (numT) (list a-t-class b-t-class))
        false))

;; ----------------------------------------
;; #8
(define initial-field-exprs : (Type -> ExprI)
  (lambda (t)
    (type-case Type t
      [numT () (numI 0)]
      [nullT () (nullI)]
      [objT (name) (nullI)]
      [arrayT (type) (newarrayI type (numI 1) (initial-field-exprs type))])))

    
(define initialize-fields : (ExprI (listof ClassT) -> ExprI)
  (lambda (expr t-classes)
    (local [(define (recur expr)
              (initialize-fields expr t-classes))]
      (type-case ExprI expr
        [numI (n) (numI n)]
        [plusI (l r) (plusI (recur l) (recur r))]
        [multI (l r) (multI (recur l) (recur r))]
        [argI () (argI)]
        [thisI () (thisI)]

        ;; #8
        [newI (class-name empty-list)
              (local [(define field-types
                        (get-all-field-types class-name t-classes))]
                (newI class-name (map initial-field-exprs field-types)))]
        [getI (obj-expr field-name)
              (getI (recur obj-expr)
                    field-name)]
        [setI (obj-expr field-name arg-expr)
              (setI (recur obj-expr)
                    field-name
                    (recur arg-expr))]
        [sendI (obj-expr method-name arg-expr)
               (sendI (recur obj-expr)
                      method-name
                      (recur arg-expr))]
        [superI (method-name arg-expr)
                (superI method-name
                        (recur arg-expr))]
        ;; #2
        [instanceofI (obj-expr class-name)
                     (instanceofI (recur obj-expr)
                                  class-name)]

        ;; #3
        [if0I (t-expr thn-expr els-expr)
              (if0I (recur t-expr)
                    (recur thn-expr)
                    (recur els-expr))]

        ;; #5
        [castI (cast-class-name obj-expr)
               (castI cast-class-name
                      (recur obj-expr))]

        ;; #7
        [nullI ()
               (nullI)]

        ;; #9 
        [newarrayI (t len-expr init-expr)
                   (newarrayI t
                              (recur len-expr)
                              (recur init-expr))]
        [arrayrefI (arr-expr ind-expr)
                   (arrayrefI (recur arr-expr)
                              (recur ind-expr))]
        [arraysetI (arr-expr ind-expr elem-expr)
                   (arraysetI (recur arr-expr)
                              (recur ind-expr)
                              (recur elem-expr))]))))

(module+ test
  (define posn-t-class
    (classT 'posn 'object
            (list (fieldT 'x (numT)) (fieldT 'y (numT)))
            (list (methodT 'mdist (numT) (numT) 
                           (plusI (getI (thisI) 'x) (getI (thisI) 'y)))
                  (methodT 'addDist (objT 'posn) (numT)
                           (plusI (sendI (thisI) 'mdist (numI 0))
                                  (sendI (argI) 'mdist (numI 0)))))))
  (define posn3-t-class
    (classT 'posn3 'object
            (list (fieldT 'x (numT))
                  (fieldT 'y (arrayT (numT)))
                  (fieldT 'z (nullT))
                  (fieldT 'a (objT 'object)))
            (list (methodT 'mdist (numT) (numT) 
                           (plusI (getI (thisI) 'x) (getI (thisI) 'y)))
                  (methodT 'addDist (objT 'posn) (numT)
                           (plusI (sendI (thisI) 'mdist (numI 0))
                                  (sendI (argI) 'mdist (numI 0)))))))
  (define posn27 (newI 'posn (list (numI 2) (numI 7))))

  (test (initialize-fields (plusI (numI 0) (numI 1))
                           empty)
        (plusI (numI 0) (numI 1)))
  (test (initialize-fields (multI (numI 0) (numI 1))
                           empty)
        (multI (numI 0) (numI 1)))
  (test (initialize-fields (multI (numI 0) (numI 1))
                           empty)
        (multI (numI 0) (numI 1)))
  (test (initialize-fields (getI (numI 1) 'fish)
                           empty)
        (getI (numI 1) 'fish))
  (test (initialize-fields (setI (numI 1) 'fish (numI 0))
                           empty)
        (setI (numI 1) 'fish (numI 0)))
  (test (initialize-fields (sendI (numI 1) 'fish (numI 0))
                           empty)
        (sendI (numI 1) 'fish (numI 0)))
  (test (initialize-fields (superI 'fish (numI 0))
                           empty)
        (superI 'fish (numI 0)))
  (test (initialize-fields (instanceofI (numI 0) 'fish)
                           empty)
        (instanceofI (numI 0) 'fish))
  (test (initialize-fields (if0I (numI 0) (numI 0) (numI 0))
                           empty)
        (if0I (numI 0) (numI 0) (numI 0)))
  (test (initialize-fields (castI 'fish (numI 0))
                           empty)
        (castI 'fish (numI 0)))
  (test (initialize-fields (castI 'fish (numI 0))
                           empty)
        (castI 'fish (numI 0)))
  (test (initialize-fields (nullI)
                           (list posn-t-class))
        (nullI))
  (test (initialize-fields (thisI)
                           (list posn-t-class))
        (thisI))
  (test (initialize-fields (argI)
                           (list posn-t-class))
        (argI))
  (test (initialize-fields (newarrayI (numT) (numI 0) (numI 0))
                           (list posn-t-class))
        (newarrayI (numT) (numI 0) (numI 0)))
  (test (initialize-fields (arrayrefI (numI 0) (numI 0))
                           (list posn-t-class))
        (arrayrefI (numI 0) (numI 0)))
  (test (initialize-fields (arraysetI (numI 0) (numI 1) (numI 1))
                           empty)
        (arraysetI (numI 0) (numI 1) (numI 1)))
  (test (initialize-fields (newI 'posn3 empty)
                           (list posn3-t-class))
        (newI 'posn3 (list (numI 0) (newarrayI (numT) (numI 1) (numI 0)) (nullI) (nullI)))))
                           
        
;; ----------------------------------------

(define typecheck-expr : (ExprI (listof ClassT) Type Type -> Type)
  (lambda (expr t-classes arg-type this-type)
    (local [(define (recur expr)
              (typecheck-expr expr t-classes arg-type this-type))
            (define (typecheck-nums l r)
              (type-case Type (recur l)
                [numT ()
                      (type-case Type (recur r)
                        [numT () (numT)]
                        [else (type-error r "num")])]
                [else (type-error l "num")]))]
      (type-case ExprI expr
        [numI (n) (numT)]
        [plusI (l r) (typecheck-nums l r)]
        [multI (l r) (typecheck-nums l r)]
        [argI () arg-type]
        [thisI () this-type]
        [newI (class-name exprs)
              (local [(define arg-types (map recur exprs))
                      (define field-types
                        (get-all-field-types class-name t-classes))]
                (if (and (= (length arg-types) (length field-types))
                         (foldl (lambda (b r) (and r b))
                                true
                                (map2 (lambda (t1 t2) 
                                        (is-subtype? t1 t2 t-classes))
                                      arg-types
                                      field-types)))
                    (objT class-name)
                    (type-error expr "field type mismatch")))]
        [getI (obj-expr field-name)
              (type-case Type (recur obj-expr)
                [objT (class-name)
                      (local [(define t-class
                                (find-classT class-name t-classes))
                              (define field
                                (find-field-in-tree field-name
                                                    t-class
                                                    t-classes))]
                        (type-case FieldT field
                          [fieldT (name type) type]))]
                ;; #7
                [nullT ()
                       (type-error obj-expr "object")]
                [else (type-error obj-expr "object")])]
        [setI (obj-expr field-name arg-expr)
              (type-case Type (recur obj-expr)
                [objT (class-name)
                      (local [(define t-class
                                (find-classT class-name t-classes))
                              (define field
                                (find-field-in-tree field-name
                                                    t-class
                                                    t-classes))]
                        (type-case FieldT field
                          [fieldT (name type)
                                  (if (is-subtype? (recur arg-expr) type t-classes)
                                      (numT)
                                      (type-error arg-expr "not a subtype"))]))]
                ;; #7
                [nullT ()
                       (type-error obj-expr "object")]
                [else (type-error obj-expr "object")])]
        [sendI (obj-expr method-name arg-expr)
               (local [(define obj-type (recur obj-expr))
                       (define arg-type (recur arg-expr))]
                 (type-case Type obj-type
                   [objT (class-name)
                         (typecheck-send class-name method-name
                                         arg-expr arg-type
                                         t-classes)]
                   ;; #7
                   [nullT ()
                          (type-error obj-expr "object")]
                   [else
                    (type-error obj-expr "object")]))]
        [superI (method-name arg-expr)
                (local [(define arg-type (recur arg-expr))
                        (define this-class
                          (find-classT (objT-class-name this-type)
                                       t-classes))]
                  (typecheck-send (classT-super-name this-class)
                                  method-name
                                  arg-expr arg-type
                                  t-classes))]

        ;; #2
        [instanceofI (obj-expr class-name)
                     (type-case Type (recur obj-expr)
                       [objT (obj-class-name)
                             (objT obj-class-name)]
                       [else (type-error obj-expr "object")])]

        ;; #3
        [if0I (t-expr thn-expr els-expr)
              (type-case Type (recur t-expr)
                [numT () (local [(define thn-type (recur thn-expr))
                                   (define els-type (recur els-expr))]
                           (if (has-least-upper-bound-type? thn-type els-type t-classes)
                               (least-upper-bound-type thn-type els-type t-classes)
                               (type-error els-expr "object")))]
                [else (type-error t-expr "number")])]

        ;; #5
        [castI (cast-class-name obj-expr)
               (local [(define obj-type (recur obj-expr))]
                 (if (or (is-subtype? (objT cast-class-name) obj-type t-classes)
                         (is-subtype? obj-type (objT cast-class-name) t-classes))
                     (objT cast-class-name)
                     (type-error obj-expr "subtype")))]

        ;; #7
        [nullI ()
               (nullT)]

        ;; #9 
        [newarrayI (t len-expr init-expr)
                   (type-case Type (recur len-expr)
                     [numT () (local [(define init-type (recur init-expr))]
                                (if (is-subtype? init-type t t-classes)
                                    (arrayT t)
                                    (type-error t (to-string init-type))))]
                     [else (type-error len-expr "number")])]
        [arrayrefI (arr-expr ind-expr)
                   (type-case Type (recur arr-expr)
                     [arrayT (t) (type-case Type (recur ind-expr)
                                   [numT () t]
                                   [else (type-error ind-expr "number")])]
                     [else (type-error arr-expr "array")])]
        [arraysetI (arr-expr ind-expr elem-expr)
                   (type-case Type (recur arr-expr)
                     [arrayT (t) (type-case Type (recur ind-expr)
                                   [numT () (if (is-subtype? (recur elem-expr) t t-classes)
                                                (numT)
                                                (type-error elem-expr "subtype"))]
                                   [else (type-error ind-expr "number")])]
                     [else (type-error arr-expr "array")])]))))

(define (typecheck-send [class-name : symbol]
                        [method-name : symbol]
                        [arg-expr : ExprI]
                        [arg-type : Type]
                        [t-classes : (listof ClassT)])
  (type-case MethodT (find-method-in-tree
                      method-name
                      (find-classT class-name t-classes)
                      t-classes)
    [methodT (name arg-type-m result-type body-expr)
             (if (is-subtype? arg-type arg-type-m t-classes)
                 result-type
                 (type-error arg-expr (to-string arg-type-m)))]))

(define (typecheck-method [method : MethodT]
                          [this-type : Type]
                          [t-classes : (listof ClassT)]) : ()
  (type-case MethodT method
    [methodT (name arg-type result-type body-expr)
             (if (is-subtype? (typecheck-expr body-expr t-classes
                                              arg-type this-type)
                              result-type
                              t-classes)
                 (values)
                 (type-error body-expr (to-string result-type)))]))

(define (check-override [method : MethodT]
                        [this-class : ClassT]
                        [t-classes : (listof ClassT)])
  (local [(define super-name 
            (classT-super-name this-class))
          (define super-method
            (try
             ;; Look for method in superclass:
             (find-method-in-tree (methodT-name method)
                                  (find-classT super-name t-classes)
                                  t-classes)
             ;; no such method in superclass:
             (lambda () method)))]
    (if (and (equal? (methodT-arg-type method)
                     (methodT-arg-type super-method))
             (equal? (methodT-result-type method)
                     (methodT-result-type super-method)))
        (values)
        (error 'typecheck (string-append
                           "bad override of "
                           (to-string (methodT-name method)))))))

(define (typecheck-class [t-class : ClassT] [t-classes : (listof ClassT)])
  (type-case ClassT t-class
    [classT (name super-name fields methods)
            (map (lambda (m)
                   (begin
                     (typecheck-method m (objT name) t-classes)
                     (check-override m t-class t-classes)))
                 methods)]))

(define (typecheck [a : ExprI] [t-classes : (listof ClassT)]) : Type
  (begin
    (map (lambda (t-class)
           (typecheck-class t-class t-classes))
         t-classes)
    (typecheck-expr a t-classes (numT) (objT 'bad))))

;; ----------------------------------------

(module+ test

  (define posn3D-t-class 
    (classT 'posn3D 'posn
            (list (fieldT 'z (numT)))
            (list (methodT 'mdist (numT) (numT)
                           (plusI (getI (thisI) 'z) 
                                  (superI 'mdist (argI)))))))

  (define square-t-class 
    (classT 'square 'object
            (list (fieldT 'topleft (objT 'posn)))
            (list)))

  (define square2-t-class 
    (classT 'square2 'object
            (list (fieldT 'z (objT 'posn3D)))
            (list (methodT 'return-obj (objT 'posn3D) (objT 'posn3D)
                           (getI (thisI) 'z)))))

  (define (typecheck-posn a)
    (typecheck a
               (list posn-t-class posn3D-t-class square-t-class square2-t-class)))
  (define posn25 (newI 'posn (list (numI 2) (numI 5))))
  (define posn531 (newI 'posn3D (list (numI 5) (numI 3) (numI 1))))

  (test (typecheck-posn (sendI posn27 'mdist (numI 0)))
        (numT))
  (test (typecheck-posn (sendI posn531 'mdist (numI 0)))
        (numT))  
  (test (typecheck-posn (sendI posn531 'addDist posn27))
        (numT))  
  (test (typecheck-posn (sendI posn27 'addDist posn531))
        (numT))

  (test (typecheck-posn (newI 'square (list (newI 'posn (list (numI 0) (numI 1))))))
        (objT 'square))
  (test (typecheck-posn (newI 'square (list (newI 'posn3D (list (numI 0) (numI 1) (numI 3))))))
        (objT 'square))
  
  (test (typecheck (multI (numI 1) (numI 2))
                   empty)
        (numT))

  ;; #1
  #;(test/exn (typecheck-posn (sendI posn27 'mdist (thisI)))
            "'this not allowed in main expression")
  #;(test/exn (typecheck-posn (sendI posn27 'mdist (argI)))
            "'arg not allowed in main expression")

  ;; #2
  (test/exn (typecheck (instanceofI (numI 0) 'fish)
                       
                       empty)
            "object")
  (test (typecheck-posn (instanceofI posn531 'fish))
        (objT 'posn3D))

  ;; #3
  (test (typecheck (if0I (numI 1) (numI 2) (numI 3))
                   empty)
        (numT))
  (test (typecheck-posn (if0I (numI 0) posn27 posn531))
        (objT 'posn))
  (test (typecheck-posn (if0I (numI 0) posn531 posn27))
        (objT 'posn))
  (test/exn (typecheck-posn (if0I (numI 0) posn531 (numI 1)))
        "object")
  ;; should work with #4 implemented
  #;(test/exn (typecheck-posn (if0I (numI 0) posn531 (newI 'square (list (newI 'posn (list (numI 0) (numI 1)))))))
        "no type")
  (test/exn (typecheck-posn (if0I posn531 (numI 2) (numI 3)))
        "number")

  ;; #4
  (test (typecheck-posn (if0I (numI 0) posn531 (newI 'square (list (newI 'posn (list (numI 0) (numI 1)))))))
        (objT 'object))

  ;; #5
  (test (typecheck-posn (castI 'posn posn531))
        (objT 'posn))
  (test (typecheck-posn (castI 'posn3D posn27))
        (objT 'posn3D))
  (test/exn (typecheck-posn (castI 'posn3D (numI 0)))
        "subtype")

  ;; #6
  (test (typecheck-posn (setI posn531 'x (numI 1)))
        (numT))
  (test/exn (typecheck-posn (setI (newI 'square2 (list posn531))
                              'z
                              posn27))
        "subtype")              
  (test/exn (typecheck (setI (numI 1) 'x (numI 1))
                       empty)
            "no type")
  (test/exn (typecheck-posn (setI posn531 'd (numI 1)))
            "not found")

  ;; #7
  (test/exn (typecheck-posn (getI (nullI) 'd))
            "object")
  (test/exn (typecheck-posn (setI (nullI) 'd (numI 1)))
            "object")
  (test/exn (typecheck-posn (sendI (nullI) 'd (numI 1)))
            "object")
  (test (typecheck-posn (setI (newI 'square2 (list (nullI)))
                              'z
                              (nullI)))
        (numT))
  (test (typecheck-posn (sendI (newI 'square2 (list (nullI)))
                              'return-obj
                              (nullI)))
        (objT 'posn3D))

  ;; #9
  (test (typecheck (newarrayI (numT) (numI 1) (numI 2))
                   empty)
        (arrayT (numT)))
  (test (typecheck (arrayrefI (newarrayI (numT) (numI 1) (numI 2)) (numI 2))
                   empty)
        (numT))
  (test (typecheck (arraysetI (newarrayI (numT) (numI 1) (numI 2)) (numI 1) (numI 2))
                   empty)
        (numT))
  (test/exn (typecheck (newarrayI (numT) (newarrayI (numT) (numI 1) (numI 2)) (numI 2))
                       empty)
            "number")
  (test/exn (typecheck (newarrayI (arrayT (numT)) (numI 1) (numI 2))
                       empty)
            "array")
  (test/exn (typecheck (arrayrefI (numI 1) (newarrayI (numT) (numI 1) (numI 2)))
                       empty)
            "array")
  (test/exn (typecheck (arrayrefI (newarrayI (numT) (numI 1) (numI 2)) (newarrayI (numT) (numI 1) (numI 2)))
                       empty)
            "number")
  (test (typecheck (arraysetI (newarrayI (numT) (numI 1) (numI 2)) (numI 1) (numI 2))
                   empty)
        (numT))
  (test/exn (typecheck (arraysetI (newarrayI (numT) (numI 1) (numI 2))
                                  (newarrayI (numT) (numI 1) (numI 2))
                                  (numI 2))
                   empty)
        "number")
  (test/exn (typecheck (arraysetI (numI 0) (numI 0) (numI 0))
                   empty)
        "array")

  ;; #10
  (test (typecheck-posn (if0I (numI 0)
                              (newarrayI (objT 'square) (numI 1) (newI 'square (list (newI 'posn (list (numI 0) (numI 1))))))
                              (newarrayI (objT 'posn3D) (numI 1) posn531)))
        (arrayT (objT 'object)))
  (test (typecheck-posn (arraysetI
                         (newarrayI (arrayT (objT 'posn)) (numI 10) (newarrayI (objT 'posn) (numI 100) posn27))
                         (numI 5)
                         (newarrayI (objT 'posn3D) (numI 3) posn531)))
        (numT))
  (test/exn (typecheck-posn (arraysetI
                             (newarrayI (arrayT (objT 'posn3D)) (numI 10) (newarrayI (objT 'posn3D) (numI 100) posn531))
                             (numI 5)
                             (newarrayI (objT 'posn) (numI 3) posn27)))
        "subtype")
                              

  (test/exn (typecheck-posn (sendI (numI 10) 'mdist (numI 0)))
            "no type")
  (test/exn (typecheck-posn (sendI posn27 'mdist posn27))
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'object empty))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (newI 'object empty) (numI 1))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'object (list (numI 1))))
                       empty)
            "no type")
  (test/exn (typecheck (getI (numI 1) 'x)
                       empty)
            "no type")
  (test/exn (typecheck (numI 10)
                       (list posn-t-class 
                             (classT 'other 'posn
                                     (list)
                                     (list (methodT 'mdist 
                                                    (objT 'object) (numT)
                                                    (numI 10))))))
            "bad override")
  (test/exn (typecheck-method (methodT 'm (numT) (objT 'object) (numI 0)) (objT 'object) empty)
            "no type")
  (test/exn (typecheck (numI 0)
                       (list square-t-class
                             (classT 'cube 'square
                                     empty
                                     (list
                                      (methodT 'm (numT) (numT)
                                               ;; No such method in superclass:
                                               (superI 'm (numI 0)))))))
            "not found"))

;; ----------------------------------------

(define strip-types : (ClassT -> ClassI)
  (lambda (t-class)
    (type-case ClassT t-class
      [classT (name super-name fields methods)
              (classI
               name 
               super-name
               (map fieldT-name fields)
               (map (lambda (m)
                      (type-case MethodT m
                        [methodT (name arg-type result-type body-expr)
                                 (methodI name body-expr)]))
                    methods))])))

(define interp-t : (ExprI (listof ClassT) -> Value)
  (lambda (a t-classes)
    (interp-i a
              (map strip-types t-classes))))

(module+ test
  (define (interp-t-posn a)
    (interp-t a
              (list posn-t-class posn3D-t-class)))
  
  (test (interp-t-posn (sendI posn27 'mdist (numI 0)))
        (numV 9))  
  (test (interp-t-posn (sendI posn531 'mdist (numI 0)))
        (numV 9))
  (test (interp-t-posn (sendI posn531 'addDist posn27))
        (numV 18))
  (test (interp-t-posn (sendI posn27 'addDist posn531))
        (numV 18)))