#lang plai-typed
(require plai-typed/s-exp-match
         "class.rkt"
         "inherit.rkt")

(module+ test
  (print-only-errors true))

;; ----------------------------------------

(define (parse-class [s : s-expression]) : ClassI
  (cond
   [(s-exp-match? `{class SYMBOL extends SYMBOL {ANY ...} ANY ...} s)
    (classI (s-exp->symbol (second (s-exp->list s)))
            (s-exp->symbol (fourth (s-exp->list s)))
            (map parse-field
                 (s-exp->list (fourth (rest (s-exp->list s)))))
            (map parse-method 
                 (rest (rest (rest (rest (rest (s-exp->list s))))))))]
   [else (error 'parse-class "invalid input")]))

(define (parse-field [s : s-expression]) : symbol
  (cond
   [(s-exp-match? `SYMBOL s)
    (s-exp->symbol s)]
   [else (error 'parse-field "invalid input")]))

(define (parse-method [s : s-expression]) : MethodI
  (cond
   [(s-exp-match? `{SYMBOL ANY} s)
    (methodI (s-exp->symbol (first (s-exp->list s)))
             (parse (second (s-exp->list s))))]
   [else (error 'parse-method "invalid input")]))

;; ----------------------------------------
;; #9 (moved from typed-parse.rkt)

(define (parse-type [s : s-expression]) : Type
  (cond
   [(s-exp-match? `num s)
    (numT)]
   [(s-exp-match? '(array ANY) s)
    (arrayT (parse-type (second (s-exp->list s))))]
   [(s-exp-match? `SYMBOL s)
    (objT (s-exp->symbol s))]
   [else (error 'parse-type "invalid input")]))

(module+ test
  (test (parse-type `num)
        (numT))
  (test (parse-type `object)
        (objT 'object))
  (test/exn (parse-type `{})
            "invalid input")
  
  ;; #9
  (test (parse-type '(array num))
        (arrayT (numT))))

;; ----------------------------------------

(define (parse [s : s-expression]) : ExprI
  (cond
   [(s-exp-match? `NUMBER s) (numI (s-exp->number s))]
   [(s-exp-match? `arg s) (argI)]
   [(s-exp-match? `this s) (thisI)]

   ;; #7
   [(s-exp-match? `null s) (nullI)]
   [(s-exp-match? '{+ ANY ANY} s)
    (plusI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
   [(s-exp-match? '{* ANY ANY} s)
    (multI (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]

   ;; #2
   [(s-exp-match? '{instanceof ANY SYMBOL} s)
    (instanceofI (parse (second (s-exp->list s)))
                 (s-exp->symbol (third (s-exp->list s))))]
   
   ;; #3
   [(s-exp-match? '{if0 ANY ANY ANY} s)
    (if0I (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]

   ;; #5
   [(s-exp-match? '{cast SYMBOL ANY} s)
    (castI (s-exp->symbol (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]

   ;; #9
   [(s-exp-match? '{newarray ANY ANY ANY} s)
    (newarrayI (parse-type (second (s-exp->list s)))
               (parse (third (s-exp->list s)))
               (parse (fourth (s-exp->list s))))]
   [(s-exp-match? '{arrayref ANY ANY} s)
    (arrayrefI (parse (second (s-exp->list s)))
               (parse (third (s-exp->list s))))]
   [(s-exp-match? '{arrayset ANY ANY ANY} s)
    (arraysetI (parse (second (s-exp->list s)))
               (parse (third (s-exp->list s)))
               (parse (fourth (s-exp->list s))))]

   ;; #8
   [(s-exp-match? '{new SYMBOL} s)
    (newI (s-exp->symbol (second (s-exp->list s)))
          empty)]
   [(s-exp-match? '{get ANY SYMBOL} s)
    (getI (parse (second (s-exp->list s)))
          (s-exp->symbol (third (s-exp->list s))))]

   ;; #6
   [(s-exp-match? '{set ANY SYMBOL ANY} s)
    (setI (parse (second (s-exp->list s)))
          (s-exp->symbol (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
   [(s-exp-match? '{send ANY SYMBOL ANY} s)
    (sendI (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
   [(s-exp-match? '{super SYMBOL ANY} s)
    (superI (s-exp->symbol (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
   [else (error 'parse "invalid input")]))

(module+ test
  (test (parse '0)
        (numI 0))
  (test (parse `arg)
        (argI))
  (test (parse `this)
        (thisI))
  (test (parse '{+ 1 2})
        (plusI (numI 1) (numI 2)))
  (test (parse '{* 1 2})
        (multI (numI 1) (numI 2)))
  (test (parse '{new posn 1 2})
        (newI 'posn (list (numI 1) (numI 2))))
  (test (parse '{get 1 x})
        (getI (numI 1) 'x))
  (test (parse '{send 1 m 2})
        (sendI (numI 1) 'm (numI 2)))
  (test (parse '{super m 1})
        (superI 'm (numI 1)))
  (test/exn (parse `x)
            "invalid input")

  (test (parse-field `x)
        'x)
  (test/exn (parse-field '{x 1})
            "invalid input")

  (test (parse-method `{m this})
        (methodI 'm (thisI)))
  (test/exn (parse-method `{m 1 2})
            "invalid input")
  
  (test (parse-class '{class posn3D extends posn
                             {x y z}
                             {m1 arg}
                             {m2 this}})
        (classI 'posn3D 'posn
                (list 'x 'y 'z)
                (list (methodI 'm1 (argI))
                      (methodI 'm2 (thisI)))))
  (test/exn (parse-class '{class})
            "invalid input")

  ;; #2
  (test (parse '{instanceof 0 fish})
        (instanceofI (numI 0) 'fish))

  ;; #3
  (test (parse '{if0 0 1 2})
        (if0I (numI 0) (numI 1) (numI 2)))

  ;; #5
  (test (parse '{cast fish 1})
        (castI 'fish (numI 1)))

  ;; #6
  (test (parse '{set 1 x 0})
        (setI (numI 1) 'x (numI 0)))

  ;; #7
  (test (parse '{set null x 0})
        (setI (nullI) 'x (numI 0)))
  
  ;; #9
  (test (parse '{newarray num 1 2})
        (newarrayI (numT) (numI 1) (numI 2)))
  (test (parse '{newarray (array num) 1 2})
        (newarrayI (arrayT (numT)) (numI 1) (numI 2)))
  (test (parse '{newarray (array (array num)) 1 2})
        (newarrayI (arrayT (arrayT (numT))) (numI 1) (numI 2)))
  (test (parse '{arrayref {newarray (array (array num)) 1 2} 1})
        (arrayrefI (newarrayI (arrayT (arrayT (numT))) (numI 1) (numI 2)) (numI 1)))
  (test (parse '{arrayset {newarray (array (array num)) 1 2} 0 2})
        (arraysetI (newarrayI (arrayT (arrayT (numT))) (numI 1) (numI 2)) (numI 0) (numI 2))))

;; ----------------------------------------

(define (interp-prog [classes : (listof s-expression)] [a : s-expression]) : s-expression
  (let ([v (interp-i (parse a)
                     (map parse-class classes))])
    (type-case Value v
      [numV (n) (number->s-exp n)]
      [objV (class-name field-vals) `object]

      ;; #7
      [nullV () `null]

      ;; #9
      [arrayV (t len arr) `array])))

(module+ test
  (test (interp-prog
         (list
          '{class empty extends object
                  {}})
         '{new empty})
        `object)

 (test (interp-prog 
        (list
         '{class posn extends object
                 {x y}
                 {mdist {+ {get this x} {get this y}}}
                 {addDist {+ {send arg mdist 0}
                             {send this mdist 0}}}}
         
         '{class posn3D extends posn
                 {z}
                 {mdist {+ {get this z} 
                           {super mdist arg}}}})
        
        '{send {new posn3D 5 3 1} addDist {new posn 2 7}})
       '18)

  (test (interp-prog 
        (list
         '{class test extends object
            {x}
            {new {newarray num 0 arg}}
            {set {arrayset {get this x} arg 0}}
            {ref {arrayref {get this x} arg}}})
        '{send {new test {newarray num 1 1}} new 1})
        `array)
  (test (interp-prog 
        (list
         '{class test extends object
            {x}
            {new {newarray num 0 arg}}
            {set {arrayset {get this x} arg 0}}
            {ref {arrayref {get this x} arg}}})
        '{send {new test {newarray posn 1 null}} ref 0})
        `null))