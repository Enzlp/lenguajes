#lang play
(require "T2.rkt")

(print-only-errors #t)

;;Tests P1.b
(test (parse-prop 'true) (tt))
(test (parse-prop 'false) (ff))
(test (parse-prop '(not true)) (p-not (tt)))
(test (parse-prop 'x) (p-id 'x))
(test (parse-prop '(and true false true)) (p-and ( list (tt) ( ff ) (tt))))
(test (parse-prop '(or true false )) (p-or ( list (tt) ( ff ))))
(test (parse-prop '(and (not true) (or false x))) (p-and (list (p-not (tt)) (p-or (list (ff) (p-id 'x))))))
(test (parse-prop '(false where [x true] )) (p-where (ff) 'x (tt)))
(test (parse-prop '(x where [x true])) (p-where (p-id 'x) 'x (tt)))

;Chequeo de excepciones
(check-exn
  (lambda (e) (string=? (exn-message e) "parse-prop: and expects at least two operands"))
  (lambda () (parse-prop '(and true))))
(check-exn
  (lambda (e) (string=? (exn-message e) "parse-prop: or expects at least two operands"))
  (lambda () (parse-prop '(or true))))

;;Tests P1.c
(test (from-Pvalue (ttV))(tt))
(test (from-Pvalue (ffV))(ff))

;;Tests P1.d
(test (p-subst (p-id 'x) 'x (tt) ) (tt))
(test (p-subst (p-id 'x) 'y (tt)) (p-id 'x))
(test (p-subst (p-where (p-id 'x) 'x (tt)) 'x (ff)) (p-where (p-id 'x) 'x (tt)))
(test (p-subst (p-where (p-id 'x) 'y (tt)) 'x (ff)) (p-where (ff) 'y (tt)))
(test (p-subst (p-not (p-id 'x)) 'x (ff)) (p-not (ff)))
(test (p-subst (p-where (p-id 'x) 'x (tt)) 'x (ff)) (p-where (p-id 'x) 'x (tt)))
(test (p-subst (p-where (p-id 'x) 'y (tt)) 'x (ff)) (p-where (ff) 'y (tt)))
(test (p-subst (p-and (list (p-id 'x) (p-id 'y))) 'x (tt)) (p-and (list (tt) (p-id 'y))))
(test (p-subst (p-not (p-id 'x)) 'x (tt)) (p-not (tt)))

;;Tests P1.e
; Pruebas de p-eval
(test (p-eval (tt)) (ttV))
(test (p-eval (ff)) (ffV))
(test (p-eval (p-not (tt))) (ffV))
(test (p-eval (p-not (ff))) (ttV))
(test (p-eval (p-and (list (tt) (tt)))) (ttV))
(test (p-eval (p-and (list (tt) (ff)))) (ffV))
(test (p-eval (p-or (list (tt) (ff)))) (ttV))
(test (p-eval (p-or (list (ff) (ff)))) (ffV))
(test (p-eval (p-where (tt) 'x (tt))) (ttV))
(test (p-eval (p-where (ff) 'y (tt))) (ffV))
(test (p-eval (p-where (p-and (list (p-id 'x) (tt))) 'x (tt)))(ttV))
; Combinaciones
(test (p-eval (p-and (list (p-or (list (tt) (ff))) (p-not (ff))))) (ttV))
(test (p-eval (p-where (p-and (list (p-where (tt) 'x (tt)) (ff))) 'y (tt))) (ffV))
; Pruebas de interprete
(test (p-eval (parse-prop '(not true))) (ffV))
(test (p-eval (parse-prop '(not false))) (ttV))
(test (p-eval (parse-prop '(and true true))) (ttV))
(test (p-eval (parse-prop '(and true false))) (ffV))
(test (p-eval (parse-prop '(or true false)))(ttV))
(test (p-eval (parse-prop '(or false false))) (ffV))
(test (p-eval (parse-prop '(true where [x true]))) (ttV))

;;Tests P2.b
(test (parse '1) (real 1))
(test (parse '42) (real 42))
(test (parse '(1 i ))(imaginary 1))
(test (parse '(0 i))(imaginary 0))
(test (parse '(+ 1 (2 i ))) (add (real 1) (imaginary 2)))
(test (parse '(+ (3 i) (4 i)))(add (imaginary 3) (imaginary 4)))
(test (parse '(- (2 i) (1 i))) (sub (imaginary 2) (imaginary 1)))
(test (parse '(- 7 2))(sub (real 7) (real 2)))
(test (parse '(if0 0 1 2))(if0 (real 0) (real 1) (real 2)))
(test (parse '(if0 (1 i) (2 i) (3 i))) (if0 (imaginary 1) (imaginary 2) (imaginary 3)))
(test (parse '(with [(x 1) (y 2)] (+ x y)))(with (list (cons 'x (real 1)) (cons 'y (real 2))) (add (id 'x) (id 'y))))
(test (parse '(with [(x 1)] (+ x 1))) (with (list (cons 'x (real 1))) (add (id 'x) (real 1))))
;Excepcion
(check-exn
  (lambda (e) (string=? (exn-message e) "parse: 'with' expects at least one definition"))
  (lambda () (parse '(with [ ] 1))))

;;Tests P2.c
;; Test para CValue
(test (from-CValue (compV 1 0)) (real 1))
(test (from-CValue (compV 0 1)) (imaginary 1))
(test (from-CValue (compV 1 1)) (add (real 1)(imaginary 1)))
(test (from-CValue (compV 0 0)) (real 0))
;;Test para cmplx+
(test (cmplx+ (compV 1 2) (compV 3 4)) (compV 4 6))
(test (cmplx+ (compV -1 -1) (compV 1 1)) (compV 0 0))
(test (cmplx+ (compV 0 0) (compV 1 1)) (compV 1 1))
(test (cmplx+ (compV 5 0) (compV 0 5)) (compV 5 5))
;;Tests para cmplx-
(test (cmplx- (compV 5 6) (compV 3 2)) (compV 2 4))
(test (cmplx- (compV 0 0) (compV 1 1)) (compV -1 -1))
(test (cmplx- (compV 1 1) (compV 0 0)) (compV 1 1))
(test (cmplx- (compV 5 5) (compV 5 5)) (compV 0 0))

;;Tests para complx0?
(test (cmplx0? (compV 0 0)) #t)
(test (cmplx0? (compV 1 0)) #t)
(test (cmplx0? (compV 0 1)) #f)
(test (cmplx0? (compV 1 1)) #f)

;;Tests P2.d
;;Test para funcion shadow
(test (shadow '() 'x) #t)
(test (shadow '((x 1)) 'x) #f) 
(test (shadow '((y 2) (x 1)) 'x) #f)
(test (shadow '((y 2) (z 3)) 'x) #t) 
(test (shadow '((x 1) (y 2)) 'y) #f)
(test (shadow '((z 3) (y 2) (x 1)) 'x) #f) 
(test (shadow '((a 1) (b 2) (c 3)) 'd) #t)
;;Tests para funcion defs-sub
(test (defs-sub '() 'x (real 1)) '())
(test (defs-sub (list (cons 'z (id'y))) 'y (real 2))
      (list (cons 'z (real 2))))
(test (defs-sub (list (cons 'x (id 'z))) 'y (real 2))
      (list (cons 'x (id'z)))) 
(test (defs-sub (list (cons 'x (id 'z)) (cons 'a (id 'y))) 'y (real 2))
      (list (cons 'x (id 'z)) (cons 'a (real 2))))
(test (defs-sub (list (cons 'x (id 'y)) (cons 'z (id 'y))) 'y (real 3))
      (list (cons 'x (real 3)) (cons 'z (real 3)))) 
;;Tests para subst
(test (subst (parse '(with [(x 2) (y z)] (+ x z))) 'z (real 1))
      (with (list (cons 'x (real 2)) (cons 'y (real 1))) (add (id 'x) (real 1))))
(test (subst (parse '(with [(x 2) (y x)] (+ x x))) 'x (real 1))
      (with (list (cons 'x (real 2)) (cons 'y (id 'x))) (add (id 'x) (id 'x))))
(test (subst (parse '(with [(x 2)] (if0 x 1 2))) 'x (real 1))
      (with (list (cons 'x (real 2))) (if0 (id 'x) (real 1) (real 2))))
(test (subst (parse '(with [(x 2) (y x)] (+ y z))) 'z (real 3))
      (with (list (cons 'x (real 2)) (cons 'y (id 'x))) (add (id 'y) (real 3))))
(test (subst (parse '(with [(x 2) (y z)] (+ x z))) 'z (real 1))
      (with (list (cons 'x (real 2)) (cons 'y (real 1))) (add (id 'x) (real 1))))
(test (subst (parse '(with [(x 2) (y x)] (+ x x))) 'x (real 1))
      (with (list (cons 'x (real 2)) (cons 'y (id 'x))) (add (id 'x) (id 'x))))

;;Tests p2.e
;;Tests interp-defs
(test (interp-defs '() (real 5)) (compV 5 0))
(test (interp-defs (list (cons 'x (real 2))) (id 'x)) (compV 2 0))
(test (interp-defs (list (cons 'x (imaginary 3))) (id 'x)) (compV 0 3 ))
(test (interp-defs (list (cons 'x (real 2)) (cons 'y (real 3))) (add (id 'x) (id 'y))) (compV 5 0))
(test (interp-defs (list (cons 'x (real 0))) (if0 (id 'x) (real 1) (real 2))) (compV 1 0))
(test (interp-defs (list (cons 'a (real 3)) (cons 'b (imaginary 2))) (add (id 'a) (id 'b))) (compV 3 2))



;;Tests interp
(test (interp (real 5)) (compV 5 0))
(test (interp (parse 5)) (compV 5 0))
(test (interp (parse '(+ 5 5))) (compV 10 0))
(test (interp (parse '(+ (2 i) (2 i)))) (compV 0 4))
(test (interp (parse '(+ (2 i) 4)) ) (compV 4 2))
(test (interp (sub (real 7) (real 4))) (compV 3 0))
(test (interp (sub (add (real 10) (imaginary 5)) (real 2))) (compV 8 5))
(test (interp (if0 (real 0) (real 1) (real 2))) (compV 1 0))
(test (interp (if0 (imaginary 1) (real 1) (real 2))) (compV 2 0))
(test (interp (add (sub (real 5) (real 3)) (imaginary 4))) (compV 2 4))
(test (interp (with (list (cons 'x (real 3))) (add (id 'x) (real 4)))) (compV 7 0))
(test (interp (with (list (cons 'x (imaginary 1))) (add (id 'x) (imaginary 2)))) (compV 0 3))
(test (interp (if0 (add (real 1) (sub (real 1) (real 1))) (real 1) (real 2))) (compV 1 0))
(test (interp (with (list (cons 'a (real 3)) (cons 'b (imaginary 2))) (add (id 'a) (id 'b)))) (compV 3 2))
(test (interp (with (list (cons 'x (real 0))) (if0 (id 'x) (real 1) (real 2))) ) (compV 1 0))