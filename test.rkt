#lang play
(require "T2.rkt")

(print-only-errors #t)

;;Tests P1.b
(test (parse-prop ' (not true)) (p-not (tt)))
(test (parse-prop ' (and true false true)) (p-and ( list (tt) ( ff ) (tt))))
(test (parse-prop ' (or true false )) (p-or ( list (tt) ( ff ))))
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
(test (from-CValue (compV 1 0)) (real 1))
(test (from-CValue (compV 0 1)) (imaginary 1))
(test (from-CValue (compV 1 1)) (add (real 1)(imaginary 1)))
(test (cmplx+ (compV 1 2) (compV 3 4)) (compV 4 6))
(test (cmplx- (compV 1 2) (compV 3 4)) (compV -2 -2))
(test (cmplx0? (compV 0 1)) #f)
(test (cmplx0? (compV 1 0)) #t)

;;Tests P2.d



;;Tests P2.e