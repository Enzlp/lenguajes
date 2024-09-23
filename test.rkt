#lang play
(require "T2.rkt")

(print-only-errors #t)

;;Tests P1.b
(test (parse-prop ' (not true)) (p-not (tt)))
(test (parse-prop ' (and true false true)) (p-and ( list (tt) ( ff ) (tt))))
(test (parse-prop ' (or true false )) (p-or ( list (tt) ( ff ))))
(test (parse-prop '(false where [x true] )) (p-where (ff) 'x (tt)))
(test (parse-prop '(x where [x true] )) (p-where (p-id 'x) 'x (tt)))

;;Tests P1.c



;;Tests P1.d
(test (p-subst (p-id 'x) 'x (tt) ) (tt))
(test (p-subst (p-id 'x) 'y (tt)) (p-id 'x))
(test (p-subst (p-where (p-id 'x) 'x (tt)) 'x (ff)) (p-where (p-id 'x) 'x (tt)))
(test (p-subst (p-where (p-id 'x) 'y (tt)) 'x (ff)) (p-where (ff) 'y (tt)))

;;Tests P1.e


;;Tests P2.a
(test (parse '1) (real 1))
(test (parse ' (1 i ))(imaginary 1))
(test (parse ' (+ 1 (2 i ))) (add (real 1) (imaginary 2)))


;;Tests P2.b
;;Tests P2.c
;;Tests P2.d
;;Tests P2.e