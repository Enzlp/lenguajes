#lang play
(require "T1.rkt")
(print-only-errors #t)

;; Test eval
;; Caso simple
(test (eval (simple 1)) 1)
;; Caso compuesto un nivel de anidación
(test (eval (compound 1 2 (simple 3))) (+ 1 (/ 2 3)))
;; Caso compuesto con múltiples niveles de anidación
(test (eval (compound 2 3 (compound 1 2 (simple 4)))) (+ 2 (/ 3 (+ 1 (/ 2 4)))))
;; Cado borde: Fraccion con 0 en la parte compuesta 
(test (eval (compound 5 0 (simple 3))) 5) 

;; Test Degree
;; Caso simple
(test (degree (simple 1)) 0)
;; Caso compuesto un grado de anidación
(test (degree (compound 1 2 (simple 3))) 1)
;; Caso compuesto múltiples grados de anidación
(test (degree (compound 1 2 (compound 1 2 (simple 3)))) 2)
;; Anidación Profunda
(test (degree (compound 1 1 (compound 2 1 (compound 3 1 (simple 4))))) 3)

;; Test fold-cfraction
(define f-simple (lambda (a) a))
(define f-compound (lambda (a b d) (+ a (/ b d))))
;; Caso simple
(test ((fold-cfraction f-simple f-compound) (simple 5)) 5)
;; Caso compuesto un nivel de anidación
(test ((fold-cfraction f-simple f-compound) (compound 1 2 (simple 3))) (+ 1 (/ 2 3)))
;; Caso compuesto varios niveles de anidación
(test ((fold-cfraction f-simple f-compound) (compound 1 2 (compound 1 2 (simple 3)))) (+ 1 (/ 2 (+ 1 (/ 2 3)))))
;; Funciones distintas para fold-cfraction
(define f-simple-2 (lambda (a) (* 2 a)))
(define f-compound-2 (lambda (a b d) (+ a d)))
(test ((fold-cfraction f-simple-2 f-compound-2) (compound 1 2 (simple 3))) 7)

;; Test eval2
;; Caso simple
(test (eval2 (simple 1)) 1)
;; Caso compuesto un nivel de anidación
(test (eval2 (compound 1 2 (simple 3))) (+ 1 (/ 2 3)))
;; Caso compuesto con múltiples niveles de anidación
(test (eval2 (compound 2 3 (compound 1 2 (simple 4)))) (+ 2 (/ 3 (+ 1 (/ 2 4)))))
;; Caso Borde: cero en el numerador
(test (eval2 (compound 7 0 (simple 5))) 7)

;; Test degree2
;; Caso simple
(test (degree2 (simple 1)) 0)
;; Caso compuesto un grado de anidación
(test (degree2 (compound 1 2 (simple 3))) 1)
;; Caso compuesto múltiples grados de anidación
(test (degree2 (compound 1 2 (compound 1 2 (simple 3)))) 2)
;; Anidación Profunda
(test (degree2 (compound 1 1 (compound 2 1 (compound 3 1 (simple 4))))) 3)

;; Test cf-builder
;; Caso simple
(test (cf-builder 1 (simple 6)) (compound 6 1 (simple 6)))
;; Caso para varios n
(test (cf-builder 2 (simple 6)) (compound 6 1 (compound 6 9 (simple 6))))
(test (cf-builder 3 (simple 6)) (compound 6 1 (compound 6 9 (compound 6 25 (simple 6)))))
;; Numero grande para n
(test (cf-builder 5 (simple 6)) (compound 6 1 (compound 6 9 (compound 6 25 (compound 6 49 (compound 6 81 (simple 6)))))))

;; Test mysterious-cf
;; Caso simple
(test (mysterious-cf 0) (simple 6))
;; Caso para múltiples n
(test (mysterious-cf 1) (compound 6 1 (simple 6)))
(test (mysterious-cf 2) (compound 6 1 (compound 6 9 (simple 6))))
;; Valores grandes
(test (mysterious-cf 3) (compound 6 1 (compound 6 9 (compound 6 25 (simple 6)))))

;; Test from-to
;; Caso base
(test (from-to 0 0) '())
;; Caso fallido
(test (from-to 5 3) '())
;; Caso para varios x e y
(test (from-to 0 1) (list 0))
(test (from-to 0 3) (list 0 1 2))
;; Gran rango
(test (from-to 1 5) (list 1 2 3 4))

;; Test mysterious-list
;; Caso base
(test (mysterious-list 0) '())
;; Caso para varios n
(test (mysterious-list 1) (list 3.0))
(test (mysterious-list 2) '(3.0 3.1666666666666665))
(test (mysterious-list 3) (list 3.0 3.1666666666666665 3.1333333333333333))

;; Test rac-to-cf
;; Caso base
(test (rac-to-cf 1) (simple 1))
;; Caso compuesto para formato racional (fracción)
(test (rac-to-cf (+ 3 49/200)) (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))
;; Racional complejo
(test (rac-to-cf 22/7) (compound 3 1 (simple 7)))
