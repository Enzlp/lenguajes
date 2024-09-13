#lang play
(require math/flonum)

#|
Complete sus datos personales: 
NOMBRE Y APELLIDO: ENZO LOPEZ
RUT: 20445626-7
|#

;; Parte a)
#|Gramática BNF
<CFraction> ::= (simple <num>)
              | (compound <num> <num> <CFraction>)
|#
;; Corresponde al constructor de una fraccion continua, que son fracciones anidadas que poseen grado de anidacion, y representan numeros racionales
(deftype CFraction
  (simple a)
  (compound a b d))


;; Parte b)
;; eval :: CFraction -> Rational
;; Computa el equivalente numero racional que representa una fraccion continua
(define (eval cf)
  (match cf
      [(simple a) a]
      [(compound a b d) (+ a (/ b (eval d)))]))


;; Parte c)
;; degree ::  CFraction -> Integer
;; Computa el grado de la funcion continua dada, es decir el nivel de anidacion de la fraccion
(define (degree cf)
  (match cf
    [(simple a) 0]
    [(compound a b d) (+ 1 (degree d))]))

;; Parte d)
;; fold-cfraction :: (Integer -> A) (Integer Integer A -> A) -> (CFraction -> A)
;; Aplica funciones fold sobre los tipos de datos numericos que integran un fraccion compuesta
(define (fold-cfraction f-simple f-compound)
  (lambda (cf)
    (match cf
      [(simple a) (f-simple a)]
      [(compound a b d) (f-compound a b ((fold-cfraction f-simple f-compound )d))])))

;; Parte e)
;; eval2 :: CFraction -> Rational
;; Computa el equivalente numero racional que representa una fraccion continua, usando la funcion fold
(define eval2
  (fold-cfraction (λ (a) a)
                  (λ (a b d) (+ a (/ b d)))))

;; degree2 ::  CFraction -> Integer
;; Calcula el grado de anidacion de la funcion continua usando la funcion fold, que aplica funciones a cada uno de los componentes de la cfraction
(define degree2
  (fold-cfraction (λ (a) 0)
                  (λ (a b d) (+ 1 d))))

;; Parte f)
;; Funcion Auxiliar
;; cf-builder :: Integer Integer -> CFraction
;; funcion que toma un numero n, y retorna la fraccion continua en la posicion equivalente de la secuencia mysterious-cf
(define (cf-builder n cf)
  (match n
    [0 cf]
    [else
     (cf-builder (- n 1)(compound 6 (sqr (+ (* 2 (- n 1)) 1)) cf))]))

;; mysterious-cf :: Integer -> CFraction
;; Genera una fraccion anidada de un tipo especial, basado en el numero proveido, propio de una secuencia de fracciones continuas que llamamos mysterious-cf
(define (mysterious-cf n)
  (match n
    [(? negative?) (error "Error: argumento negativo")]
    [0 (simple 6)]
    [else (cf-builder n (simple 6))]))


;; Parte g)
;; from-to :: Integer Integer -> ListOf Integer
;; Dados dos enteros construye una lista de los enteros comprendidos entre ellos
(define (from-to x y)
  (if (>= x y) '()
  (cons x (from-to (+ x 1) y))))

;; mysterious-list :: Integer -> ListOf Float
;; Dado un numero n, genera una una lista tal que el i-ésimo elemento es calculado como la resta de la evaluación de (mysterious-cf i ) menos 3
(define (mysterious-list n)
  (map
   (lambda (x) (fl (- (eval (mysterious-cf x)) 3 )))
   (from-to 0 n)))

;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?
;; Cuando K tiende a infinto, el numero generado por mysterious-cf tiene a pi

;; Parte h)
;; rac-to-cf :: Rational -> CFraction
;; Calcula la representacion en fraccion continua de un numero racional
(define (rac-to-cf r)
  (define i (floor r))
  (define f (- r i))
  (if (equal? f 0)
      (simple i)
      (compound i 1 (rac-to-cf (/ 1 f)))))