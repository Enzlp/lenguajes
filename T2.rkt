#lang play

#| 
NOMBRE Y APELLIDO: ENZO LOPEZ
RUT: 20445626-7
|#

#|

Hizo Ud uso de la whiteboard policy: NO
En caso que afirmativo, indique con quién y sobre qué ejercicio:
-
-

|#

;;------------ ;;
;;==== P1 ==== ;;
;;------------ ;;


;;----- ;;
;; P1.a ;;
;;----- ;;


#| Gramática BNF
<prop> ::= (tt)
         | (ff)
         | (p-id <sym>)
         | (p-not <prop>)
         | (p-and <prop> <prop> ...)
         | (p-or <prop> <prop> ...)
         | (p-where <prop> <sym> <prop>)
|#
;; Prop representa una proposición lógica o booleana, que puede componerse por un booleano simple o por una expresion compuesta de
;; varios operadores booleanos o definiciones locales.

(deftype Prop
  (tt)
  (ff)
  (p-id x)
  (p-not b)
  (p-and props)
  (p-or props)
  (p-where b1 id b2))


;;----- ;;
;; P1.b ;;
;;----- ;;

#|
Concrete syntax of propositions:

<s-prop> ::= true
          | false
          | <sym>
          | (list 'not <s-prop>)
          | (list 'and <s-prop> <s-prop>)
          | (list 'or <s-prop> <s-prop>)
          | (list <s-prop> 'where (<sym> <s-prop>))

|#

;; parse-prop : <s-prop> -> Prop
;; Parser para las proposiciones, toma el input en formato "normal" para el usuario y lo transforma en un formato que lo pueda leer nuestra estructura
;; de dato prop
(define (parse-prop s-expr)
  (match s-expr
    ['true (tt)]
    ['false (ff)]
    [(? symbol?) (p-id s-expr)]
    [(list 'not prop) (p-not (parse-prop prop))]
    [(list 'and x elems ...) 
     (if (or (null? elems) (null? x))
         (error 'and "expects at least two operands")
         (p-and (cons (parse-prop x) (map parse-prop elems))))]
    [(list 'or x elems ...)
     (if (or (null? elems) (null? x))
         (error 'or "expects at least two operands")
         (p-or (cons (parse-prop x) (map parse-prop elems))))]
    [(list p 'where (list id expr))
     (p-where (parse-prop p) id (parse-prop expr))]))


;;----- ;;
;; P1.c ;;
;;----- ;;


#|
<value> ::= (ttV)
        | (ffV)
|#

(deftype PValue
  (ttV)
  (ffV))

;; from-Pvalue : PValue -> Prop
(define (from-Pvalue p-value) '???)


;;----- ;;
;; P1.d ;;
;;----- ;;


;; p-subst : Prop Symbol Prop -> Prop
;; Sustituye un identificador en una proposición por un nuevo valor. Acepta la proposición, el identificador y el nuevo valor, y realiza la
;; sustitución si hay coincidencias. Si no encuentra coincidencias, devuelve la proposición original.
(define (p-subst target name substitution)
  (match target
    [(p-id x)
     (if (symbol=? x name)
         substitution
         (p-id x))]
    [(p-where cond id body)
     (p-where 
      (p-subst cond name substitution) 
      id
      body)]  
    [_ target]))


;;----- ;;
;; P1.e ;;
;;----- ;;


;; eval-or : (Listof Prop) -> PValue
(define (eval-or ps) '???)

;; eval-and : (Listof Prop) -> PValue
(define (eval-and ps) '???)

;; p-eval : Prop -> PValue
(define (p-eval p) '???)

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

;;----- ;;
;; P2.a ;;
;;----- ;;


#|
<expr> ::= (real <num>)
        | (imaginary <num>)
        | (add <expr> <expr>)
        | (sub <expr> <expr>)
        | (if0 <expr> <expr> <expr>)
        | (with <sym> <expr> <expr>)
        |(id <sym>)
|#
(deftype Expr
  (real a)
  (imaginary b)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with x named-expr body)
  (id x))

;;----- ;;
;; P2.b ;;
;;----- ;;

#|
Concrete syntax of expressions:

<s-expr> ::= <num>
        | (<num> 'i)
        | (+ <s-expr> <s-expr>)
        | (- <s-expr> <s-expr>)
        | (if0 <s-expr> <s-expr> <s-expr>)
        | (list 'with [(list <sym> <s-expr>)*] <s-expr>)
        | <sym>
|#

;; parse : <s-expr> -> Expr
;; Parser, se encarga de tomar la expresion en "lenguaje natural" y transformarlo en formato de la estructura de datos Expr definida arriba
(define (parse s-expr)
  (match s-expr
    [(? number? a) (real a)]
    [(list b 'i) (imaginary b)]
    [(? symbol? x) (id x)]
    [(list '+ l-sexpr r-sexpr) (add (parse l-sexpr)(parse r-sexpr))]
    [(list '- l-sexpr r-sexpr)(sub (parse l-sexpr)(parse r-sexpr))]
    [(list 'if0 c-sexpr t-sexpr f-sexpr)
     (if0 (parse c-sexpr) (parse t-sexpr) (parse f-sexpr))]))

;;----- ;;
;; P2.c ;;
;;----- ;;

;; subst :: Expr Symbol Expr -> Expr
(define (subst in what for) '???)

;;----- ;;
;; P2.d ;;
;;----- ;;

#|
<cvalue> ::= (compV <num> <num>)
|#

(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
(define (from-CValue v) '???)

;; cmplx+ :: CValue CValue -> CValue
(define (cmplx+ v1 v2) '???)

;; cmplx- :: CValue CValue -> CValue
(define (cmplx- v1 v2) '???)

;; cmplx0? :: CValue -> Boolean
(define (cmplx0? v) '???)


;;----- ;;
;; P2.e ;;
;;----- ;;

;; interp : Expr -> CValue
(define (interp expr) '???)
