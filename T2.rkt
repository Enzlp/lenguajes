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
;; La estructura Prop representa proposiciones lógicas o booleanas, que pueden ser simples (como true o false), símbolos (p-id), 
;; o combinaciones usando operadores como negación, and, or y definiciones locales.

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
;; parse-prop convierte expresiones legibles o "naturales" a la estructura Prop, para poder ser luego interpretado. Usa match 
;; para manejar los distintos casos, y posee excepciones para entradas inválidas.
(define (parse-prop s-expr)
  (match s-expr
    ['true (tt)]
    ['false (ff)]
    [(? symbol?) (p-id s-expr)]
    [(list 'not prop) (p-not (parse-prop prop))]
    [(list 'and x elems ...) 
     (if (or (null? elems) (null? x))
         (error "parse-prop: and expects at least two operands")
         (p-and (cons (parse-prop x) (map parse-prop elems))))]
    [(list 'or x elems ...)
     (if (or (null? elems) (null? x))
         (error "parse-prop: or expects at least two operands")
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
;; PValue representa valores booleanos como estructura.
(deftype PValue
  (ttV)
  (ffV))


;; from-Pvalue : PValue -> Prop
;; from-Pvalue convierte un PValue en su representación Prop.
(define (from-Pvalue p-value)
  (match p-value
    [(ttV) (tt)]
    [(ffV) (ff)]))


;;----- ;;
;; P1.d ;;
;;----- ;;


;; p-subst : Prop Symbol Prop -> Prop
;; p-subst sustituye un identificador en una proposición por un nuevo valor. Acepta la proposición, el identificador y el nuevo valor. Realiza la 
;; sustitución si hay coincidencias; de lo contrario, devuelve la proposición original.
(define (p-subst target name substitution)
  (match target
    [(p-id x)
     (if (symbol=? x name)
         substitution
         (p-id x))]
    [(p-where body id val)
     (if (symbol=? id name)
         target
         (p-where (p-subst body name substitution) id (p-subst val name substitution)))]  
    [_ target]))


;;----- ;;
;; P1.e ;;
;;----- ;;


;; eval-or : (Listof Prop) -> PValue
;; Funcion auxiliar eval-or, determina si hay una proposicion dentro de la lista que reduzca a true en cuyo caso debe reducir la expresion completa a true,
;; es decir un cortocircuito
(define (eval-or ps)
  (match ps
    ['() (ffV)]
    [(list first elems ...)
     (match first
       [(tt) (ttV)]
       [else (eval-or elems)])]))

;; eval-and : (Listof Prop) -> PValue
;; Funcion auxiliar eval-or, determina si hay una proposicion dentro de la lista que reduzca a false en cuyo caso debe reducir la expresion completa a false,
;; es decir un cortocircuito
(define (eval-and ps)
  (match ps
    ['() (ttV)]
    [(list first elems ...)
     (match first
       [(ff) (ffV)]
       [else (eval-and elems)])]))

;; p-eval : Prop -> PValue
;; Evalua una proposicion y retorna se Pvalue reducido
(define (p-eval p)
  (match p
    [(tt) (ttV)]
    [(ff) (ffV)]
    [(p-id x) (error "p-eval: Open expression (free occurrence of ~a)" x)]
    [(p-not prop)
     (match (p-eval prop)
       [(ttV) (ffV)]
       [(ffV) (ttV)])]
    [(p-and ps) (eval-and ps)]  
    [(p-or ps) (eval-or ps)]
    [(p-where body id val)
     (p-eval (p-subst body id val))]))  


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
  (with defs body)
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
     (if0 (parse c-sexpr) (parse t-sexpr) (parse f-sexpr))]
    [(list 'with defs body)
     (if (null? defs)
         (error "parse: 'with' expects at least one definition")
         (with (map (lambda (x)
                      (match x
                        [(list name val) (cons name (parse val))]))
                    defs)
               (parse body)))]))


;;----- ;;
;; P2.c ;;
;;----- ;;

#|
<cvalue> ::= (compV <num> <num>)
|#
;; Representa los valores complejos compuestos por una parte real y una parte compleja
(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
;; Convierte un elemento Cvalue en una Expr
(define (from-CValue v)
  (match v
    [(compV r i)  
     (if (= i 0)
         (real r)                 
         (if (= r 0)
             (imaginary i)            
             (add (real r) (imaginary i))))]))  


;; cmplx+ :: CValue CValue -> CValue
;; cmplx+ toma de argumento dos complejos de tipo CValue y los suma retornando el Cvalue resultante
(define (cmplx+ v1 v2)
  (match v1
    [(compV r1 i1)
     (match v2
       [(compV r2 i2)
        (compV (+ r1 r2) (+ i1 i2))])]))

;; cmplx- :: CValue CValue -> CValue
;; cmplx- toma de argumento dos complejos de tipo CValue y los resta retornando el Cvalue resultante
(define (cmplx- v1 v2)
  (match (list v1 v2)
    [(list (compV r1 i1) (compV r2 i2))
     (compV (- r1 r2) (- i1 i2))]))

;; cmplx0? :: CValue -> Boolean
;; cmplx0 verifica si el numero es real o complejo, es decir retorna true si tiene un numero imaginario 0
(define (cmplx0? v)
  (match v
  [(compV r i)
   (if (= i 0)
       #t
       #f)]))

;;----- ;;
;; P2.d ;;
;;----- ;;

;; subst :: Expr Symbol Expr -> Expr
;; subst substituye todas las ocurrencias libres del identificador escogido en al expresion que llamaremos in, por la expresion
;; encerrada en for
(define (subst in what for)
  (match in
    [(real r) (real r)]
    [(imaginary i) (imaginary i)]
    [(add l r) (add (subst l what for) (subst r what for))]
    [(sub l r) (sub (subst l what for) (subst r what for))]
    [(if0 c t f) (if0 (subst c what for)
                      (subst t what for)
                      (subst f what for))]
    [(id x)
     (if (symbol=? x what)
         for
         (id x))]
    [(with defs expr)
     (if (def-sub defs what)
         (with )
         in)]))


;; Funcion auxiliar
(define (shadow defs what)
  (match defs
    ['() #t]
    [(list first elems ...)
     (match first
       [(x v) (if (equals? x what)
                  #f
                  (def-sub elems what))])]))
;; Funcion auxiliar
(define (defs-sub defs what)
  (match defs
    []))

;;----- ;;
;; P2.e ;;
;;----- ;;

;; interp : Expr -> CValue
(define (interp expr) '???)
