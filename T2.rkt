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
;; Syntaxis concreto de proposiciones, es el formato en el que se leera
;; las operaciones pedidas

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
;; PValue representa valores booleanos de true o false como estructura de datos.
(deftype PValue
  (ttV)
  (ffV))


;; from-Pvalue : PValue -> Prop
;; from-Pvalue convierte un valor PValue en su representación Prop o en proposición.
(define (from-Pvalue p-value)
  (match p-value
    [(ttV) (tt)]
    [(ffV) (ff)]))


;;----- ;;
;; P1.d ;;
;;----- ;;


;; p-subst : Prop Symbol Prop -> Prop
;; p-subst sustituye un identificador en una proposición por un nuevo valor. Acepta la proposición, el identificador y el nuevo valor. Realiza la 
;; sustitución si hay coincidencias, de lo contrario, devuelve la proposición original.
(define (p-subst target name substitution)
  (match target
    [(p-id x)
     (if (symbol=? x name)
         substitution
         (p-id x))]
    [(p-not p)
     (p-not (p-subst p name substitution))]
    [(p-and p)
     (p-and (cons (p-subst (first p) name substitution) 
           (map (lambda (elem) (p-subst elem name substitution)) 
                (rest p))))]
    [(p-or p)
     (p-or (cons (p-subst (first p) name substitution) 
                 (map (lambda (elem) (p-subst elem name substitution)) 
                      (rest p))))]
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
;; Interprete, toma una proposicion en formato Prop, ya pasada por Parse y evalua,
;; retornando el Pvalue una vez reducido
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
;; Estructura de datos, representa una expresión, compuesta de valores reales
;; imaginarios, o combinaciones usando las operaciones de add, sub, not, definiciones
;; locales usando with y variables representadas con id
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
;; Syntaxis concreto de Expr, define como van a ser ingresado en nuestro lenguaje
;; las peticiones

;; parse : <s-expr> -> Expr
;; Parser, se encarga de tomar la expresion en "lenguaje natural" o syntaxis concreto y
;; transformarlo en formato de la estructura de datos Expr definida arriba para ser leida por el interprete
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
;; cvalue, estructura de datos que representa los valores complejos compuestos por una parte real y una parte compleja
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
;; cmplx+ toma de argumento dos numeros complejos de tipo CValue y los suma retornando el Cvalue resultante
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

;; Funcion auxiliar
;; shadow :: ListOf (Symbol Prop) Symbol -> Boolean
;; shadow determina si hay variables que han sido "obscurecidas" por otras,
;; si es así entoces retorna falso, sino retorna verdadero
(define (shadow defs what)
  (match defs
    ['() #t] 
    [(list (cons x v) elems ...) 
     (if (symbol=? x what) 
         #f 
         (shadow elems what))])) 

;; Funcion auxiliar
;; defs-sub :: ListOf(Symbol Prop) Symbol Prop -> ListOf (Pair Symbol Prop)
;; defs-sub toma una lista de pares de variables y valores, y substituye el valor for del
;; simbolo what, en el valor que se use como indentificador libre. 
(define (defs-sub defs what for)
  (match defs
    ['() '()] 
    [(list (cons var val) rest ...)
     (if (and (symbol? val) (symbol=? val what))
         (if (null? rest)
             (list (cons var for))
             (cons (cons var for) (defs-sub rest what for)))
         (cons (cons var (subst val what for)) (defs-sub rest what for)))]
    ))

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
     (if (shadow defs what)
         (with (defs-sub defs what for)(subst expr what for))
         in)]))

;;----- ;;
;; P2.e ;;
;;----- ;;

;; Funcion auxiliar
;; interp-defs :: ListOf(Symbol Prop) Expr -> Result
;; La funcion interp-defs toma una lista de pares varuables y valores, y substituye
;; estos valores en la Expr body, una vez substituidos todos retorna el resultado de
;; body interpretado llamando a la funcion interp
(define (interp-defs defs body)
  (match defs
    ['() (interp body)]  
    [(list (cons var val) elems ...)
     (interp-defs elems (subst body var val))]))  

;; interp : Expr -> CValue
;; interp es un interprete, se ocupa de evaluar la expresion dada por el parser
;; en formato Expr reduciendola a un valor del tipo compV
(define (interp expr)
  (match expr
    [(real r) (compV r 0)]
    [(imaginary i) (compV 0 i)]
    [(add l r) (cmplx+ (interp l) (interp r))]
    [(sub l r) (cmplx- (interp l) (interp r))]
    [(if0 c-expr t-expr f-expr)
     (if (cmplx0? (interp c-expr))
         (interp t-expr)
         (interp f-expr))]
    [(with defs body)
      (interp-defs defs body)] 
    [(id x) (error 'interp "Open expression (free occurrence of ~a)" x)]))
