#lang eopl

(define lexica
'((white-sp
   (whitespace) skip)
  (comment
   ("//" (arbno (not #\newline))) skip)
  (identificador
   (letter (arbno (or letter digit "?"))) symbol)
  (digitoBinario
   ("b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoBinario
   ("-?" "b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoDecimal
   (digit (arbno digit)) number)
  (digitoDecimal
   ("-" digit (arbno digit)) number)
  (digitoOctal
   ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7")(arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoOctal
   ("-" "0x" (or "0" "1" "2" "3" "4" "5" "6" "7") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoHexadecimal
   ("hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (digitoHexadecimal
   ("-?" "hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (flotante
   (digit (arbno digit) "." digit (arbno digit)) number)
  (flotante
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  ))

(define gramatica
  '(
    (programa ((arbno struct-decl) expresion) a-programa)
    (expresion (bool-expresion) bool-exp)
    (expresion (identificador) var-exp)
    (expresion (numero-exp) num-exp)    
    (expresion ("\"" identificador (arbno identificador) "\"") cadena-exp)
    (expresion (var-decl) decl-exp)

    ;;Listas y arrays
    (expresion ("list" "(" (separated-list expresion ",") ")") lista-exp)
    (expresion ("cons" "(" expresion expresion ")") cons-exp)
    (expresion ("empty") empty-list-exp)
    (expresion ("array" "(" (separated-list expresion ",") ")") array-exp)

    ;;Expresion primitivas
    ;;Primitiva numerica
    (expresion ("(" expresion primitiva expresion ")") prim-num-exp)
    ;;Primitiva booleana
    (expresion (primitivaBooleana "(" (separated-list expresion ",") ")") prim-bool-exp)
    ;;Primitiva listas
    (expresion (primitivaListas "(" expresion ")") prim-list-exp)
    ;;; ;;Primitiva array
    (expresion (primitivaArray "(" (separated-list expresion ",") ")") prim-array-exp)
    ;;Primitiva de cadenas
    (expresion (primitivaCadena "(" (separated-list expresion ",") ")") prim-cad-exp)


    ;;Condicionales
    (expresion ("if" expresion "{" expresion "else" expresion "}") if-exp)


    ;;Iteradores
    (expresion ("for" identificador "from" expresion "until" expresion "by" expresion "do" expresion) for-exp)
    (expresion ("while" expresion "{" expresion "}") while-exp)

    ;;Switch
    (expresion ("switch" "(" expresion ")" "{" (arbno "case" expresion ":" expresion) "default" ":" expresion "}") switch-exp)

    ;;Secuenciación y asignación
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador "=" expresion) set-exp)

    ;;Funciones
    ;(expresion ("func" "(" (separated-list identificador ",") ")" expresion) func-exp)
    ;(expresion ("call" expresion "(" (separated-list expresion ",") ")") call-exp)

    ;;Instanciación y uso de estructuras
    ;;; (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-struct-exp)
    ;;; (expresion ("get" expresion "." identificador) get-struct-exp)
    ;;; (expresion ("set-struct" expresion "." identificador "=" expresion) set-struct-exp)

    ;;Reconocimiento de patrones
    ;;; (expresion ("match" expresion "{" (arbno regular-exp "=>" expresion) "}") match-exp)

    ;;Numero-exp
    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoHexadecimal) hex-num)
    (numero-exp (flotante) float-num)
    
    ;;Bool-exp
    (bool-expresion ("true") true-exp)
    (bool-expresion ("false") false-exp)

    ;;primitivas numéricas
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("mod") mod-prim)
    (primitiva ("pow") elevar-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<=") menorigual-prim)
    (primitiva (">=") mayorigual-prim)
    (primitiva ("!=") diferente-prim)
    (primitiva ("==") igual-prim)

    ;;primitiva booleana
    (primitivaBooleana ("and") and-prim)
    (primitivaBooleana ("or") or-prim)
    (primitivaBooleana ("xor") xor-prim)
    (primitivaBooleana ("not") not-prim)

    ;;Primitiva listas
    (primitivaListas ("first") first-primList)
    (primitivaListas ("rest") rest-primList)
    (primitivaListas ("empty?") empty-primList)

    ;;Primitiva arrays
    (primitivaArray ("length") length-primArr)
    (primitivaArray ("index") index-primArr)
    (primitivaArray ("slice") slice-primArr)
    (primitivaArray ("setlist") setlist-primArr)

    ;;Primitiva cadenas
    (primitivaCadena ("concat") concat-primCad)
    (primitivaCadena ("string-length") length-primCad)
    (primitivaCadena ("elementAt") index-primCad)
    
    ;;Variables
    (var-decl ("var" (arbno identificador "=" expresion) "in" expresion) lvar-exp)
    (var-decl ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)
    ;void
    (expresion ("void") void-exp)
    ;;Estructuras de datos
    (struct-decl ("struct" identificador "{" (arbno identificador) "}") struct-exp)

    ;;Expresiones regulares
    ;;; (regular-exp (identificador "::" identificador) list-match-exp)
    ;;; (regular-exp ("numero" "(" identificador ")") num-match-exp)
    ;;; (regular-exp ("cadena" "(" identificador ")") cad-match-exp)
    ;;; (regular-exp ("boolean" "(" identificador ")") bool-match-exp)
    ;;; (regular-exp ("array" "(" (separated-list identificador ",") ")") array-match-exp)
    ;;; (regular-exp ("empty") empty-match-exp)
    ;;; (regular-exp ("default") default-match-exp)
    )
  )

(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))



;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))


;El Interpretador (FrontEnd + Evaluación + señal para lectura +
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program pgm))
    (sllgen:make-stream-parser 
      lexica
      gramatica)))

;*********************eval-program****************************************
;eval-program: <programa> -> numero

(define eval-program
  (lambda (pgm)
    (let ((struc '()))
      (define decl!
        (lambda (structs)
          (set! struc structs)))
      (cases programa pgm
        (a-programa (structs exp)
                    (decl! structs)
                    (eval-expression exp (init-env)))))))


(define init-env
  (lambda ()
    (extend-env
     '(x y z f)
     '()

     (empty-env))))


;;*********************eval-expression****************************************
(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      (void-exp () '())
      (num-exp (numero-exp) (eval-num-expresion numero-exp env))
      (bool-exp (bool-expresion) (eval-bool-expresion bool-expresion env))
      (var-exp (identificador) (apply-env env identificador))
      (cadena-exp (identificador args) (eval-cadena-expresion identificador args))
      (lista-exp (args) (map (lambda (x) (eval-expression x env)) args))
      (cons-exp (exp1 exp2) (cons (eval-expression exp1 env) (eval-expression exp2 env)))
      (empty-list-exp () '())
      (if-exp (exp1 exp2 exp3)(eval-if-expresion exp1 exp2 exp3 env))
      (decl-exp (var-decl) (eval-var-decl var-decl env))
      (prim-num-exp (exp1 primitiva exp2)(apply-primitive primitiva (list (eval-expression exp1 env) (eval-expression exp2 env))))
      (prim-bool-exp (primitivaBooleana args) (apply-primitive-bool primitivaBooleana (eval-rands args env)))
      (prim-cad-exp (primitivaCadena args) (apply-primitive-string primitivaCadena (eval-rands args env)))
      (prim-list-exp (primitivaListas arg) (apply-primitive-list primitivaListas (eval-rand arg env)))
      (array-exp (lista) (list->vector (eval-rands lista env)))
      (prim-array-exp (primitiva listaArray)(primitiva-array primitiva (eval-rands listaArray env)))
      
      (set-exp (id rhs-exp)
               (begin
                 (referencia
                  (apply-env-ref env id)
                  (eval-expression rhs-exp env))
                ))

      (begin-exp (exp exps) 
                  (let ((acc (eval-expression exp env)))  
                      (cond ((null? exps) acc)           
                    (else (eval-expression (car exps) env)))))

      (for-exp (identificador i from until body) (eval-for-expresion identificador i from until body env))
      (while-exp (exp body) (eval-while-expresion exp body env))
      (switch-exp (exp cases listaExp default) (eval-switch-expresion exp cases listaExp default env))   
      
    )
  )
)

;************************funciones del eval***************************
(define eval-num-expresion
  (lambda (exp env)
    (cases numero-exp exp
      (decimal-num (digitoDecimal) digitoDecimal)
      (octal-num (digitoOctal) digitoOctal)
      (bin-num (digitoBinario) digitoBinario)
      (hex-num (digitoHexadecimal) digitoHexadecimal)
      (float-num (flotante) flotante)
      (else (eopl:error 'eval-num-expresion "Numero invalido"))
    )
  )
)

(define eval-bool-expresion
  (lambda (exp env)
    (cases bool-expresion exp
      (true-exp () #t)
      (false-exp () #f)
      (else (eopl:error 'eval-bool-expresion "Booleano invalido"))
      
    )
  )
)

(define eval-cadena-expresion
  (lambda (identificador args)
    (let* ((cadena-id (symbol->string identificador))
           (cadenas-args (map symbol->string args)))
      (unir-cadenas (cons cadena-id cadenas-args) " "))))

(define unir-cadenas
  (lambda (lista sym)
    (define (unir lista)
      (if (null? lista)
          ""
          (let ((resto (unir (cdr lista))))
            (if (string=? resto "")
                (car lista)
                (string-append (car lista) sym resto)))))
    (unir lista)))


(define eval-var-decl
  (lambda (exp env)
    (cases var-decl exp
      (lvar-exp (ids rands body) 
                (let ((args (eval-rands rands env)))
                 (eval-expression body(extend-env ids args env))))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      (else (eopl:error 'eval-var-decl "Invalid variable declaration"))
    )
  )
)
(define eval-if-expresion
  (lambda (exp1 exp2 exp3 env)
    (if (eval-expression exp1 env)
        (eval-expression exp2 env)
        (eval-expression exp3 env))))

(define eval-for-expresion
  (lambda (identificador i from until body env)
    (let ((inicio (eval-expression i env))
              (fin (eval-expression from env))
              (iterar (eval-expression until env))
            )
          (let loop ((i inicio))
            (when (< i fin)
              (eval-expression body (extend-env (list identificador) (list i) env))
              (loop (+ i iterar)))))
      ))

(define eval-while-expresion
  (lambda (exp body env)
    (define (iterar)
      (when (eval-expression exp env)
        (eval-expression body env)
        (iterar)))
    (iterar)))

(define eval-switch-expresion
      (lambda (exp cases listaExp default env)
        (letrec ((valor (eval-expression exp env)))
          (let loop ((casos cases) (expresiones listaExp))
            (cond
              ((null? casos) (eval-expression default env))
              ((equal? valor (eval-expression (car casos) env)) (eval-expression (car expresiones) env))
              (else (loop (cdr casos) (cdr expresiones))))))))
   
;***********************primitivas************************************************

(define apply-primitive
  (lambda (prim args)
    (cases primitiva prim
      (sum-prim () (operaciones args +))
      (minus-prim () (operaciones args -))
      (mult-prim () (operaciones args *))
      (mod-prim () (operaciones args modulo))
      (elevar-prim () (operaciones args expt))
      (menor-prim () (operaciones args <))
      (mayor-prim () (operaciones args >))
      (menorigual-prim () (operaciones args <=))
      (mayorigual-prim () (operaciones args >=))
      (diferente-prim () (operaciones args (lambda (x y) (not (= x y)))))
      (igual-prim () (operaciones args =))
    )
  )
)


(define (operaciones args operation)
  (cond ((and (number? (car args)) (number? (cadr args)))
         (operation (car args) (cadr args)))
        ((and (string? (car args)) (string? (cadr args)))
         (let ((operand1BaseInfo (tipoBase (car args)))
               (operand2BaseInfo (tipoBase (cadr args))))
           (cond ((= (cadr operand1BaseInfo) (cadr operand2BaseInfo))  
                  (let ((result (operation
                                 (ConvertirBase-Decimal (car operand1BaseInfo) (cadr operand1BaseInfo))
                                 (ConvertirBase-Decimal (car operand2BaseInfo) (cadr operand2BaseInfo)))))
                    (string-append
                     (if (< result 0) "-" "")
                     (tipoString (cadr operand1BaseInfo))
                     (convertirDecimal-Base (abs result) (cadr operand1BaseInfo)))))
                 (else (eopl:error "Bases diferentes no se pueden operar")))))
        (else (eopl:error "Operandos no válidos"))))





(define tipoBase
  (lambda (num)
    (cond
      [(string=? (substring num 0 1) "-")
       (cond
         [(string=? (substring num 1 2) "b")
          (list (string-append "-" (substring num 2)) 2)]
         [(string=? (substring num 1 3) "0x")
          (list (string-append "-" (substring num 3)) 8)]
         [(string=? (substring num 1 3) "hx")
          (list (string-append "-" (substring num 3)) 16)]
         )]
      [(string=? (substring num 0 1) "b")
       (list (substring num 1) 2)]
      [(string=? (substring num 0 2) "0x")
       (list (substring num 2) 8)]
      [(string=? (substring num 0 2) "hx")
       (list (substring num 2) 16)]
      )
    )
  )



(define (tipoString n)
  (cond
    [(equal? n 2) "b"]
    [(equal? n 8) "0x"]
    [(equal? n 16) "hx"]
    [else ""]))


(define (convertirDecimal-Base n x)
  (define (digit i)
    (cond [(< i 10) (integer->char (+ i 48))]
          [(<= i 35) (integer->char (+ i 55))]
          [else #\?]))
  
  (define (get-digit n)
    (let ((remainder (remainder n x)))
      (if (< remainder 10)
          (integer->char (+ remainder 48))
          (integer->char (+ remainder 55)))))
  
  (let ((sign (if (< n 0) "-" "")))
    (cond [(< (abs n) x)
           (string (get-digit (abs n)))]
          [else
           (string-append
             sign
             (convertirDecimal-Base(quotient (abs n) x) x)
             (string (get-digit (remainder (abs n) x))))])))




(define (BuscarString str char [acc 0])
  (cond
    [(= (string-length str) 0) (eopl:error "No hay string")]
    [(char=? (string-ref str 0) char) acc]
    [else (BuscarString (substring str 1 (string-length str)) char (+ acc 1))]
  ))
  
(define (ConvertirBase-Decimal s x)

  (let ((sign (cond ((string=? (substring s 0 1) "-") -1)
                    (else 1)))
        (s (substring s (if (string=? (substring s 0 1) "-") 1 0)
                      (string-length s))))
    (* sign (get-number s x))))

(define (get-number s base)
  (cond
    [(zero? (string-length s)) 0]
    [else (let* ((digit (char->integer (string-ref s (- (string-length s) 1))))
                 (digit-value (cond
                                [(and (>= digit (char->integer #\0)) (<= digit (char->integer #\9)))
                                 (- digit (char->integer #\0))]
                                [(and (>= digit (char->integer #\A)) (<= digit (char->integer #\F)))
                                 (+ (- digit (char->integer #\A)) 10)]
                                [else 0])))
            (+ (* base (get-number (substring s 0 (- (string-length s) 1)) base))
               digit-value))]))





;**********************primitiva bool********************
(define apply-primitive-bool
  (lambda (prim args)
    (cases primitivaBooleana prim
      (and-prim () (operation args (lambda (acc x) (and acc x)) #t))
      (or-prim () (operation args (lambda (acc x) (or acc x)) #f))
      (xor-prim () (operation args (lambda (acc x) (xor acc x)) #f))
      (not-prim () (not (car args)))
    )
  )
)

(define xor
  (lambda (exp1 exp2)
    (cond
      [(and exp1 exp2) #F]
      [exp1 #T]
      [exp2 #T]
      [else #F]
    )
  )
)

;***************primitica strings***********************

(define apply-primitive-string
  (lambda (prim args)
    (cases primitivaCadena prim
      (concat-primCad () (unir-cadenas args ""))
      (length-primCad () (string-length (car args)))
      (index-primCad ()  (string  (string-ref (car args) (cadr args))))
    )
  )
)


(define apply-primitive-list
  (lambda (prim args)
    (cases primitivaListas prim
      (first-primList () (car args))
      (rest-primList () (cdr args))
      (empty-primList () (null? args))
    )
  )
)

;**************************primitiva array************************
(define (primitiva-array prim arg)
  (cases primitivaArray prim
    (length-primArr () 
      (vector-length (car arg)))
    (index-primArr () 
      (vector-ref (car arg) (cadr arg)))
    (slice-primArr () 
      (letrec ((lista             (lambda (vect inicio final)
                  (if (= inicio final)
                      (list (vector-ref vect inicio))
                      (cons (vector-ref vect inicio)
                            (lista vect (+ inicio 1) final))))))
        (list->vector (lista (car arg) (cadr arg) (caddr arg)))))
    (setlist-primArr () 
      (vector-set! (car arg) (cadr arg) (caddr arg))
      (car arg))
    (else 
      (eopl:error "Primitiva de array invalida"))))

      


(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))


(define operation
  (lambda (lst f acc)
    (cond
      [(null? lst) acc]
      [else
        (operation (cdr lst) f (f acc (car lst) ))])))

(define true-value?
  (lambda (x)
    (not (zero? x))))

;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))


;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))


(define empty-env  
  (lambda ()
    (empty-env-record)))    



(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))


(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (mayor len) idss bodies)
          env)))))


(define mayor
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))


(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))



(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define referencia
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))



(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

; ********************funciones auxiliares************************
(define let-binding?
  (lambda (env sym)
    (cases environment env
      (empty-env-record () #f)
      (extended-env-record (syms vals env)
                           (if (memq sym syms)
                               #t
                               (let-binding? env sym))))))


;************************pruebas simples****************************



;(interpretador)

(provide (all-defined-out))