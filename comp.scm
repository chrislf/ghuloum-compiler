(load "../inc/src/tests-driver.scm")

(define (emit-program fxn)
  (emit "  .text")
  (emit "  .p2align 4,,15")
  (emit "  .globl  scheme_entry")
  (emit "  .type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit ".LFB0:")
  (emit "  .cfi_startproc")
  (emit "  movq %rdi, %rbx") ; saving the address of the start of the heap in rbx 
  (compile-program fxn)
  (emit "  .cfi_endproc")
  (emit ".LFE0:")
  (emit "  .size scheme_entry, .-scheme_entry")
)

(define fixnum-shift 2)
(define fixnum-mask #b11) ; do I know what I'm doing here?
(define fixnum-tag  #b00) 
(define empty-list    #b00101111)
(define bool-mask     #b1111111)
(define bool-tag      #b0011111)
(define bool-t        #b10011111)
(define bool-f        #b00011111)
(define char-mask     #b11111111)
(define char-tag      #b00001111)
(define char-shift    8)
(define wordsize      8) ; this might actually be 8 on x64, but it might not matter

(define (immediate-rep x)
  (cond
    ((integer? x) (ash x fixnum-shift))
    ((equal? '() x) empty-list) ; equivalently, (null? x)
    ((equal? #t x) bool-t)
    ((equal? #f x) bool-f)
    ((char? x) (bitwise-ior (ash (char->integer x) char-shift) char-tag))
  )
)

(define (compile-program x)
  (emit-expr x (* -1 wordsize) '())
  ; -4 is our initial stack pointer/index
  ; '() is our initial (empty) environment
  (emit "  ret")
)

(define (immediate? x)
  (or (integer? x) (null? x) (boolean? x) (char? x))
)

(define (primcall-op x)
  (car x)
)

(define (primcall-operand1 x)
  (car (cdr x))
)

(define (primcall-operand2 x)
  (car (cdr (cdr x)))
)

(define (primcall? x)
  (and  (pair? x) ; note this means precisely not a singleton list
        (member (car x)
          '(  add1
              sub1
              char->integer
              integer->char
              zero?
              null?
              not
              char?
              integer?
              boolean?
              +
              -
              *
           )
        )
  )
)

(define (emit-primitive-call x si env) ; maintain the stack index
  (case (primcall-op x)
    ((add1)
      (emit-expr (primcall-operand1 x) si env)  ;; aren't we potentially clobbering computations
                                         ;; lower down the stack here?
      (emit "  addq $~s, %rax" (immediate-rep 1))
    )
    ((sub1)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  subq $~s, %rax" (immediate-rep 1))
    )
    ((char->integer)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  shr $~s, %rax" 6)
    )
    ((integer->char)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  shq $~s, %rax" 6) ; char-shift - fixnum-shift
      (emit "  or $~s, %rax" char-tag)
    )
    ((zero?)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  cmpq $0, %rax")
      (emit "  movq $0, %rax")
      (emit "  sete %al") ; conditional setting of byte to 1 based on the earlier cmpq
      (emit "  salq $7, %rax")
      (emit "  orq $31, %rax")
    )
    ((null?)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  cmpq $~s, %rax" empty-list)
      (emit "  movq $0, %rax")
      (emit "  sete %al") ; conditional setting of byte to 1 based on the earlier cmpq
      (emit "  salq $7, %rax")
      (emit "  orq $31, %rax")
    )
    ((char?)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  andq $~s, %rax" char-mask)
      (emit "  cmpq $~s, %rax" char-tag)
      (emit "  movq $0, %rax")
      (emit "  sete %al")
      (emit "  salq $7, %rax")
      (emit "  orq $31, %rax")
    )
    ((integer?)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  andq $~s, %rax" fixnum-mask)
      (emit "  cmpq $~s, %rax" fixnum-tag)
      (emit "  movq $0, %rax")
      (emit "  sete %al")
      (emit "  salq $7, %rax")
      (emit "  orq $31, %rax")
    )
    ((boolean?)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  andq $~s, %rax" bool-mask)
      (emit "  cmpq $~s, %rax" bool-tag)
      (emit "  movq $0, %rax")
      (emit "  sete %al")
      (emit "  salq $7, %rax")
      (emit "  orq $31, %rax")
    )
    ((+) ; check for correct type?
      (emit-expr (primcall-operand1 x) si env)
      (emit "  movq %rax, ~s(%rsp)" si)
      (emit-expr
        (primcall-operand2 x)
        (- si wordsize) ; going down/up the stack (depending on how you look at it)
        env
      )
      (emit "  addq ~s(%rsp), %rax" si)
    )
    ((-)
      (emit-expr (primcall-operand2 x) si env)
      (emit "  movq %rax, ~s(%rsp)" si)
      (emit-expr
        (primcall-operand1 x)
        (- si wordsize)
        env
      )
      (emit "  subq ~s(%rsp), %rax" si)
    )
    ((*)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  movq %rax, ~s(%rsp)" si)
      (emit-expr
        (primcall-operand2 x)
        (- si wordsize)
        env
      )
      (emit "  shr $~s, %rax" fixnum-shift) ; when we multiply we have to remove the shift factor from one of the numbers, otherwise it gets doubled and our answer is *4
      (emit "  imulq ~s(%rsp), %rax" si)
    )
  )
)

(define (extend var si env)
  (cons (list var si) env)
)

(define (lookup var env)
  (cond
    ((assv var env) => cadr)
    (else #f)
  )
)

(define (let? x)
  (and
    (equal? (car x) 'let)
    ;(alist? (cadr x)) ; alas this function is not defined in Petite
  )
)

(define empty-env '())
(define lhs car)
(define (rhs x) (car (cdr x)))
(define (bindings x) (car (cdr x)))
; TODO allow more than one statement in the body
; (because Scheme allows this, rather than because it is a good idea)
(define (let-body x) (car (cdr (cdr x))))

(define (emit-let bindings body si env)
  (let f ((b* bindings) (new-env env) (si si))
    (cond
      ((null? b*)
        (emit-expr body si new-env))
      (else
        (let ((b (car b*)))
          (emit-expr (rhs b) si env)
          (emit "movq %rax, ~s(%rsp)" si)
          (f (cdr b*)
              (extend (lhs b) si new-env) ; Note Extremely Well: env is a list of mappings from symbols to *stack locations*
              (- si wordsize)
))))))

(define (variable? x env)
  (lookup x env)
)

;;; if

(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (string->symbol (format "L_~s" count))])
        (set! count (add1 count))
        L
      )
    )
  )
)

(define (emit-cmpq x reg)
  (emit "  cmpq $~s, %~a" x reg)
)

(define (emit-je lbl)
  (emit "  je .~s" lbl)
)

(define (emit-jmp lbl)
  (emit "  jmp .~s" lbl)
)

(define (emit-label lbl)
  (emit ".~s:" lbl)
)


(define (emit-if test conseq altern si env)
  (let ((L0 (unique-label)) (L1 (unique-label)))
    (emit-expr test si env)
    (emit-cmpq (immediate-rep #f) "rax")
    (emit-je L0)
    (emit-expr conseq si env)
    (emit-jmp L1)
    (emit-label L0)
    (emit-expr altern si env)
    (emit-label L1)
  )
)

(define (if? x)
  (and
    (equal? 'if (car x))
    (or (= 4 (length x))
        (raise (format "invalid if expression ~s" x))
    )
  )
)

(define if-test cadr)
(define if-conseq caddr)
(define if-altern cadddr)

;;; </if>

;;; heap/cons
(define (cons? x)
  (and
    (equal? 'cons (car x))
    (or (= 3 (length x))
        (raise (format "invalid cons expression ~s" x))
    )
  )
)

(define (emit-cons tcar tcadr si env)
  ; Intuition
  ; evaluate the first form and place in rax
  ; mov to the heap 
  ; 
  ; NOTE WELL: with the stack we maintained our own list 
  ; of offsets from a base value in a register
  ; initialised by C code. With the heap, the 
  ; register is initialised by C but we modify it
  ; in place as we add objects in memory. Note we do not
  ; have garbage collection at this point so we will
  ; eventually run out of memory for most applications
  ;
  ; evaluate the second form and place in rax
  ; mov to the next location in the heap
  ; take the address of the first location, add 1 to it
  ; and place it in rax
  (emit-expr tcar si env)
  (emit "  movq %rax, 0(%rbx)")
  (emit-expr tcadr si env)
  (emit "  movq %rax, 4(%rbx)")
  (emit "  movq %rbx, %rax")
  (emit "  orq $1, %rax") ; here's our pointer to a pair
  (emit "  addq $8, %rbx") ; bump heap pointer
)

; TODO declare cons, car and cdr as primops
; TODO write the code to emit these
; TODO write the C code to display the type
;;; </pair>

(define (emit-expr x si env)
  (cond
    ((immediate? x)
      (emit "  movq $~s, %rax" (immediate-rep x))
    )
    ((primcall? x)
      (emit-primitive-call x si env)
    )
    ((variable? x env)
      (emit "  movq ~s(%rsp), %rax" (lookup x env))
    )
    ((let? x)
      (emit-let (bindings x) (let-body x) si env)
    )
    ((if? x)
      (emit-if (if-test x) (if-conseq x) (if-altern x) si env)
    )
    ((cons? x)
      (emit-cons (cadr x) (caddr x) si env)
    )
    (else
      (raise "ran out of expression types")
    )
  )
)

(define (run-compile-clf expr)
  (let ([p (open-output-file "clf.s" 'replace)])
    (parameterize ([compile-port p])
      (emit-program expr)
    )
    (close-output-port p)
  )
)


(run-compile-clf '(cons 10 20))
;(run-compile-clf '(if #f 1 2))
;(run-compile-clf '(let [(y 4) (z 5)] (+ y z)))
;(run-compile-clf '(+ 1000 (* -1 (* 2 (+ 7 20)))))
;(run-compile-clf '(- 1 20))
;(run-compile-clf '(+ 1 (+ 2 20)))
;(run-compile-clf '(boolean? #f))
;(run-compile-clf '(integer? ()))
;(run-compile-clf '(char? #\l))
;(run-compile-clf '(null? 2))
;(run-compile-clf '(zero? 0))
;(run-compile-clf '(char->integer #\k))
;(run-compile-clf '(add1 3))
;(run-compile-clf '(sub1 3))
;(run-compile-clf 4)

