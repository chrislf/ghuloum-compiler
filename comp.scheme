(load "../inc/src/tests-driver.scm")

(define (emit-program fxn)
  (emit "  .text")
  (emit "  .p2align 4,,15")
  (emit "  .globl  scheme_entry")
  (emit "  .type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit ".LFB0:")
  (emit "  .cfi_startproc")
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
(define wordsize      4) ; this might actually be 8 on x64, but it might not matter

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
      (emit "  addl $~s, %eax" (immediate-rep 1))
    )
    ((sub1)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  subl $~s, %eax" (immediate-rep 1))
    )
    ((char->integer)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  shr $~s, %eax" 6)
    )
    ((integer->char)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  shl $~s, %eax" 6) ; char-shift - fixnum-shift
      (emit "  or $~s, %eax" char-tag)
    )
    ((zero?)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  cmpl $0, %eax")
      (emit "  movl $0, %eax")
      (emit "  sete %al") ; conditional setting of byte to 1 based on the earlier cmpl
      (emit "  sall $7, %eax")
      (emit "  orl $31, %eax")
    )
    ((null?)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  cmpl $~s, %eax" empty-list)
      (emit "  movl $0, %eax")
      (emit "  sete %al") ; conditional setting of byte to 1 based on the earlier cmpl
      (emit "  sall $7, %eax")
      (emit "  orl $31, %eax")
    )
    ((char?)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  andl $~s, %eax" char-mask)
      (emit "  cmpl $~s, %eax" char-tag)
      (emit "  movl $0, %eax")
      (emit "  sete %al")
      (emit "  sall $7, %eax")
      (emit "  orl $31, %eax")
    )
    ((integer?)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  andl $~s, %eax" fixnum-mask)
      (emit "  cmpl $~s, %eax" fixnum-tag)
      (emit "  movl $0, %eax")
      (emit "  sete %al")
      (emit "  sall $7, %eax")
      (emit "  orl $31, %eax")
    )
    ((boolean?)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  andl $~s, %eax" bool-mask)
      (emit "  cmpl $~s, %eax" bool-tag)
      (emit "  movl $0, %eax")
      (emit "  sete %al")
      (emit "  sall $7, %eax")
      (emit "  orl $31, %eax")
    )
    ((+) ; check for correct type?
      (emit-expr (primcall-operand1 x) si env)
      (emit "  movl %eax, ~s(%rsp)" si)
      (emit-expr
        (primcall-operand2 x)
        (- si wordsize) ; going down/up the stack (depending on how you look at it)
        env
      )
      (emit "  addl ~s(%rsp), %eax" si)
    )
    ((-)
      (emit-expr (primcall-operand2 x) si env)
      (emit "  movl %eax, ~s(%rsp)" si)
      (emit-expr
        (primcall-operand1 x)
        (- si wordsize)
        env
      )
      (emit "  subl ~s(%rsp), %eax" si)
    )
    ((*)
      (emit-expr (primcall-operand1 x) si env)
      (emit "  movl %eax, ~s(%rsp)" si)
      (emit-expr
        (primcall-operand2 x)
        (- si wordsize)
        env
      )
      (emit "  shr $~s, %eax" fixnum-shift) ; when we multiply we have to remove the shift factor from one of the numbers, otherwise it gets doubled and our answer is *4
      (emit "  imull ~s(%rsp), %eax" si)
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

;(define carp car)
;
;(define (car x)
;  (write (format "~s~%" x))
;  (carp x))

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
          (emit "movl %eax, ~s(%rsp)" si)
          (f (cdr b*)
              (extend (lhs b) si new-env) ; Note Extremely Well: env is a list of mappings from symbols to *stack locations*
              (- si wordsize)
))))))

(define (variable? x env)
  (lookup x env)
)

(define (emit-expr x si env)
  (cond
    ((immediate? x)
      (emit "  movl $~s, %eax" (immediate-rep x))
    )
    ((primcall? x)
      (emit-primitive-call x si env)
    )
    ((variable? x env)
      (emit "  movl ~s(%rsp), %eax" (lookup x env))
    )
    ((let? x)
      (emit-let (bindings x) (let-body x) si env)
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

(run-compile-clf '(let [(y 4) (z 5)] (+ y z)))
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

