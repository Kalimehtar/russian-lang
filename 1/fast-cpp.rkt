#lang racket/base
;; Core IR → C++ for надо-быстро (no module table; no вывод/ввод).
(require racket/match
         racket/string
         racket/list
         syntax/parse)

(provide adina-fast->cpp
         type-key->c-type
         type-key->cpp
         export-preamble)

(define (type-key->cpp key stx)
  (case key
    [(целое) "int"]
    [(вещественное) "double"]
    [(логическое) "bool"]
    [else
     (raise-syntax-error
      'надо-быстро
      (format "неизвестный тип C++: ~a" key)
      stx)]))

(define (type-key->c-type key)
  (case key
    [(целое) '_int]
    [(вещественное) '_double]
    [(логическое) '_stdbool]
    [else #f]))

(define export-preamble
  (string-append
   "#ifdef _WIN32\n"
   "#define ADINA_EXPORT extern \"C\" __declspec(dllexport)\n"
   "#else\n"
   "#define ADINA_EXPORT extern \"C\"\n"
   "#endif\n\n"))

(struct env (table fresh) #:transparent)

(define (make-env fresh-fn)
  (env (make-hasheq) fresh-fn))

(define (env-bind! e id-stx)
  (define sym (syntax-e id-stx))
  (define gen ((env-fresh e)))
  (hash-set! (env-table e) sym gen)
  gen)

(define (env-lookup e id-stx)
  (define sym (syntax-e id-stx))
  (or (hash-ref (env-table e) sym #f)
      (raise-syntax-error
       'надо-быстро
       (format "неизвестное имя в быстром коде: ~a" sym)
       id-stx)))

(define (escape-c-string s)
  (string-append*
   (for/list ([ch (in-string s)])
     (case ch
       [(#\\) "\\\\"]
       [(#\") "\\\""]
       [(#\newline) "\\n"]
       [(#\return) "\\r"]
       [(#\tab) "\\t"]
       [else (string ch)]))))

(define (expr->cpp stx e)
  (syntax-parse stx
    [n:number
     (define v (syntax-e #'n))
     (cond
       [(exact-integer? v) (number->string v)]
       [(real? v)
        (number->string (exact->inexact v))]
       [else
        (raise-syntax-error
         'надо-быстро
         "неподдерживаемое число"
         stx)])]
    [s:string
     (string-append
      "\"" (escape-c-string (syntax-e #'s)) "\"")]
    [b:boolean
     (if (syntax-e #'b) "true" "false")]
    [id:id
     (env-lookup e #'id)]
    [((~datum +) a b)
     (format "(~a + ~a)" (expr->cpp #'a e) (expr->cpp #'b e))]
    [((~datum -) a b)
     (format "(~a - ~a)" (expr->cpp #'a e) (expr->cpp #'b e))]
    [((~datum *) a b)
     (format "(~a * ~a)" (expr->cpp #'a e) (expr->cpp #'b e))]
    [((~datum /) a b)
     (format "(~a / ~a)" (expr->cpp #'a e) (expr->cpp #'b e))]
    [((~datum ==) a b)
     (format "(~a == ~a)" (expr->cpp #'a e) (expr->cpp #'b e))]
    [((~datum /=) a b)
     (format "(~a != ~a)" (expr->cpp #'a e) (expr->cpp #'b e))]
    [((~datum <) a b)
     (format "(~a < ~a)" (expr->cpp #'a e) (expr->cpp #'b e))]
    [((~datum >) a b)
     (format "(~a > ~a)" (expr->cpp #'a e) (expr->cpp #'b e))]
    [((~datum <=) a b)
     (format "(~a <= ~a)" (expr->cpp #'a e) (expr->cpp #'b e))]
    [((~datum >=) a b)
     (format "(~a >= ~a)" (expr->cpp #'a e) (expr->cpp #'b e))]
    [((~datum &&) a ...)
     (string-join
      (map (λ (x) (format "(~a)" (expr->cpp x e)))
           (syntax->list #'(a ...)))
      " && ")]
    [((~datum ||) a ...)
     (string-join
      (map (λ (x) (format "(~a)" (expr->cpp x e)))
           (syntax->list #'(a ...)))
      " || ")]
    [((~datum си) s:string)
     (syntax-e #'s)]
    [(fn:id arg ...)
     (format "~a(~a)"
             (env-lookup e #'fn)
             (string-join
              (map (λ (a) (expr->cpp a e))
                   (syntax->list #'(arg ...)))
              ", "))]
    [(part parts ...+)
     (string-append*
      (map (λ (p) (expr->cpp p e))
           (syntax->list #'(part parts ...))))]
    [_
     (raise-syntax-error
      'надо-быстро
      "неподдерживаемое выражение в быстром коде"
      stx)]))

(define (stmt->cpp stx e)
  (syntax-parse stx
    [((~datum =) id:id expr)
     (define gen (env-bind! e #'id))
     (format "  auto ~a = ~a;\n"
             gen (expr->cpp #'expr e))]
    [((~or* (~datum блок) (~datum begin)) s ...)
     (stmts->cpp #'(s ...) e)]
    [_
     (format "  ~a;\n" (expr->cpp stx e))]))

(define (stmts->cpp stx-list e)
  (string-append*
   (map (λ (s) (stmt->cpp s e))
        (syntax->list stx-list))))

(define (parse-header header-stx)
  (syntax-parse header-stx
    [((fname:id arg ...) ret:id)
     (define args (syntax->list #'(arg ...)))
     (define pairs
       (let loop ([xs args] [acc '()])
         (match xs
           ['() (reverse acc)]
           [(list* a t rest)
            (unless (and (identifier? a) (identifier? t))
              (raise-syntax-error
               'надо-быстро
               "аргументы: ожидается имя тип имя тип ..."
               header-stx))
            (loop rest (cons (cons a t) acc))]
           [_
            (raise-syntax-error
             'надо-быстро
             "нечётное число элементов в списке аргументов"
             header-stx)])))
     (values #'fname #'ret pairs)]
    [_
     (raise-syntax-error
      'надо-быстро
      "ожидался заголовок вида имя(аргументы) тип"
      header-stx)]))

(define (function->cpp def-stx fresh)
  (syntax-parse def-stx
    [((~datum =) header body ...)
     (define-values (fname ret-id arg-pairs)
       (parse-header #'header))
     (define gen-fn (fresh))
     (define e (make-env fresh))
     (define arg-cpp
       (for/list ([p (in-list arg-pairs)])
         (define a (car p))
         (define t (cdr p))
         (define gen (env-bind! e a))
         (define tkey (syntax-e t))
         (format "~a ~a" (type-key->cpp tkey t) gen)))
     (define ret-key (syntax-e ret-id))
     (define ret-cpp (type-key->cpp ret-key ret-id))
     (define body-cpp (stmts->cpp #'(body ...) e))
     (define code
       (format
        "ADINA_EXPORT ~a ~a(~a)\n{\n~a}\n\n"
        ret-cpp
        gen-fn
        (string-join arg-cpp ", ")
        body-cpp))
     (define export
       (list (syntax-e fname)
             gen-fn
             ret-key
             (map (λ (p) (syntax-e (cdr p))) arg-pairs)
             fname))
     (values code export)]
    [_
     (raise-syntax-error
      'надо-быстро
      "ожидалось определение функции: имя(...) тип = тело"
      def-stx)]))

;; forms: only си / function defs (after macro expand)
;; Returns (values top-cpp functions-cpp exports)
;; top-cpp — literal fragments (includes); functions-cpp — exported funcs
(define (adina-fast->cpp forms-stx fresh)
  (define top '())
  (define functions '())
  (define exports '())
  (for ([form (in-list (syntax->list forms-stx))])
    (syntax-parse form
      [((~datum си) s:string)
       (set! top
             (append top
                     (list (string-append (syntax-e #'s) "\n"))))]
      [((~datum =) . _)
       (define-values (code export)
         (function->cpp form fresh))
       (set! functions (append functions (list code)))
       (set! exports (append exports (list export)))]
      [_
       (raise-syntax-error
        'надо-быстро
        "ожидалось си{…} или определение функции"
        form)]))
  (values (string-append* top)
          (string-append* functions)
          exports))
