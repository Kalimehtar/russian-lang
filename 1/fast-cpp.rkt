#lang racket/base
;; Translate Adina надо-быстро syntax into C++ source + export table.
(require racket/match
         racket/string
         racket/list
         syntax/parse)

(provide adina-fast->cpp
         type-key->c-type
         type-key->cpp
         include-for-module)

;; ---------------------------------------------------------------------------
;; Includes and types
;; ---------------------------------------------------------------------------

(define (include-for-module mod-id)
  (define name (syntax-e mod-id))
  (case name
    [(ввод-вывод) "#include <iostream>\n"]
    [(математика) "#include <cmath>\n"]
    [else
     (raise-syntax-error
      'надо-быстро
      (format "неизвестный модуль для C++: ~a" name)
      mod-id)]))

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

;; ---------------------------------------------------------------------------
;; Name environment
;; ---------------------------------------------------------------------------

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
  (cond
    [(eq? sym 'вывод) "std::cout"]
    [(eq? sym 'ввод) "std::cin"]
    [(hash-ref (env-table e) sym #f)]
    [else
     (raise-syntax-error
      'надо-быстро
      (format "неизвестное имя в быстром коде: ~a" sym)
      id-stx)]))

;; ---------------------------------------------------------------------------
;; Expressions
;; ---------------------------------------------------------------------------

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
    [((~datum <<) a b)
     (format "~a << ~a" (expr->cpp #'a e) (expr->cpp #'b e))]
    [((~datum >>) a b)
     (format "~a >> ~a" (expr->cpp #'a e) (expr->cpp #'b e))]
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
    [((~datum не) a)
     (format "(!~a)" (expr->cpp #'a e))]
    [((~datum си) s:string)
     ;; Буквальная вставка фрагмента Си++
     (syntax-e #'s)]
    [(fn:id arg ...)
     (format "~a(~a)"
             (env-lookup e #'fn)
             (string-join
              (map (λ (a) (expr->cpp a e)) (syntax->list #'(arg ...)))
              ", "))]
    ;; Склейка: си{sin(} x си{)} и т.п.
    [(part parts ...+)
     (string-append*
      (map (λ (p) (expr->cpp p e))
           (syntax->list #'(part parts ...))))]
    [_
     (raise-syntax-error
      'надо-быстро
      "неподдерживаемое выражение в быстром коде"
      stx)]))

;; ---------------------------------------------------------------------------
;; Statements
;; ---------------------------------------------------------------------------

(define (stmt->cpp stx e)
  (syntax-parse stx
    [((~datum =) id:id expr)
     (define gen (env-bind! e #'id))
     (format "  auto ~a = ~a;\n" gen (expr->cpp #'expr e))]
    [((~datum вернуть) part ...+)
     (format "  return ~a;\n"
             (string-append*
              (map (λ (p) (expr->cpp p e))
                   (syntax->list #'(part ...)))))]
    [((~datum <<) . _)
     (format "  ~a;\n" (expr->cpp stx e))]
    [((~datum >>) . _)
     (format "  ~a;\n" (expr->cpp stx e))]
    [_
     (format "  ~a;\n" (expr->cpp stx e))]))

(define (stmts->cpp stx-list e)
  (string-append*
   (map (λ (s) (stmt->cpp s e)) (syntax->list stx-list))))

;; ---------------------------------------------------------------------------
;; Function header: ((name) ret) or ((name a t1 b t2 ...) ret)
;; ---------------------------------------------------------------------------

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
     (define-values (fname ret-id arg-pairs) (parse-header #'header))
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
     ;; export: adina-sym, gen-name, ret-key, arg-keys, fname-stx
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

;; ---------------------------------------------------------------------------
;; Top-level forms inside надо-быстро
;; ---------------------------------------------------------------------------

(define (adina-fast->cpp forms-stx fresh)
  (define includes '())
  (define functions '())
  (define exports '())
  (for ([form (in-list (syntax->list forms-stx))])
    (syntax-parse form
      [((~datum используется) mod:id)
       (set! includes
             (append includes (list (include-for-module #'mod))))]
      [((~datum используется) mod:id ...)
       #:when (not (null? (syntax->list #'(mod ...))))
       (for ([m (in-list (syntax->list #'(mod ...)))])
         (set! includes
               (append includes (list (include-for-module m)))))]
      [((~datum =) . _)
       (define-values (code export) (function->cpp form fresh))
       (set! functions (append functions (list code)))
       (set! exports (append exports (list export)))]
      [_
       (raise-syntax-error
        'надо-быстро
        (string-append
         "ожидалось «используется …» "
         "или определение функции")
        form)]))
  (define cpp
    (string-append
     (string-append* (remove-duplicates includes))
     "\n"
     export-preamble
     (string-append* functions)))
  (values cpp exports))
