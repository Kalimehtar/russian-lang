#lang racket/base
(require racket/class 1/syn racket/contract racket/splicing
         (except-in ffi/unsafe ->) 1/run-fast
         (for-syntax (except-in racket/base =) 1/run-fast 1/fast-cpp
                     racket/contract racket/string racket/syntax
                     racket/path racket/list syntax/parse))
(provide (rename-out [module-begin #%module-begin])
         #%top-interaction
         (except-out (all-defined-out) module-begin)
         splicing-parameterize english except-in
         #%app #%datum + - / * < > <= >= => #%top (all-from-out 'syn) -> ==>
         (for-syntax #%app #%top #%datum + - / * < > <= >= =>
                     register-fast-syntax-rule!
                     (all-from-out 'syn) λ ... _))

(define-syntax (english stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([require-spec (datum->syntax stx '(except-in racket =))])
       #'(require require-spec))]))

(module syn racket/base
  (require syntax/parse (except-in racket/match ==) racket/sequence
           1/syn racket/vector (for-syntax racket/base racket/syntax))
  (provide (all-defined-out)
           syntax)
  (define-syntax (разобрать-синтаксис stx)
    (syntax-case stx ()
      [(_ правила ...)
       #'(syntax-parse правила ...)]))
  (define-syntax-rule (|| выражение ...)
    (or выражение ...))
  (define-syntax-rule (&& выражение ...)
    (and выражение ...))

  (define (ошибка . т) (apply error т))
  (define (== а . т) (andmap (λ (б) (equal? б а)) т))
  (define (=== а . т) (andmap (λ (б) (eqv? б а)) т))
  (define (/= а . т) (not (apply == а т)))
  (define (// x y) (quotient x y))
  (define (% x y) (remainder x y))
  (define не
    (case-lambda
      [(x) (not x)]
      [(x . y) (not (apply x y))]))

  ;; НАДО: сделать перевод языка шаблонов для match, match-define, match-define-values
  (define-syntax (= stx)
    (syntax-case stx (значения шаблон шаблоны)
      [(_ (значения . а) б) #'(define-values а б)]
      [(_ (значения . а) б в ...) #'(define-values а (б в ...))]
      [(_ (шаблон а) б) #'(match-define а б)]
      [(_ (шаблон а) б в ...) #'(match-define а (б в ...))]
      [(_ (шаблоны . а) б) #'(match-define-values а б)]
      [(_ (шаблоны . а) б в ...) #'(match-define-values а (б в ...))]
      [(_ (а . б) . в) #'(define (а . б) . в)]
      [(_ а б) #'(define а б)]
      [(_ а б в ...) #'(define а (б в ...))]))

  (define (значения . a) (apply values a))

  (define-syntax (==> stx)
    (syntax-case stx ()
      [(_ (а ...) б) #'(lambda (а ...) б)]
      [(_ а б) #'(lambda (а) б)]))

  (define-syntax (:= stx)
    (syntax-case stx (значения квадратные-скобки)
      [(_ (значения . а) б) #'(let () (set!-values а б) (values . а))]
      [(_ (квадратные-скобки объект поле) значение)
       #'(let ()
           (cond
             [(vector? объект) (vector-set! объект поле значение)]
             [(hash? объект) (hash-set! объект поле значение)]
             [(string? объект) (string-set! объект поле значение)]
             [(bytes? объект) (bytes-set! объект поле значение)]
             [else
              (raise-syntax-error 'квадратные-скобки
                                  "У объекта ~a нет доступа к полям через квадратные скобки"
                                  объект)])
           значение)]
      [(_ (поле а) б) #`(let () (#,(format-id #'поле "установить-~a!" (syntax-e #'поле)) а б))]
      [(_ а б) #'(let () (set! а б) а)]))
  (define истина #t)
  (define ложь #f)
  (define (булево? а) (boolean? а))

  (define (++ коллекция . коллекции)
   (cond
     [(list? коллекция) (apply append коллекция коллекции)]
     [(string? коллекция) (apply string-append коллекция коллекции)]
     [(bytes? коллекция) (apply bytes-append коллекция коллекции)]
     [(vector? коллекция) (apply vector-append коллекция коллекции)]))

  (define (квадратные-скобки объект поле)
    (cond
      [(list? объект) (list-ref объект поле)]
      [(vector? объект) (vector-ref объект поле)]
      [(hash? объект) (hash-ref объект поле #f)]
      [(string? объект) (string-ref объект поле)]
      [(bytes? объект) (bytes-ref объект поле)]
      [(sequence? объект) (sequence-ref объект поле)]
      [else (raise-syntax-error 'квадратные-скобки
                                "У объекта ~a нет доступа к полям через квадратные скобки"
                                объект)])))
(require (for-syntax 'syn) 'syn)

(define-for-syntax (замена-адины? имя)
  (and (or (identifier? имя) (string? (syntax-e имя)))
       (not (module-path? (syntax-e имя)))))

(define-for-syntax (имя-адины имя)
  (cond
    [(not (замена-адины? имя)) имя]
    [(string? (syntax-e имя))
     (datum->syntax имя
                    (list #'file (if (string-suffix? (syntax-e имя) ".1")
                                     (syntax-e имя)
                                     (format "~a.1" (syntax-e имя))))
                    имя имя)]
    [else      
     (datum->syntax имя
                    (list #'file (path->string
                                  (collection-file-path
                                   (format "~a.1" (syntax-e имя)) "1"
                                   #:check-compiled? #t)))
                    имя имя)]))

(define-syntax (используется stx)
  (syntax-case stx (с-префиксом файл кроме)
    [(_ (с-префиксом префикс имя))
     (with-syntax ([require-spec (имя-адины  #'имя)])
       #'(require (prefix-in префикс require-spec)))]
    [(_ (кроме имя имена ...))
     (with-syntax ([require-spec (имя-адины  #'имя)])
       #'(require (except-in require-spec имена ...)))]
    [(_ (файл имя))
     (with-syntax ([require-spec (datum->syntax #'имя (list #'file #'имя) #'имя #'имя)])
       #'(require require-spec))]
    [(_ имя)
     (with-syntax ([require-spec (имя-адины  #'имя)])
       #'(require require-spec))]
    [(_ x ...) #'(begin (используется x) ...)]))

(define-syntax (используется-для-синтаксиса stx)
  (syntax-case stx (с-префиксом кроме файл)
    [(_ (с-префиксом префикс имя))
     (with-syntax ([require-spec (имя-адины  #'имя)])
       #'(require (for-syntax (prefix-in префикс require-spec))))]
    [(_ (кроме имя имена ...))
     (with-syntax ([require-spec (имя-адины  #'имя)])
       #'(require (for-syntax (except-in require-spec имена ...))))]
    [(_ (файл имя))
     (with-syntax ([require-spec (datum->syntax #'имя (list #'file #'имя) #'имя #'имя)])
       #'(require (for-syntax require-spec)))]
    [(_ имя)
     (with-syntax ([require-spec (имя-адины  #'имя)])
       #'(require (for-syntax require-spec)))]
    [(_ x ...) #'(begin (используется-для-синтаксиса x) ...)]))

(define-syntax (предоставлять stx)
  (syntax-case stx (всё-из с-контрактом)
    [(_ (всё-из имя))
     (with-syntax ([require-spec (имя-адины  #'имя)])
       #'(provide (all-from-out require-spec)))]
    [(_ (с-контрактом выражение ...))
     (syntax-local-introduce
      #'(provide (contract-out выражение ...)))]
    [(_ x) #'(provide x)]
    [(_ x ...) #'(begin (предоставлять x) ...)]))

(define-syntax (предоставлять-для-синтаксиса stx)
  (syntax-case stx (всё-из)
    [(_ (всё-из имя))
     (and (identifier? #'имя) (not (module-path? (syntax-e #'имя))))
     (quasisyntax/loc stx
       (provide
         (for-syntax
          (all-from-out
           #,(datum->syntax #'имя
                           (list 'file (path->string
                                        (collection-file-path
                                         (format "~a.1" (syntax-e #'имя)) "1"
                                         #:check-compiled? #t))))))))]
    [(_ (всё-из имя))
     (syntax-local-introduce
      (quasisyntax/loc stx (provide (for-syntax (all-from-out имя)))))]
    [(_ x) (syntax/loc stx (provide (for-syntax x)))]
    [(_ x ...) (syntax/loc stx (begin (предоставлять-для-синтаксиса x) ...))]))

(define (аргументы-командной-строки) (current-command-line-arguments))
(define (в-строках порт) (in-lines порт))
(define (в-соответствии соответствие) (in-hash соответствие))

(синоним send вызвать-метод)
(синоним send+ вызвать-цепочку-методов)
(синоним send* для-объекта)

(define-for-syntax (ctype-id key)
  (case key
    [(целое) #'_int]
    [(вещественное) #'_double]
    [(логическое) #'_stdbool]
    [else
     (raise-syntax-error 'надо-быстро
                         (format "нет FFI-типа для ~a" key))]))

(define-for-syntax (export->binding export)
  ;; export: (adina-sym gen-name ret-key arg-keys fname-stx)
  (define adina-sym (list-ref export 0))
  (define gen-name (list-ref export 1))
  (define ret-key (list-ref export 2))
  (define arg-keys (list-ref export 3))
  (define fname-stx (list-ref export 4))
  (define ret-id (ctype-id ret-key))
  (define arg-ids (map ctype-id arg-keys))
  (with-syntax ([name (datum->syntax fname-stx adina-sym fname-stx)]
                [gen (string->symbol gen-name)]
                [ret ret-id]
                [(arg ...) arg-ids])
    #'(define name
        (get-ffi-obj-from-fast 'gen
                               (_cprocedure (list arg ...) ret)))))

;; Fast macros registered by имя (symbol). Used because provide from
;; inside надо-быстро carries a macro scope that lift-require cannot
;; see during local-expand of client bodies.
(define-for-syntax fast-macro-table (make-hasheq))

(define-for-syntax (fast-macro-args-of form)
  (syntax-case form ()
    [(_ . a) (syntax->list #'a)]
    [_ #f]))

;; Ellipsis template: (pat ... . more) — repeat pat for each ell-val.
(define-for-syntax (fast-subst-ellipsis tmpl form ell-var ell-vals env-fixed)
  (define (as-datum x)
    (if (syntax? x) (syntax-e x) x))
  (define (subst d env)
    (cond
      [(symbol? d)
       (define hit (assq d env))
       (if hit (cdr hit) (datum->syntax form d))]
      [(null? d) '()]
      [(pair? d)
       (datum->syntax
        form
        (let loop ([xs d])
          (cond
            [(null? xs) '()]
            [(and (pair? xs)
                  (pair? (cdr xs))
                  (eq? (cadr xs) '...))
             (append
              (for/list ([val (in-list ell-vals)])
                (as-datum
                 (subst (car xs)
                        (cons (cons ell-var val) env))))
              (loop (cddr xs)))]
            [(pair? xs)
             (cons (as-datum (subst (car xs) env))
                   (loop (cdr xs)))]
            [else (as-datum (subst xs env))])))]
      [else (datum->syntax form d)]))
  (subst tmpl env-fixed))

(define-for-syntax (register-fast-syntax-rule! name pats-stx tmpl-stx)
  (define tmpl-datum (syntax->datum tmpl-stx))
  (define pats-datum
    (if (identifier? pats-stx)
        (syntax-e pats-stx)
        (syntax->datum pats-stx)))
  (cond
    ;; (имя . ост) — rest-only pattern
    [(symbol? pats-datum)
     (define rest-sym pats-datum)
     (hash-set!
      fast-macro-table
      name
      (λ (form)
        (define args (fast-macro-args-of form))
        (and args
             (let subst ([d tmpl-datum])
               (cond
                 [(eq? d rest-sym)
                  (datum->syntax form args)]
                 [(and (pair? d) (eq? (cdr d) rest-sym))
                  (datum->syntax
                   form
                   (cons (let ([h (subst (car d))])
                           (if (syntax? h) (syntax-e h) h))
                         args))]
                 [(symbol? d) (datum->syntax form d)]
                 [(pair? d)
                  (datum->syntax
                   form
                   (cons (subst (car d))
                         (let loop ([rest (cdr d)])
                           (cond
                             [(null? rest) '()]
                             [(pair? rest)
                              (cons (subst (car rest))
                                    (loop (cdr rest)))]
                             [else (subst rest)]))))]
                 [else (datum->syntax form d)])))))]
    ;; (имя а ...) — ellipsis pattern
    [(and (list? pats-datum)
          (eqv? (length pats-datum) 2)
          (symbol? (car pats-datum))
          (eq? (cadr pats-datum) '...))
     (define ell-var (car pats-datum))
     (hash-set!
      fast-macro-table
      name
      (λ (form)
        (define args (fast-macro-args-of form))
        (and args
             (fast-subst-ellipsis
              tmpl-datum form ell-var args '()))))]
    [else
     (define pat-syms pats-datum)
     (hash-set!
      fast-macro-table
      name
      (λ (form)
        (define args (fast-macro-args-of form))
        (and args
             (list? pat-syms)
             (andmap symbol? pat-syms)
             (eqv? (length args) (length pat-syms))
             (let ([env (map cons pat-syms args)])
               (define (subst d)
                 (cond
                   [(symbol? d)
                    (define hit (assq d env))
                    (if hit
                        (cdr hit)
                        (datum->syntax form d))]
                   [(pair? d)
                    (datum->syntax
                     form
                     (cons (subst (car d))
                           (let loop ([rest (cdr d)])
                             (cond
                               [(null? rest) '()]
                               [(pair? rest)
                                (cons (subst (car rest))
                                      (loop (cdr rest)))]
                               [else (subst rest)]))))]
                   [else (datum->syntax form d)]))
               (subst tmpl-datum)))))]))

(define-for-syntax (try-fast-macro form)
  (syntax-case form ()
    [(head . _)
     (and (identifier? #'head)
          (let ([f (hash-ref fast-macro-table
                             (syntax-e #'head)
                             #f)])
            (and f (f form))))]
    [_ #f]))

(define-for-syntax (pass-form->stxs form)
  ;; Match by symbol: head may be a rename-transformer synonym.
  ;; Returns (list emit-stx ...) including begin-for-syntax register.
  (define head (fast-form-head form))
  (if (eq? head 'определение-синтаксического-правила)
      (syntax-case form ()
        [(_ (name . pats) template)
         (begin
           (register-fast-syntax-rule!
            (syntax-e #'name) #'pats #'template)
           (list
            #`(begin-for-syntax
                (register-fast-syntax-rule!
                 '#,(syntax-e #'name)
                 (quote-syntax #,#'pats)
                 (quote-syntax #,#'template)))
            form))]
        [_ (list form)])
      (list form)))

;; Directory of the module that contains stx (for relative file requires).
(define-for-syntax (syntax-module-directory stx)
  (define src (syntax-source stx))
  (cond
    [(path? src) (path-only (simplify-path src #t))]
    [(string? src)
     (path-only (simplify-path (string->path src) #t))]
    [else (current-load-relative-directory)]))

;; Resolve symbol → 1/надо-быстро/<name>.1 ;
;; string → absolute file (relative would break under module-begin local-expand)
(define-for-syntax (имя-надо-быстро имя)
  (cond
    [(not (замена-адины? имя))
     (define d (syntax->datum имя))
     (if (and (pair? d) (eq? (car d) 'file) (pair? (cdr d))
              (string? (cadr d))
              (not (absolute-path? (cadr d))))
         (let* ([dir (or (syntax-module-directory имя)
                         (current-load-relative-directory)
                         (current-directory))]
                [abs (path->string
                      (simplify-path (build-path dir (cadr d)) #t))])
           (datum->syntax имя (list #'file abs) имя имя))
         имя)]
    [(string? (syntax-e имя))
     (define rel
       (if (string-suffix? (syntax-e имя) ".1")
           (syntax-e имя)
           (format "~a.1" (syntax-e имя))))
     (define dir
       (or (syntax-module-directory имя)
           (current-load-relative-directory)
           (current-directory)))
     (define abs
       (path->string (simplify-path (build-path dir rel) #t)))
     (datum->syntax имя (list #'file abs) имя имя)]
    [else
     (datum->syntax
      имя
      (list #'file
            (path->string
             (collection-file-path
              (format "надо-быстро/~a.1" (syntax-e имя))
              "1"
              #:check-compiled? #t)))
      имя имя)]))

(define-for-syntax (require-spec-key spec-stx)
  (define d (syntax->datum spec-stx))
  (cond
    [(and (pair? d) (eq? (car d) 'file) (pair? (cdr d)))
     (path->string (simplify-path (cadr d) #f))]
    [(string? d) d]
    [else (format "~a" d)]))

(define-for-syntax (source-is-fast-stdlib? stx)
  (define src (syntax-source stx))
  (and src
       (regexp-match?
        #rx"надо-быстро[/\\\\][^/\\\\]+\\.1$"
        (if (path? src) (path->string src) (format "~a" src)))))

(define-for-syntax (fast-form-head form)
  (syntax-case form ()
    [(h . _) (and (identifier? #'h) (syntax-e #'h))]
    [_ #f]))

;; Namespace for names imported via используется inside надо-быстро.
;; #%require (via syntax-local-lift-require) uses (prefix id path),
;; not the require-macro form (prefix-in id spec).
(define-for-syntax fast-ns-prefix-str "надо-быстро:")
(define-for-syntax fast-req-intro (box values))
(define-for-syntax fast-locals-box (box '()))

(define-for-syntax (fast-ir-head? sym)
  (memq sym
        '(си + - * / == /= < > <= >= && || = блок begin
          #%app #%top #%datum)))

;; Core IR heads — do not expand further (codegen consumes them)
(define-for-syntax fast-stop-ids
  (syntax->list
   #'(си + - * / == /= < > <= >= && || = блок begin
        #%app #%datum quote #%top #%expression)))

(define-for-syntax (fast-prefix-id id)
  (format-id id "~a~a" fast-ns-prefix-str (syntax-e id)
             #:source id #:props id))

(define-for-syntax (fast-local-sym? sym)
  (memq sym (unbox fast-locals-box)))

(define-for-syntax (header-arg-syms header-stx)
  (syntax-case header-stx ()
    [(fname arg ...)
     (let loop ([xs (syntax->list #'(arg ...))] [acc '()])
       (cond
         [(null? xs) (reverse acc)]
         [(and (pair? xs) (pair? (cdr xs))
               (identifier? (car xs)))
          (loop (cddr xs)
                (cons (syntax-e (car xs)) acc))]
         [(pair? xs) (loop (cdr xs) acc)]
         [else (reverse acc)]))]
    [_ '()]))

(define-for-syntax (prefixed-form form)
  ;; Rewrite free identifier heads to надо-быстро:имя for import lookup
  (syntax-case form ()
    [(head arg ...)
     (and (identifier? #'head)
          (not (fast-ir-head? (syntax-e #'head)))
          (not (fast-local-sym? (syntax-e #'head))))
     (let* ([intro (unbox fast-req-intro)]
            [pid (intro (fast-prefix-id #'head))])
       (datum->syntax
        form
        (cons pid (syntax->list #'(arg ...)))
        form))]
    [_ form]))

;; Macros via table (same-block) or prefixed imports; else C++ name.
(define-for-syntax (expand-fast-walk form)
  (syntax-case form ()
    [(a ...)
     (with-syntax
         ([(ea ...)
           (map expand-fast-expr (syntax->list #'(a ...)))])
       #'(ea ...))]
    [_ form]))

(define-for-syntax (expand-fast-expr form)
  (syntax-case form ()
    [(head . _)
     (and (identifier? #'head)
          (not (fast-ir-head? (syntax-e #'head))))
     (cond
       [(fast-local-sym? (syntax-e #'head))
        (expand-fast-walk form)]
       [(try-fast-macro form)
        => expand-fast-expr]
       [else
        (let* ([form* (prefixed-form form)]
               [ex (local-expand form* 'expression
                                 fast-stop-ids)]
               [same?
                (equal? (syntax->datum ex)
                        (syntax->datum form*))])
          (cond
            [same? (expand-fast-walk form)]
            [else
             (syntax-case ex (#%app #%top)
               [(#%app (#%top . _) . _)
                (expand-fast-walk form)]
               [_ (expand-fast-expr ex)])]))])]
    [(a ...) (expand-fast-walk form)]
    [_ form]))

(define-for-syntax (fast-begin-head? sym)
  (memq sym '(блок begin)))

(define-for-syntax (flatten-fast-stmts stxs)
  (apply
   append
   (for/list ([s (in-list stxs)])
     (syntax-case s ()
       [(head x ...)
        (and (identifier? #'head)
             (fast-begin-head? (syntax-e #'head)))
        (flatten-fast-stmts (syntax->list #'(x ...)))]
       [_ (list s)]))))

(define-for-syntax (expand-fast-stmt form)
  (syntax-case form ()
    [(head id expr)
     (and (identifier? #'head)
          (eq? (syntax-e #'head) '=)
          (identifier? #'id))
     (begin
       (set-box! fast-locals-box
                 (cons (syntax-e #'id)
                       (unbox fast-locals-box)))
       (with-syntax ([e (expand-fast-expr #'expr)])
         #'(= id e)))]
    [(head s ...)
     (and (identifier? #'head)
          (fast-begin-head? (syntax-e #'head)))
     (with-syntax
         ([(es ...)
           (map expand-fast-stmt
                (syntax->list #'(s ...)))])
       #'(head es ...))]
    [_ (expand-fast-expr form)]))

(define-for-syntax (expand-fast-function form)
  (syntax-parse form
    [((~datum =) header body ...)
     (set-box! fast-locals-box (header-arg-syms #'header))
     (with-syntax
         ([(ebody ...)
           (flatten-fast-stmts
            (map expand-fast-stmt
                 (syntax->list #'(body ...))))])
       #'(= header ebody ...))]
    [_ form]))

(define-syntax (надо-быстро-код stx)
  (syntax-parse stx
    [(_ mod-key:str (dep-key:str ...) form ...)
     (define key (syntax-e #'mod-key))
     (set-current-fast-module-key! key)
     (define dep-keys (syntax->datum #'(dep-key ...)))
     ;; Splice dependency C++ (as #include) into this unit
     (define deps-cpp
       (string-append*
        (for/list ([dk (in-list dep-keys)])
          (or (lookup-module-cpp dk) ""))))
     (when (positive? (string-length deps-cpp))
       (add-cpp-fragment! deps-cpp))
     ;; Expand macros (table + same-block define-syntax via local-expand)
     (define raw-forms (syntax->list #'(form ...)))
     (define expanded-forms
       (for/list ([f raw-forms])
         (if (eq? (fast-form-head f) '=)
             (expand-fast-function f)
             f)))
     (define-values (top-cpp fun-cpp exports)
       (adina-fast->cpp
        (datum->syntax stx expanded-forms)
        fresh-generated-name))
     ;; Register own + deps so clients get transitive splice
     (define body-cpp
       (string-append deps-cpp top-cpp fun-cpp))
     (register-module-cpp! key body-cpp)
     (when (positive? (string-length top-cpp))
       (add-cpp-fragment! top-cpp))
     (when (positive? (string-length fun-cpp))
       (add-cpp-fragment! export-preamble)
       (add-cpp-fragment! fun-cpp))
     (for ([ex (in-list exports)])
       (add-fast-export! ex))
     (define cpp-all (take-cpp-source))
     (define so-path
       (and (pair? exports)
            (compile-cpp-to-shared cpp-all)))
     (with-syntax ([key key]
                   [body-cpp body-cpp]
                   [(bind ...)
                    (map export->binding exports)])
       (if so-path
           (with-syntax ([so so-path])
             #'(begin
                 (begin-for-syntax
                   (register-module-cpp! key body-cpp))
                 (set-box! fast-lib-box (ffi-lib so))
                 bind ...))
           #'(begin
               (begin-for-syntax
                 (register-module-cpp! key body-cpp))
               bind ...)))]))

(define-for-syntax (surface-fast-module-path)
  (path->string
   (collection-file-path "надо-быстро.1" "1"
                         #:check-compiled? #t)))

(define-for-syntax (source-is-fast-surface? stx)
  (define src (syntax-source stx))
  (and src
       (regexp-match?
        #rx"[/\\\\]надо-быстро\\.1$"
        (if (path? src) (path->string src) (format "~a" src)))))

(define-syntax (надо-быстро stx)
  (syntax-parse stx
    [(_ form ...)
     (define forms0 (syntax->list #'(form ...)))
     ;; User modules auto-pull surface (iostream + built-ins)
     (define add-surface?
       (and (not (source-is-fast-stdlib? stx))
            (not (source-is-fast-surface? stx))))
     (define forms
       (if add-surface?
           (cons
            (datum->syntax
             stx
             (list #'используется
                   (list #'file (surface-fast-module-path)))
             stx)
            forms0)
           forms0))
     (define dep-keys '())
     (define require-specs '())
     (define pass-stxs '()) ; provide / define-syntax*
     (define code-stxs '()) ; си / function defs
     (define mod-key
       (let ([src (syntax-source stx)])
         (cond
           [(path? src)
            (path->string (simplify-path src #f))]
           [src (format "~a" src)]
           [else
            (format "anon-fast-~a"
                    (eq-hash-code stx))])))
     (for ([form forms])
       (define head (fast-form-head form))
       (case head
         [(используется)
          (syntax-case form (используется)
            [(используется mod)
             (let* ([spec (имя-надо-быстро #'mod)]
                    [key (require-spec-key spec)])
               (set! require-specs
                     (append require-specs (list spec)))
               (set! dep-keys
                     (append dep-keys (list key))))]
            [(используется mod ...)
             (for ([m (syntax->list #'(mod ...))])
               (define spec (имя-надо-быстро m))
               (define key (require-spec-key spec))
               (set! require-specs
                     (append require-specs (list spec)))
               (set! dep-keys
                     (append dep-keys (list key))))])]
         [(предоставлять
           определение-синтаксиса
           определение-синтаксического-правила)
          (set! pass-stxs
                (append pass-stxs (pass-form->stxs form)))]
         [else
          (set! code-stxs (append code-stxs (list form)))]))
     ;; Lift (prefix надо-быстро: raw-module-path) — #%require form
     (define req-base (datum->syntax stx 'fast-req-base))
     (define req-marked
       (for/fold ([s req-base])
                 ([spec (in-list require-specs)])
         (define pspec
           (with-syntax ([s spec])
             #'(prefix надо-быстро: s)))
         (syntax-local-lift-require pspec s)))
     (set-box!
      fast-req-intro
      (make-syntax-delta-introducer req-marked req-base))
     (with-syntax
         ([(pass ...) pass-stxs]
          [(code ...) code-stxs]
          [key mod-key]
          [(dkey ...) dep-keys])
       #'(begin
           pass ...
           (надо-быстро-код key (dkey ...) code ...)))]))

(define-for-syntax (add-headers stx body)
  (define base-srcloc (srcloc (syntax-source stx) 1 0 1 3))
  (define (get-atom exprs)
    (define datum (syntax-e exprs))
    (cond
      [(symbol? datum) exprs]
      [(null? #f)]
      [(list? datum)
       (ormap get-atom datum)]
      [else #f]))

  (syntax-case body ()
    [(expr ...)
     (begin
       #`((используется #,(datum->syntax stx 'базовая base-srcloc
                                         (get-atom #'(expr ...))))
          expr ...))]
    [_ body]))

(define-syntax (module-begin stx)
  (syntax-case stx (системная)
    [(_ системная body ...)
     (quasisyntax/loc stx
       (#%module-begin
        (require (for-syntax 1/run-fast))
        (begin-for-syntax
          (start '#,(syntax-source stx))
          (reset-fast-state!))
        body ...
        (begin-for-syntax (end))))]
    [(_ body ...)
     (with-syntax ([(new-body ...)
                    (add-headers stx #'(body ...))])
       (quasisyntax/loc stx
         (#%module-begin
          (require (for-syntax 1/run-fast))
          (begin-for-syntax
            (start '#,(syntax-source stx))
            (reset-fast-state!))
          new-body ...
          (begin-for-syntax (end)))))]))
