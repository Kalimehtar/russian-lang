#lang racket/base
(require racket/class 1/syn racket/contract racket/splicing
         (for-syntax (except-in racket/base =) 1/run-fast racket/contract racket/string))
(provide (rename-out [module-begin #%module-begin])
         #%top-interaction
         (except-out (all-defined-out) module-begin)
         splicing-parameterize english except-in
         #%app #%datum + - / * < > <= >= => #%top (all-from-out 'syn) ->
         (for-syntax #%app #%top #%datum + - / * < > <= >= =>
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

(define-syntax (надо-быстро stx)
  1)

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
       #`((используется #,(datum->syntax stx 'базовая base-srcloc (get-atom #'(expr ...))))
          expr ...))]
    [_ body]))

(define-syntax (module-begin stx)
  (syntax-case stx (системная)
    [(_ системная body ...)
     (quasisyntax/loc stx
       (#%module-begin
        (require (for-syntax 1/run-fast))
        (begin-for-syntax (start '#,(syntax-source stx)))        
        body ...
        (begin-for-syntax (end))))]
    [(_ body ...)
     (with-syntax ([(new-body ...) (add-headers stx #'(body ...))])
       (quasisyntax/loc stx
         (#%module-begin
          (require (for-syntax 1/run-fast))
          (begin-for-syntax (start '#,(syntax-source stx)))        
          new-body ...
          (begin-for-syntax (end)))))]))
