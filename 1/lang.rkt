#lang racket/base
(require racket/vector racket/string racket/class 1/syn
         (for-syntax (except-in racket/base =) (except-in racket/match ==) 1/run-fast))
(provide (rename-out [module-begin #%module-begin])
         (except-out (all-defined-out) module-begin русифицировать-вывод old-printer #;printer)
         #%top-interaction #%app #%datum + - / * < > <= >= => #%top (all-from-out 'syn))
(provide (for-syntax #%app #%top #%datum + - / * < > <= >= =>
                     (all-from-out 'syn) λ ... _))

(module syn racket/base
  (require syntax/parse (except-in racket/match ==)
           1/syn racket/string racket/vector (for-syntax syntax/stx racket/base racket/syntax))
  (provide (all-defined-out)
           syntax quasisyntax unsyntax unsyntax-splicing quote)
  (define (translate e)
    (define dict
      '(("bad syntax" . "ошибка синтаксиса")))
    (define (replace-dict str dict)
      (if (null? dict)
          str
          (replace-dict (string-replace str (caar dict) (cdar dict)) (cdr dict))))
    (cond
      [(exn:fail:syntax? e)
       (exn:fail:syntax (replace-dict (exn-message e) dict)
                        (exn-continuation-marks e)
                        (exn:fail:syntax-exprs e))]
      [else e]))
  (define-syntax (выбор-синтаксиса stx)
    (syntax-case stx ()
      [(_ правила ...)
       #'(with-handlers ([exn:fail:syntax? (λ (e) (raise (translate e)))])
           (syntax-case правила ...))]))
  (define-syntax (разобрать-синтаксис stx)
    (syntax-case stx ()
      [(_ правила ...)
       #'(syntax-parse правила ...)]))
  (define-for-syntax (заменять-рекурсивно синтаксис)
    (syntax-case синтаксис ()
      [(s ...)
       (datum->syntax
        синтаксис
        (stx-map заменять-рекурсивно
                 (syntax-case синтаксис (не-цитируя не-цитируя-список)
                   [(не-цитируя f ...)
                    #'(unquote f ...)]
                   [(не-цитируя-список f ...)
                    #'(unquote-splicing f ...)]
                   [_ синтаксис]))
        синтаксис
        синтаксис)]
      [_ синтаксис]))
  (define-syntax (квазицитата stx)
    (syntax-case stx ()
      [(_ значение)
       #`(quasiquote #,(заменять-рекурсивно #'значение))]))
  (синоним syntax синтаксис)
  (синоним quasisyntax квазисинтаксис)
  (синоним quasisyntax/loc квазисинтаксис/место)
  (синоним unsyntax-splicing не-цитируя-список-синтаксиса)
  (синоним unsyntax не-цитируя-синтаксис)
  (синоним quote цитата)
  (синоним unquote не-цитируя)
  (синоним unquote-splicing не-цитируя-список)
  (синоним lambda функция)
  (синоним or ||)
  (синоним and &&)
  (синоним read прочитать)
  (синоним read-line прочитать-строку)
  (define (ошибка . т) (apply error т))
  (define == equal?)
  (define === eqv?)
  (define пусто (void))
  (define (/= x y) (not (== x y)))
  (define (// x y) (quotient x y))
  (define (% x y) (remainder x y))
  (define (не x) (not x))
  (синоним begin блок)
  (синоним begin-for-syntax при-компиляции)
  (синоним define-syntax определение-синтаксиса)
  (синоним define-syntax-rule определение-синтаксического-правила)

  ;; НАДО: сделать перевод языка шаблонов для match, match-define, match-define-values
  (define-syntax (= stx)
    (syntax-case stx (значения шаблон шаблоны)
      [(_ (значения . а) б) #'(define-values а б)]
      [(_ (значения . а) б в ...) #'(define-values а (б в ...))]
      [(_ (шаблон а) б) #'(match-define а б)]
      [(_ (шаблон а) б в ...) #'(match-define а (б в ...))]
      [(_ (шаблоны . а) б) #'(match-define-values а б)]
      [(_ (шаблоны . а) б в ...) #'(match-define-values а (б в ...))]
      [(_ (а ...) . б) #'(define (а ...) . б)]
      [(_ а б) #'(define а б)]
      [(_ а б в ...) #'(define а (б в ...))]))

  (define (значения . a) (apply values a))

  (define-syntax (:= stx)
    (syntax-case stx (значения)
      [(_ (значения . а) б) #'(let () (set!-values а б) (values . а))]
      [(_ объект[поле] значение)
       #'(let ()
           (cond
             [(vector? объект) (vector-set! объект поле значение)]
             [(hash? объект) (hash-set! объект поле значение)]
             [(string? объект) (string-set! объект поле значение)]
             [else
              (raise-syntax-error 'квадратные-скобки
                                  "У объекта ~a нет доступа к полям через квадратные скобки"
                                  объект)])
           значение)]
      [(_ (поле а) б) #`(let () (#,(format-id #'поле "изменить-~a!" (syntax-e #'поле)) а б))]
      [(_ а б) #'(let () (set! а б) а)]))
  (define истина #t)
  (define ложь #f)

  (define (++ коллекция . коллекции)
   (cond
     [(list? коллекция) (apply append коллекция коллекции)]
     [(string? коллекция) (apply string-append коллекция коллекции)]
     [(vector? коллекция) (apply vector-append коллекция коллекции)]))

  (define (квадратные-скобки объект поле)
    (cond
      [(list? объект) (list-ref объект поле)]
      [(vector? объект) (vector-ref объект поле)]
      [(hash? объект) (hash-ref объект поле #f)]
      [(string? объект) (string-ref объект поле)]
      [else (raise-syntax-error 'квадратные-скобки
                                "У объекта ~a нет доступа к полям через квадратные скобки"
                                объект)])))
(require (for-syntax 'syn) 'syn)

(define-syntax (используется stx)
  (syntax-case stx (с-префиксом файл)
    [(_ имя)
     (and (identifier? #'имя) (not (module-path? (syntax-e #'имя))))
     (quasisyntax/loc stx
       (require
         #,(datum->syntax #'имя
                          (list #'file (path->string
                                       (collection-file-path
                                        (format "~a.1" (syntax-e #'имя)) "1"
                                        #:check-compiled? #t)))
                          #'имя #'имя)))]
    [(_ (с-префиксом префикс имя))
     (quasisyntax/loc stx
       (require (prefix-in префикс имя)))]
    [(_ (файл имя)) (quasisyntax/loc stx
                      (require #,(datum->syntax #'имя (list #'file #'имя) #'имя #'имя)))]
    [(_ x) (syntax/loc stx (require x))]
    [(_ x ...) #'(begin (используется x) ...)]))

(define-syntax (используется-для-синтаксиса stx)
  (syntax-case stx (с-префиксом файл)
    [(_ имя)
     (and (identifier? #'имя) (not (module-path? (syntax-e #'имя))))
     (quasisyntax/loc stx
       (require
         (for-syntax
          #,(datum->syntax #'имя
                           (list #'file (path->string
                                        (collection-file-path
                                         (format "~a.1" (syntax-e #'имя)) "1"
                                         #:check-compiled? #t)))
                           #'имя #'имя))))]
    [(_ (с-префиксом префикс имя))
     (quasisyntax/loc stx
       (require (for-syntax (prefix-in префикс имя))))]
    [(_ (файл имя))
     (quasisyntax/loc stx
       (require (for-syntax #,(datum->syntax #'имя (list #'file #'имя) #'имя #'имя))))]
    [(_ x) (syntax/loc stx (require (for-syntax x)))]
    [(_ x ...) #'(begin (используется-для-синтаксиса x) ...)]))

(define-syntax (предоставлять stx)
  (syntax-case stx (всё-из)
    [(_ (всё-из имя))
     (and (identifier? #'имя) (not (module-path? (syntax-e #'имя))))
     (quasisyntax/loc stx
       (provide
         (all-from-out
          #,(datum->syntax #'имя
                           (list 'file (path->string
                                        (collection-file-path
                                         (format "~a.1" (syntax-e #'имя)) "1"
                                         #:check-compiled? #t)))))))]
    [(_ (всё-из имя))
     (syntax-local-introduce
      (quasisyntax/loc stx (provide (all-from-out имя))))]
    [(_ x) (syntax/loc stx (provide x))]
    [(_ x ...) (syntax/loc stx (begin (предоставлять x) ...))]))

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

(синоним struct структура)
;; НАДО: так нельзя, надо определять новые функции,
;;       иначе имя открыть-запись-в-строку остаётся #<procedure:open-output-string>
(синоним open-output-string открыть-запись-в-строку)
(синоним get-output-string получить-записанную-строку)
(синоним write-string записать-строку)
(синоним string-replace заменить-в-строке)
(define (русифицировать-вывод строка)
   (define (заменить-по-словарю строка словарь)
      (if (null? словарь)
          строка
          (заменить-по-словарю (заменить-в-строке строка
                                                  (caar словарь) (cdar словарь)) (cdr словарь))))
    (заменить-по-словарю строка
                  '(("#t" . "истина")
                    ("#f" . "ложь")
                    ("#<procedure:" . "#<функция:"))))

(define (вывести что [порт (current-output-port)])
   (define строковый-порт (открыть-запись-в-строку))
   (display что строковый-порт)
   (записать-строку
     (русифицировать-вывод (получить-записанную-строку строковый-порт))
     порт))
(define (вывести/пс что [порт (current-output-port)])
   (вывести что порт)
   (newline порт))
(define (записать что [порт (current-output-port)])
   (define строковый-порт (открыть-запись-в-строку))
   (write что строковый-порт)
   (записать-строку
     (русифицировать-вывод (получить-записанную-строку строковый-порт))
     порт))
(define (записать/пс что [порт (current-output-port)])
   (записать что порт)
   (newline порт))

(define old-printer (global-port-print-handler))
#;(define (printer что [порт (current-output-port)] [глубина 0])
  (define (byte-rus s start end)
    (string->bytes/utf-8
     (русифицировать-вывод
      (bytes->string/utf-8 (subbytes s start end)))))
  (define russian-port
    (make-output-port
     'byte-upcase
     ; This port is ready when the original is ready:
     порт
     ; Writing procedure:
     (lambda (s* start end non-block? breakable?)
       (parameterize ([global-port-print-handler old-printer])
         (let ([s (byte-rus s* start end)])
           (if non-block?
               (write-bytes-avail* s порт)
               (begin
                 (display s порт)
                 (bytes-length s*))))))
     ; Close procedure — close original port:
     (lambda () (close-output-port порт))
     ; write-out-special
     порт
     ; Write event:
     (and (port-writes-atomic? порт)
          (lambda (s start end)
            (write-bytes-avail-evt
             (byte-rus s start end)
             порт)))))
  (old-printer что russian-port глубина))

(define (printer что [порт (current-output-port)] [глубина 0])
  (define строковый-порт (открыть-запись-в-строку))
  (old-printer что строковый-порт глубина)
  (записать-строку
   (русифицировать-вывод (получить-записанную-строку строковый-порт))
   порт))

(define (подстрока str start [end (string-length str)]) (substring str start end))
(define (длина-строки т) (string-length т))
(define (аргументы-командной-строки) (current-command-line-arguments))
(define (в-строках порт) (in-lines порт))
(define (в-соответствии соответствие) (in-hash соответствие))
(define (развернуть список) (reverse список))
(define (обрезать строка) (string-trim строка))
(define (объединить-строки список [разделитель " "]) (string-join список разделитель))
(define (есть-файл? ф) (file-exists? ф))

(define (создать-соответствие
         #:глубокое-сравнение (глубокое #f)
         #:значение (значение null)
         #:слабое (слабое #f))
  (if слабое
      (if глубокое
          (make-weak-hash значение)
          (make-weak-hasheqv значение))
      (if глубокое
          (make-hash значение)
          (make-hasheqv значение))))

(define (значение-соответствия соответствие ключ #:если-нет [если-нет #f])
  (hash-ref соответствие ключ если-нет))
(define (вставить-в-соответствие! соответствие ключ значение) (hash-set! соответствие ключ значение))

(синоним send отправить)
(синоним send+ отправить+)

(define-syntax (надо-быстро stx)
  1)

(define-for-syntax (add-headers stx body)
  (define base-srcloc (srcloc (syntax-source stx) 1 0 1 3))
  (syntax-case body ()
    [(expr1 expr ...)
     (begin
       #`((используется #,(datum->syntax stx 'базовая base-srcloc #'orig)) expr1 expr ...))]
    [_ body]))

(define-syntax (module-begin stx)
  (syntax-case stx (системная)
    [(_ системная body ...)
     (quasisyntax/loc stx
       (#%module-begin
        (require (for-syntax 1/run-fast))
        ;(global-port-print-handler printer)
        (begin-for-syntax (start '#,(syntax-source stx)))        
        body ...
        (begin-for-syntax (end))))]
    [(_ body ...)
     (with-syntax ([(new-body ...) (add-headers stx #'(body ...))])
       (quasisyntax/loc stx
         (#%module-begin
          (require (for-syntax 1/run-fast))
          (global-port-print-handler printer)
          (begin-for-syntax (start '#,(syntax-source stx)))        
          new-body ...
          (begin-for-syntax (end)))))]))
