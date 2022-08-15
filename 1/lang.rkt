#lang racket/base
(require racket/match racket/vector racket/string racket/class         
         (for-syntax racket/base racket/match 1/run-fast))
(provide (rename-out [module-begin #%module-begin])
         (except-out (all-defined-out) module-begin русифицировать-вывод old-printer printer)
         #%top-interaction #%app #%datum + - / * < > <= >= => #%top функция)
(provide (for-syntax #%app #%top #%datum выбор-синтаксиса разобрать-синтаксис
                     синтаксис syntax λ функция ...))

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
    [(_ коллекция[элемент] значение)
     #'(cond
         [(vector? объект) (vector-set! объект поле)]
         [(hash? объект) (hash-set! объект поле)]
         [(string? объект) (string-set! объект поле)]
         [else
          (raise-syntax-error 'квадратные-скобки
                                   "У объекта ~a нет доступа к полям через квадратные скобки"
                                   объект)])]
    [(_ а б) #'(let () (set! а б) а)]))

(module syn racket
  (require syntax/parse)
  (provide выбор-синтаксиса разобрать-синтаксис синтаксис синоним квазисинтаксис функция)
  (define-syntax (синоним stx)
    (syntax-case stx ()
      [(_ старый новый)     
       #'(define-syntax (новый stx)
           (syntax-case stx ()
             [(_ . a) #'(старый . a)]))]))
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
  (синоним syntax синтаксис)
  (синоним quasisyntax квазисинтаксис)
  (синоним lambda функция))
(require (for-syntax 'syn) 'syn)

(синоним quote цитата)
(синоним quasiquote квазицитата)
(синоним unquote не-цитируя)
(синоним unquote-splicing не-цитируя-список)

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
                          #'имя #'имя #'имя)))]
    [(_ (с-префиксом префикс имя))
     (quasisyntax/loc stx
       (require (prefix-in префикс имя)))]
    [(_ (файл имя)) (quasisyntax/loc stx
                      (require #,(datum->syntax #'имя (list #'file #'имя) #'имя #'имя #'имя)))]
    [(_ x) (syntax/loc stx (require x))]
    [(_ x ...) #'(begin (используется x) ...)]))

(define-syntax (используется-для-синтаксиса stx)
  (syntax-case stx (с-префиксом файл)
    [(_ x)
     (and (identifier? #'x) (not (module-path? (syntax-e #'x))))
     (quasisyntax/loc stx
       (require
         (for-syntax
          #,(datum->syntax #'x
                           (list #'file (path->string
                                        (collection-file-path
                                         (format "~a.1" (syntax-e #'x)) "1"
                                         #:check-compiled? #t)))
                           #'имя #'имя))))]
    [(_ (с-префиксом префикс имя))
     (quasisyntax/loc stx
       (require (for-syntax (prefix-in префикс имя))))]
    [(_ (файл имя))
     (quasisyntax/loc stx
       (require (for-syntax #,(datum->syntax #'имя (list #'file #'имя) #'имя #'имя))))]
    [(_ x) (syntax/loc stx (require x))]
    [(_ x ...) #'(begin (используется x) ...)]))

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

(синоним begin блок)
(синоним begin-for-syntax при-компиляции)
(синоним define-syntax определение-синтаксиса)
(синоним define-syntax-rule определение-синтаксисического-правила)
(синоним or ||)
(синоним and &&)
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
(define (printer что [порт (current-output-port)] [глубина 0])
   (define строковый-порт (открыть-запись-в-строку))
   (old-printer что строковый-порт глубина)
   (записать-строку
     (русифицировать-вывод (получить-записанную-строку строковый-порт))
     порт))

(синоним read прочитать)
(синоним read-line прочитать-строку)
(синоним when когда)
(define (ошибка . т) (apply error т))
(define == equal?)
(define === eqv?)
(define пусто (void))
(define (/= x y) (not (== x y)))
(define (// x y) (quotient x y))
(define (% x y) (remainder x y))
(define (подстрока str start [end (string-length str)]) (substring str start end))
(define (длина-строки т) (string-length т))
(define (аргументы-командной-строки) (current-command-line-arguments))
(define (в-строках порт) (in-lines порт))
(define (в-соответствии соответствие) (in-hash соответствие))
(define (развернуть список) (reverse список))
(define (обрезать строка) (string-trim строка))
(define (объединить-строки список [разделитель " "]) (string-join список разделитель))
(define (не x) (not x))
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

(define (++ коллекция . коллекции)
   (cond
     [(list? коллекция) (apply append коллекция коллекции)]
     [(string? коллекция) (apply string-append коллекция коллекции)]
     [(vector? коллекция) (apply vector-append коллекция коллекции)]))

(определение-синтаксиса (если stx)
  (выбор-синтаксиса stx (=> иначе)
    [(_ (=> условие функция) остаток ...)
     #'(let ([t условие])
         (if t (функция t) (если остаток ...)))]
    [(_ (иначе действия ...) остаток ...)
     #'(let () действия ...)]
    [(_ (условие действия ...) остаток ...)
     #'(if условие
           (let () действия ...)
           (если остаток ...))]
    [(_) #'(void)]))

(define-for-syntax (замена-ключевого-слова stx)
  (define замена
    (case (syntax-e stx)
      [(#:когда) '#:when]
      [(#:когда-не) '#:unless]
      [(#:прервать)  '#:break]
      [(#:последняя) '#:final]
      [else #f]))
  замена)

(define-for-syntax (преобразовать-слово-цикла stx)
  (define l (syntax-e stx))
  (cond
    [(list? l)
     (define замена (замена-ключевого-слова (car l)))
     (if замена
         (datum->syntax stx (cons (datum->syntax (car l) замена) (cdr l)))
         stx)]
    [else
     (define замена (замена-ключевого-слова stx))
     (if замена
         (datum->syntax stx замена)
         stx)]))

(define-for-syntax (преобразовать-слова-цикла stx)
  (define l (syntax-e stx))
  (cond
    [(list? l)
     (datum->syntax stx (map преобразовать-слово-цикла l))]
    [else stx]))

(определение-синтаксиса (определение-синтаксиса-цикла синтаксис)
  (выбор-синтаксиса синтаксис ()
    [(_ русский английский)
     #'(определение-синтаксиса (русский синтаксис)
         (выбор-синтаксиса синтаксис (для)
           [(_ (для (А Б) . В) . Г) #`(английский ((А Б) . #,(преобразовать-слова-цикла #'В)) . Г)]
           [(_ (А Б) . Г) #'(английский ((А Б)) . Г)]
           [(_ . А) #'(английский . А)]))]))

(определение-синтаксиса-цикла цикл for)
(определение-синтаксиса-цикла цикл/первый for/first)
(определение-синтаксиса-цикла цикл/список for/list)

(определение-синтаксиса (пусть синтаксис)
  (выбор-синтаксиса синтаксис ()
    [(_ ((А Б) . В) . Г) #'(let ((А Б) . В) . Г)]
    [(_ (А Б) . Г) #'(let ((А Б)) . Г)]
    [(_ ИМЯ ((А Б) . В) . Г) #'(let ИМЯ ((А Б) . В) . Г)]
    [(_ ИМЯ (А Б) . Г) #'(let ИМЯ ((А Б)) . Г)]
    [(_ . А) #'(let . А)]))

(define истина #t)
(define ложь #f)

(define (квадратные-скобки объект поле)
  (cond
    [(list? объект) (list-ref объект поле)]
    [(vector? объект) (vector-ref объект поле)]
    [(hash? объект) (hash-ref объект поле #f)]
    [(string? объект) (string-ref объект поле)]
    [else (raise-syntax-error 'квадратные-скобки
                              "У объекта ~a нет доступа к полям через квадратные скобки"
                              объект)]))

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
        (global-port-print-handler printer)
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
