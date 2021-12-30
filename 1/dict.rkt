#lang racket/base
(require racket/match racket/vector racket/string racket/class)
(require (for-syntax racket/base racket/match 1/run-fast) (prefix-in rkt: racket))
(provide (rename-out [module-begin #%module-begin])
         (except-out (all-defined-out) module-begin синоним русифицировать-вывод old-printer printer)
         #%top-interaction #%app #%datum + - / * < > <= >= => #%top цитата квазицитата)

;; НАДО: сделать перевод языка шаблонов для match, match-define, match-define-values
(define-match-expander массив
  (λ (stx)
    (syntax-case stx ()
      [(_ pat ...)
       #'(vector pat ...)]
      [(_ pat ... . rest-pat)
       #'(app vector->list (list-rest pat ... rest-pat))]))
  (λ (stx)
    (syntax-case stx ()
      [(_ pat ...)
       #'(vector pat ...)])))

(define-match-expander список
  (λ (stx)
    (syntax-case stx ()
      [(_ pat ...)
       #'(list pat ...)]
      [(_ pat ... . rest-pat)
       #'(list-rest pat ... rest-pat)]))
  (λ (stx)
    (syntax-case stx ()
      [(_ pat ...)
       #'(list pat ...)])))

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
         [else (raise-syntax-error 'квадратные-скобки
                                   "У объекта ~a нет доступа к полям через квадратные скобки"
                                   объект)])]
    [(_ а б) #'(let () (set! а б) а)]))

(define-syntax (синоним stx)
  (syntax-case stx ()
    [(_ старый новый)     
     #'(define-syntax (новый stx)
         (syntax-case stx ()
           [(_ . a) #'(старый . a)]))]))

(синоним quote цитата)
(синоним quasiquote квазицитата)
(синоним unquote не-цитируя)
(синоним unquote-splicing не-цитируя-список)
(синоним require использовать)
(синоним provide предоставлять)
(синоним begin блок)
(синоним begin-for-syntax при-компиляции)
(синоним define-syntax определение-синтаксиса)
(синоним define-syntax-rule определение-синтаксисического-правила)
(синоним or ||)
(синоним and &&)
(синоним if ?)
;; НАДО: так нельзя, надо определять новые функции, иначе имя открыть-запись-в-строку остаётся #<procedure:open-output-string>
(синоним open-output-string открыть-запись-в-строку)
(синоним get-output-string получить-записанную-строку)
(синоним write-string записать-строку)
(синоним string-replace заменить-в-строке)
(define (пустой? список) (null? список))
(define (русифицировать-вывод строка)
   (define (заменить-по-словарю строка словарь)
      (if (пустой? словарь)
          строка
          (заменить-по-словарю (заменить-в-строке строка (caar словарь) (cdar словарь)) (cdr словарь))))
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
(синоним lambda функция)
(синоним when когда)
(define (ошибка . т) (apply error т))
(define == equal?)
(define === eqv?)
(define пусто (void))
(define (/= x y) (not (== x y)))
(define (// x y) (quotient x y))
(define (% x y) (remainder x y))
(define (подстрока str start [end (string-length str)]) (substring str start end))
(define (пара параметр1 параметр2) (cons параметр1 параметр2))
(define (пара? т) (rkt:cons? т))
(define (список? т) (list? т))
(define (массив? т) (vector? т))
(define (длина-массива т) (vector-length т))
(define (аргументы-командной-строки) (current-command-line-arguments))
(define (читая-файл имя обработка) (call-with-input-file имя обработка))
(define (в-строках порт) (in-lines порт))
(define (в-соответствии соответствие) (in-hash соответствие))

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

(module syn racket
  (require syntax/parse)
  (provide варианты-синтаксиса разобрать-синтаксис)  
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
  (define-syntax (варианты-синтаксиса stx)
    (syntax-case stx ()
      [(_ правила ...)
       #'(with-handlers ([exn:fail:syntax? (λ (e) (raise (translate e)))])
           (syntax-case правила ...))]))
  (define-syntax (разобрать-синтаксис stx)
    (syntax-case stx ()
      [(_ правила ...)
       #'(syntax-parse правила ...)])))
(require (for-syntax 'syn))

(определение-синтаксиса (условия stx)
  (варианты-синтаксиса stx (=> иначе)
    [(_ (=> условие функция) остаток ...)
     #'(let ([t условие])
         (if t (функция t) (условия остаток ...)))]
    [(_ (иначе действия ...) остаток ...)
     #'(begin действия ...)]
    [(_ (условие действия ...) остаток ...)
     #'(if условие
           (begin действия ...)
           (условия остаток ...))]
    [(_) #'(void)]))

(определение-синтаксиса (для синтаксис)
  (варианты-синтаксиса синтаксис ()
    [(_ ((А Б) . В) . Г) #'(for ((А Б) . В) Г)]
    [(_ (А Б) . Г) #'(for ((А Б)) Г)]
    [(_ . А) #'(for . А)]))

(определение-синтаксиса (пусть синтаксис)
  (варианты-синтаксиса синтаксис ()
    [(_ ((А Б) . В) . Г) #'(let ((А Б) . В) Г)]
    [(_ (А Б) . Г) #'(let ((А Б)) Г)]
    [(_ ИМЯ ((А Б) . В) . Г) #'(let ИМЯ ((А Б) . В) Г)]
    [(_ ИМЯ (А Б) . Г) #'(let ИМЯ ((А Б)) Г)]
    [(_ . А) #'(let . А)]))

(define истина #t)
(define ложь #f)

(define (квадратные-скобки объект поле)
  (cond
    [(list? объект) (list-ref объект поле)]
    [(vector? объект) (vector-ref объект поле)]
    [(hash? объект) (hash-ref объект поле #f)]
    [else (raise-syntax-error 'квадратные-скобки
                              "У объекта ~a нет доступа к полям через квадратные скобки"
                              объект)]))

(define-syntax (надо-быстро stx)
  1)

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     (quasisyntax/loc stx
       (#%module-begin
        (require (for-syntax 1/run-fast))
        (global-port-print-handler printer)
        (begin-for-syntax (start '#,(syntax-source stx)))        
        body ...
        (begin-for-syntax (end))))]))
