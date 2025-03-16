#lang racket/base
(require racket/string)
(provide русифицировать-вывод russian-port)

(define (русифицировать-вывод строка)
   (define (заменить-по-словарю строка словарь)
      (if (null? словарь)
          строка
          (заменить-по-словарю (string-replace строка
                                               (caar словарь) (cdar словарь)) (cdr словарь))))
  (заменить-по-словарю
   строка
   '(("#t" . "истина")
     ("#f" . "ложь")
     ("procedure" . "функция")
     ("application: not a procedure;
 expected a procedure that can be applied to arguments" .
                                                        "вызов функции:
 ожидалась функция, которую можно применить к аргументам")
     ("mutable-string" . "изменяемая-строка")
     ("given:" . "получено:")
     ("expected:" . "ожидалось:")
     ("real?" . "вещественное?")
     ("#\\backspace" . "#\\забой")
     ("#\\space" . "#\\пробел")
     ("#\\newline" . "#\\перенос")
     ("#\\return" . "#\\возврат")
     ("#\\nul" . "#\\пусто")
     ("#\\vtab" . "#\\втаб")
     ("#\\tab" . "#\\таб")
     ("#\\page" . "#\\страница")
     ("#\\rubout" . "#\\удаление")
     ("string-set!" . "установить-элемент-строки!")
     ("vector-ref:" . "элемент-массива:")
     ("index is out of range" . "позиция за границами")
     ("index:" . "позиция:")
     ("valid range:" . "границы массива:")
     ("index:" . "позиция:")
     ("vector:" . "массив:")
     ("context" . "окружение")
     ("body of" . "тело")
     ("contract violation" . "нарушение контракта")
     ("cannot reference an identifier before its definition"
      . "не могу использовать идентификатор до его определения")
     ("define-values: assignment disallowed" . "=: переопределение запрещено")
     ("define: bad syntax" . "=: неправильный синтаксис")
     ("undefined" . "не определён")
     ("cannot re-define a constant" . "нельзя переопределять константу")
     ("cannot modify a constant" . "нельзя изменять константу")
     ("constant:" . "константа:")
     ("byte string:" . "строка байтов:")
     ("bytes->string/locale: byte string is not a valid encoding for the current locale"
      . "байты->строка/местные: строка байтов не является правильной в местной кодировке")
     ("in module:" . "в модуле:")
     ("'anonymous-module" . "'безымянный-модуль")
     ("#%datum: keyword misused as an expression"
      . "#%данные: ключевое слово ошибочно использовано в выражении")
     ("module: identifier already defined"
      . "модуль: идентификатор уже определён")
     ("in:" . "в:")
     ("(list " . "(список ")
     ("(list* " . "(список* ")
     ("(cons " . "(пара ")
     ("application" . "применение")
     ("required keyword argument not supplied" . "требуемый ключевой аргумент не предоставлен")
     ("required keyword" . "требуемый ключевой аргумент")
     ("does not expect an argument with given keyword" . "не ожидает данного ключевого аргумента")
     ("given keyword" . "указанный ключевой аргумент")
     ("arguments" . "аргументы")
     ("unbound identifier" . "неизвестный идентификатор"))))

(define old-printer (global-port-print-handler))
(define (byte-rus s start end)
  (string->bytes/utf-8
   (русифицировать-вывод
    (bytes->string/utf-8 (subbytes s start end)))))

(define (russian-port порт [преобразователь byte-rus])
  (if (eq? (object-name порт) 'russian-port)
      порт
      (make-output-port
       'russian-port
       ; This port is ready when the original is ready:
       порт
       ; Writing procedure:
       (lambda (s* start end non-block? breakable?)
         (if (= start end) (flush-output порт) (void))
         (let ([s (преобразователь s* start end)])
           (if non-block?
               (write-bytes-avail* s порт)
               (begin
                 (display s порт)
                 (bytes-length s*)))))
       ; Close procedure — close original port:
       (lambda () (close-output-port порт))
       ; write-out-special
       порт
       ; Write event:
       (and (port-writes-atomic? порт)
            (lambda (s start end)
              (write-bytes-avail-evt
               (преобразователь s start end)
               порт)))
       (and (port-writes-atomic? порт)
            (lambda (s)
              (write s порт))))))