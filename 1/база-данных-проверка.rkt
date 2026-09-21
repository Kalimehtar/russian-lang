#lang racket/base
(module+ test
  (require rackunit
           (file "база-данных.1")
           (prefix-in д: (file "дата.1"))
           (file "база-данных-mysql.1"))

  (check-true (нуль? нуль))
  (check-false (нуль? 1))
  (check-equal? (нуль->ложь нуль) #f)
  (check-equal? (нуль->ложь "а") "а")
  (check-eq? (ложь->нуль #f) нуль)
  (check-equal? (ложь->нуль 3) 3)

  (define d (дата 1980 12 25))
  (check-true (дата? d))
  (check-equal? (год d) 1980)
  (check-equal? (месяц d) 12)
  (check-equal? (день d) 25)

  (define t (время 7 30 0 0 #f))
  (check-true (время? t))
  (check-equal? (час t) 7)
  (check-equal? (минута t) 30)
  (check-equal? (секунда t) 0)
  (check-equal? (наносекунда t) 0)
  (check-equal? (зона t) #f)

  (define ts (метка-времени 1970 1 1 0 0 0 0 #f))
  (check-true (метка-времени? ts))
  (check-equal? (год-метки-времени ts) 1970)
  (check-equal? (зона-метки-времени ts) #f)

  (define iv (интервал 0 0 0 7 30 0 0))
  (check-true (интервал? iv))
  (check-true (интервал-день-время? iv))
  (check-equal? (часы iv) 7)
  (define t2 (интервал->время iv))
  (check-true (время? t2))
  (check-equal? (час t2) 7)
  (check-equal? (часы (время->интервал t2)) 7)

  (define adina-d (дата-из-базы-данных d))
  (check-equal? (д:год adina-d) 1980)
  (check-equal? (д:месяц adina-d) 12)
  (check-equal? (д:день adina-d) 25)
  (define back (дата-в-базу-данных adina-d))
  (check-true (дата? back))
  (check-equal? (год back) 1980)
  (check-equal? (месяц back) 12)
  (check-equal? (день back) 25)

  (define adina-ts (метка-времени-из-базы-данных ts))
  (check-equal? (д:год adina-ts) 1970)
  (define ts-back (метка-времени-в-базу-данных adina-ts))
  (check-true (метка-времени? ts-back))
  (check-equal? (год-метки-времени ts-back) 1970)

  (check-false (запрос? 1))
  (check-true (запрос? "select 1"))

  (check-exn
   (λ (e) (and (exn:fail? e) (not (exn:fail:contract? e))))
   (λ ()
     (подключить #:пользователь "x"
                 #:сервер "127.0.0.1"
                 #:порт 1
                 #:шифрование 'да)))
  (check-true (procedure? угадать-путь-сокета)))
