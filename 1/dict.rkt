#lang racket
(require (for-syntax syntax/parse racket/match) (prefix-in rkt: racket))
(provide (rename-out [@#%app #%app]))
(provide (except-out (all-from-out racket) #%app) (all-defined-out))

(define-syntax (= stx)
  (syntax-case stx (значения шаблон шаблоны)
    [(= (значения . а) б) #'(define-values а б)]
    [(= (шаблон а) б) #'(match-define а б)]
    [(= (шаблоны . а) б) #'(match-define-values а б)]
    [(= а . б) #'(define а . б)]))

(define-syntax (:= stx)
  (syntax-case stx (значения)
    [(:= (значения . а) б) #'(set!-values а б)]
    [(:= а б) #'(set! а б)]))

(define-syntax (синоним stx)
  (syntax-case stx ()
    [(_ старый новый)
     #'(define-syntax (новый stx)
         (syntax-case stx ()
           [(_ . a) #'(старый . a)]))]))

(define-syntax (пусть stx)
  (syntax-case stx ()
    [(_ . a) #'(let . a)]))

(синоним if если)
(= == rkt:=)
(= (/= x y) (not (== x y)))

(define-for-syntax приоритеты (make-hasheq))
(define-for-syntax (оператор! оп приоритет [ассоциативность 'лево])
  (hash-set! приоритеты оп (cons приоритет ассоциативность)))

(begin-for-syntax
  (оператор! #'* 7)
  (оператор! #'/ 7)
  (оператор! #'+ 6)
  (оператор! #'- 6)
  (оператор! #'== 4)
  (оператор! #'/= 4)
  (оператор! #'< 4)
  (оператор! #'> 4)
  (оператор! #':= 0))

(define-for-syntax (оператор? stx)
  (define s (syntax-e stx))
  (define (имя-оператора? имя)
    (or (regexp-match #rx"^[!#$%&⋆+./<=>?@^~:*-]+$" имя)
        (regexp-match #rx"^\\^.*\\^$" имя)))
  (and (symbol? s)
       (имя-оператора? (symbol->string s))))

(define-for-syntax (приоритет-оператора оп)
  (hash-ref приоритеты оп (λ () (cons 9 'лево))))

(define-for-syntax (обработать-операторы stx)
  (define l (syntax-e stx))
  (cond
    [(pair? l)
     (define операторы
       (let собрать-операторы ([список (cdr l)] [результат null])
         (cond
           [(or (null? список)
                (not (pair? список))
                (null? (cdr список))
                (not (pair? (cdr список)))) результат]
           [else (собрать-операторы (cdr список) (if (оператор? (car список)) (cons (car список) результат) результат))])))
     (cond
       [(null? операторы) stx]
       [else
        (define-values (приоритет направление)
          (let минимум ([операторы операторы] [приоритет 10] [направление #f])
            (cond
             [(null? операторы) (values приоритет направление)]
             [else
              (match-define (cons прио напр) (приоритет-оператора (car операторы)))
              (cond
                [(< прио приоритет) (минимум (cdr операторы) прио напр)]
                [(= прио приоритет)
                 (unless (eq? направление напр)
                   (raise-syntax-error 'обработать-операторы (format "в выражении для приоритета ~a есть операторы с направлением чтения ~a и ~a" прио направление напр)
                                       stx (car операторы)))
                 (минимум (cdr операторы) прио напр)]
                [else (минимум (cdr операторы) приоритет направление)])])))
        (when (eq? направление 'нет)
          (define отобранные (filter (λ (оп)
                                          (= (car (приоритет-оператора оп)) приоритет))
                                     операторы))
          (when (< 1 (length отобранные))
            (raise-syntax-error 'обработать-операторы (format "в выражении для приоритета ~a и отмктмаующей ассоциатитвности несколько операторов: ~a" приоритет отобранные)
                                stx (car отобранные))))
        (define (list1? x) (and
                            (not (null? x))
                            (null? (cdr x))))
        (if (eq? направление 'право)
            (let разделить-по-оператору ([список (cdr l)] [лево (list (car l))] [оператор #f] [право null])
              (cond
                [(or (null? список)
                     (not (pair? список))
                     (null? (cdr список))
                     (not (pair? (cdr список))))
                 (define право* (append (reverse право) список))
                 (append (list оператор)
                         (if (list1? лево) лево (list (datum->syntax stx (reverse лево))))
                         (if (list1? право*) право* (list (datum->syntax stx право*))))]
                [else
                 (define элем (car список))
                 (define найден? (and (оператор? элем)
                                      (= (car (приоритет-оператора элем)) приоритет)))
                 (разделить-по-оператору (cdr список)
                                         (cond
                                           [найден? (append право лево)]
                                           [оператор лево]
                                           [else (cons элем лево)])
                                         (if найден? элем оператор)
                                         (cond
                                           [найден? null]
                                           [оператор (cons элем право)]
                                           [else null]))]))
            (let разделить-по-оператору ([список (cdr l)] [лево (list (car l))])
              (define элем (car список))              
              (cond
                [(and (оператор? элем)
                      (= (car (приоритет-оператора элем)) приоритет))
                 (define право (cdr список))
                 (append (list элем)
                         (if (list1? лево) лево (list (datum->syntax stx (reverse лево))))
                         (if (list1? право) право (list (datum->syntax stx право))))]
                [else
                 (define элем (car список))
                 (разделить-по-оператору (cdr список)
                                         (cons элем лево))])))])]
    [else stx]))

(define-syntax (@#%app stx)
  (syntax-parse stx
    ;[(_ a (~and + (~literal +)) . b) #'(+ a . b)]
    [(_ . a) #`(#%app . #,(обработать-операторы #'a))]))