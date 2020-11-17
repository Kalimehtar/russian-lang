#lang racket
(require (for-syntax syntax/parse racket/match 1/run-fast) (prefix-in rkt: racket))
(provide (rename-out [module-begin #%module-begin])
         (except-out (all-defined-out) module-begin синоним синоним-данных)
         (except-out (all-from-out racket) #%module-begin))

(define-syntax (= stx)
  (syntax-case stx (значения шаблон шаблоны)
    [(_ (значения . а) б) #'(define-values а б)]
    [(_ (шаблон а) б) #'(match-define а б)]
    [(_ (шаблоны . а) б) #'(match-define-values а б)]
    [(_ а . б) #'(define а . б)]))

(define (значения . a) (apply values a))

(define-syntax (:= stx)
  (syntax-case stx (значения)
    [(_ (значения . а) б) #'(let () (set!-values а б) (values . а))]
    [(_ коллекция[элемент] значение)
     #'(cond
         [(list? объект) (list-ref объект поле)]
         [(vector? объект) (vector-ref объект поле)]
         [(hash? объект) (hash-ref объект поле #f)]
         [else (raise-syntax-error 'квадратные-скобки
                                   "У объёкта ~a нет доступа к полям через квадратные скобки"
                                   объект)])]
    [(_ а б) #'(let () (set! а б) а)]))

(define-syntax (синоним stx)
  (syntax-case stx ()
    [(_ старый новый)     
     #'(define-syntax (новый stx)
         (syntax-case stx ()
           [(_ . a) #'(старый . a)]))]))

(define-syntax (синоним-данных stx)
  (syntax-case stx ()
    [(_ старый новый)     
     #'(begin
         (define-match-expander новый
           (λ (stx)
             (syntax-case stx ()
               [(_ . a) #'(старый . a)]))
           (λ (stx)
             (syntax-case stx ()
               [(_ . a) #'(старый . a)]))))]))

(синоним if если)
(синоним let пусть)
(синоним begin команды)
(синоним begin-for-syntax при-компиляции)
(синоним define-syntax определение-синтаксиса)
(синоним define-syntax-rule определение-синтаксисического-правила)
(синоним or ||)
(синоним and &&)
(= == rkt:=)
(= (/= x y) (not (== x y)))
(= (// x y) (quotient x y))
(= (% x y) (remainder x y))
(синоним-данных cons пара)
(синоним-данных list список)
(синоним-данных vector массив)

(module syn racket
  (provide варианты-синтаксиса)
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
      [(_ a ...)
       #'(with-handlers ([(λ (e) #t) (λ (e) (raise (translate e)))])
           (syntax-case a ...))])))
(require (for-syntax 'syn))

(определение-синтаксиса (условия stx)
  (варианты-синтаксиса stx (=> иначе)
    [(_ (условие => функция) остаток ...)
     #'(let ([t условие])
         (if t (функция t) (условия остаток ...)))]
    [(_ (условие действия ...) остаток ...)
     #'(if условие
           (begin действия ...)
           (условия остаток ...))]
    [(_) #'(void)]))

(define истина #t)
(define ложь #f)

(define (bracket объект поле)
  (cond
    [(list? объект) (list-ref объект поле)]
    [(vector? объект) (vector-ref объект поле)]
    [(hash? объект) (hash-ref объект поле #f)]
    [else (raise-syntax-error 'квадратные-скобки
                              "У объёкта ~a нет доступа к полям через квадратные скобки"
                              объект)]))

(define-syntax (надо-быстро stx)
  1)

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     (quasisyntax/loc stx
       (#%module-begin
        (require (for-syntax 1/run-fast))
        (begin-for-syntax (start '#,(syntax-source stx)))
        body ...
        (begin-for-syntax (end))))]))
