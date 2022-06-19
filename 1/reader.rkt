#lang racket
(require syntax/readerr syntax/srcloc syntax/parse racket/list syntax/stx)
(provide my-read my-read-syntax)

(define (my-read [p (current-input-port)]) (syntax->datum (my-read-syntax #f p)))

(define (my-read-syntax [source-name (object-name (current-input-port))]
                        [port (current-input-port)])
  (with-handlers ([(λ (e) #t) (λ (e) (raise (translate e)))])
    (parameterize ([current-source-name source-name]
                   [current-input-port port])      
      (разобрать-список-с-одной-точкой (indent-read)))))

(define (translate e)
  (define dict
    '(("unexpected" . "неожиданно встретилась")
      ("expected" . "ожидалась")
      ("to close preceding" . "для закрытия предшествующей")
      ("found instead" . "вместо этого встретилась")
      ("read-syntax" . "чтение-синтаксиса")))
  (define (replace-dict str dict)
    (if (null? dict)
        str
        (replace-dict (string-replace str (caar dict) (cdar dict)) (cdr dict))))
  (cond
    [(exn:fail:syntax:unbound? e)
     (exn:fail:syntax:unbound (exn-message e) (exn-continuation-marks e) (exn:fail:syntax-exprs e))]
    [(exn:fail:read? e) (exn:fail:read
                         (replace-dict (exn-message e) dict)
                         (exn-continuation-marks e) (exn:fail:read-srclocs e))]
    [else e]))

(define (rt-char=? c default-c [c=? char=?])
  (define-values (c2 _1 _2)
    (if (char? c)
        (let ([r (current-readtable)])
          (if r (readtable-mapping r c) (values c #f #f)))
        (values #f #f #f)))
  (and (char? c2) (c=? c2 default-c)))

(define (accumulate-hspace)
  (define c (peek-char-or-special))
  (if (and (char? c)
           (or (char-whitespace? c) (char=? c #\!))
           (not (eqv? c #\newline)))
      (cons (read-char)
            (accumulate-hspace))
      null))

(define (comment? c)
  (and (rt-char=? c #\-) (rt-char=? (peek-char-or-special (current-input-port) 1) #\-)))

(define (block-comment? c)
  (and (rt-char=? c #\#) (rt-char=? (peek-char-or-special (current-input-port) 1) #\|)))

(define (indentation-level)
  (define indent (accumulate-hspace))
  (define c (peek-char-or-special))
  (cond [(eof-object? c) ""]
        [(comment? c)
         (consume-to-eol!)
         (indentation-level)]
        [(eqv? c #\newline)
         (read-char)
         (indentation-level)]
        [else
         (when (rt-char=? c #\;) (read-char))
         (list->string indent)]))

(define (indentation>? indentation1 indentation2)
  (define len1 (string-length indentation1))
  (define len2 (string-length indentation2))
  (and (> len1 len2)
       (string=? indentation2 (substring indentation1 0 len2))))

(define (consume-to-eol!)
  (define c (read-char-or-special))
  (unless (or (eof-object? c)
              (rt-char=? c #\newline))
    (consume-to-eol!)))

(define (consume-block-comment!)
  (define (rec)
    (define c (peek-char-or-special))
    (unless (and (rt-char=? c #\|) (rt-char=? (peek-char-or-special (current-input-port) 1) #\#))
      (when (block-comment? c)
        (consume-block-comment!))
      (read-char-or-special)
      (rec)))
  (read-char) (read-char) (rec) (read-char) (read-char))
     
(define (consume-whitespaces!)
  (define c (peek-char-or-special))
  (cond
    [(and (char? c) (char-whitespace? c))
     (read-char)
     (consume-whitespaces!)]
    [(comment? c)
     (consume-to-eol!)
     (consume-whitespaces!)]
    [(block-comment? c)
     (consume-block-comment!)
     (consume-whitespaces!)]))

(define (indent-read)
  (define indentation (list->string (accumulate-hspace)))
  (define c (peek-char-or-special))
  (cond    
    [(eof-object? c) (read-char) c]
    [(comment? c) (consume-to-eol!) (indent-read)]
    [(block-comment? c)
     (consume-block-comment!)
     (indent-read)]
    [(rt-char=? c #\newline) (read-char) (indent-read)]
    [(> (string-length indentation) 0)
     (define-values (ln col pos) (port-next-location (current-input-port)))
     (raise-read-error "Выражения верхнего уровня должны начнинаться с начала строки"
                       (current-source-name) ln 0 (- pos col) col)]
    [else
     (define-values (ln col pos) (port-next-location (current-input-port)))
     (match-define (cons level stx) (read-block-clean ""))
     (cond
       [(dot? stx)
        (raise-read-error "неожиданная `.`" (current-source-name) ln col pos 1)]
       [(operator? stx) => (λ (parsed)                             
                             (apply оператор! (syntax->datum parsed))
                             (indent-read))]
       [else stx])]))

(define (operator? stx)
  (define (операция-и-приоритет? оп прио оператор! ассоциативность)
    (and (symbol? (syntax-e оп))
         (real? (syntax-e прио))
         (eq? (syntax-e оператор!) 'оператор!)
         (memq (syntax-e ассоциативность) '(право лево нет))))
  (syntax-case stx ()
    [(оп оператор! прио)
     (операция-и-приоритет? #'оп #'прио #'оператор! #'нет)
     #'(оп прио)]
    [(оп оператор! прио ассоциативность)
     (операция-и-приоритет? #'оп #'прио #'оператор! #'ассоциативность)
     #'(оп прио лево)]
    [else #f]))

(define (read-block-clean level)
  (define-values (ln col pos) (port-next-location (current-input-port)))
  (match-define (cons next-level stx) (read-block level))
  (cons next-level
        (if (or (eof-object? stx) (null? stx))
            stx
            (match stx
                 [(list x) x]
                 [_
                  (define-values (_1 _2 end-pos) (port-next-location (current-input-port)))
                  (clean (datum->syntax #f stx
                                        (vector (current-source-name)
                                                ln col pos (- end-pos pos))
                                        (read-syntax #f (open-input-string "orig"))))]))))



(define (split-sc x)
  (define r null)
  (define c null)
  (let loop ([r null] [c null] [l (syntax-e x)])
    (cond
      [(null? l)
       (datum->syntax x
                      (map (λ (elem)
                             (define elem* (match elem
                                             [(list x) x]
                                             [_ elem]))
                             (clean (datum->syntax x elem*)))
                           (filter (λ (x) (not (null? x)))
                                   (reverse (cons c r)))))]
      [(not (pair? l)) (loop r (append c l) null)]
      [(eq? (syntax-e (car l)) '|;|) (loop (cons c r) null (cdr l))]
      [else (loop r (append c (list (car l))) (cdr l))])))

(define sym-= (datum->syntax #f '=))
(define sym-if (datum->syntax #f '?))
(define sym-cond (datum->syntax #f 'если))
(define sym-else (datum->syntax #f 'иначе))
(define sym-begin (datum->syntax #f 'блок))

(define приоритеты (make-hasheq))
(define (оператор! оп приоритет [ассоциативность 'лево])
  (hash-set! приоритеты оп (cons приоритет ассоциативность)))

(оператор! '* 7)
(оператор! '/ 7)
(оператор! '// 7)
(оператор! '% 7)
(оператор! '+ 6)
(оператор! '- 6)
(оператор! '== 4)
(оператор! '/= 4)
(оператор! '< 4)
(оператор! '> 4)
(оператор! '<= 4)
(оператор! '>= 4)
(оператор! '|| 3)
(оператор! '&& 3)
(оператор! '? 2)
(оператор! ':= 1 'право)
(оператор! '= 0)

(define (оператор? stx)
  (define s (syntax-e stx))
  (define (имя-оператора? имя)
    (and
     (or (regexp-match #rx"^[!#$%&⋆+./<=>?@^~:*-]*$" имя)
         (regexp-match #rx"^\\^.*\\^$" имя))
     (not (string=? имя "..."))
     (not (string=? имя "."))))
  (and (symbol? s)
       (имя-оператора? (symbol->string s))))

(define (очистить-оператор stx)
  (define имя (symbol->string (syntax-e stx)))
  (if (regexp-match #rx"^[!#$%&⋆+./<=>?@^~:*-]*$" имя)
      stx
      (datum->syntax stx
                     (string->symbol (substring имя 1 (sub1 (string-length имя)))))))

(define (приоритет-оператора оп)
  (hash-ref приоритеты (syntax-e оп) (λ () (cons 9 'лево))))

(define (обработать-операторы stx)
  (define l (syntax-e stx))
  (cond
    [(pair? l)
     (define операторы
       (let собрать-операторы ([список (cdr l)] [результат null])
         (if (or (null? список)
                 (not (pair? список))
                 (null? (cdr список))
                 (not (pair? (cdr список))))
             результат
             (собрать-операторы (cdr список)
                                (if (оператор? (car список))
                                    (cons (car список) результат)
                                    результат)))))
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
                   (raise-syntax-error 'обработать-операторы (format "\
в выражении для приоритета ~a есть операторы с направлением чтения ~a и ~a" прио направление напр)
                                       stx (car операторы)))
                 (минимум (cdr операторы) прио напр)]
                [else (минимум (cdr операторы) приоритет направление)])])))
        (when (eq? направление 'нет)
          (define отобранные (filter (λ (оп)
                                          (= (car (приоритет-оператора оп)) приоритет))
                                     операторы))
          (when (< 1 (length отобранные))
            (raise-syntax-error 'обработать-операторы (format "\
в выражении для приоритета ~a и отсутствующей ассоциативности несколько операторов: ~a"
                                                              приоритет отобранные)
                                stx (car отобранные))))
        (define (list1? x) (and
                            (not (null? x))
                            (null? (cdr x))))
        (define (разделить-по-оператору список лево)
          (define элем (car список))
          (cond
            [(and (оператор? элем)
                  (= (car (приоритет-оператора элем)) приоритет))
             (define право (cdr список))
             (values (очистить-оператор элем) лево право)]
            [else
             (define элем (car список))
             (разделить-по-оператору (cdr список)
                                     (cons элем лево))]))
        (define-values (оператор лево право)
          (cond
            [(eq? направление 'право)
             (разделить-по-оператору (cdr l) (list (car l)))]
            [else
             (define rev-l (reverse l))
             (define-values (оператор лево право)
               (разделить-по-оператору (cdr rev-l) (list (car rev-l))))
             (values оператор право лево)]))
        (map clean
             (append (list оператор)
                     (if (list1? лево) лево (list (datum->syntax stx (reverse лево))))
                     (if (or (list1? право)
                             (and (eq? '= (syntax-e оператор)) (описание-функции лево))
                             (eq? '? (syntax-e оператор)))
                         право
                         (list (datum->syntax stx право)))))])]
    [else stx]))

(define (описание-функции список)
  (cond
    [(syntax? список) (описание-функции (syntax-e список))]
    [(not (cons? список)) #f]
    [(null? (cdr список))
     (описание-функции (car список))]
    [else
     (not (memq (car список) '(значения шаблон шаблоны)))]))

(define (обработать-если x)
  (datum->syntax
   x
   (syntax-e              
    (syntax-parse x
      [(если a (~datum тогда) b ... (~datum иначе) c ...)
       #`(#,sym-cond (a b ...) (#,sym-else c ...))]
      [(если a ... (~datum тогда) b ... (~datum иначе) c ...)
       #`(#,sym-cond (#,(clean (datum->syntax x (syntax-e #'(a ...)))) b ...)
                     (#,sym-else c ...))]
      [(если a (~datum тогда) b ...)
       #`(#,sym-cond (a b ...))]
      [(если a ... (~datum тогда) b ...)
       #`(#,sym-cond (#,(clean (datum->syntax x (syntax-e #'(a ...)))) b ...))]
      [_ x]))))

(define (clean x)
  (define y (datum->syntax x (обработать-операторы (обработать-если x))))
  (syntax-parse y
    [(b ... (kw:keyword c) d ...) (clean #'(b ... kw c d ...))]
    [(b ... (kw:keyword c ...) d ...) (clean #'(b ... kw (c ...) d ...))]
    [(a ... b (~datum |.|) c (~datum |.|) d e ...) #'(c a ... b d e ...)]
    [((~and q
            (~or (~datum quote)
                 (~datum unquote)
                 (~datum quasiquote)
                 (~datum unquote-splicing)
                 (~datum цитата)
                 (~datum квазицитата)
                 (~datum не-цитируя)
                 (~datum не-цитируя-список)))
      b c d ...)
     #`(q #,(clean #'(b c d ...)))]
    [_ y]))

(define (clean-list x)
  (syntax-parse x
    [(a ... (~datum |;|) . b) (clean (split-sc x))]
    [_ (clean x)]))

(define (разобрать-список-с-одной-точкой x)
  (syntax-parse x
    [(a ... (~datum |.|) c) #'(a ... . c)]
    [(a ... (~and dot (~datum |.|)) . b)
     (apply raise-read-error "неожиданная `.`" (build-source-location-list #'dot))]
    [(a ...) (datum->syntax x (stx-map разобрать-список-с-одной-точкой x))]
    [_ x]))

(define current-source-name (make-parameter #f))

(define (parse-dot first rest ln col pos)
  (match rest
    [(list x) (list first x)]
    [(list) (raise-read-error "неожиданная `.`" (current-source-name) ln col pos 1)]
    [(list-rest a ... b) (cons first rest)]))

(define (expand-booleans x)
  (cond
    [(syntax? x)
     (define sym (syntax-e x))
     (case sym
       [(истина) (datum->syntax x #t)]
       [(ложь) (datum->syntax x #f)]
       [else x])]
    [else x]))

(define (read-block level)
  (define char (peek-char-or-special))
  (cond
    [(eof-object? char)
     (read-char)
     (cons -1 null)]
    [(comment? char)
     (consume-to-eol!)
     (read-block level)]
    [(block-comment? char)
     (consume-block-comment!)
     (read-block level)]
    [(and (rt-char=? char #\\)
          (rt-char=? (peek-char-or-special (current-input-port) 1) #\newline))
     (read-char) (read-char) (read-block level)]
    [(rt-char=? char #\newline)
     (read-char)     
     (define next-level (indentation-level))
     (if (indentation>? next-level level)
         (read-blocks next-level)
         (cons next-level null))]
    [(rt-char=? char #\;)
     (read-char)
     (consume-whitespaces!)
     (cons level null)]
    [(and (char? char) (char-whitespace? char))
     (read-char)
     (read-block level)]
    [else
     (define-values (ln col pos) (port-next-location (current-input-port)))
     (define first (read-item))
     (match-define (cons new-level rest) (read-block level))
     (define-values (_1 _2 end-pos) (port-next-location (current-input-port)))
     (cons new-level
           (cond
             [(eof-object? first) first]
             [(eof-object? rest) first]
             [(dot? first) (parse-dot first rest ln col pos)]
             [($? first)
              (define rest1 (syntax-e (clean-list (datum->syntax #f rest))))
              (match rest1                
                [(list a b c ...) (list rest1)]
                [_ rest1])]
             [else
              (cons first rest)]))]))

(define (readquote qt)
  (define char (peek-char-or-special))
  (define-values (ln col pos) (port-next-location (current-input-port)))  
  (define stx (if (char-whitespace? char) qt (list qt (read-item))))
  (define-values (_1 _2 end-pos) (port-next-location (current-input-port)))
  (datum->syntax #f stx
                 (vector (current-source-name)
                         ln col pos (- end-pos pos))
                 (read-syntax #f (open-input-string "orig"))))

(define (read-list end)
  (consume-whitespaces!)
  (define char (peek-char-or-special))
  (define-values (ln col pos) (port-next-location (current-input-port)))
  (cond
    [(eof-object? char)
     (raise-read-eof-error "файл закончился внутри списка" (current-source-name) ln col pos 1)]
    [(rt-char=? char end)
     (read-char)
     null]
    [(and
      (rt-char=? char #\.)
      (let ([next (peek-char-or-special (current-input-port) 1)])
        (or (rt-char=? next #\newline)
            (rt-char=? next #\space)
            (rt-char=? next #\tab))))
     (parse-dot (read-item) (read-list end) ln col pos)]
    [(rt-char=? char #\$)
     (define first (read-item))
     (define rest (syntax-e (clean-list (datum->syntax #f (read-list end)))))
     (match rest
       [(list a b c ...) (list rest)]
       [_ rest])]
    [else
     (cons (read-item) (read-list end))]))

(define (read-item)
  (define char (peek-char-or-special))
  (define-values (ln col pos) (port-next-location (current-input-port)))
  (cond [(eof-object? char) char]
        [(and
          (rt-char=? char #\.)
          (let ([next (peek-char-or-special (current-input-port) 1)])
            (or (rt-char=? next #\newline)
                (rt-char=? next #\space)
                (rt-char=? next #\tab))))
         (read-char)
         (datum->syntax #f '|.| (vector (current-source-name) ln col pos 1))]
        [(rt-char=? char #\;)
         (read-char)
         (datum->syntax #f '|;| (vector (current-source-name) ln col pos 1))]
        [(rt-char=? char #\`)
         (read-char)
         (readquote 'квазицитата)]
        [(rt-char=? char #\')
         (read-char)
         (readquote 'цитата)]
        [(rt-char=? char #\,)
         (read-char)
         (cond
           [(eqv? (peek-char-or-special) #\@)
            (read-char)
            (readquote 'не-цитируя-список)]
           [else (readquote 'не-цитируя)])]
        [else
         (define res
           (cond
             [(rt-char=? char #\()
              (read-char)
              (clean-list (datum->syntax #f (read-list #\))))]
             [(rt-char=? char #\[)
              (read-char)
              (clean-list (datum->syntax #f (read-list #\])))]
             [(rt-char=? char #\{)
              (read-char)
              (clean-list (datum->syntax #f (read-list #\})))]
             [else (read-syntax (current-source-name))]))
         (let loop ([res res])
           (cond
             [(rt-char=? (peek-char-or-special) #\()
              (read-char)
              (loop (datum->syntax #f (cons res (clean-list (datum->syntax #f (read-list #\)))))))]
             [(rt-char=? (peek-char-or-special) #\[)
              (read-char)
              (loop (datum->syntax #f (cons 'квадратные-скобки (cons res (read-list #\])))))]
             [(rt-char=? (peek-char-or-special) #\{)
              (read-char)
              (define l (clean-list (datum->syntax #f (read-list #\}))))
              (loop (datum->syntax #f
                                   (cons (if (cons? (car (syntax->datum l))) 'отправить+ 'отправить)
                                         (cons res l))))]
             [else (expand-booleans res)]))]))

(define (parse-block-dot stx [next-blocks null])
  (cond
    [(null? stx) null]
    [(dot? stx) next-blocks]
    [else (cons stx next-blocks)]))

(define (read-blocks level)
  ;; indent -> (listof syntax?)
  (match-define (cons next-level stx) (read-block-clean level))
  (cond [(equal? next-level level)
         (match-define (cons next-next-level next-blocks) (read-blocks level))
         (cons next-next-level (parse-block-dot stx next-blocks))]
        [else
         (cons next-level (parse-block-dot stx null))]))

(define (dot? x) (and (syntax? x) (eq? (syntax-e x) '|.|)))
(define ($? x) (and (syntax? x) (eq? (syntax-e x) '$)))

(module+ test
  (require rackunit)
  (define (test a b)
    (check-equal? (with-input-from-string a my-read) b))
  (test "(1 2 . 3)" '(1 2 . 3))
  (test "1 2 . 3" '(1 2 . 3))
  (check-equal? (with-input-from-string "1 2; 3" (λ () (list (my-read) (my-read)))) '((1 2) 3))
  (check-equal? (with-input-from-string "1 2; 3 4" (λ () (list (my-read) (my-read)))) '((1 2) (3 4)))
  (test "1 2\n 3 4;  \n \n    5 6\n  7 8"
                '(1 2 (3 4) (5 6 (7 8))))
  (test "(2 #|23 32|# . 3)"  '(2 . 3))
  (test "2 3 -- sadasd  sad as\n 4 5"  '(2 3 4 5))
  (test "f(a) f(g(d))" '((f a) (f (g d))))
  (test "f(a; b c; d)" '(f a (b c) d))
  (test "цикл/первый\n ;\n  p points\n  #:когда $ tau < p[0]\n bonus := bonus + p[1]"
        '(цикл/первый ((p points) #:когда (< tau (квадратные-скобки p 0)))
                      (:= bonus (+ bonus (квадратные-скобки p 1)))))
  (test "цикл/первый (p points; #:когда $ tau < p[0])\n bonus := bonus + p[2]"
        '(цикл/первый ((p points) #:когда (< tau (квадратные-скобки p 0)))
                      (:= bonus (+ bonus (квадратные-скобки p 2)))))
  (test "new(point%){move-x 5; move-y 7; move-x 12}"
        '(отправить+ (new point%) (move-x 5) (move-y 7) (move-x 12)))
  (test "new(point%){move-x 5}"
        '(отправить (new point%) move-x 5))
  (test "если 2 > 3 тогда 3 иначе 2"
        '(если ((> 2 3) 3) (иначе 2)))
  (test "если 2 > 3 тогда\n  a := 3\n  иначе\n  a := 2"
        '(если ((> 2 3) (:= a 3)) (иначе (:= a 2))))
  (test "..." '...)
  (test "f(x) =\n  1 + 2\n  2 - 3" '(= (f x) (+ 1 2) (- 2 3)))
  (test "тест = проверка 5" '(= тест (проверка 5)))
  (test "2 3 #| sadasd  sad as\n 4 |# 5"  '(2 3 5)))
