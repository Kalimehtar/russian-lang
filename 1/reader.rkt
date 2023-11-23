#lang racket
(require syntax/readerr syntax/srcloc syntax/parse racket/list syntax/stx syntax/strip-context)
(provide my-read my-read-syntax)

(define (my-read [p (current-input-port)]) (syntax->datum (my-read-syntax #f p)))

(define (my-read-syntax [source-name (object-name (current-input-port))]
                        [port (current-input-port)])
  (with-handlers ([(λ (e) #t) перевести-ошибку])
    (parameterize ([current-source-name source-name]
                   [current-input-port port])      
      (strip-context (разобрать-список-с-одной-точкой (чтение-кода-с-отступами))))))

;; перевести-ошибку ошибка - Переводит строку описания ошибки exn:fail:read на русский язык.
;;                    Вызывает исключение с переданным аргументов.
;; : любое? -> вызывает исключение
;; translate-error error - Translates message of exn:fail:read to russian.
;;                    Raises (calls as an exception) given argument.
;; : any/c -> exception call
(define (перевести-ошибку ошибка)
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
  (raise
   (cond
     [(exn:fail:read? ошибка) (exn:fail:read
                               (replace-dict (exn-message ошибка) dict)
                               (exn-continuation-marks ошибка) (exn:fail:read-srclocs ошибка))]
     [else ошибка])))

;; литеры-равны? литера литера2 -
;;       сравнивает литеры с учётом преобразования первого аргумента по таблице чтения
;; : литера? литера? -> логический?
;; char-equal? char char2 - compares chars, translating first arg with current readtable
;; : char? char? -> boolean?
(define (литеры-равны? литера литера2)
  (define-values (литера* _1 _2)
    (if (char? литера)
        (let ([r (current-readtable)])
          (if r (readtable-mapping r литера) (values литера #f #f)))
        (values #f #f #f)))
  (and (char? литера*) (char=? литера* литера2)))

;; прочитать-пробелы! - читает пробельные символы и собирает их в список
;; : -> список литер
;; read-indent! - reads whitespaces and collect them into a list
;; : -> list-of char?
(define (прочитать-пробелы!)
  (define c (peek-char-or-special))
  (if (and (char? c)
           (or (char-whitespace? c) (char=? c #\!))
           (not (eqv? c #\newline)))
      (cons (read-char)
            (прочитать-пробелы!))
      null))

;; комментарий? c c2 - определяет начало строчного комментария "--"
;; : литера? литера? -> логический?
;; comment? c c2 - checks begin of line comment "--"
;; : char? char? -> boolean?
(define (комментарий? c c2)
  (and (литеры-равны? c #\-) (литеры-равны? c2 #\-)))

;; блочный-комментарий? - определяет начало блочного комментария "#-"
;; : литера?литера? -> логический?
;; block-comment? - checks begin of block comment "#|"
;; : char? char? -> boolean?
(define (блочный-комментарий? c c2)
  (and (литеры-равны? c #\#) (литеры-равны? c2 #\|)))

;; посмотреть-две-литеры - возвращает две следующих непрочитанных литеры
;; : -> значения литера? литера?
;; peek-two-chars - peeks next two chars
;; : -> values char? char?
(define (посмотреть-две-литеры)
  (define литера1 (peek-char-or-special))
  (if (eof-object? литера1)
      (values литера1 литера1)
      (values литера1
              (peek-char-or-special (current-input-port) 1))))

(define (прочитать-отступ! [без-переносов #t])
  (define indent (прочитать-пробелы!))
  (define c (peek-char-or-special))
  (cond
    [(eof-object? c) ""]
    [(eqv? c #\newline)
     (read-char)
     (if без-переносов
         (прочитать-отступ!)
         "")]
    [else
     (define c2 (peek-char-or-special (current-input-port) 1))
     (cond
       [(комментарий? c c2)
        (пропустить-до-конца-строки!)
        (прочитать-отступ! без-переносов)]
       [(блочный-комментарий? c c2)
        (пропустить-блочный-комментарий!)
        (прочитать-отступ! без-переносов)]
       [else
        (when (литеры-равны? c #\;) (read-char))
        (list->string indent)])]))

;; отступ-увеличен? новый старый - отступ `новый` больше отступа `старый` и их начала совпадают
;; : строка? строка? -> логический?
;; indent>? new old - first arg is more than second arg and begins with it
;; : string? string? -> boolean?
(define (отступ-увеличен? новый старый)
  (define len1 (string-length новый))
  (define len2 (string-length старый))
  (and (> len1 len2)
       (string=? старый (substring новый 0 len2))))

;; пропустить-до-конца-строки! - читает и игнорирует до конца строки или файла
;; skip-to-end-of-line - reads and skips until the end of line or file
(define (пропустить-до-конца-строки!)
  (define c (read-char-or-special))
  (unless (or (eof-object? c)
              (литеры-равны? c #\newline))
    (пропустить-до-конца-строки!)))

;; пропустить-блочный-комментарий! - читает и игнорирует блок #|...|# с учётом вложенных комментариев
;; skip-block-comment! - reads and skips block #|...|# considering nested
(define (пропустить-блочный-комментарий!)
  (define (rec)
    (define-values (c c2) (посмотреть-две-литеры))
    (unless (and (литеры-равны? c #\|)
                 (литеры-равны? c2 #\#))
      (when (блочный-комментарий? c c2)
        (пропустить-блочный-комментарий!))
      (read-char-or-special)
      (rec)))
  (read-char) (read-char) (rec) (read-char) (read-char))

;; пропустить-незначащее! без-переносов -
;;                   читает и пропускает незначащие литеры (пробелы и комментарии).
;; Если `без-переносов` ложь или отсутствует, тогда переносы тоже незначащие.
;; : логический? -> пусто?
;; skip-meaningless! without-newlines - reads and skips whitespace and comments.
;; When without-newlines is #f, newlines are whitespaces.
;; : boolean? -> void?
(define (пропустить-незначащее! [без-переносов #f])
  (define-values (c c2) (посмотреть-две-литеры))
  (cond
    [(and без-переносов (литеры-равны? c #\newline))
     (void)]
    [(and (char? c) (char-whitespace? c))
     (read-char)
     (пропустить-незначащее! без-переносов)]
    [(комментарий? c c2)
     (пропустить-до-конца-строки!)
     (пропустить-незначащее! без-переносов)]
    [(блочный-комментарий? c c2)
     (пропустить-блочный-комментарий!)
     (пропустить-незначащее! без-переносов)]))

;; чтение-кода-с-отступами - основаной читатель из текущего порта ввода. Читает блок с отступами,
;;   применяет правила операторов и спецопераций, возвращает синтаксис-список.
;; indent-read - main read function. Reads a block with indents, applies rules for operators and
;;   special chars, return syntax list.
(define (чтение-кода-с-отступами)
  (define indentation (прочитать-отступ!))
  (define-values (ln col pos) (port-next-location (current-input-port)))
  (define c (peek-char-or-special))
  (cond    
    [(eof-object? c) (read-char) c]
    [(> (string-length indentation) 0)
     (raise-read-error "Выражения верхнего уровня должны начнинаться с начала строки"
                       (current-source-name) ln 0 (- pos col) col)]
    [else
     (match-define (cons level stx) (прочитать-блок-с-правилами ""))
     (cond
       [(and (syntax? stx) (eq? (syntax-e stx) '|.|))
        (raise-read-error "неожиданная `.`" (current-source-name) ln col pos 1)]
       [(директива-оператор? stx) => (λ (parsed)                             
                                       (apply оператор! (syntax->datum parsed))
                                       (чтение-кода-с-отступами))]
       [else stx])]))

;; директива-оператор? синтаксис - является ли переданный синтаксис директивой `оператор!`
;; : синтаксис? -> логический?
;; operator-statement? syntax - is given syntax `оператор!` (operator!) statement
;; : syntax? -> boolean?
(define (директива-оператор? синтаксис)
  (define (операция-и-приоритет? оп прио оператор! ассоциативность)
    (and (symbol? (syntax-e оп))
         (real? (syntax-e прио))
         (eq? (syntax-e оператор!) 'оператор!)
         (memq (syntax-e ассоциативность) '(право лево нет))))
  (syntax-case синтаксис ()
    [(оп оператор! прио)
     (операция-и-приоритет? #'оп #'прио #'оператор! #'нет)
     #'(оп прио)]
    [(оп оператор! (прио ассоциативность))
     (операция-и-приоритет? #'оп #'прио #'оператор! #'ассоциативность)
     #'(оп прио ассоциативность)]
    [else #f]))

;; прочитать-блок-с-правилами уровень - читает блок на указанном уровне и применяет правила
;; : строка? -> синтаксис?
;; read-block-with-rules level - reads block at the given level
;; : string? -> syntax?
(define (прочитать-блок-с-правилами уровень)
  (define-values (ln col pos) (port-next-location (current-input-port)))
  (match-define (cons next-level список-синтаксисов) (прочитать-блок уровень))
  (cons next-level
        (match список-синтаксисов
          [(list x) x]
          [_
           (define-values (_1 _2 end-pos) (port-next-location (current-input-port)))
           (применить-правила (datum->syntax #f список-синтаксисов
                                 (vector (current-source-name)
                                         ln col pos (- end-pos pos))))])))

;; разделить-по-точке-с-запятой синтаксис - делит (a ... ; b ... ; ...) -> ((a ...) (b ...) ...)
;; : синтаксис? -> синтаксис?
;; split-by-semicolon - split (a ... ; b ... ; ...) -> ((a ...) (b ...) ...)
;; : syntax? -> syntax?
(define (разделить-по-точке-с-запятой синтаксис)
  (let loop ([r null] [c null] [l (syntax-e синтаксис)])
    (define (cons-cr) (cons (reverse c) r))
    (cond
      [(null? l)
       (datum->syntax синтаксис
                      (map (λ (elem)
                             (применить-правила
                              (обработать-одноэлементный (datum->syntax синтаксис elem))))
                           (filter (λ (x) (not (null? x)))
                                   (reverse (cons-cr)))))]
      [(eq? (syntax-e (car l)) '|;|) (loop (cons-cr) null (cdr l))]
      [else (loop r (cons (car l) c) (cdr l))])))

(define (обработать-одноэлементный elem)
  (syntax-parse elem
    [(x) #'x]
    [_ elem]))

(define приоритеты (make-hasheq))
(define (оператор! оп приоритет [ассоциативность 'лево])
  (hash-set! приоритеты оп (cons приоритет ассоциативность)))

(оператор! '* 8)
(оператор! '/ 8)
(оператор! '// 8)
(оператор! '% 8)
(оператор! '+ 7)
(оператор! '- 7)
(оператор! '++ 6)
(оператор! '== 5)
(оператор! '/= 5)
(оператор! '< 5)
(оператор! '> 5)
(оператор! '<= 5)
(оператор! '>= 5)
(оператор! '&& 4)
(оператор! '|| 3)
(оператор! '? 2)
(оператор! ': 2 'право)
(оператор! ':= 1 'право)
(оператор! '= 0)

(define шаблон-оператора #rx"^[!#$%&⋆+./<=>?@^~:*-]*$")

(define (оператор? stx)
  (define s (syntax-e stx))
  (define (имя-оператора? имя)
    (and
     (or (regexp-match шаблон-оператора имя)
         (regexp-match #rx"^\\^.*\\^$" имя))
     (not (string=? имя "..."))
     (not (string=? имя "."))))
  (and (symbol? s)
       (имя-оператора? (symbol->string s))))

(define (очистить-оператор stx)
  (define имя (symbol->string (syntax-e stx)))
  (if (regexp-match шаблон-оператора имя)
      stx
      (datum->syntax stx
                     (string->symbol (substring имя 1 (sub1 (string-length имя))))
                     stx
                     stx)))

(define максимальный-приоритет 42)

(define (приоритет-оператора оп)
  (hash-ref приоритеты (syntax-e оп) (λ () (cons максимальный-приоритет 'лево))))

(define (обработать-операторы stx)
  (define l (syntax-e stx))
  (cond
    [(pair? l)
     (define операторы
       (let собрать-операторы ([список (cdr l)] [результат null])
         (match список
           [(cons голова остаток)
            #:when (pair? остаток)
            (собрать-операторы остаток
                               (if (оператор? голова)
                                   (cons голова результат)
                                   результат))]
           [else результат])))
     (cond
       [(null? операторы) stx]
       [else
        (define-values (приоритет направление)
          (let минимум ([операторы операторы]
                        [приоритет (add1 максимальный-приоритет)]
                        [направление #f])
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
        (define-values (оператор лево право)
          (cond
            [(eq? направление 'право)
             (разделить-по-оператору (cdr l) (list (car l)) приоритет)]
            [else
             (define rev-l (reverse l))
             (define-values (оператор лево право)
               (разделить-по-оператору (cdr rev-l) (list (car rev-l)) приоритет))
             (values оператор право лево)]))
        (datum->syntax
         stx
         (map применить-правила
              (append (list оператор)
                      (if (список1? лево) лево (list (datum->syntax stx (reverse лево) stx stx)))
                      (if (or (список1? право)
                              (and (eq? '= (syntax-e оператор)) (описание-функции лево))
                              (eq? '? (syntax-e оператор)))
                          право
                          (list (datum->syntax stx право stx stx)))))
         stx
         stx)])]
    [else stx]))

(define (разделить-по-оператору список лево приоритет)
  (match-define (cons элем право) список)
  (cond
    [(and (оператор? элем)
          (= (car (приоритет-оператора элем)) приоритет))
     (values (очистить-оператор элем) лево право)]
    [else
     (разделить-по-оператору право (cons элем лево) приоритет)]))

(define (список1? x)
  (and
   (not (null? x))
   (null? (cdr x))))

(define (описание-функции список)
  (cond
    [(syntax? список)     (описание-функции (syntax-e список))]
    [(not (cons? список)) #f]
    [(null? (cdr список)) (описание-функции (car список))]
    [else                 (not (memq (car список) '(значения шаблон шаблоны)))]))

(define (обработать-если x)
  (syntax-parse x
    [(если a (~datum тогда) b ... (~datum иначе) c ...)
     #'(если (a b ...) (иначе c ...))]
    [(если a ... (~datum тогда) b ... (~datum иначе) c ...)
     #`(если (#,(применить-правила (datum->syntax x (syntax-e #'(a ...)) x x)) b ...)
             (иначе c ...))]
    [(если a (~datum тогда) b ...)
     #'(если (a b ...))]
    [(если a ... (~datum тогда) b ...)
     #`(если (#,(применить-правила (datum->syntax x (syntax-e #'(a ...)) x x)) b ...))]
    [_ x]))

(define (учесть-буквально x)
  (syntax-parse x
    [((~and q
            (~or (~datum quote)
                 (~datum unquote)
                 (~datum quasiquote)
                 (~datum unquote-splicing)
                 (~datum буквально)
                 (~datum почти-буквально)
                 (~datum не-буквально)
                 (~datum не-буквально-списком)
                 (~datum синтаксис)
                 (~datum почти-синтаксис)
                 (~datum не-синтаксис)
                 (~datum не-синтаксис-списком)))
      b c d ...)
     #`(q #,(применить-правила #'(b c d ...)))]
    [_ x]))

(define (не-буквально? синтаксис)
  (syntax-parse синтаксис
    [((~and q
            (~or (~datum quote)
                 (~datum unquote)
                 (~datum quasiquote)
                 (~datum unquote-splicing)
                 (~datum буквально)
                 (~datum почти-буквально)
                 (~datum не-буквально)
                 (~datum не-буквально-списком)
                 (~datum синтаксис)
                 (~datum почти-синтаксис)
                 (~datum не-синтаксис)
                 (~datum не-синтаксис-списком)))
      d ...)
     #f]
    [_ #t]))

(define (обработать-$ x)
  (syntax-parse x
    [(a ... (~datum $) b) #'(a ... b)]
    [(a ... (~datum $) b ...)
     (применить-правила (datum->syntax x (syntax-e #`(a ... #,(применить-правила #'(b ...)))) x x))]
    [_ x]))

(define (применить-правила x)
  (define y (обработать-операторы
             (учесть-буквально
              (обработать-$
               (обработать-если x)))))
  (syntax-parse y
    [(b ... (kw:keyword c) d ...)
     #:when (не-буквально? #'(b ...))
     (применить-правила #'(b ... kw c d ...))]
    [(b ... (kw:keyword c ...) d ...)
     #:when (не-буквально? #'(b ...))
     (применить-правила #'(b ... kw (c ...) d ...))]
    [(a ... b (~datum |.|) c (~datum |.|) d e ...) #'(c a ... b d e ...)]
    [_ y]))

(define (список->синтаксис список [преобразовать (λ (x) x)])
  (define source (if (null? список) #f (car список)))
  (datum->syntax source (преобразовать список) source orig))

(define (применить-правила-к-списку список)
  (define x (список->синтаксис список))
  (применить-правила
   (syntax-parse x
     [(a ... (~datum |;|) . b) (разделить-по-точке-с-запятой x)]
     [_ x])))

(define (разобрать-список-с-одной-точкой x)
  (syntax-parse x
    [(a ... (~datum |.|) c)
     (datum->syntax x (append* (stx-map разобрать-список-с-одной-точкой #'(a ...))
                               (list (разобрать-список-с-одной-точкой #'c)))
                    x x)]
    [(a ... (~datum |.|))
     (datum->syntax x (stx-map разобрать-список-с-одной-точкой #'(a ... пустой-список)) x x)]
    [(a ... (~and dot (~datum |.|)) . b)
     (apply raise-read-error "неожиданная `.`" (build-source-location-list #'dot))]
    [(a ...) (datum->syntax x (stx-map разобрать-список-с-одной-точкой x) x x)]
    [_ x]))

(define current-source-name (make-parameter #f))

(define (заменить-логические x)
  (cond
    [(syntax? x)
     (define sym (syntax-e x))
     (case sym
       [(истина) (datum->syntax x #t x x)]
       [(ложь) (datum->syntax x #f x x)]
       [else x])]
    [else x]))

(define (прочитать-блок level)
  (пропустить-незначащее! #t)
  (define-values (c c2) (посмотреть-две-литеры))
  (cond
    [(eof-object? c)
     (read-char)
     (cons -1 null)]
    [(and (литеры-равны? c #\\)
          (литеры-равны? c2 #\newline))
     (read-char) (read-char) (прочитать-блок level)]
    [(литеры-равны? c #\newline)
     (read-char)     
     (define next-level (прочитать-отступ! (not (terminal-port? (current-input-port)))))
     (if (отступ-увеличен? next-level level)
         (прочитать-блоки next-level)
         (cons next-level null))]
    [(литеры-равны? c #\;)
     (read-char)
     (пропустить-незначащее!)
     (cons level null)]
    [else
     (define first (прочитать-элемент))
     (match-define (cons new-level rest) (прочитать-блок level))
     (cons new-level (if (eof-object? first) first (cons first rest)))]))

(define (прочитать-цитату qt)
  (define char (peek-char-or-special))
  (define-values (ln col pos) (port-next-location (current-input-port)))  
  (define stx (if (char-whitespace? char) qt (list qt (прочитать-элемент))))
  (define-values (_1 _2 end-pos) (port-next-location (current-input-port)))
  (datum->syntax #f stx
                 (vector (current-source-name) ln col pos (- end-pos pos))))

(define (прочитать-список end)
  (пропустить-незначащее!)
  (define char (peek-char-or-special))
  (define-values (ln col pos) (port-next-location (current-input-port)))
  (cond
    [(eof-object? char)
     (raise-read-eof-error "файл закончился внутри списка" (current-source-name) ln col pos 1)]
    [(литеры-равны? char end)
     (read-char)
     null]
    [else
     (cons (прочитать-элемент) (прочитать-список end))]))

(define orig (read-syntax #f (open-input-string "orig")))

(define (прочитать-список-с-правилами последний-символ)
  (применить-правила-к-списку (прочитать-список последний-символ)))

(define (прочитать-литеру)
  (define сч 2)
  (define строка-поиска (make-string 9 #\space))  
  (let loop ([сч 2])
    (define литера (peek-char-or-special (current-input-port) (bytes-length
                                                               (string->bytes/utf-8
                                                                (substring строка-поиска 0 сч)))))
    (when (and (char? литера) (char-alphabetic? литера))
      (string-set! строка-поиска (- сч 2) литера)
      (when (< сч 11) (loop (add1 сч)))))

  (let loop ([литеры '(("перенос " . #\newline)
                       ("пусто " . #\nul)
                       ("забой " . #\backspace)
                       ("таб " . #\tab)
                       ("страница . " #\page)
                       ("возврат " . #\return)
                       ("втаб " . #\vtab)
                       ("пробел " . #\space)
                       ("удаление " . #\rubout))])
    (cond
      [(null? литеры) (read-syntax (current-source-name))]
      [else
       (match-define (cons поиск литера) (car литеры))
       (cond
         [(string-prefix? строка-поиска поиск)
          ; в строке лишний пробел, но не хватает #\
          (for ([сч (add1 (string-length поиск))]) (read-char))
          литера]
         [else (loop (cdr литеры))])])))
  
(define (прочитать-элемент)
  (define-values (c c2) (посмотреть-две-литеры))
  (define-values (ln col pos) (port-next-location (current-input-port)))
  (define (позиция)
    (define-values (_ __ end-pos) (port-next-location (current-input-port)))
    (vector (current-source-name) ln col pos
            (- end-pos pos)))
  (define элемент
    (cond
      [(eof-object? c) c]
      [(and
        (литеры-равны? c #\.)
        (or (eof-object? c2)
            (литеры-равны? c2 #\newline)
            (литеры-равны? c2 #\space)
            (литеры-равны? c2 #\tab)))
       (read-char)
       '|.|]
      [(литеры-равны? c #\;)
       (read-char)
       '|;|]
      [(литеры-равны? c #\`)
       (read-char)
       (прочитать-цитату 'почти-буквально)]
      [(литеры-равны? c #\')
       (read-char)
       (прочитать-цитату 'буквально)]
      [(литеры-равны? c #\,)
       (read-char)
       (cond
         [(литеры-равны? c2 #\@)
          (read-char)
          (прочитать-цитату 'не-буквально-списком)]
         [else (прочитать-цитату 'не-буквально)])]
      [else
       (define res
         (cond
           [(литеры-равны? c #\()
            (read-char)
            (прочитать-список-с-правилами #\))]
           [(литеры-равны? c #\[)
            (read-char)
            (прочитать-список-с-правилами #\])]
           [(литеры-равны? c #\{)
            (read-char)
            (прочитать-список-с-правилами #\})]
           [(литеры-равны? c #\#)         
            (cond
              [(литеры-равны? c2 #\`)
               (read-char) (read-char)
               (прочитать-цитату 'почти-синтаксис)]
              [(литеры-равны? c2 #\')
               (read-char) (read-char)
               (прочитать-цитату 'синтаксис)]
              [(литеры-равны? c2 #\,)
               (read-char) (read-char)
               (cond
                 [(литеры-равны? (peek-char-or-special) #\@)
                  (read-char)
                  (прочитать-цитату 'не-синтаксис-списком)]
                 [else (прочитать-цитату 'не-синтаксис)])]
              [(литеры-равны? c2 #\()
               (read-char) (read-char)
               (список->синтаксис (прочитать-список #\)) list->vector)]
              [(литеры-равны? c2 #\\)
               (прочитать-литеру)]
              [else (read-syntax (current-source-name))])]
           [else (read-syntax (current-source-name))]))
       (let loop ([res res])
         (define следующий-символ (peek-char-or-special))
         (cond
           [(литеры-равны? следующий-символ #\()
            (read-char)
            (loop (datum->syntax #f (cons res (прочитать-список-с-правилами #\)))
                                 (позиция)
                                 res))]
           [(литеры-равны? следующий-символ #\[)
            (read-char)
            (loop (datum->syntax
                   res
                   `(квадратные-скобки ,res
                                       ,(обработать-одноэлементный
                                         (прочитать-список-с-правилами #\])))
                   (позиция)
                   res))]
           [(литеры-равны? следующий-символ #\{)
            (read-char)
            (define l (прочитать-список-с-правилами #\}))
            (loop (datum->syntax
                   res
                   (list* (if (cons? (car (syntax->datum l))) 'для-объекта 'вызвать-метод) res l)
                   (позиция)
                   res))]
           [else (заменить-логические res)]))]))
  (cond
    [(eof-object? элемент) элемент]
    [(syntax? элемент) (datum->syntax элемент (syntax-e элемент) (позиция) элемент)]
    [else (datum->syntax #f элемент (позиция))]))

(define (прочитать-блоки level)
  ;; indent -> (listof syntax?)
  (match-define (cons next-level stx) (прочитать-блок-с-правилами level))
  (cond
    [(null? stx) (cons next-level null)]
    [(equal? next-level level)
     (match-define (cons next-next-level next-blocks) (прочитать-блоки level))
     (list* next-next-level stx next-blocks)]
    [else
     (cons next-level (list stx))]))

(module+ test
  (require rackunit)
  (define (test a b)
    (check-equal? (with-input-from-string a my-read) b))
  (test "(1 2 . 3)" '(1 2 . 3))
  (test "1 2 . 3" '(1 2 . 3))
  (test "1 . (2 . 3 . 4) . 3" '((3 2 4) 1 3))
  (test "1 (. +) 3" '(1 + 3))
  (test "1 . f . 2 + 2 . g . 3" '(+ (f 1 2) (g 2 3)))
  (test "1 + 3" '(+ 1 3))
  (test ": 2 ." '(: 2 пустой-список))
  (check-equal? (with-input-from-string "1 2; 3" (λ () (list (my-read) (my-read)))) '((1 2) 3))
  (check-equal? (with-input-from-string "1 2; 3 4" (λ () (list (my-read) (my-read)))) '((1 2) (3 4)))
  (test "1 2\n 3 4;  \n \n    5 6\n  7 8"
                '(1 2 (3 4) (5 6 (7 8))))
  (test "(2 #|23 32|# . 3)"  '(2 . 3))
  (test "2 3 -- sadasd  sad as\n 4 5"  '(2 3 4 5))
  (test "(2 3 -- sadasd  sad as\n 4 5)"  '(2 3 4 5))
  (test "f(a) f(g(d))" '((f a) (f (g d))))
  (test "f(a; b c; d)" '(f a (b c) d))
  (test "цикл/первый
  ;
    p points
    #:когда $ tau < p[0]
  bonus := bonus + p[1]"
        '(цикл/первый ((p points) #:когда (< tau (квадратные-скобки p 0)))
                      (:= bonus (+ bonus (квадратные-скобки p 1)))))
  (test "цикл/первый
  $
    p points
  bonus := bonus + p[1]"
        '(цикл/первый ((p points))
                      (:= bonus (+ bonus (квадратные-скобки p 1)))))
  (test "цикл/первый
  $ p points
  bonus := bonus + p[1]"
        '(цикл/первый ((p points))
                      (:= bonus (+ bonus (квадратные-скобки p 1)))))
  (test "` 1 2 3"
        '(почти-буквально (1 2 3)))
  (test "#` 1 2 3"
        '(почти-синтаксис (1 2 3)))
  (test "цикл/первый (p points; #:когда $ tau < p[0])\n bonus := bonus + p[2]"
        '(цикл/первый ((p points) #:когда (< tau (квадратные-скобки p 0)))
                      (:= bonus (+ bonus (квадратные-скобки p 2)))))
  (test "new(point%){move-x 5; move-y 7; move-x 12}"
        '(для-объекта (new point%) (move-x 5) (move-y 7) (move-x 12)))
  (test "new(point%){move-x 5}"
        '(вызвать-метод (new point%) move-x 5))
  (test "если 2 > 3 тогда 3 иначе 2"
        '(если ((> 2 3) 3) (иначе 2)))
  (test "если 2 > 3 тогда\n  a := 3\n  иначе\n  a := 2"
        '(если ((> 2 3) (:= a 3)) (иначе (:= a 2))))
  (test "..." '...)
  (test "f x y ? 1 2" '(? (f x y) 1 2))
  (test "f(x) =\n  1 + 2\n  2 - 3" '(= (f x) (+ 1 2) (- 2 3)))
  (test "тест = проверка 5" '(= тест (проверка 5)))
  (test "2 3 #| sadasd  sad as\n 4 |# 5"  '(2 3 5)))
