#lang scribble/manual

@(require 1/lang scribble/example scribble/core scribble/racket
          (for-label 1/all-base))


@title[#:tag "essentials"]{Основы языка}

Программа состоит из команд. Команда может быть вызовом функции, синтаксической конструкцией или
определением переменной.
Первая строка в программе определяет используемый язык программирования и является строкой
@litchar{#!1}.

Комментарий начинается с литер @litchar{--}.
и заканчивается концом строки.

Например, эта программа запрашивает имя и выводит приветствие:
@codeblock|{
#!1
вывести "введите имя: "
имя = прочитать-строку()
вывести
  "Привет, " ++ имя
}|

@section[#:tag "simple values"]{Простые значения}

Значения языка программирования включают числа, логические значения, строки и массивы байтов.
В DrRacket и документации они выделены зелёным цветом.

@defterm{Числовые значения} могут быть записаны как целые произвольной длины, в виде десятичных
или простых дробей, с экспонентой или мнимой частью.
@racketblock[
10      2.5
1/3     1.02e+13
5+6i    12345678123456781234567812345678
]

Бесконечные целые и простые дроби позволяют выполнять арифметические операции без потери точности
и риска переполнения. Числа с десятичной точкой или экспонентой являются
вещественными числами двойной точности и хранят только 15-17 литер.

@defterm{Логические значения} — это @racketvalfont{истина} и @racketvalfont{ложь}. При проверках
в логических операциях любое значение, не равное
@racketvalfont{ложь} трактуется как истина.

@defterm{Строковые значения} записываются между двойными кавычками. Для записи кавычек используется
последовательность литер @litchar{\"}, для записи литеры @litchar{\} --- @litchar{\\}.
Все остальные литеры Юникода можно писать как есть.

@racketblock[
"Привет!"
"Автомобиль \"Москвич\""
"你好"
]

Второй вариант ввода строковых значений --- между кавычками @litchar{«} и @litchar{»}. В этом случае
внутри строки можно использовать двойные кавычки без обратной черты и при соблюдении парности можно
писать кавычки внутри кавычек. Если парность нарушается, то также можно поставить обратную черту
перед кавычкой: @litchar{\«} и @litchar{\»}.

Когда константа выводится в окне интерпретатора, как правило, она имеет тот же вид, в котором она
была введена, но иногда при выводе происходит нормализация.
В окне интерпретатора и в документации результат вычислений выводится синим, а не зелёным, чтобы
было видно, где результат, а где введённое значение.

@examples[#:label "Примеры:"
 (eval:alts (unsyntax (racketvalfont "1.0000")) 1.0)
 (eval:alts (unsyntax (racketvalfont "\"\\u0022ok\\u0022\"")) "\u0022ok\u0022")
 (eval:alts (unsyntax (racketvalfont "«Язык «Адина»»")) "Язык «Адина»")
 (eval:alts (unsyntax (racketvalfont "«Кавычка \\«»")) "Кавычка «")
 (eval:alts (unsyntax (racketvalfont "«Кавычки \" можно писать как есть»"))
            "Кавычки \" можно писать как есть")]

@section[#:tag "expressions"]{Выражения}

Выражение --- это команда языка Адина, которая возвращает значение.

Выражения записываются в виде последовательности слов, разделённых пробельными литерами.

Обычно первое слово определяет синтаксис выражения. Если первое слово является функцией, то
остальные слова --- аргументы этой функции.
@examples[#:label "Примеры:"
(eval:alts (unsyntax (elem (racket список) (racketvalfont " 1 2 3"))) '(1 2 3))
(eval:alts (unsyntax (elem (racket пара) (racketvalfont " 5 6"))) '(5 . 6))
]

Если какие-то аргументы также являются функциями, то можно использовать отступы
@examples[#:label #f
(eval:alts (unsyntax (elem (racket список) (racketvalfont " 1 2 3 4")
                           (linebreak) (hspace 4) (racket список) (racketvalfont " 5 6")
                           (linebreak) (hspace 4) (racketvalfont "7")))
           '(1 2 3 (5 6) 7))
]

После любого элемента строки можно следующие элементы писать по одному на строке.
Отступ этих элементов должен быть больше отступа текущей строки и одинаков.
Если элемент состоит из одного слова, он является значением, если же из нескольких,
то командой, результат которой будет значением элемента.

Если по какой-либо причине выписывать последние элементы по одному на строке некрасиво,
например, если первый аргумент является командой, а остальные простыми значениями,
то можно функцию писать в виде «функция(аргумент1 аргумент2 ...)».
Предыдущий пример тогда будет выглядеть как
@examples[#:label #f
(eval:alts (unsyntax (elem (racket список)
                           (racketvalfont " 1 2 3 4 ")
                           (racket список) (racketvalfont "(5 6) 7")))
           '(1 2 3 (5 6) 7))
]
Следует запомнить, что в таком случае скобка должна идти сразу за именем функции.

Ещё один альтернативный способ записи: в стиле лиспа. Можно просто взять всю команду в скобки:
@examples[#:label #f
(eval:alts (unsyntax (elem (racket список)
                           (racketvalfont " 1 2 3 4 (")
                           (racket список) (racketvalfont " 5 6) 7")))
           '(1 2 3 (5 6) 7))
]
и тогда внутри скобок переносы и отступы игнорируются.

Если строка очень длинная, то можно перед переносом вставить литеру @litchar{\}, тогда перенос
не будет нести синтаксического смысла.

Выбор способа написания определяется удобством чтения. При вводе в окно интерпретатора
ввод заканчивается после пустой строки, так как до этого возможно продолжение команды.

Также есть ещё две особые синтаксические конструкции: «список 1 2 3 4 список(5 6)» можно
записать как «список 1 2 3 4 $ список 5 6», то есть оператор @litchar{$} позволяет
слова после неё выделить в отдельную команду. Чтобы объединить несколько коротких команд
или значений в одну строку, можно использовать оператор @litchar{;}.
@examples[#:label "Пример:"
(eval:alts (unsyntax (elem (racket список) (racketvalfont " 1 2 3 4")
                           (linebreak) (hspace 4) (racket список) (racketvalfont " 5 6")
                           (linebreak) (hspace 4) (racketvalfont "7") (racketparenfont ";")
                           (hspace 1) (racket список)
                           (racketvalfont " 8") (racketparenfont ";") (racketvalfont " 9")))
           '(1 2 3 4 (5 6) 7 (8) 9))
]
Можно заметить, что перед @litchar{;} пробел не обязателен.

Операторы @litchar{$} и @litchar{;} работают также и в скобках, но
@litchar{;} разбивает выражение на подвыражения
равного уровня, то есть
@examples[#:label "Пример:"
(eval:alts (unsyntax (elem (racketparenfont "(") (racket список) (racketparenfont ";")
                           (hspace 1) (racket список 1 2)
                           (racketparenfont ";") (hspace 1) (racket список 3 4)
                           (racketparenfont ")")))
           '((1 2) (3 4)))
]

Аналогичная конструкция для стандартного синтаксиса требует одинакового отступа для подвыражений,
поэтому её корень будет пустым и замещаться @litchar{;}:
@examples[#:label "Пример:"
(eval:alts (unsyntax (elem (racketparenfont ";")
                           (linebreak) (hspace 4) (racket список)
                           (linebreak) (hspace 4) (racket список 1 2)
                           (linebreak) (hspace 4) (racket список 3 4)))
           '((1 2) (3 4)))
]

Также внутри скобок можно использовать функциональный синтаксис со скобкой сразу после имени функции.
@examples[#:label "Пример:"
(eval:alts (unsyntax (elem (racketparenfont "(") (racket список) (racketparenfont ";") (hspace 1)
                           (racket список) (racket (1 2))
                           (racketparenfont ";") (hspace 1) (racket список 3 4) (racketparenfont ";")
                           (hspace 1) (racket список) (racket ()) (racketparenfont ")")))
           '((1 2) (3 4) ()))
]

Некоторые слова являются операторами. Оператором является слово, состоящее только из литер
@litchar{!#$%&⋆+./<=>?@"@"^~:*-}, которые называются операторными литерами. Исключения:
слова @litchar{.} и @litchar{...} операторами не являются.
Также оператором является любое слово, которое начинается и заканчивается на @litchar{^}.
Примеры операторов: @litchar{+}, @litchar{-}, @litchar{^пара^}.

Если оператор встречается в команде и не является первым словом, то из выражений до оператора
будет собрано одно выражение, а из выражений после --- второе. Затем будет сформирована команда,
в которой первым словом будет оператор, а его аргументами --- эти два выражения.
Например @racket[(список 2 3 ++ список 4 5 6)] то же самое, что
@racket[(++ (список 2 3) (список 4 5 6))]. Особым образом обрабатываются операторы
@racket[=] и @racket[?]: выражения после них не объединяются в одно, а переносятся в результирующую
команду как есть, потому что эти операторы требуют больше двух аргументов.

Если оператор начинается и заканчивается на @litchar{^} и между ними есть литеры кроме операторных,
то он вызывает функцию по имени между @litchar{^} со своими аргументами.
Например, @racket[(2 ^пара^ 3)] то же самое, что @racket[(пара 2 3)]. Таким образом можно любую
двухаргументную функцию использовать как оператор.

Если в строке несколько операторов, то порядок их применения определяется
@seclink["priorities" "приоритетами"]. Например, @racket[(2 + 2 * 2)] будет равно 6, как и должно
с точки зрения приоритетов арифметических операторов: на первом шаге преобразуется в
@racket[(+ 2 (2 * 2))], затем в @racket[(+ 2 (* 2 2))], и затем вычислится как @racket[(+ 2 4) = 6].

@section[#:tag "basic definitions"]{Основы определений}

При описании синтаксиса «...» обозначает, что предыдущий элемент может повторяться 0 и более раз,
«...+» --- 1 и более раз. В угловых скобках указываются синтаксические переменные. Например, вместо
<идентификатор> может быть подставлен любой допустимый идентификатор языка.

Определение в форме
@racketblock[
(<идентификатор> = <выражение>)
]
связывает <идентификатор> с результатом вычисления выражения, а в форме
@racketblock[
(<идентификатор>(<идентификатор> ...) = <команда> ... <выражение>)
]
связывает первый <идентификатор> с функцией, которая принимает аргументы, именованные остальными
идентификаторами. Последовательность команд и выражение являются телом функции. При вызове функции
её результатом является результат последнего выражения. Если аргументы есть, то скобки можно не
писать, а просто перечислить аргументы через пробел, как описано в предыдущем разделе.

Команда внутри функции может также являться определением. В этом случае связывание видно только
внутри функции.

При разборе определения функции есть исключение синтаксиса: в этом случае оператор @racket[=]
объединяет слова в команду только с левой стороны, иначе в функции могла бы быть только одна команда.
Поэтому даже если функция состоит из одной команды, она обязательно должна быть выделена или
скобками или переносом.

@examples[#:label "Примеры:"
(eval:alts (unsyntax (elem (racketidfont "часть ") (racket =) (racketvalfont " 3"))) (void))
(eval:alts (unsyntax (elem (racketidfont "кусок строка ") (racket =) (linebreak) (hspace 4)
                                         (racket подстрока)
                           (racketvalfont " строка 0 часть"))) (void))
(eval:alts (unsyntax (racketvalfont "часть")) 3)
(eval:alts (unsyntax (racketvalfont "кусок \"три литеры\"")) "три")
]

Определение функции может включать несколько выражений. Тогда значение последнего выражения будет
значением функции, а остальные выражения вычисляются только для побочных эффектов, таких как вывод.
@examples[#:label "Примеры:"
(eval:alts (unsyntax (elem (racketidfont "испечь вкус " (racket =))
                           (linebreak) (hspace 4)
                           (racket вывести) (hspace 1) (racket "разогрев печи...\n")
                           (linebreak) (hspace 4)
                           (racket вкус) (hspace 1) (racket ++) (hspace 1) (racket " пирог")))
           (void))
(eval:alts (unsyntax (elem (racketidfont "испечь") (hspace 1) (racket "вишнёвый")
                           (linebreak) (racketoutput "разогрев печи...")))
           "вишнёвый пирог")
]

Если попробовать записать функцию в одну строку, то получится
@examples[#:label "Примеры:"
(eval:alts (unsyntax (elem (racketidfont "не-печётся вкус ") (racket =) (hspace 1)
                           (racket вкус) (hspace 1) (racket ++) (hspace 1) (racket " пирог")))
           (void))
(eval:alts (unsyntax (elem (racketidfont "не-печётся") (hspace 1) (racket "вишнёвый")))
           " пирог")
]

Это потому, что определение прочитано как
@codeblock|{
не-печётся вкус =
  вкус
  ++
  " пирог"
}|
и последовательно выполняется: вычисление значения переменной, значения операции и строки.
Последнее возвращается как результат функции.

И, на самом деле, определение функции, так же, как и определение не функции, всего лишь связывает
идентификатор с значением, и этот идентификатор можно тоже использовать как выражение.

@examples[#:label "Примеры:"
(eval:alts #,(racketidfont "кусок") (eval:result (racketresultfont "#<функция:кусок>") "" ""))
(eval:alts #,(racketidfont "подстрока") (eval:result (racketresultfont "#<функция:подстрока>") "" ""))
]

@section[#:tag "identifiers"]{Идентификаторы}

Синтакисис для идентификаторов максимально свободный. В них могут быть использованы любые литеры
кроме пробелов, скобок, кавычек, апострофов, точки с запятой, запятой, решётки, вертикальной черты
и обратной косой черты. Если очень надо, запретную литеру можно экранировать обратной косой чертой.
Более того, можно вводить идентификатор между вертикальным чертами, тогда
допустимы вообще любые литеры кроме вертикальной черты.

Примеры идентификаторов:
@codeblock|{
не-печётся
++
=
Проверка
проверка/вывод
а+б
с+1
1\;2\\3
|идентификатор со спецлитерами ( ) [ ] { } " , ' ` ; # \|
}|

@section[#:tag "function call"]{Вызовы функций}

Мы уже видели много вызовов функций. Синтаксис вызова
@codeblock|{
(<имя> <выражение> ...)
}|
где количество выражений определяется количеством аргументов функции с именем <имя>.

Разумеется, при записи с начала строки скобки можно опустить.

Язык Адина предопределяет множество функций, таких как @racket[подстрока] и @racket[добавить-строки].
Ниже будут ещё примеры.

В коде примеров в документации использования предопределённых имён оформлены ссылками на
документацию. Таким образом можно просто щёлкнуть по имени функции и получить полную информацию о
её использовании.

@examples[#:label #f
(eval:alts (unsyntax (elem (racket добавить-строки) (hspace 1) (racket "рос") (hspace 1)
                           (racket "сель") (hspace 1) (racket "торг")
                           (racketcommentfont "  -") (racketcommentfont "- добавить строки")))
            "россельторг")
(eval:alts (unsyntax (elem (racket подстрока) (hspace 1) (racket "паровоз") (hspace 1)
                           (racket 0) (hspace 1) (racket 3)
                           (racketcommentfont "  -") (racketcommentfont "- извлечь подстроку")))
           "пар")
(eval:alts (unsyntax (elem (racket строка?) (hspace 1) (racket "это строка")
                           (racketcommentfont "  -") (racketcommentfont "- распознать строку")))
           (eval:result (racketvalfont "истина")))
(eval:alts (unsyntax (elem (racket строка?) (hspace 1) (racket 42)))
           (eval:result (racketvalfont "ложь")))
(eval:alts (unsyntax (elem (racket корень) (hspace 1) (racket 16)
                           (racketcommentfont "  -")
                           (racketcommentfont "- вычислить квадратный корень")))
           (sqrt 16))
(eval:alts (unsyntax (elem (racket корень) (hspace 1) (racket -16)))
           (sqrt -16))
(eval:alts (unsyntax (elem (racket +) (hspace 1) (racket 1) (hspace 1) (racket 2)
                           (racketcommentfont "  -")
                           (racketcommentfont "- сложить")))
           (+ 1 2))
(eval:alts (unsyntax (elem (racket -) (hspace 1) (racket 2) (hspace 1) (racket 2)
                           (racketcommentfont "  -")
                           (racketcommentfont "- вычесть")))
           (- 2 1))
(eval:alts (unsyntax (elem (racket <) (hspace 1) (racket 2) (hspace 1) (racket 2)
                           (racketcommentfont "  -")
                           (racketcommentfont "- сравнить")))
           (eval:result (racketvalfont "ложь")))
(eval:alts (unsyntax (elem (racket >=) (hspace 1) (racket 2) (hspace 1) (racket 2)))
           (eval:result (racketvalfont "истина")))
(eval:alts (unsyntax (elem (racket число?) (hspace 1) (racket "это не число")
                           (racketcommentfont "  -")
                           (racketcommentfont "- распознать число")))
           (eval:result (racketvalfont "ложь")))
(eval:alts (unsyntax (elem (racket число?) (hspace 1) (racket 1)))
           (eval:result (racketvalfont "истина")))
(eval:alts (unsyntax (elem (racket ==) (hspace 1) (racket 6) (hspace 1) (racket "шесть")
                           (racketcommentfont "  -")
                           (racketcommentfont "- сравнить что угодно")))
           (eval:result (racketvalfont "ложь")))
(eval:alts (unsyntax (elem (racket ==) (hspace 1) (racket 6) (hspace 1) (racket 6)))
           (eval:result (racketvalfont "истина")))
(eval:alts (unsyntax (elem (racket ==) (hspace 1) (racket "шесть") (hspace 1) (racket "шесть")))
           (eval:result (racketvalfont "истина")))]

Если функция является оператором, то её можно писать вторым словом.

@examples[#:label "Примеры:"
(eval:alts (unsyntax (elem (racket 1) (hspace 1) (racket +) (hspace 1) (racket 2)
                           (racketcommentfont "  -")
                           (racketcommentfont "- сложить")))
           (+ 1 2))
(eval:alts (unsyntax (elem (racket 2) (hspace 1) (racket -) (hspace 1) (racket 1)
                           (racketcommentfont "  -")
                           (racketcommentfont "- вычесть")))
           (- 2 1))
(eval:alts (unsyntax (elem (racket 2) (hspace 1) (racket <) (hspace 1) (racket 1)
                           (racketcommentfont "  -")
                           (racketcommentfont "- сравнить")))
           (eval:result (racketvalfont "ложь")))
(eval:alts (unsyntax (elem (racket 2) (hspace 1) (racket >=) (hspace 1) (racket 1)))
           (eval:result (racketvalfont "истина")))
(eval:alts (unsyntax (elem (racket 6) (hspace 1) (racket ==) (hspace 1) (racket "шесть")
                           (racketcommentfont "  -")
                           (racketcommentfont "- сравнить что угодно")))
           (eval:result (racketvalfont "ложь")))
(eval:alts (unsyntax (elem (racket 6) (hspace 1) (racket ==) (hspace 1) (racket 6)))
           (eval:result (racketvalfont "истина")))
(eval:alts (unsyntax (elem (racket "шесть") (hspace 1) (racket ==) (hspace 1) (racket "шесть")))
           (eval:result (racketvalfont "истина")))]

@section[#:tag "conditionals expressions"]{Условные конструкции с @racket[если] и
 операторами @racket[?], @racket[&&] и @racket[||]}

Следующий простейший вид выражения --- это условное выражение:
@codeblock|{
(? <выражение-условия> <выражение-если-истина> <выражение-если-ложь>)
}|

Первое выражение вычисляется всегда. Если его результат равен @racketvalfont{ложь}, тогда
условное выражение вычисляет @racket[выражение-если-ложь] и возвращает его резулоьтат. Если же
результат любой другой, то вычисляется и возвращается @racket[выражение-если-истина].
Обратите внимание, что оба выражения (на истину и ложь) обязательны.

Оператор @racket[?] имеет три аргумента и поэтому обрабатывается особым образом. Слова слева от него
объединяются в одно выражение, а слова справа остаются как есть, так же, как в определении функции.

@examples[#:label "Пример:"
(eval:alts (unsyntax (elem (racket 2) (hspace 1) (racket >)
                           (hspace 1) (racket 3) (hspace 1) (racket ?)
                           (linebreak) (hspace 4) (racket "2 больше, чем 3")
                           (linebreak) (hspace 4) (racket "2 не больше, чем 3")))
           "2 не больше, чем 3")]

Для следующих примеров необходимо ввести команду
@examples[#:label #f
(eval:alts (unsyntax (elem (racket используется) (hspace 1) (racket строка)))
           (void))]
чтобы была возможность использовать функции @racket[строка-начинается-с?]
и @racket[строка-заканчивается-на?].

@examples[#:label #f
(eval:alts
 (eval:no-prompt
  (unsyntax (elem (racket ответ) (hspace 1) (racket запрос) (hspace 1) (racket =)
                  (linebreak) (hspace 2)
                  (racket строка-начинается-с?) (hspace 1) (racket запрос) (hspace 1)
                  (racket "Привет") (hspace 1) (racket ?)
                  (hspace 1) (racket "Привет!")
                  (hspace 1) (racket "Чего?"))))
           (void))
(eval:alts (unsyntax (elem (racket ответ) (hspace 1) (racket "Приветствую, Адина!")))
           "Привет!")
(eval:alts (unsyntax (elem (racket ответ) (hspace 1) (racket "λx:(μα.α→α).xx")))
           "Чего?")
]

В случае, если при выполнении условия необходимо не только вернуть результат, но и выполнить
какие-либо действия, есть вариант синтаксиса с ключевыми словами:

@codeblock|{
(если <выражение-условия> ... тогда <выражения-если-истина> ... иначе <выражения-если-ложь> ...)
(если <выражение-условия> ... тогда <выражения-если-истина> ...)
}|

Выражение условия может состоять из нескольких слов: всё, что находится между ключевыми словами
«если» и «тогда» объединяется в одно выражение. После «тогда» и после «иначе» может быть несколько
выражений. Так же, как в функции, они вычисляются все и возвращают значение последнего выражения.
Вариант без «иначе», как правило, используется, когда результат выражения не нужен, а нужны только
побочные эффекты.

@examples[#:label "Пример:"
(eval:alts
 (eval:no-prompt
  (unsyntax (elem (racket ответ) (hspace 1) (racket запрос) (hspace 1) (racket =)
                  (linebreak) (hspace 2)
                  (racket если) (hspace 1) (racket строка-начинается-с?) (hspace 1)
                  (racket запрос) (hspace 1)
                  (racket "Привет") (hspace 1) (racket тогда)
                  (linebreak) (hspace 4) (racket "Привет!")
                  (linebreak) (hspace 4) (racket иначе)
                  (linebreak) (hspace 4) (racket "Чего?"))))
           (void))]

Обратите внимание, что «иначе» не имеет отступа относительно команд до и после него, как встречается
в других языках программирования.

Сложные условия могут формироваться путём вложения условных выражений. Например, в предыдущем
примере в @racket[ответ] должна передаваться строка, так как @racket[подстрока] завершится с ошибкой,
если ей передать не строку. Можно убрать это ограничение, добавив ещё одну проверку:

@examples[#:label #f
(eval:alts
 (eval:no-prompt
  (unsyntax (elem (racket ответ-на-что-угодно) (hspace 1) (racket запрос) (hspace 1) (racket =)
                  (linebreak) (hspace 2)
                  (racket строка?) (hspace 1) (racket запрос) (hspace 1) (racket ?)
                  (linebreak) (hspace 4)
                  (racket строка-начинается-с?) (hspace 1) (racket запрос) (hspace 1)
                  (racket "Привет") (hspace 1) (racket ?)
                  (linebreak) (hspace 6) (racket "Привет!")
                  (linebreak) (hspace 6) (racket "Чего?")
                  (linebreak) (hspace 4) (racket "Чего?"))))
           (void))]

Вместо того, чтобы дублировать ветку «Чего?», лучше записать эту функцию как:

@examples[#:label #f
(eval:alts
 (eval:no-prompt
  (unsyntax (elem (racket ответ-на-что-угодно) (hspace 1) (racket запрос) (hspace 1) (racket =)
                  (linebreak) (hspace 2) (racket ?) (linebreak) (hspace 4)
                  (racket строка?) (hspace 1) (racket запрос) (hspace 1) (racket ?)
                  (linebreak) (hspace 6)
                  (racket строка-начинается-с?) (hspace 1) (racket запрос) (hspace 1)
                  (racket "Привет") (hspace 1)
                  (linebreak) (hspace 6) (racketvalfont "ложь")
                  (linebreak) (hspace 4) (racket "Привет!")
                  (linebreak) (hspace 4) (racket "Чего?"))))
           (void))]


Но такие вложенные условия сложно читать. Адина предоставляет удобочитаемые короткие формы:

@codeblock|{
(&& <выражение>*)
(|| <выражение>*)
}|

Форма @racket[&&] выполняет выражения-аргументы. Если текущее выражение возвращает
@racketvalfont{ложь}, то остальные выражения не вычисляются.
Возвращается результат последнего вычисленного выражения. Такой способ вычисления
называется вычислением по короткой схеме.

Форма @racket[||] аналогично выполняет выражения пока они возвращают @racketvalfont{ложь}.

Также обратите внимание, что эти обе формы являются операторами, поэтому если выражений всего два,
то можно ставить оператор между выражениями. Приоритет @racket[&&] выше, чем приоритет @racket[||].

@examples[#:label "Примеры:"
(eval:alts
 (eval:no-prompt
  (unsyntax (elem (racket ответ-на-что-угодно) (hspace 1) (racket запрос) (hspace 1) (racket =)
                  (linebreak) (hspace 2)
                  (racket строка?) (hspace 1) (racket запрос) (hspace 1) (racket &&) (hspace 1)
                  (racket строка-начинается-с?) (hspace 1) (racket запрос) (hspace 1)
                  (racket "Привет") (hspace 1) (racket ?)
                  (linebreak) (hspace 4) (racket "Привет!")
                  (linebreak) (hspace 4) (racket "Чего?"))))
           (void))
(eval:alts (unsyntax (elem (racket ответ-на-что-угодно) (hspace 1) (racket "Приветствую, Адина!")))
           "Привет!")
(eval:alts (unsyntax (elem (racket ответ-на-что-угодно) (hspace 1) (racket 17)))
           "Чего?")
]

Обратите внимание, что здесь в одном выражении есть операторы @racket[&&] и @racket[?].
Так как @seclink["priorities" "приоритет"] оператора @racket[&&] выше, то сначала группируется
всё выражение слева от @racket[?], а потом уже результат сравнения используется как условие.

Обратите внимание, что @racket[&&] и @racket[||] работают с любым количеством выражений:
@examples[#:label "Примеры:"
(eval:alts
 (eval:no-prompt
  (unsyntax (elem (racket ответ-на-восклицание) (hspace 1) (racket запрос) (hspace 1) (racket =)
                  (linebreak) (hspace 2) (racket ?)
                  (linebreak) (hspace 4) (racket &&)
                  (linebreak) (hspace 6) (racket строка?) (hspace 1) (racket запрос)
                  (linebreak) (hspace 6) (racket строка-начинается-с?)
                  (hspace 1) (racket запрос) (hspace 1) (racket "Привет")
                  (linebreak) (hspace 6) (racket строка-заканчивается-на?)
                  (hspace 1) (racket запрос) (hspace 1) (racket "!")
                  (linebreak) (hspace 4) (racket "Привет!")
                  (linebreak) (hspace 4) (racket "Чего?"))))
           (void))
(eval:alts (unsyntax (elem (racket ответ-на-восклицание) (hspace 1) (racket "Приветствую, Адина!")))
           "Привет!")
(eval:alts (unsyntax (elem (racket ответ-на-восклицание) (hspace 1) (racket "Приветствую.")))
           "Чего?")
]

То же самое можно сделать в операторном стиле:

@examples[#:label #f
(eval:alts
 (eval:no-prompt
  (unsyntax (elem (racket ответ-на-восклицание) (hspace 1) (racket запрос) (hspace 1) (racket =)
                  (linebreak) (hspace 2) (racket строка?) (hspace 1) (racket запрос) (hspace 1)
                  (racket &&) (hspace 1) (racket строка-начинается-с?)
                  (hspace 1) (racket запрос) (hspace 1) (racket "Привет") (hspace 1)
                  (racketparenfont "\\")
                  (linebreak) (hspace 16)
                  (hspace 1) (racket &&) (hspace 1) (racket строка-заканчивается-на?)
                  (hspace 1) (racket запрос) (hspace 1) (racket "!") (hspace 1) (racket ?)
                  (linebreak) (hspace 4) (racket "Привет!")
                  (linebreak) (hspace 4) (racket "Чего?"))))
           (void))]

Как видно, пришлось сделать перенос строки, чтобы она была не слишком длинной.
Выбирайте ту синтаксическую конструкцию, которую потом будет легче читать.

Часто вложенные условия используются для проверки последовательности условий, каждое из которых
возвращает свой результат:

@examples[#:label #f
(eval:alts
 (eval:no-prompt
  (unsyntax (elem (racket больше-ответов) (hspace 1) (racket запрос) (hspace 1) (racket =)
                  (linebreak) (hspace 2) (racket строка-начинается-с?) (hspace 1)
                  (racket запрос) (hspace 1) (racket "Привет") (hspace 1)(racket ?)
                  (linebreak) (hspace 4) (racket "Привет!")
                  (linebreak) (hspace 4) (racket строка-начинается-с?) (hspace 1)
                  (racket запрос) (hspace 1) (racket "Пока") (hspace 1) (racket ?)
                  (linebreak) (hspace 6) (racket "Пока!")
                  (linebreak) (hspace 6) (racket строка-заканчивается-на?) (hspace 1)
                  (racket запрос) (hspace 1) (racket "?") (hspace 1) (racket ?)
                  (linebreak) (hspace 8) (racket "Я не знаю.")
                  (linebreak) (hspace 8) (racket "Чего?"))))
 (void))]

Короткая форма записи для последовательности проверок @racket[если] без «тогда».

@codeblock|{
(если (<выражение> <команда> ... <выражение>) ...)
}|

В этом варианте синтаксиса тело формы @racket[если] состоит из последовательности правил.
Каждое правило состоит из выражения и последовательности команд. Если выражение
истинно (не равно @racketvalfont{ложь}), то выполняется последовательность команд и
возвращается результат последнего выражения. Если ложно, то аналогично обрабатывается следующее
правило. В последнем правиле можно писать «иначе» вместо @racketvalfont{истина}.
Если команды вводят определения, то они видны только внутри правила.

Таким образом можно переписать функцию @racket[больше-ответов] как:

@examples[#:label #f
 (eval:alts
  (eval:no-prompt
   (unsyntax (elem (racket больше-ответов) (hspace 1) (racket запрос) (hspace 1) (racket =)
                   (linebreak) (hspace 2) (racket если)
                   (linebreak) (hspace 4) (racket строка-начинается-с?) (racketparenfont "(")
                   (racket запрос) (hspace 1) (racket "Привет") (racketparenfont ")")
                   (hspace 1) (racket "Привет!")
                   (linebreak) (hspace 4) (racket строка-начинается-с?) (racketparenfont "(")
                   (racket запрос) (hspace 1) (racket "Пока") (racketparenfont ")")
                   (hspace 1) (racket "Пока!")
                   (linebreak) (hspace 4) (racket строка-заканчивается-на?) (racketparenfont "(")
                   (racket запрос) (hspace 1) (racket "?") (racketparenfont ")")
                   (hspace 1) (racket "Я не знаю.")
                   (linebreak) (hspace 4) (racket иначе) (hspace 1) (racket "Чего?"))))
  (void))
 (eval:alts (unsyntax (elem (racket больше-ответов) (hspace 1) (racket "Приветствую!")))
           "Привет!")
 (eval:alts (unsyntax (elem (racket больше-ответов) (hspace 1) (racket "Пока, Адина.")))
           "Пока!")
 (eval:alts (unsyntax (elem (racket ответ-на-восклицание) (hspace 1)
                            (racket "Какой твой любимый цвет?")))
            "Я не знаю.")
 (eval:alts (unsyntax (elem (racket больше-ответов) (hspace 1) (racket "Мой зелёный.")))
            "Чего?")]

Обратите внимание, что условное выражение обязательно должно быть одним элементом. То есть оно либо
должно быть одним словом, либо вызовом функции со скобками как в этом примере, либо просто
взято в скобки. Кроме того, обязателен отступ после каждого условия или «иначе».

Если условное выражение очень сложное, то можно использовать синтаксис с «;» в качестве начала
правила:
@examples[#:label "Пример:"
(eval:alts
 (eval:no-prompt
  (unsyntax (elem (racket ответ-на-восклицание) (hspace 1) (racket запрос) (hspace 1) (racket =)
                  (linebreak) (hspace 2) (racket если)
                  (linebreak) (hspace 4) (racketparenfont ";")
                  (linebreak) (hspace 6) (racket &&)
                  (linebreak) (hspace 8) (racket строка?) (hspace 1) (racket запрос)
                  (linebreak) (hspace 8) (racket строка-начинается-с?)
                  (hspace 1) (racket запрос) (hspace 1) (racket "Привет")
                  (linebreak) (hspace 8) (racket строка-заканчивается-на?)
                  (hspace 1) (racket запрос) (hspace 1) (racket "!")
                  (linebreak) (hspace 6) (racket "Привет!")
                  (linebreak) (hspace 4) (racket иначе)
                  (linebreak) (hspace 6) (racket "Чего?"))))
 (void))]

@section[#:tag "function call2"]{Вызовы функций, снова}

Предыдущий пример грамматики для вызова функций мы чрезмерно упростили.
На самом деле вместо имени функции можно использовать произвольное выражение

@codeblock|{
(<выражение> <выражение> ...)
}|

Первое выражение может быть идентификатором переменной, содержащей функции, такой как
@racket[добавить-строки] или @racket[+]. Но может быть и любым другим выражением, результатом которого
является функция. Например, это может быть условное выражение:
@examples[#:label #f
 (eval:alts
  (eval:no-prompt
   (unsyntax (elem (racket удвоить) (hspace 1) (racket п) (hspace 1) (racket =)
                   (linebreak) (hspace 2) (racketparenfont "(") (racket строка?) (hspace 1)
                   (racket п) (hspace 1)
                   (racket ?) (hspace 1) (racket добавить-строки) (hspace 1) (racket +)
                   (racketparenfont ")")
                   (hspace 1) (racket п) (hspace 1) (racket п))))
  (void))
 (eval:alts (unsyntax (elem (racket удвоить) (hspace 1) (racket "бла")))
           "блабла")
 (eval:alts (unsyntax (elem (racket удвоить) (hspace 1) (racket 5)))
           10)]

Если выражение, вычисляющее функцию достаточно сложно, то можно использовать синтаксис с «;» в начале
вызова:
@examples[#:label #f
 (eval:alts
  (eval:no-prompt
   (unsyntax (elem (racket удвоить) (hspace 1) (racket п) (hspace 1) (racket =)
                   (linebreak) (hspace 2) (racketparenfont ";")
                   (linebreak) (hspace 4) (racket строка?) (hspace 1) (racket п)
                   (hspace 1) (racket ?) (hspace 1) (racket добавить-строки) (hspace 1) (racket +)
                   (linebreak) (hspace 4) (racket п) (racketparenfont ";") (hspace 1) (racket п))))
  (void))]

Также не забывайте, что если используете оператор в качестве значения и он не на первом и не
на последнем месте в выражении, то его надо писать в виде @racket[(#,(elem ".") +)].

Синтаксически, первый элемент списка может быть любым значением, но при выполнении будет ошибка:

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket 1) (hspace 1) (racket 2) (hspace 1) (racket 3) (hspace 1) (racket 4)))
  (void))
 (eval:alts
  (eval:no-prompt
   (unsyntax (elem (racketerror "вызов функции:
 ожидалась функция, которую можно применить к аргументам
  получено: 1")))
   )
  (void))]

Если Вы случайно пропустите имя функции или поставите лишние скобки вокруг выражения, то
чаще всего будете получать ошибку «ожидалась функция» как в примере выше.

@section[#:tag "lambda"]{Безымянные функции}

Программирование было бы утомительным, если приходилось именовать все значения.
Вместо того, чтобы написать @racket[1 + 2], пришлось бы писать:
@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket а) (hspace 1) (racket =) (hspace 1) (racket 1)))
  (void))
 (eval:alts
  (unsyntax (elem (racket б) (hspace 1) (racket =) (hspace 1) (racket 2)))
  (void))
 (eval:alts
  (unsyntax (elem (racket а) (hspace 1) (racket +) (hspace 1) (racket б)))
  3)]

Оказывается, что необходимость именовать все функции также может быть утомительной.
Например, можно сделать функцию @racket[дважды], которая принимает функцию и аргумент.
Её удобно использовать, если для функции уже есть имя:
@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket дважды) (hspace 1) (racket ф) (hspace 1) (racket п) (hspace 1) (racket =)
                  (linebreak) (hspace 4)
            (racket ф) (hspace 1) (racket $) (hspace 1) (racket ф) (hspace 1) (racket п)))
  (void))
 (eval:alts
  (unsyntax (elem (racket дважды) (hspace 1) (racket корень) (hspace 1) (racket 16)))
  2)]

Здесь @racket[ф $ ф п] является бесскобочным вариантом @racket[ф (ф п)]. Если скобка
заканчивается с концом команды, то вместо открывающей скобки можно поставить разделитель «$»
и убрать закрывающую.

Если в функцию @racket[дважды] надо передать ещё не определённую функцию, то её придётся
сначала определить, а потом передать в @racket[дважды].

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket громче строка =)
                  (linebreak) (hspace 4) (racket строка ++ "!")))
  (void))
 (eval:alts
  (unsyntax (elem (racket дважды) (hspace 1) (racket громче) (hspace 1) (racket "Привет")))
  "Привет!!")]

Но если вызов @racket[дважды] --- это единственное место, где используется @racket[громче],
то жаль писать целое определение. В Адине можно использовать выражение @racket[функция],
чтобы создавать функцию напрямую. Форма @racket[функция] содержит список аргументов
и команды тела функции.

@codeblock|{
(функция (<идентификатор> ...) <команда> ... <выражение>)
}|

Вызов этой формы возвращает новую функцию:
@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket функция (строка))
                  (linebreak) (hspace 4) (racket строка ++ "!")))
  (eval:result (racketresultfont "#<функция>") "" ""))]

Короткий вариант --- оператор @racket[==>]: слева аргументы, справа одно
выражение-тело. Записи @racket[строка ==> строка ++ "!"] и
@racket[функция (строка) $ строка ++ "!"] равносильны. Несколько
аргументов перечисляются слева через пробел:
@racket[с ч ==> подстрока с 0 ч].
В отличие от @racket[функция], справа у @racket[==>] только одно
выражение; несколько команд можно собрать в @racket[блок].

Приоритет @racket[==>] выше, чем у @racket[=], поэтому
@racket[громче = строка ==> строка ++ "!"] связывает имя с новой
функцией.

Так вышеприведённый пример может быть переписан как:
@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket дважды)
                  (linebreak) (hspace 4) (racket функция (строка) $ строка ++ "!")
                  (linebreak) (hspace 4) (racket "Привет")))
  "Привет!!")
 (eval:alts
  (unsyntax (elem (racket дважды)
                  (linebreak) (hspace 4) (racket строка ==> строка ++ "?!")
                  (linebreak) (hspace 4) (racket "Привет")))
  "Привет?!?!")]

Другое применение выражения @racket[функция] и оператора @racket[==>] ---
результат для функции, принимающей функции.

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket добавлятель-суффикса строка2 =)
                  (linebreak) (hspace 4) (racket функция (строка) $ строка ++ строка2)))
  (void))
 (eval:alts
  (unsyntax (elem (racket дважды добавлятель-суффикса) (racket ("!")) (hspace 1) (racket "Привет")))
  "Привет!!")
 (eval:alts
  (unsyntax (elem (racket дважды добавлятель-суффикса) (racket ("?!")) (hspace 1) (racket "Привет")))
  "Привет?!?!")
 (eval:alts
  (unsyntax (elem (racket дважды добавлятель-суффикса) (racket ("...")) (hspace 1) (racket "Привет")))
  "Привет......")]

Адина --- язык с лексической областью видимости. Это значит, что @racket[строка2] в функции,
возвращённой из вызова @racket[добавлятель-суффикса], всегда ссылается на аргумент вызова,
который создал функцию. Другими словами, функция, полученная выражением @racket[функция],
помнит правильное значение @racket[строка2].

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket громче = добавлятель-суффикса "!")))
  (void))
 (eval:alts
  (unsyntax (elem (racket неувереннее = добавлятель-суффикса "?")))
  (void))
 (eval:alts
  (unsyntax (elem (racket дважды неувереннее "действительно")))
  "действительно??")
 (eval:alts
  (unsyntax (elem (racket дважды громче "действительно")))
  "действительно!!")]

Когда используется определение в форме @racket[<идентификатор> = <выражение>], то также можно
определить функцию. Эти определения эквивалентны:

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket громче строка =)
                  (linebreak) (hspace 4) (racket строка ++ "!")))
  (void))
 (eval:alts
  (unsyntax (elem (racket громче = функция (строка))
                  (linebreak) (hspace 4) (racket строка ++ "!")))
  (void))
 (eval:alts
  (unsyntax (elem (racket громче = строка ==> строка ++ "!")))
  (void))
 (eval:alts
  (unsyntax (elem (racket громче)))
  (eval:result (racketresultfont "#<функция:громче>") "" ""))]

Обратите внимание, что несмотря на то, что во втором случае используется безымянная функция,
компилятор всё равно выводит имя функции, чтобы сделать печать и сообщения об ошибках максимально
информативными.

Также можно и @racket[добавлятель-суффикса] написать без использования формы @racket[функция].

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket добавлятель-суффикса) (racket (строка2)) (racket (строка)) (hspace 1)
                  (racket = $ строка ++ строка2)))
  (void))]

@section[#:tag "let"]{Локальное связывание внутри функций и через выражение @racket[пусть]}

Ещё раз обратим внимание на область видимости связей, которые определены внутри функций.

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket преобразовать строка =)
                  (linebreak) (hspace 4) (racket начинается? строка2 =) (racketcommentfont "  -")
                  (racketcommentfont "- видно только в функции «преобразовать»")
                  (linebreak) (hspace 6) (racket строка2-с-пробелом = строка2 ++ " ")
                  (racketcommentfont "  -")
                  (racketcommentfont "- видно только в функции «начинается?»")
                  (linebreak) (hspace 6) (racket строка-начинается-с? строка строка2-с-пробелом)
                  (linebreak) (hspace 4) (racket если)
                  (linebreak) (hspace 6) (racket начинается?) (racket ("Привет"))
                  (hspace 1) (racket "Привет!")
                  (linebreak) (hspace 6) (racket начинается?) (racket ("Пока"))
                  (hspace 1) (racket "Пока!")
                  (linebreak) (hspace 6) (racket иначе)
                  (hspace 1) (racket "Чего?")))
  (void))

 (eval:alts
  (unsyntax (elem (racket преобразовать "Привет, мир!")))
  "Привет!")
 (eval:alts
  (unsyntax (elem (racket преобразовать "Приветствую, Земля!")))
  "Чего?")
 (eval:alts
  (unsyntax (elem (racket преобразовать "Пока, друзья.")))
  "Пока!")
 (eval:alts
  (unsyntax (elem (racket преобразовать "Гы")))
  "Чего?")
 (eval:alts
  (unsyntax (elem (racket начинается?) (racketcommentfont "  -")
                  (racketcommentfont "- вне функции «преобразовать», поэтому ...")))
  (void))
 (eval:alts
  (eval:no-prompt
   (unsyntax (elem (racketerror "начинается?: не определено;
 не могу использовать идентификатор до его определения"))))
  (void))]

Возвращяесь к предыдущей теме, приведу пример определения @racket[добавлятель-суффикса] через
локальную связь.

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket добавлятель-суффикса строка2 =)
                  (linebreak) (hspace 4) (racket результат строка =)
                  (linebreak) (hspace 6) (racket строка ++ строка2)
                  (linebreak) (hspace 4) (racket результат)))
  (void))]

Ещё один способ сделать ограниченную привязку --- использование выражения @racket[пусть].

@codeblock|{
(пусть ((<имя> <выражение>) ...) <команда> ... <выражение>)
}|

Внутри этой формы после заголовка с парами имён и выражений значения выражений
связываются с соответствующими им именами. Форму @racket[пусть] можно использовать,
если нужно ввести имена внутри выражения или если выражения в заголовке
ссылаются на такие же имена вне формы @racket[пусть].

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket а = 5)))
  (void))
 (eval:alts
  (unsyntax (elem (racket пусть) (hspace 1) (racketparenfont "(")
                  (racket а (а + 2)) (racketparenfont "; ") (racket б (а - 1))
                  (racketparenfont ")")
                  (linebreak) (hspace 4) (racket список а б)))
  (list (+ 5 2) (- 5 1)))]

В выражениях @racket[(а + 2)] и @racket[(а - 1)] используется значение «а», установленное перед
формой @racket[пусть].

@section[#:tag "list"]{Списки, их перебор и рекурсия}

Адина семантически является диалектом языка Лисп. Поэтому в ней есть мощные встроенные средства
работы со списками.

Функция @racket[список] получает любое количество значений и возвращает список из этих значений.

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket список "красный" "зелёный" "синий")))
  (list "красный" "зелёный" "синий"))
 (eval:alts
  (unsyntax (elem (racket список 1 2 3 4 5)))
  (list 1 2 3 4 5))]

Как можно видеть, при выводе список выводится как апостроф, после которого идёт открывающая скобка,
значения в массиве, разделённые пробелами, и закрывающая скобка.

Для работы со списками есть множество функций и несколько операторов. Вот несколько примеров:
@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket длина $ список "раз" "два" "три") (racketcommentfont "  -")
                  (racketcommentfont "- считаем элементы")))
  3)
 (eval:alts
  (unsyntax (elem (racket элемент-списка список) (racket ("раз" "два" "три")) (hspace 1) (racket 1)
                  (racketcommentfont "  -")
                  (racketcommentfont "- получаем элемент по номеру позиции")))
  "два")
 (eval:alts
  (unsyntax (elem (racket список) (racket ("раз" "два" "три")) (racket [0])
                  (racketcommentfont "  -")
                  (racketcommentfont "- то же самое оператором")))
  "раз")
 (eval:alts
  (unsyntax (elem (racket добавить) (hspace 1) (racket список) (racket ("раз" "два"))
                  (hspace 1) (racket список) (racket ("три")) (racketcommentfont "  -")
                  (racketcommentfont "- объединяем элементы")))
  (list "раз" "два" "три"))
 (eval:alts
  (unsyntax (elem (racket список "раз" "два" ++ список "три")
                  (racketcommentfont "  -")
                  (racketcommentfont "- то же самое оператором")))
  (list "раз" "два" "три"))
 (eval:alts
  (unsyntax (elem (racket подсписок "четыре" $ список "раз" "два" "три")
                  (racketcommentfont "  -")
                  (racketcommentfont "- проверяем наоичие элемента")))
  (eval:result (racketvalfont "ложь")))]

@subsection[#:tag "listloop"]{Предопределённые циклы по спискам}

Кроме простых операций, таких как @racket[добавить], в Адине есть функции
для обработки элементов списка. Действие для обработки должно быть функцией,
поэтому здесь часто удобно применять выражение @racket[функция].

Разные функции обработки элементов по-разному комбинируют результаты обработки.
Функция @racket[отобразить] собирает результаты выполнения обработки в новый список
То есть математически отображает обрабатываемый список на новый список.

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket отобразить корень $ список 1 4 9 16)))
  '(1 2 3 4))
 (eval:alts
  (unsyntax (elem (racket отобразить)
                  (linebreak) (hspace 4) (racket функция (с) $ с ++ "!")
                  (linebreak) (hspace 4) (racket список "орехи" "печенье" "шоколад")))
  '("орехи!" "печенье!" "шоколад!"))]

Функции @racket[отобразить/и] и @racket[отобразить/или] объединяют результаты через @racket[&&]
и @racket[||] соответственно.

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket отобразить/и строка? $ список "а" "б" "в")))
  (eval:result (racketvalfont "истина")))
 (eval:alts
  (unsyntax (elem (racket отобразить/и строка? $ список "а" "б" 6)))
  (eval:result (racketvalfont "ложь")))
 (eval:alts
  (unsyntax (elem (racket отобразить/или число? $ список "а" "б" "в")))
  (eval:result (racketvalfont "истина")))]

Функции @racket[отобразить], @racket[отобразить/и] и @racket[отобразить/или] также могут работать
с несколькими списками параллельно. В этом случае списки должны иметь одинаковую длину, а функция
должна принимать по аргументу из каждого списка:

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket отобразить)
                  (linebreak) (hspace 4) (racket функция (с ч) $ подстрока с 0 ч)
                  (linebreak) (hspace 4) (racket список "орехи" "печенье" "шоколад")
                  (linebreak) (hspace 4) (racket список 4 6 3)))
  '("орех" "печень" "шок"))]

Функция @racket[отобрать] оставляет только те элементы, для которых результат функции не равен
@racketvalfont{ложь}.

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket отобрать строка? $ список "а" "б" 6)))
  '("а" "б"))
 (eval:alts
  (unsyntax (elem (racket отобрать)
                  (linebreak) (hspace 4) (racket функция (ч) $ ч > 0)
                  (linebreak) (hspace 4) (racket список 1 -2 6 7 0)))
  '(1 6 7))]

Функция @racket[свернуть] обобщает обработку списка. Она передаёт в функцию обработки
элемент и текущее значение, поэтому ей требуется дополнительный аргумент.
Начальное текущее значение должно быть передано перед списками.

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket свернуть)
                  (linebreak) (hspace 4) (racket функция (элемент текущее))
                  (linebreak) (hspace 6) (racket текущее + элемент * элемент)
                  (linebreak) (hspace 4) (racket 0)
                  (linebreak) (hspace 4) (racket '(1 2 3))))
  14)]

@subsection[#:tag "listloopscratch"]{Обход списка вручную}

Хотя @racket[отобразить] и другие функции обхода списка предопределены, они не являются
примитивами. Вы можете написать эквивалентный обход используя примитивы для работы со списками.

Так как в Адине список является односвязным списком, то базовыми операциями для непустого списка
являются:

@itemlist[(item (racket первый) ": получает первый элемент списка")
          (item (racket оставшиеся) ": получает оставшиеся элемент списка")]

@examples[#:label "Примеры:"
 (eval:alts
  (unsyntax (elem (racket первый $ список 1 2 3)))
  1)
 (eval:alts
  (unsyntax (elem (racket оставшиеся $ список 1 2 3)))
  '(2 3))]

Чтобы создать новый узел, то есть добавить элемент в голову списка, используйте функцию @racket[пара].
Чтобы получить пустой список можно использовать константу @racket[пустой-список].

@examples[#:label "Примеры:"
 (eval:alts
  (unsyntax (elem (racket пустой-список)))
  '())
 (eval:alts
  (unsyntax (elem (racket пара "голова" пустой-список)))
  '("голова"))
 (eval:alts
  (unsyntax (elem (racket пара "белая" $ пара "голова" пустой-список)))
  '("белая" "голова"))]

Также для конструирования можно использовать операцию @racket[:]. Эта операция имеет группировку
справа, то есть @racket[а : б : в] трактуется как @racket[а : (б : в)]. И если точка стоит в конце
выражения, то она трактуется как пустой список:

@examples[#:label "Примеры:"
 (eval:alts
  (unsyntax (elem (racket "голова" :) (hspace 1) (racketparenfont ".")))
  '("голова"))
 (eval:alts
  (unsyntax (elem (racket "белая" : "голова" :) (hspace 1) (racketparenfont ".")))
  '("белая" "голова"))]

Чтобы обработать список, надо иметь возможность отличать пустой список от непустого, потому
что @racket[первый] и @racket[оставшиеся] работают только с непустыми списками. Функция
@racket[пустой?] выявляет пустые списки, а @racket[пара?] непустые списки и пары,
не являющиеся списками.

@examples[#:label "Примеры:"
 (eval:alts
  (unsyntax (elem (racket пустой? пустой-список)))
  (eval:result (racketvalfont "истина")))
 (eval:alts
  (unsyntax (elem (racket пустой? $ пара "голова" пустой-список)))
  (eval:result (racketvalfont "ложь")))
 (eval:alts
  (unsyntax (elem (racket пара? пустой-список)))
  (eval:result (racketvalfont "ложь")))
 (eval:alts
  (unsyntax (elem (racket пара? $ пара "голова" пустой-список)))
  (eval:result (racketvalfont "истина")))]

При помощи этих кусочков можно написать собственные варианты функций @racket[длина],
@racket[отобразиить] и аналогичных.

@examples[#:label "Примеры:"
 (eval:alts
  (eval:no-prompt
   (unsyntax (elem (racket моя-длина сп =)
                   (linebreak) (hspace 2) (racket пустой? сп ?)
                   (linebreak) (hspace 4) (racket 0)
                   (linebreak) (hspace 4) (racket 1 + моя-длина $ оставшиеся сп))))
  (void))         
 (eval:alts
  (unsyntax (elem (racket моя-длина пустой-список)))
  0)
 (eval:alts
  (unsyntax (elem (racket моя-длина $ список "а" "б" "в")))
  3)]
@examples[#:label #f
 (eval:alts
  (eval:no-prompt
   (unsyntax (elem (racket моё-отобразить ф сп =)
                   (linebreak) (hspace 2) (racket пустой? сп ?)
                   (linebreak) (hspace 4) (racket пустой-список)
                   (linebreak) (hspace 4) (racket ф первый)
                   (racket (сп) : моё-отобразить ф $ оставшиеся сп))))
  (void))
 (eval:alts
  (unsyntax (elem (racket моё-отобразить прописные $ список "на старт" "внимание" "марш")))
  '("НА СТАРТ" "ВНИМАНИЕ" "МАРШ"))]

Алгоритмы для списочных структур удобно описывать через рекурсию как в вышеприведённых примерах.
При реализации алгоритма для списка длины Н предполагаем, что для списка длины Н-1 реализация уже
есть и описываем всего два варианта результата: значение для Н = 0 и вычисление для всех остальных,
используя описываемую функцию для хвоста списка.

@subsection[#:tag "listlooptail"]{Хвостовая рекурсия}

И @racket[моя-длина] и @racket[моё-отобразить] при работе требуют место для хранения временных
значений пропорционально длине обрабатываемого списка. Иначе говоря, использумая память О(n).

Это легко увидеть, если представить, как @racket[моя-длина $ список "а" "б" "в"] должна вычисляться:
@examples[#:label #f
 (eval:alts
  (eval:no-prompt
   (unsyntax (elem (racket моя-длина $ список "а" "б" "в")
                   (linebreak) (racket = 1 + моя-длина $ список "б" "в")
                   (linebreak) (racket = 1 + (1 + моя-длина $ список "в"))
                   (linebreak) (racket = 1 + (1 + (1 + моя-длина пустой-список)))
                   (linebreak) (racket = 1 + (1 + (1 + 0)))
                   (linebreak) (racket = 1 + (1 + 1))
                   (linebreak) (racket = 1 + 2)
                   (linebreak) (racket = 3))))
  (void))]

Для списка из n элементов вычисление будет запоминать операции сложения n раз и выполнять их
только когда список закончится.

Чтобы избежать накопления операций надо, чтобы в рекурсивном вызове результатом был вызов функции
с какими-то аргументами. Можно создать функцию, аргументами которой являются длина обработанной части
и список из оставшихся элементов.

@examples[#:label #f
 (eval:alts
  (eval:no-prompt
   (unsyntax (elem (racket моя-длина сп =)
                   (linebreak) (hspace 2) (racketcommentfont "-")
                   (racketcommentfont "- локальная функция цикл")
                   (linebreak) (hspace 2) (racket цикл сп н)
                   (linebreak) (hspace 4) (racket пустой? сп ?)
                   (linebreak) (hspace 6) (racket н)
                   (linebreak) (hspace 6) (racket цикл)
                   (linebreak) (hspace 8) (racket оставшиеся сп)
                   (linebreak) (hspace 8) (racket 1 + н)
                   (linebreak) (hspace 2)
                   (racketcommentfont "-")
                   (racketcommentfont "- тело функции моя-длина - вызов функции цикл")
                   (linebreak) (hspace 2) (racket цикл сп 0))))
  (void))]

Теперь вычисление будет выглядеть так:

@examples[#:label #f
 (eval:alts
  (eval:no-prompt
   (unsyntax (elem (racket моя-длина $ список "а" "б" "в")
                   (linebreak) (racket = цикл список) (racket ("а" "б" "в") 0)
                   (linebreak) (racket = цикл список) (racket ("б" "в") 1)
                   (linebreak) (racket = цикл список) (racket ("в") 2)
                   (linebreak) (racket = цикл пустой-список 3)
                   (linebreak) (racket = 3))))
  (void))]

Переделанная @racket[моя-длина] использует постоянный объём памяти для списков любой длины,
как видно из шагов выполнения. То есть когда результатом выполнения функции является результат
вызова другой функции (или той же с другими аргументами), то не обязательно запоминать состояние
вычисления и ждать результата от того другого вызова, можно сразу подменить вызов текущей функции.

Такое поведение при вычислении иногда называют «оптимизацией хвостовых вызовов», так как в более
примитивных языках программирования каждый вызов всё равно тратит кадр памяти, даже если результат
вызова сразу должен стать результатом вызывающей функции. Но на самом деле это не оптимизация,
а гарантия того, как будут производиться вычисления. Если точнее, то выражение в хвостовой позиции
всегда не требует дополнительного места.

В случае @racket[моё-отобразить] место для результирующего списка и место для временных данных
суммарно в любой момент времени пропорциональны длине исходного списка, поэтому смысла
как-то переписывать нет.

@subsection[#:tag "recursion"]{Рекурсия против цикла}

Вышеприведённые примеры показывают, что цикл --- это всего лишь частный случай рекурсии.
Во многих языках важно использовать форму цикла вместо рекурсии, иначе производительность будет
намного меньше и возможно переполнение стека. В Адине также иногда важно использовать хвостовую
рекурсию, чтобы избежать излишнего расходования памяти.

В то же время, в Адине рекусрия не уменьшает производительность и в ней не бывает переполнения стека.
Если вычисление требует сохранить слишком много контекста, можно исчерпать оперативную память, но
памяти доступно намного больше, чем в других языках стека. Эти соображения в сочетании с тем фактом,
что хвостовая рекурсия идентична циклу, позволяют программистам на Адине использовать рекурсивные
алгоритмы, а не избегать их.

Предположим, что надо написать функцию, удаляющую последовательные дубли из списка.
Хотя такую функцию можно написать в виде цикла, запоминая предыдущий элемент для каждой итерации,
программист на Адине скорее реализует более естественный алгоритм:

@examples[#:label #f
 (eval:alts
  (eval:no-prompt
   (unsyntax (elem (racket удалить-повторы сп =)
                   (linebreak) (hspace 2) (racket пустой? сп || пустой? оставшиеся) (racket (сп) ?)
                   (linebreak) (hspace 4) (racket сп)
                   (linebreak) (hspace 4) (racket первый сп == первый оставшиеся) (racket (сп) ?)
                   (linebreak) (hspace 6) (racket удалить-повторы $ оставшиеся сп)
                   (linebreak) (hspace 6) (racket первый сп : удалить-повторы $ оставшиеся сп))))
  (void))
 (eval:alts
  (unsyntax (elem (racket удалить-повторы $ список "а" "б" "б" "б" "в" "в")))
  '("а" "б" "в"))]

В общем, эта функция использует память пропорционально длине обрабатываемого списка.
Но это нормально, так как результат функции также пропорционален О(n). Но если обрабатываемый список
состоит большей частью из повторов, то результат будет значительно меньше и функция
@racket[удалить-повторы] также будет использовать гораздо меньше памяти! Причина в том, что когда
отбрасываются повторы, то происходит прямой вызов @racket[удалить-повторы] и работает
оптимизация хвостовых вызовов.

@examples[#:label #f
 (eval:alts
  (eval:no-prompt
   (unsyntax (elem (racket удалить-повторы $ список "а" "б" "б" "б" "б" "б")
                   (linebreak) (racket = "а" : удалить-повторы $ список "б" "б" "б" "б" "б")
                   (linebreak) (racket = "а" : удалить-повторы $ список "б" "б" "б" "б")
                   (linebreak) (racket = "а" : удалить-повторы $ список "б" "б" "б")
                   (linebreak) (racket = "а" : удалить-повторы $ список "б" "б")
                   (linebreak) (racket = "а" : удалить-повторы $ список "б")
                   (linebreak) (racket = "а" : список "б")
                   (linebreak) (racket = список "а" "б"))))
  (void))]

@section[#:tag "pairlists"]{Пары, списки и синтаксис Адины}

Функция @racket[пара] (и соответствующая операция «@racket[:]») на самом деле принимает любые два
аргумента, а не только список в качестве второго аргумента. Если второй аргумент на создан при
помощи этой функции и не является пустым списком, то результат выводится особым образом.
Два значения, объединённые при помощи функции @racket[пара] выводятся в скобках как список,
но с точкой между ними.

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket пара 1 2)))
  (cons 1 2))
 (eval:alts
  (unsyntax (elem (racket пара "мир" "дверь")))
  (cons "мир" "дверь"))]

То есть, значение возвращаемое функцией @racket[пара] не всегда список. На самом деле это
может быть произвольная пара. Функция @racket[оставшиеся] в этом случае возвращает второй
элемент пары.

@examples[#:label "Примеры:"
 (eval:alts
  (unsyntax (elem (racket первый $ пара 1 2)))
  1)
 (eval:alts
  (unsyntax (elem (racket оставшиеся $ 1 : 2)))
  2)
 (eval:alts
  (unsyntax (elem (racket пара? пустой-список)))
  (eval:result (racketvalfont "ложь")))
 (eval:alts
  (unsyntax (elem (racket пара? $ 1 : 2)))
  (eval:result (racketvalfont "истина")))
 (eval:alts
  (unsyntax (elem (racket пара? $ список 1 2 3)))
  (eval:result (racketvalfont "истина")))]

Наверное, чаще всего такие пары встречаются, когда при построении списка программист путает
аргументы местами:

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket пара список) (racket (2 3) 1)))
  (cons (list 2 3) 1))
 (eval:alts
  (unsyntax (elem (racket пара 1 список) (racket (2 3))))
  (cons 1 (list 2 3)))]

Пары, не являющиеся списками, иногда используются намеренно.
Например, функция @racket[создать-соответствие] использует список пар,
в которых первый элемент --- ключ, а второй --- значение.

Если второй элемент пара, но не список, то результат выводится так:
@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket 0 : 1 : 2)))
  (cons 0 (cons 1 2)))]

В целом, можно считать, что запись через точку используется всегда,
но если после точки идёт пара, то тогда убирается точка и та пара сразу пишется
через пробел. Таким образом, @racket['(0 . (1 . 2))] сокращается до @racket['(0 1 . 2)],
а @racket['(1 . (2 . (3 . ())))] сокращается до @racket['(1 2 3)].

@subsection[#:tag "quoting"]{Буквальный вывод пар и символов формой @racket[буквально]}

Списки выводятся с апострофом перед ними, но если элемент списка тоже список, то
апострофа перед ним нет.

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket список список) (racket (1) список) (racket (2 3) список) (racket (4))))
  '((1) (2 3) (4)))]

Форма @racket[буквально] позволяет писать списки таким же образом

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket буквально "красный" "зелёный" "синий")))
  '("красный" "зелёный" "синий"))
 (eval:alts
  (unsyntax (elem (racket буквально (1) (2 3) (4))))
  '((1) (2 3) (4)))
 (eval:alts
  (unsyntax (elem (racket буквально ())))
  '())]

Эта форма также позволяет писать через точку:

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket буквально 1) (racketparenfont " . ") (racket 2)))
  '(1 . 2))
 (eval:alts
  (unsyntax (elem (racket буквально 0 1) (racketparenfont " . ") (racket 2)))
  '(0 1 . 2))]

Разумеется, можно вкладывать списки любого вида:
@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket список список) (racket (1 2 3) 5 список) (racket ("a" "b" "c"))))
  '((1 2 3) 5 ("a" "b" "c")))
 (eval:alts
  (unsyntax (elem (racket буквально (1 2 3) 5 ("a" "b" "c"))))
  '((1 2 3) 5 ("a" "b" "c")))]

Если в форму @racket[буквально] передать идентификатор, то будет выведено нечто, выглядящее как
идентификатор с апострофом перед ним.

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket буквально иван-иванович)))
  'иван-иванович)]

Такое значение называется @racket[символ]. Чтобы не путать с теми буквами и цифрами, из которых
состоят строки и которые тоже иногда называют символами, содержимое строк будем называть
только литерами.

Также не следует путать символы и идентификаторы. Символ @racket['отобразить] не имеет отношения
к идентификатору @racket[отобразить] за исключением того, что они оба состоят из одинаковых литер.

Фактически, символ хранит только строку со своим именем. В этом смысле символы и строки отличаются
только тем, как они выводятся. Функции @racket[символ->строка] и @racket[строка->символ] преобразуют
их друг в друга.

@examples[#:label "Примеры:"
 (eval:alts
  (unsyntax (elem (racket отобразить)))
  (eval:result (racket отобразить)))
 (eval:alts
  (unsyntax (elem (racket буквально отобразить)))
  'отобразить)
 (eval:alts
  (unsyntax (elem (racket символ? $ буквально отобразить)))
  (eval:result (racketvalfont "истина")))
 (eval:alts
  (unsyntax (elem (racket символ? отобразить)))
  (eval:result (racketvalfont "ложь")))
 (eval:alts
  (unsyntax (elem (racket функция? отобразить)))
  (eval:result (racketvalfont "истина")))
 (eval:alts
  (unsyntax (elem (racket строка->символ "отобразить")))
  'отобразить)
 (eval:alts
  (unsyntax (elem (racket символ->строка $ буквально отобразить)))
  "отобразить")]

Так же, как форма @racket[буквально] для списков автоматически применяется для вложенных списков,
также она автоматически применяется для идентификаторов в списках и возвращает соответствующие
им символы.

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket первый $ буквально (имя фамилия))))
  'имя)
 (eval:alts
  (unsyntax (elem (racket символ? $ первый $ буквально (имя фамилия))))
  (eval:result (racketvalfont "истина")))]

При выводе, когда символ внутри списка, который выводится с апострофом, апостроф перед символом не
выводится, так как апостроф перед списком уже указывает, что все имена в списке являются символами.

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket буквально (имя фамилия))))
  '(имя фамилия))]

Форма @racket[буквально] не оказывает влияния на уже буквальные выражения, например, числа и строки:
@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket буквально 42)))
  42)
 (eval:alts
  (unsyntax (elem (racket буквально "для записи")))
  "для записи")]

@subsection[#:tag "quoting2"]{Сокращение @racket[буквально] до апострофа}

Как уже возможно стало понятно, можно сократить форму @racket[буквально], просто ставя вместо неё
апостроф.

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket '(1 2 3))))
  '(1 2 3))
 (eval:alts
  (unsyntax (elem (make-element value-color '("' ")) (racket 1 2 3)))
  '(1 2 3))
 'имя
 '((1 2 3) имя ("а" "б" "в"))]

В документации апостроф с трактуемыми буквально значениями отображается зелёным цветом,
чтобы показать, что это константа.

Апостроф преобразуется в @racket[буквально] простой подстановкой:
@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket первый ''имя)))
  'буквально)
 (eval:alts
  (unsyntax (elem (racket первый '(буквально имя))))
  'буквально)]

При выводе аналогично. Если печататель видит символ @racket['буквально] как первый элемент
двухэлементного списка, то он вместо этого печатает апостроф:

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket буквально буквально имя)))
  '(буквально имя))
 (eval:alts
  (unsyntax (elem (racket '(буквально имя))))
  '(буквально имя))
 (eval:alts
  (unsyntax (elem (racket ''имя)))
  '(буквально имя))]

@subsection[#:tag "syntax"]{Списки и синтаксис Адины}

Синтаксис Адины не определяется напрямую в терминах потоков литер. Вместо этого
синтаксис определяется двумя слоями:

@itemlist[(list @item{слой читателя, который превращает литеры в списки, символы и другие константы.}
@item{слой раскрывателя, который преобразовывает получившиеся списки и константы в выражения.})]

Правила для чтения списков и выражений одинаковы. В частности, это позволяет использовать запись
через точку при записи выражений:
@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket + 1) (racketparenfont " . ") (racket (2))))
  3)]

Это работает, так как «+ 1 . (2)» всего лишь другой метод для записи «+ 1 . 2».

Операторы также обрабатываются на стадии чтения с учётом приоритетов. Результат чтения можно увидеть
при помощи @racket[буквально].

@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket буквально 2 + 2 * 2)))
  '(+ 2 (* 2 2)))]

Это можно использовать, если есть сомнения в приоритете операторов или понимании сложной конструкции.

@section[#:tag "gnetwork"]{Сеть}

В Адине три вида соединений. Нужный модуль подключают командой
@racket[используется]. Функции описаны в @seclink["network"]{справочнике}.

@defterm{Ненадёжное соединение} --- модуль
@racketidfont{ненадёжное-соединение}. Соответствует протоколу UDP:
обмен идёт отдельными пакетами, доставка и порядок не
гарантируются.

@defterm{Надёжное соединение} --- модуль
@racketidfont{надёжное-соединение}. Соответствует протоколу TCP:
это поток с гарантией доставки и порядка. Клиент подключается к
узлу, сервер ждёт входящие подключения; обе стороны получают порты
ввода и вывода.

@defterm{Постоянное соединение} --- модуль
@racketidfont{постоянное-соединение}. Строится поверх надёжного.
Позволяет передавать сериализуемые значения Адины и автоматически
восстанавливает связь, если она оборвётся. Подходят строки, числа,
списки, массивы и другие данные, которые @racket[написать] выводит
в читаемом обратно виде. Функции, объекты и @racketvalfont{пусто}
так передать нельзя.



