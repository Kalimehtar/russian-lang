#lang scribble/manual

@(require 1/lang scribble/example scribble/core scribble/racket
          (for-label 1/all-base))

@title{Русский язык программирования Ади́на}
@author[(author+email "Клочков Роман" "kalimehtar@mail.ru")]

Документация основана на @other-doc['(lib "scribblings/guide/guide.scrbl")].

@defmodulelang["1" #:module-path 1/all-base #:packages ("russian-lang")]

Это руководство описывает русскоязычный язык программирования, основанный
на идеях из расширения синтаксиса Scheme @hyperlink["http://www.dwheeler.com/readable/"]{readable}.

Название Ади́на взято из названия симпатичного
@hyperlink["https://www.plantarium.ru/page/view/item/67917.html"]{кустарника} и похоже
на название цифры 1. Поэтому в дальнейшем для идентификации имени языка и расширения файлов
на этом языке используется эта цифра. Использование кириллического имени не поддерживается в Racket,
а использование иностранных слов неудобно, если вся остальная программа на русском.

Семантика языка на данный момент полностью унаследована от Racket,
обеспечивая полную совместимость: из этого
языка можно вызывать любые функции и синтаксические конструкции Racket,
а из Racket можно вызывать модули Адины.

Для установки скачайте дистрибутив Racket с
@hyperlink["https://download.racket-lang.org/"]{сайта}. Затем запустите DrRacket, в меню Файл
выберите "Install Package..." и в открывшемся окне введите "russian-lang".

Для включения синтаксиса данного языка просто укажите в модуле Racket в первой строке

@nested[#:style 'code-inset]{
  #lang 1
}

или

@codeblock|{
  #!1
}|

Второй вариант рекомендуется при использовании русского языка для написания программы.

Можно использовать англоязычный вариант, написав во второй строке
@nested[#:style 'code-inset]{
  english()
}

Тогда доступны все команды Racket, но по-прежнему работают отступы и операторы.

@section[#:tag "essentials"]{Основы языка}

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

@subsection[#:tag "simple values"]{Простые значения}

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

@subsection[#:tag "expressions"]{Выражения}

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

@subsection[#:tag "basic definitions"]{Основы определений}

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

@subsection[#:tag "identifiers"]{Идентификаторы}

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

@subsection[#:tag "function call"]{Вызовы функций}

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

@subsection[#:tag "conditionals expressions"]{Условные конструкции с @racket[если] и
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

@subsection[#:tag "function call2"]{Вызовы функций, снова}

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

@subsection[#:tag "lambda"]{Безымянные функции}

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

@subsection[#:tag "let"]{Локальное связывание внутри функций и через выражение @racket[пусть]}

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

@subsection[#:tag "list"]{Списки, их перебор и рекурсия}

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

@subsubsection[#:tag "listloop"]{Предопределённые циклы по спискам}

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

@subsubsection[#:tag "listloopscratch"]{Обход списка вручную}

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

@subsubsection[#:tag "listlooptail"]{Хвостовая рекурсия}

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

@subsubsection[#:tag "recursion"]{Рекурсия против цикла}

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

@subsection[#:tag "pairlists"]{Пары, списки и синтаксис Адины}

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

@subsubsection[#:tag "quoting"]{Буквальный вывод пар и символов формой @racket[буквально]}

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

@subsubsection[#:tag "quoting2"]{Сокращение @racket[буквально] до апострофа}

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

@subsubsection[#:tag "syntax"]{Списки и синтаксис Адины}

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

@section[#:tag "builtin"]{Встроенные типы данных}

В предыдущей главе были рассмотрены некоторые встроенноые типы Адины: числа, логические, строки,
списки и функции. Этот раздел предоставляет более полное описание встроенных типов
для простых форм данных.

@subsection[#:tag "glogicals"]{Логические выражения}

В Адине есть две константы для представления логических (булевых) значений: @racketvalfont{истина} и
@racketvalfont{ложь}. Функция @racket[булево?] распознаёт эти две константы. Но при использовании
в @racket[если], @racket[?], @racket[&&], @racket[||], ... любое значение кроме
@racketvalfont{ложь} трактуется как истинное.

@examples[#:label "Примеры:"
 (eval:alts
  (unsyntax (elem (racket 1 + 1 == 2)))
  (eval:result (racketvalfont "истина")))
 (eval:alts
  (unsyntax (elem (racket булево?) (racketvalfont " истина")))
  (eval:result (racketvalfont "истина")))
 (eval:alts
  (unsyntax (elem (racket булево?) (racketvalfont " ложь")))
  (eval:result (racketvalfont "истина")))
 (eval:alts
  (unsyntax (elem (racket булево? "нет")))
  (eval:result (racketvalfont "ложь")))
 (eval:alts
  (unsyntax (elem (racket "нет" ? 1 0)))
  1)]

@subsection[#:tag "gnumbers"]{Числа}

Числа в Адине бывают точные и неточные.

@itemlist[
 (list
  @item{К точным числам относятся
  @itemlist[
   (list
     @item{целые числа любой длины, такие как @racketvalfont{5},
    @racketvalfont{99999999999999999} или @racketvalfont{-17};}
     @item{рациональные числа, являющиеся дробью с целыми числителем и знаменателем, например,
    @racketvalfont{1/2}, @racketvalfont{99999999999999999/2} или @racketvalfont{-3/4};}
     @item{комплексные числа с точными вещественной и мнимой частью, такие как @racketvalfont{1+2i}
    или @racketvalfont{1/2+3/4i}.})]}
  @item{К неточным числам относятся
   @itemlist[
   (list
     @item{вещественные числа в формате IEEE, такие как @racketvalfont{2.0} и
    @racketvalfont{3.14e+87}, где бесконечности и не числа
    записываются как @racketvalfont{+inf.0}, @racketvalfont{-inf.0}, @racketvalfont{+nan.0}
    и @racketvalfont{-nan.0};}
     @item{комплексные числа с неточной вещественной или мнимой частью, такие как
    @racketvalfont{2.0+3.0i}
    или @racketvalfont{-inf.0+nan.0i.}})]})]

Неточные числа выводятся с десятичной точкой или показателем экспоненты, а точные числа выводятся
как целые числа или простые дроби. Такое же соглашение используется при чтении, но если необходимо,
перед числом можно написать (латинскую) @litchar{#e}, тогда число будет прочитано как точное или
@litchar{#i} --- тогда оно будет прочитано как неточное. Префиксы (латинскими)
@litchar{#b}, @litchar{#o} и @litchar{#x} позволяют вводить числа в двоичной, восьмеричной и
шестнадцатеричной системах счисления.

@examples[#:label "Примеры:"
 0.5
 (eval:alts
  (unsyntax (elem (racketvalfont "#e0.5")))
  #e0.5)
 (eval:alts
  (unsyntax (elem (racketvalfont "#x03BB")))
  #x03BB)]

Вычисление, включающее неточные числа (кроме логических операций), возвращает неточный результат,
так что неточность действует на числа в каком-то смысле как зараза.
Процедуры @racket[точное->неточное] и @racket[неточное->точное] позволяют преобразовывать
точные и неточные числа друг в друга.

@examples[#:label "Примеры:"
 0.5
 (eval:alts
  (unsyntax (elem (racket 1 / 2)))
  1/2)
 (eval:alts
  (unsyntax (elem (racket 1 / 2.0)))
  0.5)
 (eval:alts
  (unsyntax (elem (racket 3.0 == 2.999 ? 1 2)))
  2)
 (eval:alts
  (unsyntax (elem (racket неточное->точное 0.1)))
  (inexact->exact 0.1))]

Неточные числа также возвращаются такими функциями как @racket[корень], @racket[логарифм]
и @racket[синус], если точный результат не может быть представлен рациональным числом.
@examples[#:label "Примеры:"
 (eval:alts
  (unsyntax (elem (racket синус 0) (hspace 4) (racketcommentfont "-")
                  (racketcommentfont "- рациональный результат")))
  0)
 (eval:alts
  (unsyntax (elem (racket синус 1/2) (hspace 2) (racketcommentfont "-")
                  (racketcommentfont "- иррациональный результат")))
  (sin 1/2))]

Вычисления с небольшими целыми числами производятся быстрее. Под небольшими подразумеваются числа,
занимающие на несколько бит меньше, чем машинное представление знаковых чисел. Например, для 64-битной
системы таковыми являются числа @racketvalfont{-1152921504606846976..1152921504606846975},
но конкретные границы могут зависеть от используемой платформы. Для конкретного компьютера
можно проверять при помощи функции @racket[небольшое-число?].

Вычисления с большими целыми числами или с точными нецелыми числами медленнее, чем вычисления
с неточными числами.

@examples[#:label #f
 (eval:alts
  (eval:no-prompt
   (unsyntax (elem (racket сумма ф а б =)
                   (linebreak) (hspace 2) (racket а == б ? 0)
                   (linebreak) (hspace 4) (racket ф а + сумма ф (а + 1) б))))
  (void))
 (eval:alts
  (unsyntax (elem (racket замерить-время $ округлить $ сумма (функция (ч) $ 1.0 / ч) 1 2000)
                  (linebreak) (racketoutput "время процессора: 24 реальное: 12 сборки мусора: 0")))
  (void))
 (eval:alts
  (unsyntax (elem (racket замерить-время $ округлить $ сумма (функция (ч) $ 1 / ч) 1 2000)
                  (linebreak) (racketoutput "время процессора: 0 реальное: 0 сборки мусора: 0")))
  (void))]

Можно определить отношение числа к множествам целых, рациональных, вещественных и комплексных
при помощи функций @racket[целое?], @racket[рациональное?], @racket[вещественное?]
и @racket[комплексное?]. Некоторые математические функции работают только с вещественными числами,
но большинство реализует стандартные расширения на комплексные числа.

@examples[#:label "Примеры:"
  (eval:alts
   (unsyntax (elem (racket целое? 5)))
   (eval:result (racketvalfont "истина")))
  (eval:alts
   (unsyntax (elem (racket комплексное? 5)))
   (eval:result (racketvalfont "истина")))
  (eval:alts
   (unsyntax (elem (racket целое? 5.0)))
   (eval:result (racketvalfont "истина")))
  (eval:alts
   (unsyntax (elem (racket целое? 1+2i)))
   (eval:result (racketvalfont "ложь")))
  (eval:alts
   (unsyntax (elem (racket комплексное? 1+2i)))
   (eval:result (racketvalfont "истина")))
  (eval:alts
   (unsyntax (elem (racket комплексное? 1.0+2.0i)))
   (eval:result (racketvalfont "истина")))
  (eval:alts
   (unsyntax (elem (racket абс -5)))
   5)   
  (eval:alts
   (unsyntax (elem (racket абс -5+2i)))
   (void))
  (eval:alts
   (eval:no-prompt
    (unsyntax (elem (racketerror "абс: нарушение контракта
  ожидалось: вещественное?
  получено: -5+2i"))))
   (void))
  (eval:alts
   (unsyntax (elem (racket синус -5+2i)))
   3.6076607742131563+1.0288031496599335i)]

Операторы @racket[===] и @racket[==] сравнивают числа с учётом того, точное ли число

@examples[#:label "Примеры:"
  (eval:alts
   (unsyntax (elem (racket 1 === 1.0)))
   (eval:result (racketvalfont "ложь")))
  (eval:alts
   (unsyntax (elem (racket 1 == 1.0)))
   (eval:result (racketvalfont "ложь")))
  (eval:alts
   (unsyntax (elem (racket 1 >= 1.0 && 1 <= 1.0)))
   (eval:result (racketvalfont "истина")))]

Сравнение неточных чисел может приводить к неожиданным результатам. Даже достаточно простые неточные
числа могут обозначать не то, что можно было бы подумать. Например, формат IEEE, будучи основанным
на степенях двойки, может представить @racketvalfont{1/2} точно, но @racketvalfont{1/10} только
приближённо.

@examples[#:label "Примеры:"
  (eval:alts
   (unsyntax (elem (racket 1/2 >= 0.5 && 0.5 <= 1/2)))
   (eval:result (racketvalfont "истина")))
  (eval:alts
   (unsyntax (elem (racket 1/10 >= 0.1)))
   (eval:result (racketvalfont "ложь")))          
  (eval:alts
   (unsyntax (elem (racket неточное->точное 0.1)))
   (inexact->exact 0.1))]

@subsection[#:tag "gcharacters"]{Литеры}

Литеры Адины соответствуют @italic{кодам Юникода}. Код Юникода можно
трактовать как беззнаковое целое число, которое можно отобразить в 21 бит и которое
соответствует символу естественного языка или части символа. Технически, код является
более простым понятием, чем то, что в стандарте Юникода называется символом, но его достаточно
в большинстве случаев. Например, любую акцентированную латинскую букву, любую кириллическую букву или
любой обычный китайский иероглиф можно представить в виде кода.

Несмотря на то, что каждая литера Адины соответствует числу, литеральный тип отделён от числового.
Функции @racket[литера->число] и @racket[число->литера] позволяют преобразовывать целые числа
и соответствующие литеры друг в друга.

Печатные литеры обычно выводятся как @litchar{#\} и отображаемая литера. Непечатные обычно выводятся
как @litchar{#\u} и код литеры в виде шестнадцатеричного числа. Некоторые литеры печатаются особым
образом: например, пробел и перенос строки выводятся как @racketvalfont{#\пробел} и
@racketvalfont{#\перенос}.

@examples[#:label "Примеры:"
  (eval:alts
   (unsyntax (elem (racket число->литера 1025)))
   #\Ё)
  (eval:alts
   (unsyntax (elem (racket литера->число #\Ё)))
   1025)
  #\λ
  (eval:alts
   (unsyntax (elem (racket число->литера 17)))
   #\u0011)
  (eval:alts
   (unsyntax (elem (racket литера->число) (hspace 1) (racketvalfont "#\\пробел")))
   32)]

Функция @racket[вывести] прямо пишет переданную литеру в текущий порт вывода, а не использует
синтаксис для вывода литерных констант.

@examples[#:label "Примеры:"
  #\Ё
  (eval:alts
   (unsyntax (elem (racket вывести #\Ё)))
   "Ё")]

@subsection[#:tag "gstrings"]{Строки}

Строка --- это массив литер фиксированной длины. Она выводится при помощи двойных
кавычек (знаков дюйма). Если в строке встречается двойная кавычка, она выводится как
@litchar{\"}, если встречается обратная косая черта, то @litchar{\\}. Также, при помощи
обратной косой черты выводятся пробельные литеры: @litchar{\n} --- перенос строки,
@litchar{\r} --- возврат каретки. Большинство непечатных литер выводится
как @litchar{\u} и четырёхзначный шестнадцатеричный номер литеры.

Функция @racket[вывести] прямо пишет литеры строк, не используя синтаксис из предыдущего абзаца.

@examples[#:label "Примеры:"
  "Пример"
  (eval:alts
   (unsyntax (elem (racketvalfont "\"\\u03BB\"")))
   "\u03BB")
  (eval:alts
   (unsyntax (elem (racket вывести "Пример")
                   (linebreak) (racketoutput "Пример")))

   (void))
  (eval:alts
   (unsyntax (elem (racket вывести "Пример с \"кавычками\"")
                   (linebreak) (racketoutput "Пример с \"кавычками\"")))
   (void))
  (eval:alts
   (unsyntax (elem (racket вывести "две\nстроки") (linebreak) (racketoutput "две\nстроки")))
   (void))
  (eval:alts
   (unsyntax (elem (racket вывести) (hspace 1) (racketvalfont "\"\\u03BB\"")
                   (linebreak) (racketoutput "\u03BB")))
   (void))]

Строка может быть изменяемой или неизменяемой. Строки, введённые в тексте программы,
являются неизменяемыми. Строки, полученные из функций, обычно изменяемые. Функция
@racket[новая-строка] создаёт изменяемую строку заданной длины и, при необходимости,
заполняет её указанной литерой. Функция @racket[элемент-строки] получает литеру на
указанной позиции (нумерация начинается с нуля). Функция @racket[установить-элемент-строки!]
изменяет литеру в изменяемой строке. Вместо последних двух функций можно использовать
синтаксис с квадратными скобками.

@examples[#:label "Примеры:"
  (eval:alts
   (unsyntax (elem (racket элемент-строки "Эльбрус" 0)))
   #\Э)
  (eval:alts
   (unsyntax (elem (racket "Эльбрус")
                   (racketparenfont "[") (racketvalfont "0") (racketparenfont "]")))
   #\Э)
  (eval:alts
   (unsyntax (elem (racket с = новая-строка 5 #\.)))
   (void))
  (eval:alts
   (unsyntax (elem (racket с)))
   ".....")
  (eval:alts
   (unsyntax (elem (racket установить-элемент-строки! с 2 #\λ)))
   (void))
  (eval:alts
   (unsyntax (elem (racket с)))
   "..λ..")
  (eval:alts
   (unsyntax (elem (racket с) (racketparenfont "[") (racketvalfont "0") (racketparenfont "] ")
                   (racket := #\ё)))
   #\ё)
  (eval:alts
   (unsyntax (elem (racket с)))
   "ё.λ..")]

Упорядочивание строк и операции с регистром литер обычно не зависят от региональных настроек
пользователя, то есть они работают для всех пользователей одинаково. Но также предоставлены
функции для смены регистра и упорядочивания в зависимости от местонахождения (региональных настроек)
пользователя. Сортируйте строки при помощи @racket[строки-возрастают?] или
@racket[строки-возрастают?/без-регистра], чтобы результат был одинаков на всех компьютерах, но
используйте @racket[строки-возрастают?/местные] или
@racket[строки-возрастают?/местные/без-регистра], если результат необходим исключительно для
упорядочивания для конечного пользователя.

Например, в Unicode «Ё» раньше, чем «Б», а в русском алфавите, наоборот:

@examples[#:label "Примеры:"
  (eval:alts
   (unsyntax (elem (racket строки-возрастают? "Ёж" "Белка")))
   (eval:result (racketvalfont "истина")))
  (eval:alts
   (unsyntax (elem (racket строки-возрастают?/местные "Ёж" "Белка")))
   (eval:result (racketvalfont "ложь")))
  (eval:alts
   (unsyntax (elem
              (racket параметризуя)
              (linebreak) (hspace 2)
              (racket $ текущее-место) (hspace 1) (racketvalfont "ложь")
              (linebreak) (hspace 2)
              (racket строки-возрастают?/местные "Ёж" "Белка")))
   (eval:result (racketvalfont "истина")))]

Функция @racket[разделить-по-регулярному-выражению] разбивает строку на части по
вхождениям шаблона регулярного выражения. В качестве шаблона можно передать значение,
полученное функцией @racket[регулярное-выражение], или литерал @racket[#rx"..."].

@examples[#:label "Примеры:"
  (eval:alts
   (unsyntax (elem (racket разделить-по-регулярному-выражению #rx"," "a,b,c")))
   (list "a" "b" "c"))]

Для работы с представлением строки в виде байтов нужно использовать байтовые строки.

@subsection[#:tag "gbytes"]{Байты и байтовые строки}

Байт --- это точное целое число с 0 по 255. Предикат (то есть функция проверяющая условие)
@racket[байт?] позволяет определить, является ли значение байтом. 

@examples[#:label "Примеры:"
  (eval:alts
   (unsyntax (elem (racket байт? 0)))
   (eval:result (racketvalfont "истина")))
  (eval:alts
   (unsyntax (elem (racket байт? 256)))
   (eval:result (racketvalfont "ложь")))]

Байтовая строка --- это массив байтов фиксированной длины. Работа с ней аналогична работе со строкой,
но вместо литер в байтовой строке хранятся байты. При выводе байтовой строки байты от 32 по 126
выводятся как литеры с этими номерами, но перед кавычкой и обратной чертой как и при выводе строк
выводится обратная черта. Байты с 7 по 13 и 33 выводятся по их традиционным именам:
@litchar{#\a\b\t\n\v\f\r\e}.
Остальные выводятся в виде обратной черты и числа в восьмеричной кодировке.

@examples[#:label "Примеры:"
  #"Elbrus"
  (eval:alts
   (unsyntax (elem (racket элемент-байтов #"Elbrus" 0)))
   (bytes-ref #"Elbrus" 0))
  (eval:alts
   (unsyntax (elem (racket #"Elbrus") (racketparenfont "[") (racketvalfont "0")
                   (racketparenfont "]")))
   (bytes-ref #"Elbrus" 0))
  (eval:alts
   (unsyntax (elem (racket новые-байты 3 65)))
   (make-bytes 3 65))
  (eval:alts
   (unsyntax (elem (racket а = новые-байты 2)))
   (void))
  (eval:alts
   (unsyntax (elem (racket а)))
   (make-bytes 2))
  (eval:alts
   (unsyntax (elem (racket установить-элемент-байтов! а 0 1)))
   (void))                
  (eval:alts
   (unsyntax (elem (racket а) (racketparenfont "[") (racketvalfont "1")
                   (racketparenfont "] ") (racket :=) (racketvalfont " #o377")))
   #o377)
  (eval:alts
   (unsyntax (elem (racket а)))
   (bytes 1 #o377))]

Функция @racket[вывести] выводит байтовую строку как поток байтов в порт вывода.
Технически, вывод обычной (литерной) строки сводится к переводу строки в байты
в кодировке UTF-8 и выводу полученных байтов в порт вывода, так как операционная
система понимает вывод только в байтах.

@examples[#:label "Примеры:"
  (eval:alts
   (unsyntax (elem (racket вывести #"Elbrus")
                   (linebreak) (racketoutput "Elbrus")))
   (void))
  (eval:alts
   (unsyntax (elem (racket вывести #"\316\273") (hspace 2) (racketcommentfont "-")
                   (racketcommentfont "- λ в кодировке UTF-8")
                   (linebreak) (racketoutput "λ")))
   (void))]

Для явного преобразования между строками и байтами Адина поддерживает UTF-8 и местную
кодировку операционной системы. Также есть функции для преобразования между произвольными
кодировками.

@examples[#:label "Примеры:"
  (eval:alts
   (unsyntax (elem (racket байты->строка #"\316\273")))
   "λ")
  (eval:alts
   (unsyntax (elem
              (racket параметризуя)
              (linebreak) (hspace 2)
              (racket $ текущее-место "C")
              (linebreak) (hspace 2)
              (racket байты->строка/местные #"\316\273") (racketcommentfont "-")
              (racketcommentfont "- кодировка C понимает только байты от 0 по 127")))
   (void))
  (eval:alts
   (eval:no-prompt
    (unsyntax (elem (racketerror "байты->строка/местные: строка байтов не является правильной \
в местной кодировке
  строка байтов: #\"\\316\\273\""))))
   (void))]

@subsection[#:tag "gsymbols"]{Символы}

Символ --- это атомарное значение, которое выводится в виде идентификатора с апострофом перед ним.
При вводе также: выражение из апострофа и идентификатора является символом.

@examples[#:label "Примеры:"
   'а
   (eval:alts
    (unsyntax (elem (racket символ? 'а)))
    (eval:result (racketvalfont "истина")))]

Для любой последовательности литер существует только один символ.
Вызов функции @racket[строка->символ] или чтение идентификатора в тексте
программы регистрирует символ, таким образом дальнейшее сравнение прочитанных символов
работает очень быстро. Поэтому рекомендуется использовать символы для значений перечислений:
строки долго сравниваются, а числа не очевидно, что обозначают.

@examples[#:label "Примеры:"
   (eval:alts
    (unsyntax (elem (racket 'а === 'а)))
    (eval:result (racketvalfont "истина")))
   (eval:alts
    (unsyntax (elem (racket 'а === строка->символ "а")))
    (eval:result (racketvalfont "истина")))
   (eval:alts
    (unsyntax (elem (racket 'а === 'б)))
    (eval:result (racketvalfont "ложь")))
   (eval:alts
    (unsyntax (elem (racket 'а === 'А)))
    (eval:result (racketvalfont "ложь")))]

При вводе и выводе идентификаторов с символами

@litchar{(} @litchar{)} @litchar{[} @litchar{]} @litchar["{"] @litchar["}"] @litchar{"} @litchar{,}
       @litchar{'} @litchar{`} @litchar{;} @litchar{#} @litchar{|} @litchar{\}

может использоваться экранирование при помощи @litchar{|} и @litchar{\}.

@examples[#:label "Примеры:"
   (eval:alts
    (unsyntax (elem (racket строка->символ "один, два")))
    '|один, два|)
   (eval:alts
    (unsyntax (elem (racket строка->символ "6")))
    '|6|)]

Функция @racket[написать] выводит символ без апострофа. Функция @racket[вывести]
выводит имя символа как строку.

@examples[#:label "Примеры:"
   (eval:alts
    (unsyntax (elem (racket написать 'символ)
                    (linebreak) (racketoutput "символ")))
    (void))
   (eval:alts
    (unsyntax (elem (racket вывести 'символ)
                    (linebreak) (racketoutput "символ")))
    (void))
   (eval:alts
    (unsyntax (elem (racket написать '|6|)
                    (linebreak) (racketoutput "|6|")))
    (void))
   (eval:alts
    (unsyntax (elem (racket вывести '|6|)
                    (linebreak) (racketoutput "6")))
    (void))]

Функция @racket[новый-символ] создаёт новый, ничему другому не равный символ. Её можно использовать
для генерации значений, которые не могут встретиться в переданных данных.

@subsection[#:tag "gkeywords"]{Ключевые слова}

Ключевые слова похожи на символы и при вводе/выводе выглядят почти как символы из идентификаторов,
начинающихся на @litchar{#:}.

@examples[#:label "Примеры:"
   (eval:alts
    (unsyntax (elem (racket строка->ключевое-слово "тыква")))
    '#:тыква)
   '#:тыква
   (eval:alts
    (unsyntax (elem (racket '#:тыква === строка->ключевое-слово "тыква")))
    (eval:result (racketvalfont "истина")))]

Но символами не являются и не могут именовать переменные:

@examples[#:label "Примеры:"
   (eval:alts
    (unsyntax (elem (racket символ? '#:тыква)))
    (eval:result (racketvalfont "ложь")))
   (eval:alts
    (unsyntax (elem (racket ключевое-слово? '#:тыква)))
    (eval:result (racketvalfont "истина")))]
   
Используются ключевые слова при работе с именованными параметрами функций и макросов.

@examples[#:label "Примеры:"
   (eval:alts
    (unsyntax (elem (racket каталог = системный-путь 'временный-каталог)
                    (racketcommentfont "-")
                    (racketcommentfont "- здесь символ")))
    (void))
   (eval:alts
    (unsyntax (elem (racket записывая-файл построить-путь(каталог "что-то.txt"))
                    (linebreak)
                    (hspace 4) (racket функция () $ вывести "пример")
                    (linebreak)
                    (hspace 4) (racketcommentfont "-")
                    (racketcommentfont "- именованный аргумент #:если-существует ")
                    (racketcommentfont "может быть 'заменить, 'обрезать, ...")
                    (linebreak) (hspace 4) (racket #:если-существует 'заменить)))
    (void))]

В описании синтаксиса ключевые слова для краткости называются ключами.

@subsection[#:tag "glists"]{Пары и списки}

Пара объединяет два произвольных значения. Функция @racket[пара] позволяет
создавать пары. Функции @racket[первый] и @racket[оставшиеся] позволяют
получать первый и второй элемент из пары, а функция @racket[пара?] распознаёт
пары. Для удобства записи вместо функции @racket[пара] можно использовать оператор
@racket[:]. Этот оператор имеет правую ассоциативность, то есть 1 : 2 : 3 трактуется как 1 : (2 : 3).
Это позволяет с его помощью описывать списки без лишних скобок.

Пара обычно выводится как апостроф @litchar["'"], после которого в скобках выводятся значения
элементов пары, разделённые точкой.

@examples[#:label "Примеры:"
   (eval:alts
    (unsyntax (elem (racket 1 : 2)))
    '(1 . 2))
   (eval:alts
    (unsyntax (elem (racket (1 : 2) : 3)))
    '((1 . 2) . 3))
   (eval:alts
    (unsyntax (elem (racket первый $ 1 : 2)))
    1)
   (eval:alts
    (unsyntax (elem (racket оставшиеся $ 1 : 2)))
    2)
   (eval:alts
    (unsyntax (elem (racket пара? $ 1 : 2)))
    (eval:result (racketvalfont "истина")))]   

Как правило, из пар составляется список. Тогда в первый элемент кладётся значение
первого элемента списка, а во второй --- список из оставшихся элементов. Список может быть либо
парой, либо специальным значением @racket[пустой-список], представляющим пустой список.

Для его формирования можно использовать функцию @racket[список] или конструкцию
@codeblock[#:keep-lang-line? #f]|{
#!1
значение1 : значение2 : последнее-значение : .
}|

Эта конструкция аналогична
@codeblock[#:keep-lang-line? #f]|{
#!1
список значение1 значение2 последнее-значение
}|

Список обычно выводится как апостроф @litchar["'"], после которого в скобках выводятся значения
элементов списка.

@examples[#:label "Примеры:"
   (eval:alts
    (unsyntax (elem (racket пустой-список)))
    '())
   (eval:alts
    (unsyntax (elem (racket 0 : 1 : 2 :) (hspace 1) (racketvalfont ".")))
    '(0 1 2))
   (eval:alts
    (unsyntax (elem (racket список? пустой-список)))
    (eval:result (racketvalfont "истина")))
   (eval:alts
    (unsyntax (elem (racket список? $ 1 : 2 :) (hspace 1) (racketvalfont ".")))
    (eval:result (racketvalfont "истина")))
   (eval:alts
    (unsyntax (elem (racket список? $ 1 : 2)))
    (eval:result (racketvalfont "ложь")))]

Функции @racket[написать] и @racket[вывести] печатают пары и списки без начального апострофа.
Вывод этих функций отличается только тем, как они выводят элементы списков.

@examples[#:label "Примеры:"
   (eval:alts
    (unsyntax (elem (racket написать $ 1 : 2)
                    (linebreak) (racketoutput "'(1 . 2)")))
    (void))
   (eval:alts
    (unsyntax (elem (racket вывести $ 1 : 2)
                    (linebreak) (racketoutput "'(1 . 2)")))
    (void))
   (eval:alts
    (unsyntax (elem (racket написать $ список 1 2 "3")
                    (linebreak) (racketoutput "'(1 2 \"3\")")))
    (void))
   (eval:alts
    (unsyntax (elem (racket вывести $ список 1 2 "3")
                    (linebreak) (racketoutput "'(1 2 3)")))
    (void))]

Наиболее полезны среди функций, работающих со списками, те, которые позволяют
перебирать элементы списка:

@examples[#:label ""
   (eval:alts
    (unsyntax (elem (racket отобразить)
                    (linebreak) (hspace 4) (racket функция (х))
                    (linebreak) (hspace 6) (racket 1 / х)
                    (linebreak) (hspace 4) (racketvalfont (literal "'")) (hspace 1) (racket 1 2 3)))
    '(1 1/2 1/3))
   (eval:alts
    (unsyntax (elem (racket отобразить/и)
                    (linebreak) (hspace 4) (racket функция (х))
                    (linebreak) (hspace 6) (racket х < 3)
                    (linebreak) (hspace 4) (racketvalfont (literal "'")) (hspace 1) (racket 1 2 3)))
    (eval:result (racketvalfont "ложь")))
   (eval:alts
    (unsyntax (elem (racket отобразить/или)
                    (linebreak) (hspace 4) (racket функция (х))
                    (linebreak) (hspace 6) (racket х < 3)
                    (linebreak) (hspace 4) (racketvalfont (literal "'")) (hspace 1) (racket 1 2 3)))
    (eval:result (racketvalfont "истина")))
   (eval:alts
    (unsyntax (elem (racket отобрать)
                    (linebreak) (hspace 4) (racket функция (х))
                    (linebreak) (hspace 6) (racket х < 3)
                    (linebreak) (hspace 4) (racketvalfont (literal "'")) (hspace 1) (racket 1 2 3)))
    '(1 2))
   (eval:alts
    (unsyntax (elem (racket свернуть)
                    (linebreak) (hspace 4) (racket функция (сум х))
                    (linebreak) (hspace 6) (racket х + сум)
                    (linebreak) (hspace 4) (racket 10)
                    (linebreak) (hspace 4) (racketvalfont (literal "'")) (hspace 1) (racket 1 2 3)))
    16)
   (eval:alts
    (unsyntax (elem (racket подсписок "осёл")
                    (linebreak) (hspace 4) (racketvalfont (literal "'")) (hspace 1)
                    (racket "козёл" "осёл" "мартышка")))
    '("осёл" "мартышка"))
   (eval:alts
    (unsyntax (elem (racket ассоциация 'где)
                    (linebreak) (hspace 4) (racketvalfont (literal "'")) (hspace 1)
                    (racketvalfont "кто") (racket ("Чебурашка")) (hspace 1)
                    (racketvalfont "где") (racket ("Москва")) (hspace 1)
                    (racketvalfont "когда") (racket ("Сейчас"))))
    '(где "Москва"))]

@subsection[#:tag "garrays"]{Массивы}

Массив --- это набор произвольных значений фиксированной длины.

В отличие от списка, у которого чем ближе элемент к первому, тем быстрее до него можно добраться,
в массиве чтение или изменение любого элемента происходит за одинаковое время. 

С другой стороны, у списка можно очень быстро получить список из все элементов кроме первого или
список из дополнительного элемента и всех существующих. В случае массива такие операции будут
тем дольше, чем больше элементов в массиве.

Таким образом, выбор формы хранения набора значений определяется тем, какие операции над этим
набором нужны.

Также, как к литерам в строке и байтам в байтовой строке, для доступа к элементам можно
использовать квадратные скобки. И также, если массив задаётся непосредственным значением,
то он неизменяемый.

Массив печатается подобно списку, но после апострофа @litchar["'"] добавляется решётка @litchar["#"].
При вводе апостроф можно не писать. Также можно после решётки указать длину массива, тогда все
элементы после явно указанных будут заполнены зхначением последнего.

@examples[#:label "Примеры:"
   (eval:alts
    (unsyntax (elem (racketvalfont "#(«а» «б» «в»)")))
    '#("а" "б" "в"))
   (eval:alts
    (unsyntax (elem (racketvalfont "#(имя (список из четырёх элементов))")))
    '#(имя (список из четырёх элементов)))
   (eval:alts
    (unsyntax (elem (racketvalfont "#4(два имени)")))
    '#(два имени имени имени))
   (eval:alts
    (unsyntax (elem (racketvalfont "#(имя (список из четырёх элементов))[1]")))
    '(список из четырёх элементов))
   (eval:alts
    (unsyntax (elem (racket элемент-массива) (hspace 1) (racketvalfont "#(«а» «б» «в»)")
                    (hspace 1) (racket 1)))
    "б")]

Из массивов можно получать списки и наоборот при помощи функций @racket[массив->список] и
@racket[список->массив]. Такие преобразования полезны, например, для использования функций,
работающих со списками.

@examples[#:label "Пример:"
   (eval:alts
    (unsyntax (elem (racket список->массив) (linebreak)
                    (hspace 4) (racket отобразить прописные) (linebreak)
                    (hspace 6) (racket массив->список) (racketvalfont " #(«раз» «два» «три»)")))
    '#("РАЗ" "ДВА" "ТРИ"))]

@subsection[#:tag "ghashs"]{Соответствия}

Соответствие позволяет сопоставить произвольным значениям-ключам произвольные значения.
Ключи сравниваются либо при помощи @racket[==], если соответствие создано при помощи
@racket[соответствие] или @racket[новое-соответствие], либо при помощи @racket[===],
если соответствие создано при помощи @racket[соответствие===] или @racket[новое-соответствие===].

@examples[#:label "Пример:"
   (eval:alts
    (unsyntax (elem (racket справочник = новое-соответствие) (racketparenfont "()")))
    (void))
   (eval:alts
    (unsyntax (elem (racket справочник) (racketparenfont "[") (racketvalfont "«яблоко»")
                    (racketparenfont "] ")
                    (racket := '(красное круглое))))
    '(красное круглое))
   (eval:alts
    (unsyntax (elem (racket установить-значение-соответствия! справочник) (racketvalfont " «банан»")
                    (racket '(жёлтый длинный))))
    (void))
   (eval:alts
    (unsyntax (elem (racket справочник) (racketparenfont "[") (racketvalfont "«яблоко»")
                    (racketparenfont "]")))
    '(красное круглое))
   (eval:alts
    (unsyntax (elem (racket значение-соответствия справочник) (racketvalfont " «яблоко»")))
    '(красное круглое))
   (eval:alts
    (unsyntax (elem (racket значение-соответствия справочник) (racketvalfont " «кокос»")))
    (void))
   (eval:alts
    (eval:no-prompt
     (unsyntax (elem (racketerror "значение-соответствия: нет значения для ключа
  ключ: \"кокос\""))))
    (void))
   (eval:alts
    (unsyntax (elem (racket значение-соответствия справочник)
                    (racketvalfont " «кокос» «такого нет»")))
    "такого нет")]

@subsection[#:tag "gstructures"]{Структуры}

Вышеописанных структур данных достаточно для реализации любого алгоритма. Списки позволяют
работать с однородными наборами данных, которые обрабатываются последовательно и количество
которых в наборе может меняться. Массивы позволяют работать с наборами данных только
фиксированной длины, но предоставляют равную (и очень высокую) скорость доступа ко всем элементам.

Соответствия позволяют сопоставить произвольные значения другим произвольным значениям, но также, как
для работы с фиксированным набором данных лучше использовать массивы, также для сопоставления
фиксированного набора имён значениям лучше использовать структуры.

Структура --- это тип, у которого есть имя и набор именованных полей. Таким образом, можно различать
структуры разных типов, даже если у них одинаковые поля. Объявление типа осуществляется формой
@racket[структура].

В первом приближении синтаксис этой формы выглядит так:

@codeblock|{
(структура <имя> (<поле> ...))
}|

@examples[#:label "Пример:"
   (eval:alts
    (unsyntax (elem (racket структура позиция)
                    (linebreak) (hspace 4) (racket ряд колонка)))
    (void))]

Форма @racket[структура] создаёт функции для работы с определяемым типом и связывает с ними
переданное имя, а также идентификаторы, полученные из имени и полей.

Из имени создаётся конструктор: функция, которая создаёт новую структуру с заданными значениями полей.
У неё столько аргументов, сколько полей в описании структуры.

@examples[#:label "Пример:"
   (eval:alts
    (unsyntax (elem (racket позиция 1 2)))
    (eval:result (racketresultfont "#<позиция>")))]

Из имени с вопросительным знаком создаётся предикат типа, то есть функция от одного аргумента,
возвращающая истину, если это значение является структурой данного типа.

@examples[#:label "Примеры:"
   (eval:alts
    (unsyntax (elem (racket позиция? 3)))
    (eval:result (racketvalfont "ложь")))
   (eval:alts
    (unsyntax (elem (racket позиция? $ позиция 1 2)))
    (eval:result (racketvalfont "истина")))]

Из имени структуры и имени поля создаётся функция чтения значения поля.

@examples[#:label "Примеры:"
   (eval:alts
    (unsyntax (elem (racket позиция-ряд $ позиция 1 2)))
    1)
   (eval:alts
    (unsyntax (elem (racket позиция-колонка $ позиция 1 2)))
    2)]

Можно добавить ключ суффикс, чтобы имя функции чтения значений было согласовано по падежам:

@examples[#:label "Примеры:"
   (eval:alts
    (unsyntax (elem (racket структура позиция)
                    (linebreak) (hspace 4) (racket ряд колонка)
                    (linebreak) (hspace 4) (racket #:суффикс -позиции)))
    (void))
   (eval:alts
    (unsyntax (elem (racket ряд-позиции $ позиция 1 2)))
    1)
   (eval:alts
    (unsyntax (elem (racket колонка-позиции $ позиция 1 2)))
    2)]

Также можно делать структуру с изменяемыми полями. Тогда создаётся функция
для установки значений полей путём добавления перед именем функции «установить-», а после имени
восклицательного знака. Или можно устанавливать значения при помощи функции чтения и оператора
@racket[:=].

@examples[#:label "Примеры:"
   (eval:alts
    (unsyntax (elem (racket структура позиция)
                    (linebreak) (hspace 4) (racket ряд) (racket (#:изменяемое))
                    (hspace 1)(racket колонка)
                    (linebreak) (hspace 4) (racket #:суффикс -позиции)))
    (void))
   (eval:alts
    (unsyntax (elem (racket моя-позиция = позиция 1 2)))
    (void))
   (eval:alts
    (unsyntax (elem (racket ряд-позиции моя-позиция)))
    1)
   (eval:alts
    (unsyntax (elem (racket ряд-позиции моя-позиция := 5)))
    (void))
   (eval:alts
    (unsyntax (elem (racket ряд-позиции моя-позиция)))
    5)
   (eval:alts
    (unsyntax (elem (racket установить-ряд-позиции! моя-позиция 6)))
    (void))
   (eval:alts
    (unsyntax (elem (racket ряд-позиции моя-позиция)))
    6)]

Есть возможность сделать структуру-подтип. Тогда значения данного типа будут
проходить проверку предикатом надтипа, у него будут все поля надтипа и к значениям
можно применять все функции доступа надтипа. То есть, фактически, значения данного типа
являются одновременно и значениями надтипа. Синтаксис для этого случая:

@codeblock|{
(структура <имя> <имя-надтипа> (<поле> ...))
}|

@examples[#:label "Примеры:"
   (eval:alts
    (unsyntax (elem (racket структура человек (имя) #:суффикс -человека)))
    (void))
   (eval:alts
    (unsyntax (elem (racket структура ученик человек (класс) #:суффикс -ученика)))
    (void))
   (eval:alts
    (unsyntax (elem (racket Вася = ученик «Вася» 5)))
    (void))
   (eval:alts
    (unsyntax (elem (racket имя-человека Вася)))
    "Вася")
   (eval:alts
    (unsyntax (elem (racket класс-ученика Вася)))
    5)]

@subsection[#:tag "gclass"]{Классы}

Вышеприведённых типов данных уже достаточно для описания любого алгоритма.

Но бывает алгоритмы, которые однообразно работают с разынми типами данных.
Например, вывод элементов графическогго интерфейса на экран. Элементы интерфейса разные,
но у них всех есть некая функция «вывести» с координатами.

Если реализовывать вывод через функцию, а данные хранить в структурах, то внутри функции
придётся делать огромную конструкцию @racket[если], которую придётся расширять при каждом
добавлении нового типа экранных элементов.

Можно сделать функцию вывода полем структуры, но даже для того, чтобы прочитать это поле,
структуры должны иметь одинаковый тип. Например, быть потомком одного надтипа. И если идти
этим путём, то у надтипа должны быть поля для всех функций, которые могут быть общими у его потомков.

Чтобы не переписывать надтип при добавлении каждой функции и иметь возможность иметь разные функции
у разных иерархий объектов, были придуманы классы. Класс это тип, в котором описаны поля
и функции (чтобы отличить от обычных, эти функции называются «методы»).
Класс может быть наследниковм (подтипом) другого класса. Тогда в нём есть все поля и методы
родительского класса, но методы можно переопределить. В отличие от структур, для вызова метода
класса и для доступа к полю класса не используется имя типа. Поэтому можно единообразно обращаться
к полям с одинаковыми именами или вызывать методы с одинаковыми именами для совершенно разных классов.

Значения классов традиционно называются объектами или экземплярами классов.

Чтобы работать с классами, сначала надо включить необходимую библиотеку.

@examples[#:label ""
   (eval:alts
    (unsyntax (elem (racket используется класс)))
    (void))]

Общая структура определения класса выглядит так:

@codeblock|{
(класс <имя-родительского-класса> <команда> ...)
}|

По соглашению имена классов заканчиваются знаком процента. Встроенный класс без полей и методов
@racket[объект%] можно использовать как родителя для классов, которые ничего не должны наследовать.

Команды внутри класса выполняются при создании объекта. Среди команд обязательно должна быть
ровно одна команда @racket[базовый-объект], выполняющая инициализацию данных,
унаследованных от родительского объекта. Даже если этот родительский
объект всего лишь @racket[объект%].

Определим, например, класс рыб с методами @racket[получить-размер], @racket[расти] и @racket[кушать].

@racketblock[
(unsyntax
 (elem
  (racket рыба% = класс объект%)
  (linebreak) (hspace 2)
  (racket инициализировать размер)
  (linebreak) (hspace 2)
  (racket текущий-размер = размер)
  (linebreak) (hspace 2)
  (racket базовый-объект) (racket ())
  (linebreak) (hspace 2)
  (racket методы получить-размер вырасти кушать)
  (linebreak) (hspace 2)
  (racket получить-размер) (racket ()) (hspace 1) (racket = текущий-размер)
  (linebreak) (hspace 2)
  (racket вырасти количество =)
  (linebreak) (hspace 4)
  (racket текущий-размер := текущий-размер + количество)
  (linebreak) (hspace 4)
  (racket пусто)
  (linebreak) (hspace 2)
  (racket кушать другая-рыба =)
  (linebreak) (hspace 4)              
  (racket вырасти другая-рыба) (racket {получить-размер})))]

Теперь можно создавать объекты этого класса:

@racketblock[
(unsyntax
 (elem
  (racket флаундер = объект рыба%) (linebreak) (hspace 2) (racket размер 10)))]

Аргумент инициализации @racket[размер] должен быть передан при создании объекта.
Он доступен только в процессе инициализации и не может быть прочитан из метода.
Поэтому значение этого аргумента мы сохраняем в поле @racket[текущий-размер].

Командой @racket[методы] определяется список методов, то есть функций, которые можно
выполнять с объектом. Методы можно вызывать при помощи функций @racket[вызвать-метод],
@racket[вызвать-цепочку-методов] или @racket[для-объекта]. Или при помощи синтаксиса
с фигурными скобками: тогда перед ними указывается значение объекта, а в скобках
имя метода и значения его аргументов.
@examples[#:label ""
   (eval:alts
    (unsyntax
     (elem
      (racket флаундер) (racket{вырасти 6})))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (racket флаундер) (racket{получить-размер})))
    16)
   (eval:alts
    (unsyntax
     (elem
      (racket вызвать-метод флаундер получить-размер)))
    16)]

При наследовании к методам родительского класса можно обращаться через переменную @racket[этот].

@racketblock[
 (unsyntax
  (elem
   (racket голодная-рыба% = класс рыба%)
   (linebreak) (hspace 2)
   (racket базовый-объект) (racket ())
   (linebreak) (hspace 2)
   (racket методы кушать-больше)
   (linebreak) (hspace 2)
   (racket кушать-больше рыба1 рыба2 =)
   (linebreak) (hspace 4)
   (racket этот) (racket {кушать рыба1})
   (linebreak) (hspace 4)
   (racket этот) (racket {кушать рыба2})))]

Или лучше использовать специальную команду @racket[унаследованные]. Тогда к методу родителя
можно обращаться как к своему и этот вызов работает быстрее.

@racketblock[
 (unsyntax
  (elem
   (racket голодная-рыба% = класс рыба%)
   (linebreak) (hspace 2)
   (racket базовый-объект) (racket ())
   (linebreak) (hspace 2)
   (racket методы кушать-больше)
   (linebreak) (hspace 2)
   (racket унаследованные кушать)
   (linebreak) (hspace 2)
   (racket кушать-больше рыба1 рыба2 =)
   (linebreak) (hspace 4)
   (racket кушать рыба1)
   (linebreak) (hspace 4)
   (racket кушать рыба2)))]

Неважно, вызывается метод через прямой вызов или через @racket[этот], в любом случае работает
переопределение методов.

@racketblock[
 (unsyntax
  (elem
   (racket разборчивая-рыба% = класс рыба%)
   (linebreak) (hspace 2)
   (racket базовый-объект) (racket())
   (linebreak) (hspace 2)
   (racket переопределить вырасти)
   (linebreak) (hspace 2)
   (racket вырасти количество =)
   (linebreak) (hspace 4)
   (racket базовый вырасти (количество * 3/4))))]

@examples[#:label ""
   (eval:alts
    (unsyntax
     (elem
      (racket фрося = объект разборчивая-рыба% размер) (racket (20))))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (racket фрося) (racket{кушать флаундер})))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (racket фрося) (racket{получить-размер})))
    32)]

Мы указали командой @racket[переопределить вырасти], что метод @racket[вырасти] в этом классе
не новый, а переопределяемый метод родительского класса. Командой @racket[базовый] можно вызвать
метод базового класса даже если он переопределён в текущем.

@subsubsection[#:tag "gclassinit"]{Аргументы инициализации}

Так как @racket[разборчивая-рыба%] была определена без своих аргументов инициализации, то аргументы,
переданные в функции @racket[объект], отправляются дальше в класс @racket[рыба%]
при вызове @racket[базовый-объект].

Можно передать аргументы базовому классу явным образом:

@examples[#:label ""
   (eval:alts
    (unsyntax
     (elem
      (racket рыба-размером-10% = класс рыба% $ базовый-объект размер) (racket (10))))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (racket объект) (racket (рыба-размером-10%)) (racket {получить-размер})))
    10)]

@subsubsection[#:tag "gclassfields"]{Поля}

Как было сказано выше, поле можно определить равенством в контексте класса. Но бывает удобно сделать
поле публичным. Если в классе @racket[рыба%] вместо @racket[текущий-размер = размер]
написать команду @racket[поля текущий-размер(размер)], то класс будет работать также, но появится
возможность устанавливать значение поля напрямую:
@examples[#:label ""
   (eval:alts
    (unsyntax
     (elem
      (racket установить-поле! текущий-размер фрося )))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (racket объект) (racket (рыба-размером-10%)) (racket {получить-размер})))
    10)]

@section[#:tag "gui"]{Графический интерфейс}

Модули каталога @filepath{графический-интерфейс} дают русскоязычную оболочку над
@racketmodname[racket/gui/base]: окна, кнопки, поля ввода, панели, меню и текстовый редактор.
Классы Адины хранят внутри объект Racket (поле @racket[внутренний]) и переводят
имена методов, аргументов инициализации и событий.

Чтобы подключить сразу основные классы, напишите:

@codeblock|{
#!1
используется графический-интерфейс
}|

Можно подключать отдельные файлы каталога, например
@racketidfont{графический-интерфейс/кнопка} или
@racketidfont{графический-интерфейс/табличная-панель}.
Модуль @racketidfont{графический-интерфейс} сам подтягивает @racket[класс],
поэтому отдельно писать @racket[(используется класс)] не обязательно.

Объекты создаются формой @racket[объект], как в разделе @secref["gclass"].
Окно нужно показать методом @racket[показать]:

@codeblock|{
#!1
используется графический-интерфейс

окно = объект главное-окно%
  заголовок "Привет"
  ширина 320
  высота 140

поле = объект текстовое-поле%
  родитель окно
  заголовок "Имя:"

объект кнопка%
  родитель окно
  заголовок "Сказать привет"
  команда
    функция (кнопка событие)
      вывести/перенос
        "Привет, " ++ поле{значение}

окно{показать истина}
}|

Родитель элемента задаётся аргументом инициализации @racket[родитель].
У кнопок действие передаётся аргументом @racket[команда]
(функция от элемента и события). У текстового поля текущая строка читается
и записывается методом @racket[значение].

@subsection[#:tag "gui-architecture"]{Как устроены классы}

Иерархия интерфейсов повторяет Racket GUI:

@itemlist[
 @item{@racket[область<%>] --- общие размеры и растягивание;}
 @item{@racket[окно<%>] расширяет область: заголовок, показ, фокус, курсор;}
 @item{@racket[подобласть<%>] --- поля (отступы) внутри вместилища;}
 @item{@racket[подокно<%>] --- окно, которое живёт внутри родителя
  (@racket[кнопка%], @racket[надпись%], @racket[текстовое-поле%], панели);}
 @item{@racket[вместилище<%>] --- контейнер элементов
  (@racket[группа%], @racket[панель%], @racket[главное-окно%]);}
 @item{@racket[вместилище-окно<%>] --- вместилище, которое само является окном;}
 @item{@racket[главное-окно<%>] --- рамка или диалог верхнего уровня;}
 @item{@racket[элемент-управления<%>] --- типичный виджет на панели;}
 @item{@racket[картина<%>] --- холст или поле редактора.}
]

События объявлены как методы с приставкой @racketidfont{при-}
(@racket[при-закрытии], @racket[при-изменении-размера])
или вопросом (@racket[можно-закрыть?]).
В базовых классах они ничего не делают или возвращают разумное значение по умолчанию;
их можно переопределить в наследнике. Часть действий задаётся и колбэком при создании
(@racket[команда] у кнопки, @racketidfont{при-изменении} у поля,
@racketidfont{обработка} у картины).

Многие методы работают как «геттер и сеттер»: без аргументов читают значение,
с одним аргументом записывают его. Так устроены @racket[заголовок],
@racket[значение], @racket[минимальная-ширина], @racket[выравнивание].

Символы стиля и выравнивания русские. Для выравнивания вместилища:
@racket['лево], @racket['центр], @racket['право] по горизонтали и
@racket['верх], @racket['центр], @racket['низ] по вертикали.
Для текстового поля стиль по умолчанию @racket['(однострочное)];
допустимы также @racket['многострочное] и @racket['скрытая]
(элемент создан, но ещё не показан в родителе). У кнопки в стиле бывают
@racket['граница], @racket['многострочная], @racket['скрытая].

Подробные описания интерфейсов, классов, аргументов инициализации и методов
собраны в справочнике: @secref["gui-reference"]. Классы и функции рисования ---
@secref["draw-reference"].

@subsection[#:tag "gui-windows"]{Окна и диалоги}

@racket[главное-окно%] --- окно верхнего уровня.
Аргументы инициализации: @racket[заголовок] (строка, по умолчанию пустая),
@racket[ширина] и @racket[высота]
(число или @racketvalfont{ложь} --- размер по содержимому),
плюс общие аргументы вместилища: @racket[родитель],
@racket[граница], @racket[интервал], @racket[выравнивание]
(по умолчанию @racket['(центр верх)]),
@racket[минимальная-ширина], @racket[минимальная-высота],
@racketidfont{растягивается-ширина}, @racketidfont{растягивается-высота},
@racketidfont{включен}.

Основные методы: @racket[показать], @racket[включить],
@racket[заголовок], @racket[ширина], @racket[высота],
@racket[переместить], @racket[установить-размер], @racket[установить-иконку],
@racket[в-центр] (направление @racket['оба], @racket['по-горизонтали]
или @racket['по-вертикали]),
@racket[создать-строку-состояния], @racket[показать-состояние],
@racket[сфокусировать], @racket[обновить].
События: @racket[можно-закрыть?], @racket[при-закрытии],
@racket[при-активации], @racket[при-выходе],
@racket[при-изменении-дисплея].

@racket[диалог%] --- модальное или вспомогательное окно.
Те же аргументы инициализации и методы @racket[главное-окно<%>], что у
@racket[главное-окно%].

Функция @racket[получить-файл] показывает стандартный модальный диалог
выбора файла и возвращает путь к выбранному файлу или @racketvalfont{ложь},
если пользователь отменил выбор.

Интерфейс @racket[область<%>] задаёт @racket[родитель], @racketidfont{главное-окно},
@racket[минимальная-ширина], @racket[минимальная-высота],
@racketidfont{минимальные-размеры-вывода},
@racketidfont{растягивается-ширина}, @racketidfont{растягивается-высота}.

@racket[окно<%>] расширяет @racket[область<%>]. Показ и активность:
@racket[показать], @racket[показано?],
@racket[включить], @racket[включено?],
@racket[есть-фокус?], @racket[сфокусировать].
Геометрия: @racket[ширина], @racket[высота],
@racket[лево], @racket[верх], @racket[размеры],
@racket[внутренние-размеры], @racket[окно->экран],
@racket[экран->окно]. Прочее: @racket[заголовок],
@racket[курсор], @racket[принимать-файлы],
@racket[контекстное-меню], @racket[режим-колеса-мыши]
(@racket['по-одному], @racket['по-целым], @racket['сразу]).

@racket[главное-окно<%>] расширяет @racket[вместилище-окно<%>].
Дополнительно: @racket[пространство-событий],
@racket[объект-в-фокусе], @racket[окно-в-фокусе],
@racket[установить-иконку].

@subsection[#:tag "gui-layout"]{Вместилища: панели и группы}

@racket[группа%] только раскладывает детей и сама не рисуется.
@racket[панель%] --- видимый контейнер.
Горизонтальные и вертикальные варианты задают направление раскладки.

@racket[группа%] --- контейнер без собственной рамки.
Реализует @racket[вместилище<%>] и @racket[подобласть<%>].
Аргументы: @racket[родитель], @racket[граница], @racket[интервал],
@racket[выравнивание] (по умолчанию @racket['(центр центр)]),
размеры и растягивание области, @racket[горизонтальные-поля],
@racket[вертикальные-поля].

@racket[горизонтальная-группа%] --- дети слева направо.
Выравнивание по умолчанию @racket['(лево центр)].

@racket[вертикальная-группа%] --- дети сверху вниз. Выравнивание по умолчанию @racket['(центр верх)].

@racket[панель%] --- видимая панель (@racket[вместилище-окно<%>] и @racket[подокно<%>]).
Дополнительный аргумент @racket[стиль] --- список символов стиля.

@racket[горизонтальная-панель%] --- горизонтальная панель.
Метод @racket[ориентация] читает и задаёт направление.

@racket[вертикальная-панель%] --- вертикальная панель с тем же методом @racket[ориентация].

У @racket[вместилище<%>]: @racket[элементы], @racket[добавить-элемент],
@racket[удалить-элемент], @racket[изменить-элементы],
@racket[начать-изменения] и @racket[закончить-изменения]
(пакет обновлений раскладки),
@racket[пересчитать-положения], @racket[граница],
@racket[интервал], @racket[выравнивание].

Не входит в модуль @racketidfont{графический-интерфейс}, подключается отдельно:

@racket[табличная-панель%] --- панель-таблица из модуля
@racketidfont{графический-интерфейс/табличная-панель}.
Аргументы: @racket[измерения] --- список @racket['(ряды колонки)],
по умолчанию @racket['(1 1)];
@racket[заполнять] --- @racket['ряд] или @racket['колонку];
@racket[колонки-растягиваются] и @racket[ряды-растягиваются] ---
@racket['любая] или @racket['каждая].

@subsection[#:tag "gui-controls"]{Элементы управления и меню}

@racket[кнопка%] --- кнопка. Аргументы: @racket[заголовок], @racket[родитель],
@racket[команда] (функция от кнопки и события; по умолчанию ничего не делает),
@racketidfont{шрифт} (по умолчанию @racket[шрифт-элемента-управления]),
@racket[стиль], размеры и растягивание
(по умолчанию кнопка не растягивается).

@racket[надпись%] --- статический текст или картинка-подпись.
Аргументы: @racket[заголовок] (строка или изображение),
@racket[цвет], @racket[размер-по-заголовку].
Методы @racket[цвет] и @racket[размер-по-заголовку]
читают и меняют эти свойства.

@racket[текстовое-поле%] --- однострочное или многострочное поле.
Аргументы: @racket[заголовок], @racket[значение] (начальная строка, по умолчанию пустая),
@racketidfont{при-изменении} (функция от поля и @racketidfont{событие-управления}),
@racket[стиль] (по умолчанию @racket['(однострочное)]),
@racketidfont{растягивается-ширина} (по умолчанию @racketvalfont{истина}).
Метод @racket[значение] читает и записывает текст.

@racket[поле-даты%] --- поле ввода даты из @racketidfont{графический-интерфейс/поле-даты}
(отдельное подключение). Наследует @racket[текстовое-поле%].

@racket[строка-меню%] --- строка меню окна.
Аргумент @racket[родитель] --- главное окно или диалог.

@racket[меню%] --- выпадающее меню. Аргументы @racket[родитель] и @racket[заголовок].

@racket[пункт-меню%] --- пункт меню. Аргументы: @racket[родитель], @racket[заголовок],
@racketidfont{действие} (колбэк). Методы интерфейсов
@racket[пункт-меню<%>] и @racket[пункт-меню-с-заголовком<%>]:
@racket[удалить], @racket[восстановить], @racket[удалён?],
@racket[родитель], @racket[заголовок], @racket[включить],
@racket[включён?], @racket[строка-помощи].

@subsection[#:tag "gui-fonts-events"]{Шрифты и события}

@racket[шрифт%] описывает шрифт. Аргументы инициализации:
@racket[размер] (12), @racket[семейство],
@racket[имя], @racket[стиль]
(@racket['нормальный], @racket['наклонный], @racket['курсив]),
@racket[насыщенность]
(@racket['тонкая], @racket['светлая], @racket['нормальная], @racket['жирная],
@racket['тяжёлая] и промежуточные значения в том же ряду),
@racket[подчёркнутый?], @racket[сглаживание]
(@racket['стандартное], @racket['частичное], @racket['полное], @racket['отсутствует]),
@racket[размер-в-пикселях?], @racket[округлять-метрики?],
@racket[свойства]. Методы с теми же именами читают параметры.

@racket[шрифт-элемента-управления] --- шрифт, совпадающий со стандартным шрифтом элементов управления.

Функция @racket[на-основе-шрифта] строит новый шрифт, копируя исходный;
не указанные ключевые аргументы берутся из исходного шрифта.

@racket[событие%] --- базовое событие с методом @racket[время].
События мыши, клавиатуры и элементов управления создаются обёртками
при доставке в методы Адины. У события мыши: @racket[тип]
(@racket['вход], @racket['выход], @racket['левая-нажата],
@racket['левая-отпущена], @racket['средняя-нажата], @racket['средняя-отпущена],
@racket['правая-нажата], @racket['правая-отпущена], @racket['движение]),
@racket[лево], @racket[верх],
@racket[нажата-кнопка?] и @racket[отпущена-кнопка?]
с аргументом @racket['левая], @racket['правая], @racket['средняя] или @racket['любая].
У события управления @racket[тип] бывает
@racket['кнопка], @racket['текстовое-поле], @racket['ввод-в-текстовом-поле],
@racket['меню], @racket['флажок] и другие значения из того же словаря.

@subsection[#:tag "gui-editors"]{Картина, редактор и куски}

@racket[картина%] --- холст для рисования.
Аргумент @racketidfont{обработка} --- функция от картины и холста,
вызываемая при перерисовке. Методы @racket[картина<%>]:
@racket[холст], @racket[фон], @racket[отрисовать-немедленно].

@racket[поле-редактора%] --- холст редактора.
Аргумент @racket[редактор] --- объект @racket[текст%] или другой
@racket[редактор<%>].

@racket[текст%] --- текстовый редактор. Аргументы: @racketidfont{межстрочный-промежуток} (1.0),
@racketidfont{автоматический-перенос}.
Методы: @racket[вставить], @racket[очистить],
@racket[только-просмотр], @racket[найти-позицию],
@racket[найти-кусок], @racket[изменить-стиль],
@racket[последняя-позиция], @racket[начало-выделения], @racket[конец-выделения],
@racket[текст], @racket[установить-выделение],
@racket[конец-абзаца],
@racket[первый-кусок].

Функция @racket[для-каждого-куска] вызывает переданную функцию для каждого куска документа по порядку.

@racket[кусок%] --- фрагмент документа редактора. Методы: @racket[следующий],
@racket[предыдущий], @racket[текст], @racket[количество],
@racket[параметры], @racket[стиль], @racket[скопировать].

@racket[кусок-со-строкой%] --- текстовый кусок.
Аргумент @racketidfont{содержимое} --- строка или начальный размер.

@racket[кусок-с-изображением%] --- кусок-картинка.
Метод @racket[изображение] читает и задаёт растр.

@racket[кусок-с-редактором%] --- вложенный редактор.
Аргументы @racket[редактор] и @racketidfont{есть-граница}.

Стили редактора живут в @racketidfont{графический-интерфейс/стиль}
(@racket[список-стилей%], @racket[стиль%], @racket[отклонение-стиля%]),
привязки клавиш --- в @racketidfont{графический-интерфейс/набор-команд}
(@racket[набор-команд%]).

@subsection[#:tag "gui-catalog"]{Файлы каталога}

Коллекция @filepath{1/графический-интерфейс} соответствует модулю
@racketidfont{графический-интерфейс} и его подмодулям:

@tabular[#:sep @hspace[1]
         (list (list @bold{Модуль} @bold{Что экспортирует})
               (list @racketidfont{графический-интерфейс}
                     "сводка: окна, диалог, виджеты, меню, шрифт, редактор, куски")
               (list @racketidfont{графический-интерфейс/главное-окно} @racket[главное-окно%])
               (list @racketidfont{графический-интерфейс/диалог} @racket[получить-файл])
               (list @racketidfont{графический-интерфейс/кнопка} @racket[кнопка%])
               (list @racketidfont{графический-интерфейс/надпись} @racket[надпись%])
               (list @racketidfont{графический-интерфейс/текстовое-поле} @racket[текстовое-поле%])
               (list @racketidfont{графический-интерфейс/поле-даты} @racket[поле-даты%])
               (list @racketidfont{графический-интерфейс/панель}
                     @elem{@racket[панель%], горизонтальная и вертикальная})
               (list @racketidfont{графический-интерфейс/группа}
                     @elem{@racket[группа%], горизонтальная и вертикальная})
               (list @racketidfont{графический-интерфейс/табличная-панель} @racket[табличная-панель%])
               (list @racketidfont{графический-интерфейс/шрифт}
                     @elem{@racket[шрифт%], @racket[шрифт-элемента-управления],
                           @racket[на-основе-шрифта]})
               (list @racketidfont{графический-интерфейс/картина} @racket[картина%])
               (list @racketidfont{графический-интерфейс/поле-редактора} @racket[поле-редактора%])
               (list @racketidfont{графический-интерфейс/текст}
                     @elem{@racket[текст%], @racket[кусок%], @racket[для-каждого-куска]})
               (list @racketidfont{графический-интерфейс/событие}
                     @elem{@racket[событие%] и обёртки событий})
               (list @racketidfont{графический-интерфейс/стиль} "список стилей, стиль, отклонение")
               (list @racketidfont{графический-интерфейс/набор-команд} @racket[набор-команд%])
               (list @racketidfont{графический-интерфейс/объект} "общий предок виджетов Адины")
               (list @elem{@racketidfont{графический-интерфейс/интерфейс-…}}
                     "интерфейсы и синтаксис методов для остальных файлов"))]

@section[#:tag "reference"]{Справочник}

@subsection[#:tag "definitions"]{Синтаксические формы}

@defform*[#:kind "синтаксис" #:id =
   ((идентификатор = выражение)
    (#,(racketidfont "значения") идентификатор ... = выражение)
    (заголовок(аргументы) = команда ... выражение))
   #:grammar [(заголовок идентификатор (заголовок аргументы))
              (аргументы (code:line аргумент ...)
                         (code:line аргумент ... @#,racketparenfont{.} аргумент-оставшихся))
              (аргументы идентификатор [идентификатор выражение] (code:line ключ идентификатор)
                        (code:line [ключ идентификатор выражение]))
              ]]{
   Эта команда определяет новые переменные.                 
   Первая форма связывает идентификатор с результатом вычисления выражения.
 Вторая позволяет одновременно связать несколько идентификаторов с значениями
(выражение должно в этом случае возвращать необходимое количество значений).
 Третья связывает идентификатор с функцией, здесь особым образом обрабатывается оператор: каждый
 элемент после @racket[=] считается отдельной командой. То есть, если надо сделать
 функцию из одного выражения, выражение должно быть одним элементом или
 обязательно после @racket[=] делать перенос и отступ. Если @racket[заголовок] является списком,
 то создаётся функция, возвращающая функцию с аргументами, указанными после первого элемента
 заголовка.}

@defform*[#:kind "синтаксис" #:id :=
   ((идентификатор := выражение)
    (#,(racketidfont "значения") идентификатор ... := выражение)
    (выражение-коллекция[индекс] := выражение)
    (доступ-к-полю выражение-структура := выражение))]{
  Эта команда позволяет изменить значение существующей переменной. Первые две формы аналогичны
первым двум формам команды @racket[=] и позволяют изменить значение определённых ранее переменных.
Третья форма позволяет при помощи квадратных скобок изменить значение элемента изменяемой коллекции:
массива, строки, соответствия или списка. Учитывайте, что для списка время доступа пропорционально
номеру элемента. Четвёртая форма позволяет изменить значение изменяемого поля структуры.
Результатом этой команды является значение выражения.}

@defform*[#:kind "синтаксис" #:id структура
   ((структура имя (поле ...) параметр ...)
    (структура имя надструктура (поле ...) параметр ...))
   #:grammar [(поле (code:line идентификатор)
                    (code:line (идентификатор #:изменяемое)))
              (параметр
               (code:line #:суффикс идентификатор)
               (code:line #:префикс идентификатор)
               (code:line #:читаемая))
              ]]{Описывает тип структуры.
Создаёт функции для создания структур и доступа к полям.
Имя функции для создания совпадает с именем структуры.
Имя функции для проверки типа создаётся путём склеивания имени структуры и вопросительного знака.
Имя функции для доступа к поля создаётся путём склеивания префикса, имени поля и суффикса.
Если ни префикс ни суффикс не указаны, считается, что префиксом является склейка имени структуры
 и дефиса. Если у поля указан ключ @racket[#:изменяемое], для него осоздаётся функция изменения
 путём склейки слова «установить», дефиса, имени функции доступа и восклицательного знака.

Если указан ключ @racket[#:читаемая], то при выводе структуры будут видны значения её полей и
новые структуры можно создавать не только через функцию создания. Если значения структуры не могут
быть произвольными и значения полей проверяютсяпри создании, не используйте этот ключ.

Если указана @racket[надструктура], которая должна быть идентификатором типа структуры,
то в создаваемый тип перед указанными полями добавляются все поля надструктуры
 и созданные структуры считаются также относящимся также к типу надструктуры.}

@defform*[#:kind "синтаксис" #:id функция
   ((функция (аргументы) = команда ... выражение))
   #:grammar [(аргументы (code:line аргумент ...)
                         (code:line аргумент ... @#,racketparenfont{.} аргумент-оставшихся))
              (аргумент идентификатор (идентификатор выражение) (code:line ключ идентификатор)
                        (code:line (ключ идентификатор выражение)))
              ]]{
   Возвращает функцию с указанными аргументами и телом.}

@defform[#:kind "оператор" #:id ==> (аргументы ==> выражение)
         #:grammar
         [(аргументы идентификатор (идентификатор ...))]]{
Возвращает функцию с телом @racket[выражение]. Если слева один
идентификатор, он становится единственным аргументом. Если слева
несколько идентификаторов, все они --- аргументы.

Оператор собирает слова слева в аргументы, а справа --- в одно
выражение. Приоритет выше, чем у @racket[=], поэтому
@racket[имя = аргумент ==> выражение] связывает имя с новой функцией.

В отличие от @racket[функция], тело --- одно выражение. Несколько
команд можно записать через @racket[блок].}

@defform[#:kind "синтаксис" (буквально элемент ...+)]{
Если вызвана с одним элементом, то возвращает постоянное значение,
 соответствующее переданному элементу (то есть фрагменту программы).
 Если с несколькими, то формирует из них список. Возвращаемое значение всегда
 неизменяемое.}

@defform[#:kind "синтаксис" (блок команда ...)]{
Выполняет команды слева направо. Результат — значение последней
команды. Так несколько команд можно записать там, где ожидается одна.

Если @racket[блок] стоит среди определений, его содержимое вставляется
в окружающий контекст как отдельные команды: определения из блока видны
снаружи, как если бы они были записаны на месте @racket[блок].}

@defform*[#:kind "синтаксис" #:id попытка
          #:literals (исключения)
          ((попытка обработчик команда ...)
           (попытка (условие обработчик) команда ...)
           (попытка (исключения (условие обработчик) ...)
                    команда ...))]{
Выполняет команды. Если при этом возникает исключение, вызывается
подходящий @racket[обработчик]: ему передаётся исключение, результат
обработчика становится результатом @racket[попытка]. Если исключения
нет, результат --- значение последней команды.

@racket[обработчик] --- функция от одного аргумента (исключения).
@racket[условие] --- функция от исключения: обработчик вызывается,
если она вернула не @racketvalfont{ложь}.

В первой форме обрабатывается любое исключение.

Во второй форме проверяется одно @racket[условие].

В третьей форме пары условия и обработчика перебираются по порядку;
срабатывает первая подходящая. Если ни одна не подошла, исключение
передаётся дальше.}

@defthing[#:kind "значение" пусто пусто?]{Если значением функции является @racket[пусто],
тогда результат не выводится}

@defproc[#:kind "функция" (пусто? [аргумент любой])
         булево?]{Возвращает @racketvalfont{истина}, если @racket[аргумент] равен @racket[пусто]}

@subsection[#:tag "classes"]{Классы}

Команды доступны после подключения модуля @racket[класс]:
@racketblock[(используется класс)]

@defform[#:kind "синтаксис"
         #:literals (инициализировать инициализированные-поля поля унаследованные-поля
                     методы дополняемые-методы переопределить дополнить
                     абстрактные унаследованные блок)
         (класс выражение-базового-класса команда-класса ...)
         #:grammar
         [(команда-класса
           (инициализировать объявление-инициализации ...)
           (инициализированные-поля объявление-инициализации ...)
           (поля объявление-поля ...)
           (унаследованные-поля возможно-переименованное ...)
           (методы возможно-переименованное ...)
           (дополняемые-методы возможно-переименованное ...)
           (переопределить возможно-переименованное ...)
           (дополнить возможно-переименованное ...)
           (абстрактные идентификатор ...)
           (унаследованные возможно-переименованное ...)
           определение
           выражение
           (блок команда-класса ...))
          (объявление-инициализации
           (code:line идентификатор)
           (code:line (переименованное))
           (code:line (возможно-переименованное выражение-значения-по-умолчанию)))
          (объявление-поля
           (code:line (возможно-переименованное выражение-значения-по-умолчанию)))
          (возможно-переименованное
           (code:line идентификатор)
           (code:line переименованное))
          (переименованное
           (code:line (внутреннее-имя внешнее-имя)))]]{
Возвращает значение класса.

Класс задаёт набор полей, набор методов, выражения начальных значений полей
и переменные инициализации, которые связываются с аргументами при создании объекта.
В системе классов объектом называется набор привязок полей, созданный по описанию класса.

Новый класс можно определить через уже существующий @deftech{базовый класс}
с помощью наследования, переопределения и дополнения:

@itemize[
 @item{@deftech{наследование}: объект производного класса поддерживает методы
  и создаёт поля, объявленные в базовом классе, а также методы и поля,
  объявленные в выражении производного класса;}
 @item{@deftech{переопределение}: некоторые методы базового класса можно заменить
  в производном. Обращения к переопределённому методу из базового класса
  используют реализацию из производного класса;}
 @item{@deftech{дополнение}: некоторые методы базового класса можно лишь расширить
  в производном. Метод базового класса явно передаёт управление дополняющему
  методу производного класса.}
]

@racket[выражение-базового-класса] вычисляется при вычислении команды @racket[класс].
Результат должен быть значением класса (в том числе встроенным классом
@racket[объект%]), иначе вызывается исключение. Этот результат становится
базовым классом нового класса. У @racket[объект%] нет полей и методов;
от него порождаются все остальные классы.

Остальные @racket[команда-класса] задают аргументы инициализации, открытые
и закрытые поля, открытые и закрытые методы. Для каждого имени в командах
@racket[методы], @racket[переопределить], @racket[дополнить]
или @racket[дополняемые-методы] должна быть ровно одна соответствующая
команда определения метода. Команда @racket[абстрактные] объявляет имена методов
без реализации: объект такого класса нельзя создать, пока производный класс
не задаст реализацию командой @racket[переопределить]. Все прочие определения
внутри класса создают закрытые поля. Оставшиеся выражения — выражения
инициализации; они выполняются при создании объекта.

Если команда класса — выражение @racket[блок], его подвыражения поднимаются
из блока и обрабатываются как отдельные команды класса.

Результат команды @racket[класс] — новый класс, порождённый указанным
базовым классом. Объекты создаются формой @racket[объект] или функцией
@racket[создать-объект].

Внутри команды @racket[класс] для экземпляров нового класса
@racket[этот] связан с самим объектом;
@racket[этот%] — с классом текущего объекта;
@racket[базовый-объект] и @racket[создать-базовый-объект] — формы
инициализации полей базового класса;
@racket[базовый] вызывает метод базового класса;
@racket[производный] вызывает дополнение метода в производном классе.
Использование @racket[этот], @racket[этот%], @racket[базовый] и @racket[производный]
вне тела команды @racket[класс] является ошибкой синтаксиса.

@bold{Переменные инициализации.}
Переменные, объявленные командами @racket[инициализировать]
и @racket[инициализированные-поля], создаются для каждого объекта класса.
Их можно использовать в выражениях начальных значений полей, в выражениях
значений по умолчанию для аргументов инициализации и в выражениях
инициализации. Из методов доступны только переменные, объявленные
@racket[инициализированные-поля]; обращение к прочим переменным инициализации
из метода — ошибка синтаксиса.

Значения этих переменных — аргументы, переданные форме @racket[объект]
или функции @racket[создать-объект], если объект создаётся как непосредственный
экземпляр класса; либо аргументы, переданные форме инициализации базового
класса, если объект создаётся как экземпляр производного класса.

Если аргумент не передан, а у переменной есть
@racket[выражение-значения-по-умолчанию], вычисляется это выражение.
Оно вычисляется только при отсутствии аргумента. В его окружение входят
все переменные инициализации, все поля и все методы класса.
Если вычисляется несколько выражений по умолчанию, они вычисляются слева направо.
Если значения по умолчанию нет, при создании объекта или инициализации
базового класса аргумент обязателен, иначе вызывается исключение.

Аргументы можно передать по имени или по позиции. Внешнее имя переменной
инициализации используется с формой @racket[объект] и с формой инициализации
базового класса. Функция @racket[создать-объект] и процедура инициализации
базового класса принимают только аргументы по позиции.

Аргументы по позиции превращаются в именные в порядке команд
@racket[инициализировать] и @racket[инициализированные-поля]
и в порядке переменных внутри каждой команды. Неиспользованные именные
аргументы передаются базовому классу.

@bold{Поля.}
Каждая команда @racket[поля], @racket[инициализированные-поля]
и каждое определение, не являющееся методом, объявляет одно или несколько
новых полей класса. Поля из @racket[поля] и @racket[инициализированные-поля]
открытые: к ним можно обращаться и их можно изменять в производных классах
через @racket[унаследованные-поля], а снаружи класса — функциями
@racket[поле] и @racket[установить-поле!]. Поля из обычных определений
доступны только внутри класса.

Поле из @racket[инициализированные-поля] одновременно является открытым полем
и переменной инициализации.

Команда @racket[унаследованные-поля] делает открытое поле базового класса
напрямую доступным в выражении класса. Если указанного поля в базовом классе
нет, при вычислении выражения класса вызывается исключение.
Каждое поле базового класса присутствует и в производном, даже без
@racket[унаследованные-поля]; эта команда не управляет наследованием,
а только лексической областью видимости внутри выражения класса.

При создании объекта все поля сначала не определены. Поля класса
инициализируются одновременно с вычислением выражений инициализации класса.
Среди выражений инициализации должна быть инициализация базового класса
(@racket[базовый-объект] или @racket[создать-базовый-объект]).

@bold{Методы.}
Каждая команда @racket[методы], @racket[переопределить], @racket[дополнить]
или @racket[дополняемые-методы] объявляет одно или несколько имён методов.
У каждого имени должна быть соответствующая команда определения метода.
Порядок таких команд и определений (между собой и относительно других
команд класса) неважен.

Определение метода синтаксически ограничено формами функций. Выражение
процедуры метода не вычисляется напрямую: для каждого метода создаётся
процедура, которая помимо обычных аргументов получает объект.
Тело преобразуется так, чтобы доступ к методам и полям шёл через этот объект.

Метод, объявленный @racket[методы] или @racket[дополняемые-методы],
вводится в класс как новый. Его ещё не должно быть в базовом классе,
иначе при вычислении выражения класса вызывается исключение.
Метод из @racket[методы] можно переопределить в производном классе
командой @racket[переопределить]. Метод из @racket[дополняемые-методы]
можно дополнить в производном классе командой @racket[дополнить].

Метод, объявленный @racket[переопределить], заменяет определение,
уже присутствующее в базовом классе. Если метода там нет, вызывается
исключение. Такой метод можно снова переопределить в следующем
производном классе.

Метод, объявленный @racket[дополнить], расширяет метод базового класса,
объявленный как дополняемый. Если дополняемого метода нет, вызывается
исключение.

Команда @racket[унаследованные] делает метод базового класса напрямую
вызываемым в выражении класса (как свой метод). Если метода нет
в базовом классе, вызывается исключение. Наличие метода в производном
классе не зависит от @racket[унаследованные]: команда задаёт только
область видимости.

Если в @racket[возможно-переименованное] указано
@racket[переименованное], @racket[внутреннее-имя] используется в теле
класса, а @racket[внешнее-имя] — при вызове метода или доступе к полю
снаружи и в производных классах. Все внутренние имена в одном классе
должны быть различны; различны должны быть и внешние имена методов,
полей и аргументов инициализации.

Команда @racket[класс] не принимает список интерфейсов. Если класс
должен явно реализовывать интерфейсы, используйте @racket[класс*].}

@defform[#:kind "синтаксис"
         (класс* выражение-базового-класса (выражение-интерфейса ...) команда-класса ...)]{
Как @racket[класс], но новый класс реализует указанные интерфейсы.
Команды класса те же, что у @racket[класс].

@racket[выражение-базового-класса] вычисляется при вычислении команды
@racket[класс*]. Результат должен быть значением класса, иначе вызывается
исключение. Этот результат становится базовым классом нового класса.

Выражения интерфейсов вычисляются следом, в порядке записи. Результат каждого
должен быть значением интерфейса, иначе вызывается исключение.
Новый класс реализует все эти интерфейсы. Для каждого имени метода каждого
интерфейса класс или один из его предков должен объявить открытый метод
с тем же именем, иначе вызывается исключение. Базовый класс должен
удовлетворять требованию порождения каждого интерфейса, иначе вызывается
исключение.

Результат — новый класс, порождённый указанным базовым классом и реализующий
указанные интерфейсы. Объекты создаются формой @racket[объект] или функцией
@racket[создать-объект].

Внутри команды @racket[класс*] действуют те же привязки, что внутри
@racket[класс]: @racket[этот], @racket[этот%], @racket[базовый-объект],
@racket[создать-базовый-объект], @racket[базовый] и @racket[производный].}

@defform[#:kind "синтаксис" (инициализировать объявление-инициализации ...)]{
См. @racket[класс*] и @racket[класс] (переменные инициализации);
использование вне тела @racket[класс] или @racket[класс*] — ошибка синтаксиса.
@examples[#:label ""
   (eval:alts
    (unsyntax
     (elem
      (racket класс объект%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket инициализировать репа)
      (linebreak) (hspace 4)
      (racket (внутренняя-картошка картошка))
      (linebreak) (hspace 4)
      (racket (морковь 'хорошая))
      (linebreak) (hspace 4)
      (racket ((внутренняя-брюква брюква) 'нормальная))))
    (void))]}

@defform[#:kind "синтаксис" (инициализированные-поля объявление-инициализации ...)]{
См. @racket[класс*] и @racket[класс] (переменные инициализации, поля);
использование вне тела @racket[класс] или @racket[класс*] — ошибка синтаксиса.
@examples[#:label ""
   (eval:alts
    (unsyntax
     (elem
      (racket класс объект%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket инициализированные-поля индейка)
      (linebreak) (hspace 4)
      (racket (внутренний-страус страус))
      (linebreak) (hspace 4)
      (racket (курица 7))
      (linebreak) (hspace 4)
      (racket ((внутренний-эму эму) 13))))
    (void))]}

@defform[#:kind "синтаксис" (поля объявление-поля ...)]{
См. @racket[класс*] и @racket[класс] (поля);
использование вне тела @racket[класс] или @racket[класс*] — ошибка синтаксиса.
@examples[#:label ""
   (eval:alts
    (unsyntax
     (elem
      (racket класс объект%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket поля (минестроне 'готово))
      (linebreak) (hspace 4)
      (racket ((внутреннее-рагу рагу) 'тушится))))
    (void))]}

@defform[#:kind "синтаксис" (унаследованные-поля возможно-переименованное ...)]{
См. @racket[класс*] и @racket[класс] (поля);
использование вне тела @racket[класс] или @racket[класс*] — ошибка синтаксиса.
@examples[#:label ""
   (eval:alts
    (unsyntax
     (elem
      (racket книга-рецептов% = класс объект%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket поля (рецепты '(суп яичница)) (страницы 389))))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (racket класс книга-рецептов%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket унаследованные-поля рецепты (внутренние-страницы страницы))))
    (void))]}

@defform[#:kind "синтаксис" (методы возможно-переименованное ...)]{
См. @racket[класс*] и @racket[класс] (методы);
использование вне тела @racket[класс] или @racket[класс*] — ошибка синтаксиса.
@examples[#:label ""
   (eval:alts
    (unsyntax
     (elem
      (racket прыгун% = класс объект%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket пропуск) (racket ()) (hspace 1) (racketidfont "=") (hspace 1) (racket 'пропуск)
      (linebreak) (hspace 2)
      (racket подскок) (racket ()) (hspace 1) (racketidfont "=") (hspace 1) (racket 'подскок)
      (linebreak) (hspace 2)
      (racket методы пропуск (подскок прыжок))))
    (void))
   (eval:alts
    (unsyntax
     (elem (racket объект) (racket (прыгун%)) (racket{пропуск})))
    'пропуск)
   (eval:alts
    (unsyntax
     (elem (racket объект) (racket (прыгун%)) (racket{прыжок})))
    'подскок)]}

@defform[#:kind "синтаксис" (дополняемые-методы возможно-переименованное ...)]{
См. @racket[класс*] и @racket[класс] (методы);
использование вне тела @racket[класс] или @racket[класс*] — ошибка синтаксиса.
@examples[#:label ""
   (eval:alts
    (unsyntax
     (elem
      (racket бегун% = класс объект%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket бег) (racket ()) (hspace 1) (racketidfont "=") (hspace 1) (racket 'бег)
      (linebreak) (hspace 2)
      (racket рысь) (racket ()) (hspace 1) (racketidfont "=") (hspace 1) (racket 'рысь)
      (linebreak) (hspace 2)
      (racket дополняемые-методы бег (рысь трусца))))
    (void))
   (eval:alts
    (unsyntax
     (elem (racket объект) (racket (бегун%)) (racket{бег})))
    'бег)
   (eval:alts
    (unsyntax
     (elem (racket объект) (racket (бегун%)) (racket{трусца})))
    'рысь)]}

@defform[#:kind "синтаксис" (переопределить возможно-переименованное ...)]{
См. @racket[класс*] и @racket[класс] (методы);
использование вне тела @racket[класс] или @racket[класс*] — ошибка синтаксиса.
@examples[#:label ""
   (eval:alts
    (unsyntax
     (elem
      (racket овца% = класс объект%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket методы блеять)
      (linebreak) (hspace 2)
      (racket блеять) (racket ()) (hspace 1) (racketidfont "=")
      (hspace 1) (racket вывести/перенос) (racket («бе-е-е»))))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (racket растерянная-овца% = класс овца%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket блеять) (racket ()) (hspace 1) (racketidfont "=")
      (linebreak) (hspace 4)
      (racket базовый блеять)
      (linebreak) (hspace 4)
      (racket вывести/перенос) (racket («???»))
      (linebreak) (hspace 2)
      (racket переопределить блеять)))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (elem (racket объект) (racket (овца%)) (racket{блеять}))
      (linebreak)
      (racketoutput "бе-е-е")))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (elem (racket объект) (racket (растерянная-овца%)) (racket{блеять}))
      (linebreak)
      (racketoutput "бе-е-е\n???")))
    (void))]}

@defform[#:kind "синтаксис" (дополнить возможно-переименованное ...)]{
См. @racket[класс*] и @racket[класс] (методы);
использование вне тела @racket[класс] или @racket[класс*] — ошибка синтаксиса.
@examples[#:label ""
   (eval:alts
    (unsyntax
     (elem
      (racket зуммер% = класс объект%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket дополняемые-методы жужжать)
      (linebreak) (hspace 2)
      (racket жужжать) (racket ()) (hspace 1) (racketidfont "=")
      (linebreak) (hspace 4)
      (racket вывести/перенос) (racket («жжжт»))
      (linebreak) (hspace 4)
      (elem (racket производный) (racket (пусто жужжать)))))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (racket громкий-зуммер% = класс зуммер%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket жужжать) (racket ()) (hspace 1) (racketidfont "=")
      (hspace 1) (racket вывести/перенос) (racket («ЖЖЖЖТ»))
      (linebreak) (hspace 2)
      (racket дополнить жужжать)))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (elem (racket объект) (racket (зуммер%)) (racket{жужжать}))
      (linebreak)
      (racketoutput "жжжт")))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (elem (racket объект) (racket (громкий-зуммер%)) (racket{жужжать}))
      (linebreak)
      (racketoutput "жжжт\nЖЖЖЖТ")))
    (void))]}

@defform[#:kind "синтаксис" (абстрактные идентификатор ...)]{
См. @racket[класс*] и @racket[класс] (методы);
использование вне тела @racket[класс] или @racket[класс*] — ошибка синтаксиса.
@examples[#:label ""
   (eval:alts
    (unsyntax
     (elem
      (racket поезд% = класс объект%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket абстрактные скорость)
      (linebreak) (hspace 2)
      (racket инициализированные-поля (положение 0))
      (linebreak) (hspace 2)
      (racket методы ехать)
      (linebreak) (hspace 2)
      (racket ехать) (racket ()) (hspace 1) (racketidfont "=")
      (linebreak) (hspace 4)
      (elem
       (racket объект)
       (hspace 1)
       (racket этот%)
       (hspace 1)
       (racket положение)
       (racketparenfont "(")
       (elem (racket положение) (hspace 1) (racket +) (hspace 1) (racket скорость) (racket ()))
       (racketparenfont ")"))))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (racket скорый% = класс поезд%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket скорость) (racket ()) (hspace 1) (racketidfont "=") (hspace 1) (racket 241)
      (linebreak) (hspace 2)
      (racket переопределить скорость)))
    (void))
   (eval:alts
    (unsyntax
     (elem (racket объект) (racket (поезд%))))
    (void))
   (eval:alts
    (eval:no-prompt
     (unsyntax (elem (racketerror "объект: нельзя создать объект класса
 с нереализованными методами
  класс: #<класс:поезд%>
  методы:
   скорость"))))
    (void))
   (eval:alts
    (unsyntax
     (elem (racket объект) (racket (скорый%)) (racket{ехать})))
    (eval:result (racketvalfont "(объект:скорый% ...)")))]}

@defform[#:kind "синтаксис" (унаследованные возможно-переименованное ...)]{
См. @racket[класс*] и @racket[класс] (методы);
использование вне тела @racket[класс] или @racket[класс*] — ошибка синтаксиса.
@examples[#:label ""
   (eval:alts
    (unsyntax
     (elem
      (racket сирена% = класс объект%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket методы сирена)
      (linebreak) (hspace 2)
      (racket сирена) (racket ()) (hspace 1) (racketidfont "=")
      (hspace 1) (racket вывести/перенос) (racket («бииип»))))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (racket автомобильная-сирена% = класс сирена%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket инициализированные-поля близость)
      (linebreak) (hspace 2)
      (racket унаследованные сирена)
      (linebreak) (hspace 2)
      (elem (racket если близость < 10 тогда) (hspace 1) (racket сирена) (racket ()))))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (elem (racket объект автомобильная-сирена% близость) (racket (5)))
      (linebreak)
      (racketoutput "бииип")))
    (eval:result (racketvalfont "(объект:автомобильная-сирена% ...)")))]}

@defform[#:kind "синтаксис" (базовый-объект (идентификатор выражение) ...)]{
Вызывает инициализацию базового класса с указанными именными аргументами.
См. @racket[класс*] и @racket[объект]. Использование вне тела
@racket[класс] или @racket[класс*] — ошибка синтаксиса.}

@defform[#:kind "синтаксис" #:id создать-базовый-объект создать-базовый-объект]{
Возвращает функцию, которая принимает позиционные аргументы и вызывает
инициализацию базового класса. См. @racket[класс*] и @racket[создать-объект].
Использование вне тела @racket[класс] или @racket[класс*] — ошибка синтаксиса.}

@defform[#:kind "синтаксис"
         (интерфейс (выражение-базового-интерфейса ...) имя ...)]{
Возвращает интерфейс. Интерфейс — набор имён методов, которые должен
реализовать класс, вместе с требованием порождения. Класс реализует
интерфейс, когда он

@itemize[
 @item{объявляет (или наследует) открытый метод для каждого имени
  интерфейса;}
 @item{порождён от класса, которого требует интерфейс, если такое
  требование есть;}
 @item{явно указывает намерение реализовать интерфейс.}
]

Класс может реализовывать любое число интерфейсов. Производный класс
автоматически реализует все интерфейсы своего базового класса.
Каждый класс также реализует неявно определённый интерфейс со всеми
своими открытыми методами; этот интерфейс требует, чтобы все прочие
его реализации порождались от данного класса.

Новый интерфейс может расширять один или несколько интерфейсов
дополнительными именами методов; каждый класс, реализующий расширенный
интерфейс, реализует и исходные. Требования порождения исходных
интерфейсов должны быть согласованы; расширенный интерфейс наследует
наиболее конкретное требование.

@racket[имя] должны быть попарно различны.

Каждое @racket[выражение-базового-интерфейса] вычисляется по порядку
при вычислении команды @racket[интерфейс]. Результат каждого должен
быть значением интерфейса, иначе вызывается исключение. Эти интерфейсы
становятся базовыми для нового: новый их расширяет. Любой класс,
реализующий новый интерфейс, реализует и все базовые.

Результат включает все указанные @racket[имя], а также все имена
из базовых интерфейсов. Повторяющиеся имена среди базовых интерфейсов
игнорируются.

Если выражения базовых интерфейсов не заданы, требование порождения
тривиально: реализующий класс должен быть порождён от @racket[объект%].
Иначе требование — наиболее конкретное из требований базовых интерфейсов.
Если требования несогласованы, вызывается исключение.

Классы, объекты и интерфейсы — значения. Однако класс или интерфейс
не является объектом.}

@defthing[#:kind "класс" объект% любой]{
Встроенный класс без методов и полей. Реализует только свой собственный
интерфейс. Непосредственные экземпляры считаются равными в смысле
@racket[==]. Все остальные классы порождаются от @racket[объект%].}

@defform[#:kind "синтаксис"
         (объект выражение-класса (идентификатор выражение) ...)]{
Создаёт экземпляр значения @racket[выражение-класса] (оно должно дать
класс). Значение каждого @racket[выражение] передаётся как именной
аргумент инициализации с соответствующим @racket[идентификатор].

Все поля нового объекта сначала не определены. Переменные инициализации
с выражениями по умолчанию (если значение не передано) тоже сначала
не определены. После подстановки аргументов вычисляются выражения
в командах @racket[поля], в @racket[инициализированные-поля]
и @racket[инициализировать] без переданного аргумента, определения
закрытых полей и прочие выражения — в порядке записи в классе.

Во время этих вычислений инициализация, объявленная базовым классом,
должна быть выполнена ровно один раз формой @racket[базовый-объект]
или @racket[создать-базовый-объект]. Если до конца инициализации
какого-либо класса в иерархии базовый класс так и не инициализирован,
вызывается исключение. Повторный вызов инициализации базового класса
тоже вызывает исключение.

Именные аргументы, для которых в классе нет переменной инициализации,
неявно добавляются к вызову инициализации базового класса после явных
аргументов. Если для одного имени передано несколько аргументов,
используется первый, остальные передаются базовому классу.
Инициализация @racket[объект%] не принимает аргументов: лишние именные
аргументы вызывают исключение.

Унаследованные поля инициализируются только при вызове инициализации
базового класса. Методы доступны сразу после создания объекта;
переопределение методов не зависит от хода инициализации.}

@defform[#:kind "синтаксис" #:id этот этот]{
Внутри команды @racket[класс] или @racket[класс*] обозначает текущий
объект: тот, который инициализируется, или тот, чей метод вызван.
Использование вне тела @racket[класс] или @racket[класс*] — ошибка
синтаксиса.}

@defform[#:kind "синтаксис" #:id этот% этот%]{
Внутри команды @racket[класс] или @racket[класс*] обозначает класс
текущего объекта: того, который инициализируется, или того, чей метод
вызван. Использование вне тела @racket[класс] или @racket[класс*] —
ошибка синтаксиса.
@examples[#:label ""
   (eval:alts
    (unsyntax
     (elem
      (racket счёт% = класс объект%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket инициализированные-поля баланс)
      (linebreak) (hspace 2)
      (racket методы добавить)
      (linebreak) (hspace 2)
      (racket добавить n =)
      (linebreak) (hspace 4)
      (elem
       (racket объект)
       (hspace 1)
       (racket этот%)
       (hspace 1)
       (racket баланс)
       (racket (n + баланс)))))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (racket вклад% = класс счёт%)
      (linebreak) (hspace 2)
      (racket базовый-объект) (racket ())
      (linebreak) (hspace 2)
      (racket унаследованные-поля баланс)
      (linebreak) (hspace 2)
      (racket процент = 0.04)
      (linebreak) (hspace 2)
      (racket методы начислить-процент)
      (linebreak) (hspace 2)
      (racket начислить-процент) (racket ()) (hspace 1) (racketidfont "=")
      (linebreak) (hspace 4)
      (elem (racket этот) (racket{добавить процент * баланс}))))
    (void))
   (eval:alts
    (unsyntax
     (elem
      (racket счёт = объект вклад% баланс) (racket (500))))
    (void))
   (eval:alts
    (unsyntax
     (elem (racket счёт) (racket{добавить 500})))
    (void))
   (eval:alts
    (unsyntax
     (elem (racket счёт) (racket{начислить-процент})))
    (void))
   (eval:alts
    (unsyntax
     (elem (racket поле баланс счёт)))
    1040.0)]}

@defform*[#:kind "синтаксис" #:id базовый
          ((базовый идентификатор аргумент ...)
           (базовый идентификатор аргумент ... @#,racketparenfont{.} выражение-списка-аргументов))]{
Всегда вызывает метод базового класса (или базового интерфейса) с именем
@racket[идентификатор], независимо от того, переопределён ли метод
в производных классах. Использование вне тела @racket[класс] или
@racket[класс*] — ошибка синтаксиса.

Вторая форма передаёт дополнительные аргументы списком, как
@racket[применить]; @racket[выражение-списка-аргументов] не должно быть
выражением в скобках.

Если метод объявлен командой @racket[переопределить], реализацию
базового класса можно вызвать через @racket[базовый]. Если несколько
базовых интерфейсов дают разные реализации переопределяемого метода,
при вычислении @racket[базовый] вызывается исключение.}

@defform*[#:kind "синтаксис" #:id производный
          ((производный выражение-по-умолчанию идентификатор аргумент ...)
           (производный выражение-по-умолчанию идентификатор аргумент ...
                        @#,racketparenfont{.} выражение-списка-аргументов))]{
Если класс объекта не задаёт дополняющий метод с именем
@racket[идентификатор], вычисляется @racket[выражение-по-умолчанию],
а выражения аргументов не вычисляются. Иначе вызывается дополняющий
метод с результатами аргументов, а @racket[выражение-по-умолчанию]
не вычисляется. Если для метода ни разу не вычислить @racket[производный],
дополнения из производных классов не используются. Использование вне
тела @racket[класс] или @racket[класс*] — ошибка синтаксиса.

Вторая форма передаёт дополнительные аргументы списком, как
@racket[применить]; @racket[выражение-списка-аргументов] не должно быть
выражением в скобках.

Если метод объявлен командой @racket[дополняемые-методы] или
@racket[дополнить], дополнение из производного класса вызывается
через @racket[производный].}

@defform[#:kind "синтаксис"
         (поле идентификатор выражение-объекта)]{
Возвращает значение поля с внешним именем @racket[идентификатор]
у значения @racket[выражение-объекта].

Если результат @racket[выражение-объекта] не объект, вызывается
исключение. Если у объекта нет поля @racket[идентификатор],
вызывается исключение.}

@defform[#:kind "синтаксис"
         (установить-поле! идентификатор выражение-объекта выражение)]{
Записывает значение @racket[выражение] в поле с внешним именем
@racket[идентификатор] у значения @racket[выражение-объекта].

Если результат @racket[выражение-объекта] не объект, вызывается
исключение. Если у объекта нет поля @racket[идентификатор],
вызывается исключение.}

@defproc[#:kind "функция" (создать-объект [класс любой] [параметр любой] ...)
         любой]{
Создаёт экземпляр @racket[класс]. Значения @racket[параметр] передаются
как позиционные аргументы инициализации и связываются с переменными
инициализации класса, как описано для команды @racket[класс].
Если @racket[класс] не является классом, вызывается исключение.}

@defproc[#:kind "функция" (объект-класса? [значение любой] [тип любой])
         булево?]{
Возвращает @racketvalfont{истина}, если @racket[значение] — экземпляр
класса @racket[тип] или класса, реализующего интерфейс @racket[тип],
иначе возвращает @racketvalfont{ложь}.}

@defproc[#:kind "функция" (класс? [значение любой])
         булево?]{
Возвращает @racketvalfont{истина}, если @racket[значение] — класс,
иначе возвращает @racketvalfont{ложь}.

Класс — значение, но не объект: для экземпляра класса
@racketvalfont{ложь}, для самого класса @racketvalfont{истина}.}

@defproc[#:kind "функция" (интерфейс? [значение любой])
         булево?]{
Возвращает @racketvalfont{истина}, если @racket[значение] — интерфейс,
иначе возвращает @racketvalfont{ложь}.

Интерфейс — значение, но не объект.}

@subsection[#:tag "logicals"]{Логические выражения}

@defproc[#:kind "функция" (булево? [аргумент любой])
         булево?]{Возвращает @racketvalfont{истина}, если @racket[аргумент]
 @racketvalfont{истина} или @racketvalfont{ложь}, в противном случае возвращает
 @racketvalfont{ложь}.}

@defproc[#:kind "функция" (== [аргумент любой] ...+)
         булево?]{Возвращает @racketvalfont{истина}, если @racket[аргумент]ы
 равны. Списки и массивы считаются равными, если равны их элементы.}

@defproc[#:kind "функция" (=== [аргумент любой] ...+)
         булево?]{Возвращает @racketvalfont{истина}, если @racket[аргумент]ы
 равны. Списки и массивы считаются равными, если являются одним и тем же объектом,
 а не просто состоят из одинаковых элементов.}

@defproc[#:kind "функция" (/= [аргумент любой] ...+)
         булево?]{Возвращает @racketvalfont{ложь}, если @racket[аргумент]ы
 равны в смысле @racket[==].}

@subsection[#:tag "conditionals"]{Условия}

@defform[#:kind "синтаксис" (? условие выражение-если-истина выражение-если-ложь)
         #:contracts ([условие булево?])]{Если @racket[условие] истинно,
 выполняет @racket[выражение-если-истина] иначе выполняет @racket[выражение-если-ложь].
Возвращает результат выполненного выражения.

При использовании как оператор не объединяет в одно выражение слова справа от себя.}

@defform[#:kind "синтаксис" (&& выражение ...)]{Выполняет выражения слева направо, пока
одно из них не вернёт @racketvalfont{ложь} или они не закончатся.
 Возвращает результат последнего выполненного выражения.}

@defform[#:kind "синтаксис" (|| выражение ...)]{Выполняет выражения слева направо, пока
одно из них не вернёт что-то кроме @racketvalfont{ложь} или они не закончатся.
 Возвращает результат последнего выполненного выражения.}

@defform*[#:kind "синтаксис" #:id если
          ((если слова-условия ... тогда команда ... иначе команда ...)
           (если слова-условия ... тогда команда ...)
           (если правило ...)
           (если правило ... (иначе команда ... выражение)))
          #:grammar [(правило (условие команда ... выражение)
                              (выполнить команда ...)
                              (условие => выражение))]]{
 Выполняет выражения по условиям. Если команда создаёт переменную, то эта переменная
имеет область видимости только внутри блока с условием. В конструкции с @racket[=>]
выражение должно возвращать функцию от одного аргумента, в эту функцию будет передан
результат вычисления условия. Правило @racket[выполнить] позволяет выполнить любые команды
перед проверкой следующего условия, в том числе определять переменные, которые можно использовать
в следующих поавилах.}

@subsection[#:tag "symbols"]{Символы}

@defproc[#:kind "функция" (символ? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является символом.}

@defproc[#:kind "функция" (символ->строка [символ символ?])
         строка?]{Возвращает имя символа.}

@defproc[#:kind "функция" (строка->символ [строка строка?])
         символ?]{Возвращает символ с заданным именем. Для одинаковых строк
 возвращает одинаковые символы.}

@defproc[#:kind "функция" (новый-символ)
         символ?]{Возвращает новый символ, который невозможно получить ни из какой строки.
Он выводится с некоторым именем, но не равен ни одному другому, только самому себе.}

@subsection[#:tag "keywords"]{Ключевые слова}

@defproc[#:kind "функция" (ключевое-слово? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является ключевым словом.}

@defproc[#:kind "функция" (строка->ключевое-слово [строка строка?])
         символ?]{Возвращает ключевое слово с заданным именем. Для одинаковых строк
 возвращает одинаковые ключевые слова.}

@subsection[#:tag "numbers"]{Числа}

@defproc[#:kind "функция" (число? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является числом.}

@defproc[#:kind "функция" (точное? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является точным числом.}

@defproc[#:kind "функция" (неточное? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является неточным числом.}

@defproc[#:kind "функция" (целое? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является целым числом.
 Внимание, неточное число тоже может быть целым!}

@defproc[#:kind "функция" (точное-целое? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является точным целым числом.}

@defproc[#:kind "функция" (целое-неотрицательное? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является точным целым
 неотрицательным числом.}

@defproc[#:kind "функция" (вещественное? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является вещественным числом.}

@defproc[#:kind "функция" (рациональное? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является рациональным числом.}

@defproc[#:kind "функция" (комплексное? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является комплексным числом.}

@defproc[#:kind "функция" (округлить [число вещественное?])
         (одно-из целое? +inf.0 -inf.0 +nan.0)]{
 Возвращает целое, ближайшее к аргументу. Если @racket[число] одно из +inf.0, -inf.0 или +nan.0,
 возвращает его же.}

@defproc[#:kind "функция" (корень [число число?])
         число?]{Возвращает главный (для положительных вещественных совпадает с арифметическим)
 квадратный корень из значения аргумента @racket[число].
 Результат точный, если @racket[число] точное и квадратный корень из него рациональный.}

@defproc[#:kind "функция" (синус [число число?])
         число?]{Возвращает синус угла в радианах.}

@defproc[#:kind "функция" (косинус [число число?])
         число?]{Возвращает косинус угла в радианах.}

@defproc[#:kind "функция" (тангенс [число число?])
         число?]{Возвращает тангенс угла в радианах.}

@defproc[#:kind "функция" (арксинус [число число?])
         число?]{Возвращает арксинус в радианах.}

@defproc[#:kind "функция" (арккосинус [число число?])
         число?]{Возвращает арккосинус в радианах.}

@defproc[#:kind "функция" (арктангенс [число число?])
         число?]{Возвращает арктангенс в радианах.}

@defproc[#:kind "функция" (экспонента [число число?])
         число?]{Возвращает число Эйлера (e) в степени @racket[число].}

@defproc[#:kind "функция" (логарифм [число число?] [основание число? (экспонента 1)])
         число?]{Возвращает натуральный логарифм.
 Если передано основание, то возвращает логарифм по этом основанию.}

@defproc[#:kind "функция" (неточное->точное [число число?])
         точное?]{Преобразовывает число в точное. Если @racket[число] одно из +inf.0, -inf.0,
 +nan.0, +inf.f, -inf.f или +nan.f, тогда вызывается исключение.}

@defproc[#:kind "функция" (точное->неточное [число число?])
         неточное?]{Преобразовывает число в неточное.}

@defproc[#:kind "функция" (небольшое-число? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является небольшим числом.
Вычисления с небольшими числами выполняются быстрее.}

@defproc[#:kind "функция" (абс [число вещественное?])
         вещественное?]{Возвращает абсолютное значение аргумента.}

@defproc[#:kind "функция" (строка->число [строка строка?] [основание (одно-из 2 8 10 16) 10])
         (одно-из число? #,(elem (racketvalfont "ложь")))]{
Возвращает число из строкового представления числа или ложь,
 если это не число.}

@defproc[#:kind "функция" (число->строка [число число?] [основание (одно-из 2 8 10 16) 10])
         строка?]{Возвращает строковое представление числа.}

@subsection[#:tag "characters"]{Литеры}

@defproc[#:kind "функция" (литера? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является литерой.}

@defproc[#:kind "функция" (литера->число [аргумент литера?])
         точное-целое?]{Возвращает код литеры.}

@defproc[#:kind "функция" (число->литера [аргумент точное-целое?])
         литера?]{Возвращает литеру по коду.}

@subsection[#:tag "lists"]{Списки}

@defproc[#:kind "функция" (список? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является списком.
Любой список также является парой.}

@defproc[#:kind "функция" (пустой? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является пустым списком.}

@defthing[#:kind "константа" пустой-список пустой?]{Пустой список.}

@defproc[#:kind "функция" (список [аргумент любой] ...)
         список?]{Возвращает список из произвольных значений.}

@defproc[#:kind "функция" (пара? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является парой.}

@defproc[#:kind "функция" (пара [аргумент1 любой] [аргумент2 любой])
         пара?]{Возвращает пару из переданных аргументов. Если второй аргумент список,
 то возвращаемое значение тоже список.}

@defproc[#:kind "функция" (: [аргумент1 любой] [аргумент2 любой])
         пара?]{Аналогично функции @racket[пара] возвращает пару из переданных аргументов.
 Если второй аргумент список, то возвращаемое значение тоже список.}

@defproc[#:kind "функция" (первый [аргумент пара?])
         любой]{Возвращает первый элемент пары. Если пара является списком, то он же первый
элемент списка.}

@defproc[#:kind "функция" (оставшиеся [аргумент пара?])
         любой]{Возвращает второй элемент пары. Если пара является списком, то это список из
 всех элементов кроме первого (то есть «оставшиеся» элементы списка).}

@defproc[#:kind "функция" (длина [аргумент список?]) число?]{Возвращает
 количество элементов списка.}

@defproc[#:kind "функция" (развернуть [аргумент список?]) список?]{Возвращает
 список из значений аргумента в обратном порядке.}

@defproc[#:kind "функция" (элемент-списка [аргумент список?] [позиция число?]) любой]{Возвращает
 элемент списка в указанной позиции. Нумерация позиций начинается с нуля.}

@defproc[#:kind "функция" (подсписок [значение любой] [аргумент список?]) любой]{Если
в списке @racket[аргумент] есть элемент @racket[значение], то возвращает хвост списка,
начиная с этого элемента. Если нет, возвращает @racketvalfont{ложь}.

Сравнение элементов с значением происходит при помощи @racket[==].

Если значение есть, то @racket[аргумент] может быть не совсем списком.
 Достаточно, чтобы он начинался с цепочки пар, в которой есть искомый элемент. В этом случае
результат будет не списком, а тем, что является вторым значением в той паре, где первое значение
 совпало с элементом.}

@defproc[#:kind "функция" (подсписок=== [значение любой] [аргумент список?]) любой]{Функция
 полностью аналогична функции @racket[подсписок] за исключением того, что для сравнения используется
 @racket[===].}

@defproc[#:kind "функция" (отобрать [обработчик функция?] [аргумент список?]) список?]{Применяет
@racket[обработчик] к элементам переданного списка. Возвращает список элементов, для которых
@racket[обработчик] вернул не @racketvalfont{ложь}.}

@defproc[#:kind "функция" (отобразить [обработчик функция?] [аргумент список?] ...) список?]{Применяет
@racket[обработчик] к элементам переданных списков. Функция @racket[обработчик] должна принимать
 столько аргументов, сколько передано списков и все списки должны иметь одинаковое количество
 элементов. Возвращает список результатов.

Вызов @racketparenfont{(}@racket[отобразить ф список]@racket[(а б в)]@racketparenfont{)}
 аналогичен @racketparenfont{(}@racket[список ф]@racket[(а)] @racket[ф]@racket[(б)]
 @racket[ф]@racket[(в)]@racketparenfont{)}..}

@defproc[#:kind "функция" (отобразить/и [обработчик функция?] [аргумент список?] ...) любой]{
 Аналогична @racket[отобразить], но возвращает значение последнего вызова обработчика.
 Если результат обработки @racketvalfont{ложь}, то дальнейшие элементы не обрабатываются.

Вызов @racketparenfont{(}@racket[отобразить/и ф список]@racket[(а б в)]@racketparenfont{)}
 аналогичен @racketparenfont{(}@racket[&& ф]@racket[(а)] @racket[ф]@racket[(б)]
 @racket[ф]@racket[(в)]@racketparenfont{)}.}

@defproc[#:kind "функция" (отобразить/или [обработчик функция?] [аргумент список?] ...) любой]{
 Аналогична @racket[отобразить], но возвращает значение последнего вызова обработчика.
 Если результат обработки не равен @racketvalfont{ложь}, то дальнейшие элементы не обрабатываются.

Вызов @racketparenfont{(}@racket[отобразить/или ф список]@racket[(а б в)]@racketparenfont{)}
 аналогичен @racketparenfont{(}@racket[|| ф]@racket[(а)] @racket[ф]@racket[(б)]
 @racket[ф]@racket[(в)]@racketparenfont{)}.}

@defproc[#:kind "функция" (свернуть [обработчик функция?] [начальное любое]
                                    [аргумент список?] ...) список?]{
Как @racket[отобразить], @racket[свернуть] применяет функцию поочередно к элементам переданных
 списков, но если @racket[отобразить] комбинирует результаты в список, то @racket[свернуть]
 позволяет из скомбинировать результаты произвольным образом, определяемым переданным обработчиком.

Если @racket[свернуть] вызывается с @racket[n] списками, то @racket[обработчик] должен принимать
 @racket[n]+1 аргументов. Последний аргумент получает результат предыдущего вызова
 @racket[обработчик]а, при первом вызове получает значение аргумента  @racket[начальное].
 Результатом функции @racket[свернуть] является последний результат вызова @racket[обработчик]а.}

@defproc*[#:kind "функция"
          ([(добавить [аргумент список?] ...) список?]
           [(добавить [аргумент список?] ... [последний-аргумент любой]) любой])]{Возвращает сцепку
 переданных аргументов. Если все аргументы списки, тогда результатом является список,
 содержащий все элементы аргументов по порядку. Последний аргумент используется напрямую
 в хвостовой позиции.

 Если последний аргумент не список, он всё равно используется в хвостовой позиции.

 Если передан всего один аргумент, он возвращается как есть. Если передано ноль аргументов,
 возвращается пустой список.

 Время выполнения пропорционально сумме длин аргументов кроме последнего.

@examples[
 #:label "Примеры:"
 (eval:alts
  (unsyntax (elem (racket добавить список) (racket (1 2)) (hspace 1) (racket список) (racket (3 4))))
  (append (list 1 2) (list 3 4)))
 (eval:alts
  (unsyntax (elem (racket добавить список) (racket (1 2)) (hspace 1) (racket 3)))
  (append (list 1 2) 3))
 (eval:alts
  (unsyntax (elem (racket добавить 4)))
  (append 4))]}

@defproc[#:kind "функция" (++ [аргумент (одно-из список? строка? массив? байты?)] ...)
         (одно-из список? строка? массив? байты?)]{Возвращает сцепку переданных аргументов.
Создаётся новая изменяемая коллекция достаточного размера для всех элементов аргументов,
затем все элементы всех аргументов последовательно копируются в новую коллекцию.
 Тип аргументов должен быть одинаковый.}

@defproc[#:kind "функция" (ассоциация [значение любой] [список список?]
                                      [равенство (любой любой . -> . любой) ==])
         (одно-из пара? #,(elem (racketvalfont "ложь")))]{Считает, что @racket[список] начинается со
 списка пар. Ищет среди них первую, для которой @racket[первый] элемент равен
 аргументу @racket[значение] в том смысле, что @racket[равенство] возвращает не @racketvalfont{ложь}.
 Возвращает найденную пару.
 Если таковой нет, то весь @racket[список] должен состоять из пар и @racket[ассоциация]
 вернёт @racketvalfont{ложь}.}

@@subsection[#:tag "arrays"]{Массивы}

@defproc[#:kind "функция" (массив? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является массивом.}

@defproc[#:kind "функция" (массив [аргумент любой] ...)
         список?]{Возвращает массив из произвольных значений.}

@defproc[#:kind "функция" (длина-массива [массив массив?]) число?]{Возвращает
 количество элементов массива.}

@defproc[#:kind "функция" (элемент-массива [массив массив?] [позиция целое-неотрицательное?])
         литера?]{Возвращает значение на заданной позиции. Позиции нумеруются с нуля.}

@defproc[#:kind "функция" (установить-элемент-массива! [массив массив?]
                                                       [позиция целое-неотрицательное?]
                                                       [значение любой])
         пусто?]{Устанавливает значение элемента на заданной позиции. Позиции нумеруются с нуля.}

@defproc[#:kind "функция" (массив->список [массив массив?])
         список?]{Возвращает список из значений массива.}

@defproc[#:kind "функция" (список->массив [список список?])
         список?]{Возвращает массив из значений списка.}

@subsection[#:tag "hashs"]{Соответствия}

@defproc[#:kind "функция" (соответствие? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является соответствием.}

@defproc[#:kind "функция" (соответствие===? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является соответствием, ключи
 которого сравниваются @racket[===].}

@defproc[#:kind "функция" (соответствие [ключ любой] [значение любой] ...)
         соответствие?]{Возвращает неизменяемое соответствие из произвольных значений.}

@defproc[#:kind "функция" (соответствие=== [ключ любой] [значение любой] ...)
         соответствие?]{Возвращает неизменяемое соответствие, ключи которого сравниваются
 @racket[===], из произвольных значений.}

@defproc[#:kind "функция" (новое-соответствие [список-пар (список-из пара?) пустой-список])
         соответствие?]{Возвращает новое соответствие. Если список пар не пуст, то заполняет
созданное соответствие ключами и значениями из него.}

@defproc[#:kind "функция" (новое-соответствие=== [список-пар (список-из пара?) пустой-список])
         соответствие?]{Возвращает новое соответствие, ключи которого сравниваются @racket[===].
 Если список пар не пуст, то заполняет созданное соответствие ключами и значениями из него.}

@defproc[#:kind "функция" (значение-соответствия [соответствие соответствие?] [ключ любой]
                                                 [не-найден любой ошибка-нет-ключа])
         любой]{Возвращает значение для заданного ключа. Если ключа в соответствии нет,
то используется значение @racket[не-найден]: если это функция, она выполняется и возвращается
её значение, иначе возвращается само значение @racket[не-найден].}

@defproc[#:kind "функция" (установить-значение-соответствия! [соответствие соответствие?]
                                                       [ключ любой]
                                                       [значение любой])
         пусто?]{Устанавливает значение соответствия для заданного ключа.}

@subsection[#:tag "strings"]{Строки}

@defproc[#:kind "функция" (строка? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является строкой.}

@defproc[#:kind "функция" (новая-строка [длина целое-неотрицательное?]
                                        [литера литера? (число->литера 0)])
         строка?]{Создаёт строку заданной длины и заполняет её указанным значением аргумента
 @racket[литера]. Если литера не указана, заполняет литерой с нулевым кодом, то есть
 @racketvalfont{#\пусто}.}

@defproc[#:kind "функция" (длина-строки [строка строка?])
         целое-неотрицательное?]{Возвращает длину строки в литерах.}

@defproc[#:kind "функция" (элемент-строки [строка строка?] [позиция целое-неотрицательное?])
         литера?]{Возвращает литеру на заданной позиции. Позиции нумеруются с нуля.}

@defproc[#:kind "функция" (установить-элемент-строки! [строка строка?]
                                                      [позиция целое-неотрицательное?]
                                                      [литера литера?])
         пусто?]{Устанавливает литеру на заданной позиции. Позиции нумеруются с нуля.}

@defproc[#:kind "функция" (подстрока [строка строка?] [начало целое-неотрицательное?]
                                     [конец целое-неотрицательное? (длина-строки строка)])
         строка?]{Возвращает подстроку из аргумента @racket[строка] с позиции @racket[начало]
 по позицию @racket[конец].}

@defproc[#:kind "функция" (добавить-строки [строка строка?] ...)
         строка?]{Возвращает сцепку строк.
Создаётся новая изменяемая строка достаточного размера,
затем все литеры всех строк последовательно копируются в новую.}

@defproc[#:kind "функция" (прописные  [строка строка?])
         строка?]{Возвращает строку, в которой все литеры заменены на прописные.}

@defproc[#:kind "функция" (строчные  [строка строка?])
         строка?]{Возвращает строку, в которой все литеры заменены на строчные.}

@defproc[#:kind "функция" (строки-равны? [строка строка?] ...)
         строка?]{Возвращает истину, если все строки равны.}

@defproc[#:kind "функция" (строки-возрастают? [строка строка?] ...)
         строка?]{Возвращает истину, если строки возрастают в лексикографическом (алфавитном)
 порядке.}

@defproc[#:kind "функция" (строки-не-убывают? [строка строка?] ...)
         строка?]{Возвращает истину, если каждая следующая строка равна или больше предыдущей
 в лексикографическом (алфавитном) порядке.}

@defproc[#:kind "функция" (строки-убывают? [строка строка?] ...)
         строка?]{Возвращает истину, если строки убывают
 в лексикографическом (алфавитном) порядке.}

@defproc[#:kind "функция" (строки-не-возрастают? [строка строка?] ...)
         строка?]{Возвращает истину, если каждая следующая строка равна или меньше предыдущей
 в лексикографическом (алфавитном) порядке.}

@defproc[#:kind "функция" (строки-равны?/без-регистра [строка строка?] ...)
         строка?]{Возвращает истину, если все строки равны без учёта регистра.}

@defproc[#:kind "функция" (строки-возрастают?/без-регистра [строка строка?] ...)
         строка?]{Возвращает истину, если строки возрастают в лексикографическом (алфавитном)
 порядке без учёта регистра.}

@defproc[#:kind "функция" (строки-не-убывают?/без-регистра [строка строка?] ...)
         строка?]{Возвращает истину, если каждая следующая строка равна или больше предыдущей
 в лексикографическом (алфавитном) порядке без учёта регистра.}

@defproc[#:kind "функция" (строки-убывают?/без-регистра [строка строка?] ...)
         строка?]{Возвращает истину, если строки убывают
 в лексикографическом (алфавитном) порядке без учёта регистра.}

@defproc[#:kind "функция" (строки-не-возрастают?/без-регистра [строка строка?] ...)
         строка?]{Возвращает истину, если каждая следующая строка равна или меньше предыдущей
 в лексикографическом (алфавитном) порядке без учёта регистра.}

@defproc[#:kind "функция" (прописные/местные [строка строка?])
         строка?]{Возвращает строку, в которой все литеры заменены на прописные
 с учётом региональных настроек.}

@defproc[#:kind "функция" (строчные/местные [строка строка?])
         строка?]{Возвращает строку, в которой все литеры заменены на строчные
 с учётом региональных настроек.}

@defproc[#:kind "функция" (строки-равны?/местные [строка строка?] ...)
         строка?]{Возвращает истину, если все строки равны с учётом региональных настроек.}

@defproc[#:kind "функция" (строки-возрастают?/местные [строка строка?] ...)
         строка?]{Возвращает истину, если строки возрастают в лексикографическом (алфавитном)
 порядке с учётом региональных настроек.}

@defproc[#:kind "функция" (строки-убывают?/местные [строка строка?] ...)
         строка?]{Возвращает истину, если строки убывают
 в лексикографическом (алфавитном) порядке с учётом региональных настроек.}

@defproc[#:kind "функция" (строки-равны?/местные/без-регистра [строка строка?] ...)
         строка?]{Возвращает истину, если все строки равны с учётом региональных настроек
 без учёта регистра.}

@defproc[#:kind "функция" (строки-возрастают?/местные/без-регистра [строка строка?] ...)
         строка?]{Возвращает истину, если строки возрастают в лексикографическом (алфавитном)
 порядке с учётом региональных настроек без учёта регистра.}

@defproc[#:kind "функция" (строки-убывают?/местные/без-регистра [строка строка?] ...)
         строка?]{Возвращает истину, если строки убывают
 в лексикографическом (алфавитном) порядке с учётом региональных настроек без учёта регистра.}

Следующие функции доступны только при использовании модуля @racket[строка].

@defproc[#:kind "функция" (строка-начинается-с? [строка строка?] [подстрока строка?])
         булево?]{Возвращает истину, если @racket[строка] начинается с литер
 в аргументе @racket[подстрока].}

@defproc[#:kind "функция" (строка-заканчивается-на? [строка строка?] [подстрока строка?])
         булево?]{Возвращает истину, если @racket[строка] заканчивается на литеры
 в аргументе @racket[подстрока].}

@subsection[#:tag "regexps"]{Регулярные выражения}

@defproc[#:kind "функция" (регулярное-выражение [строка (один-из строка? байты?)])
         любой]{Компилирует @racket[строка] в регулярное выражение. Для обычной строки
используется синтаксис egrep, для байтовой строки --- байтовое регулярное выражение.
Литералы @racket[#rx"..."] и @racket[#rx#"..."] задают уже скомпилированное выражение
того же вида.}

@defproc[#:kind "функция" (расширенное-регулярное-выражение [строка (один-из строка? байты?)])
         любой]{Компилирует @racket[строка] в регулярное выражение с расширенным синтаксисом
(как в Perl). Для байтовой строки получается байтовое регулярное выражение. Литералы
@racket[#px"..."] и @racket[#px#"..."] задают уже скомпилированное выражение того же вида.}

@defproc[#:kind "функция" (совпадение-с-регулярным-выражением [регулярное-выражение любой]
                                                        [строка (один-из строка? байты?)])
         любой]{Ищет первое вхождение @racket[регулярное-выражение] в @racket[строка].
При успехе возвращает список: полное совпадение, затем фрагменты, соответствующие
скобочным подвыражениям; иначе @racketvalfont{ложь}.}

@defproc[#:kind "функция" (совпадения-с-регулярным-выражением [регулярное-выражение любой]
                                                         [строка (один-из строка? байты?)])
         (список-из (список-из (один-из строка? байты?)))]{Ищет все неперекрывающиеся
вхождения @racket[регулярное-выражение] в @racket[строка] и возвращает список результатов
в том же формате, что @racket[совпадение-с-регулярным-выражением].}

@defproc[#:kind "функция" (совпадает-с-регулярным-выражением? [регулярное-выражение любой]
                                                          [строка (один-из строка? байты?)])
         булево?]{Возвращает @racketvalfont{истина}, если @racket[регулярное-выражение]
встречается в @racket[строка].}

@defproc[#:kind "функция" (заменить-по-регулярному-выражению [регулярное-выражение любой]
                                                      [строка (один-из строка? байты?)]
                                                      [замена (один-из строка? байты?)])
         (один-из строка? байты?)]{Заменяет первое вхождение @racket[регулярное-выражение]
в @racket[строка] на @racket[замена] и возвращает новую строку или байтовую строку.
В @racket[замена] можно использовать @racket["\\1"], @racket["\\2"] и т. д. для ссылок
на скобочные подвыражения.}

@defproc[#:kind "функция" (заменить-по-регулярному-выражению/все [регулярное-выражение любой]
                                                             [строка (один-из строка? байты?)]
                                                             [замена (один-из строка? байты?)])
         (один-из строка? байты?)]{Как @racket[заменить-по-регулярному-выражению], но
заменяет все неперекрывающиеся вхождения.}

@defproc[#:kind "функция" (разделить-по-регулярному-выражению [регулярное-выражение любой]
                                                     [строка строка?])
         (список-из строка?)]{Разбивает @racket[строка] на части по вхождениям
@racket[регулярное-выражение] и возвращает список подстрок. Между двумя соседними
совпадениями в списке может оказаться пустая строка.}

@subsection[#:tag "bytes"]{Байты}

@defproc[#:kind "функция" (байт? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является целым точным числом
 в диапазоне 0..255.}

@defproc[#:kind "функция" (байты? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является байтовой строкой.}

@defproc[#:kind "функция" (новые-байты [длина целое-неотрицательное?]
                                       [значение байт? 0])
         байты?]{Создаёт строку заданной длины и заполняет её указанным значением аргумента
 @racket[значение]. Если @racket[значение] не указано, заполняет числом 0.}

@defproc[#:kind "функция" (длина-байтов [байты байты?])
         целое-неотрицательное?]{Возвращает длину байтовой строки в байтах.}

@defproc[#:kind "функция" (элемент-байтов [байты байты?] [позиция целое-неотрицательное?])
         байт?]{Возвращает число на заданной позиции. Позиции нумеруются с нуля.}

@defproc[#:kind "функция" (установить-элемент-байтов! [байты байты?]
                                                      [позиция целое-неотрицательное?]
                                                      [байт байт?])
         пусто?]{Устанавливает число на заданной позиции. Позиции нумеруются с нуля.}

@defproc[#:kind "функция" (байты->строка [байты байты?]
                                         [литера-ошибки
                                          (одно-из литера? #,(elem (racketvalfont "ложь")))
                                          #,(elem (racketvalfont "ложь"))]
                                         [начало целое-неотрицательное? 0]
                                         [конец целое-неотрицательное? (длина-байтов байты)])
         строка?]{Преобразует отрезок байтов в строку, трактуя байты в кодировке UTF-8. Если
@racket[литера-ошибки] не ложь, то она подставляется вместо байтов, не являющихся частью
корректной последовательности, иначе вызывается исключение.}

@defproc[#:kind "функция" (байты->строка/местные
                           [байты байты?]
                           [литера-ошибки
                            (одно-из литера? #,(elem (racketvalfont "ложь")))
                            #,(elem (racketvalfont "ложь"))]
                           [начало целое-неотрицательное? 0]
                           [конец целое-неотрицательное? (длина-байтов байты)])
         строка?]{Преобразует отрезок байтов в строку, трактуя байты в кодировке
региональных настроек. Если @racket[литера-ошибки] не ложь, то она подставляется
вместо байтов, не являющихся частью корректной последовательности, иначе вызывается исключение.}

@subsection[#:tag "inout"]{Ввод-вывод}

@defproc[#:kind "функция" (порт-вывода? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является портом вывода.}

@defproc[#:kind "функция" (порт-ввода? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является портом ввода.}

@defproc[#:kind "функция" (порт? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является портом ввода или вывода.}

@defproc[#:kind "функция" (написать [аргумент любой] [вывод порт? (текущий-порт-вывода)])
         пусто?]{Выводит значение @racket[аргумент]а в @racket[вывод] таким образом,
 чтобы результат можно было прочитать обратно.}

@defproc[#:kind "функция" (вывести [аргумент любой] [вывод порт? (текущий-порт-вывода)])
         пусто?]{Выводит значение @racket[аргумент]а в @racket[вывод]. В отличие от функции
 @racket[написать] для байтов, символов и строк выводится их содержимое.}

@defproc[#:kind "функция" (вывести/перенос [аргумент любой] [вывод порт? (текущий-порт-вывода)])
         пусто?]{Как @racket[вывести], но после значения дополнительно выводит литеру
 переноса строки.}

@defproc[#:kind "функция"
         (прочитать-строку [ввод порт? (текущий-порт-ввода)]
                           [режим (одно-из 'перенос 'возврат 'перенос-возврат 'любой 'любой-один)])
         строка?]{Читает строку из порта @racket[ввод]. Аргумент @racket[режим] определяет
разделитель строки:
@itemlist[
 (list
  @item{@racket['перенос] --- литера переноса @racketvalfont{#\перенос} (с кодом 10);}
  @item{@racket['возврат] --- литера возврата каретки @racketvalfont{#\возврат} (с кодом 13);}
  @item{@racket['перенос-возврат] --- пара литер перенос и возврат каретки;}
  @item{@racket['любой] --- любой из перечисленных выше;}
  @item{@racket['любой-один] --- перенос или возврат каретки, но не их комбинация.})]}

@defproc*[#:kind "параметр" ([(текущий-порт-вывода) порт-вывода?]
                             [(текущий-порт-вывода [порт порт-вывода?]) пусто?])]{
Параметр, определяющий текущий порт вывода.}

@defproc*[#:kind "параметр" ([(текущий-порт-ввода) порт-ввода?]
                             [(текущий-порт-ввода [порт порт-ввода?]) пусто?])]{
Параметр, определяющий текущий порт ввода.}

@defproc*[#:kind "параметр" ([(текущее-место)
                              (одно-из строка? #,(elem (racketvalfont "ложь")))]
                             [(текущее-место
                               [место (один-из строка? #,(elem (racketvalfont "ложь")))])
                              пусто?])]{
 Параметр, определяющий текущее место (региональные настройки) для функций с суффиксом
«/местные», например, @racket[строки-равны?/местные/без-регистра].

Когда этот параметр установлен в @racketvalfont{ложь}, результат функций с суффиксом
«/местные» должен быть переносим и совпадать с результатом функций без суффикса «/местные».

Значение @racketvalfont{""} является псевдонимом для региональных настроек операционной
системы и является значением по умолчанию. Значение @racketvalfont{"C"} (латинская)
всегда доступно и для него результат совпадает с тем, который получен при значении
@racketvalfont{ложь}, для литер с кодами от 0 до 127 (цифры, латинский алфавит, ...).

Другие доступные имена мест определяются операционной системой.

Вывод при помощи функции @racket[написать] и аналогичных не зависит от данного параметра.}

@subsection[#:tag "files"]{Файлы}

@defproc[#:kind "функция" (существует-файл? [путь (один-из строка? путь?)])
         булево?]{
Проверяет, существует ли файл по пути @racket[путь]. Модуль
@racketidfont{базовая/файл}.}

@defproc[#:kind "функция" (существует-каталог? [путь (один-из строка? путь?)])
         булево?]{
Проверяет, существует ли каталог по пути @racket[путь]. Модуль
@racketidfont{базовая/файл}.}

@subsection[#:tag "functions"]{Функции}

@defproc[#:kind "функция" (функция? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является функцией.}

@subsection[#:tag "parameters"]{Параметры}

@defproc[#:kind "функция" (параметр? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является параметром.}

@defproc[#:kind "функция" (параметр [аргумент любой]
                                    [охрана
                                     (один-из (любой . -> . любой)
                                              #,(elem (racketvalfont "ложь")))
                                     #,(elem (racketvalfont "ложь"))]
                                    [имя символ? 'функция-параметра])
         параметр?]{Возвращает параметр с начальным значением @racket[аргумент].
Если @racket[охрана] не ложь, то когда функция параметра вызывается с аргументом,
аргумент передаётся в функцию @racket[охрана], а уже результат этой функции записывается
в параметр. Также @racket[охрана] может вызывать исключение, если значение неприемлемо.
К начальному значению эта функция не применяется.}


@defform[#:kind "синтаксис"
         (параметризуя ((выражение-параметра выражение-значения) ...)
                       команда ... выражение)
         #:grammar [(выражение-параметра параметр?)]]{Выполняет переданные
команды, возвращает результат выражения. Значения, полученные из выражений
@racket[выражение-параметра], определяют, какие параметры устанавливать. Значения, полученные из
выражений @racket[выражение-значения] определяют их значения. Эти выражения вычисляются
слева направо. Значения параметров связываются с соответствующими параметрами во время выполнения
команд и выражения в теле формы. По окончании этой формы значения параметров остаются теми,
которыми были до формы.
}

@subsection[#:tag "sequences"]{Последовательности}

@defproc[#:kind "функция" (последовательность? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является последовательностью.}

@defproc*[#:kind "функция"
          ([(в-диапазоне [конец число?]) последовательность?]
           [(в-диапазоне [начало число?] [конец число?] [шаг число? 1]) последовательность?])]{
 Возвращает последовательность чисел начиная с 0 или @racket[начало] до @racket[конец]
 с шагом @racket[шаг].}

@defproc[#:kind "функция" (в-списке [аргумент список?])
         последовательность?]{Возвращает последовательность элементов списка.}

@subsection[#:tag "modules"]{Модули}

@defform[#:kind "синтаксис" (используется выражение-модуля ...)]{Подключает
указанные модули. Выражение модуля может быть строкой с именем файла относительно текущего
каталога, символом с именем модуля или конструкцией использования.}

@subsection[#:tag "draw-reference"]{Рисование}

Цвета, холст, растровые изображения и значки доступны из каталога
@filepath{рисование}. Отдельные файлы подключаются по имени:

@itemlist[
 @item{@racketidfont{рисование/цвет} --- @racket[цвет%],
  @racket[стандартные-цвета], @racket[внутренний-цвет];}
 @item{@racketidfont{рисование/холст} --- @racket[холст-адины]
  (объекты класса @racket[холст%]);}
 @item{@racketidfont{рисование/изображение} --- @racket[изображение%];}
 @item{@racketidfont{рисование/картинка} --- функции значков
  @racket[переработка], @racket[галочка], @racket[лямбда],
  @racket[стоп] и @racket[текст].}
]

Модули сами подключают @racket[класс]. Объекты создаются формой
@racket[объект]. Многие методы холста читают значение без аргументов
и записывают его, если аргумент передан.

@subsubsection[#:tag "ref-цвет%"]{@racket[цвет%]}

@defthing[#:kind "класс" цвет% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Цвет: красный, зелёный и синий составляющие (целые числа от 0 до 255) и укрывистость (вещественное
число от 0 до 1). Например, все нули и укрывистость 1 --- непрозрачный чёрный; все 255 и укрывистость
1 --- непрозрачный белый; красный 255 и укрывистость 0,5 --- полупрозрачный красный.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (цвет% [красный целое-неотрицательное? 0]
       [зелёный целое-неотрицательное? 0]
       [синий целое-неотрицательное? 0]
       [укрывистость число? 0.0]
       [образец любой #,(elem (racketvalfont "ложь"))]
       [внутренний любой #,(elem (racketvalfont "ложь"))])
         цвет%]{
Создаётся формой @racket[объект]:
@racketblock[
объект цвет%
  красный красный
  зелёный зелёный
  синий синий
  укрывистость укрывистость
  образец образец
  внутренний внутренний
]
Вместо составляющих можно передать @racket[образец]: другой @racket[цвет%], символ имени из
@racket[стандартные-цвета] или уже готовое значение цвета.
Если задан @racket[внутренний] или @racket[образец], составляющие игнорируются; по умолчанию оба
@racketvalfont{ложь}. Без образца и без внутреннего объекта создаётся цвет из составляющих;
укрывистость по умолчанию @racketvalfont{0.0} --- цвет полностью прозрачный, пока её не задать явно.}

@defthing[#:kind "константа" стандартные-цвета объект%]{
Справочник имён цветов из базы Racket. Модуль @racketidfont{рисование/цвет}. Символы имён можно
передавать в @racket[образец] конструктора @racket[цвет%], в @racket[внутренний-цвет] и туда, где
ожидается цвет (например, @racket[кисть] на @racket[холст%]).}

@defproc[#:kind "метод" (найти [имя символ?])
         любой]{
Ищет цвет по символу @racket[имя]. Для русских имён есть встроенный перевод: @racket['белый],
@racket['синий], @racket['голубой], @racket['красный], @racket['оранжевый], @racket['чёрный],
@racket['жёлтый], @racket['зелёный], @racket['тёмно-зелёный], @racket['серый],
@racket['светло-жёлтый]. Иначе @racket[имя] ищется в базе как есть. Возвращает внутреннее значение
цвета для @racket[цвет%] и холста.}

@defproc[#:kind "метод" (имена)
         список?]{
Список всех имён цветов, известных базе Racket.}

@defproc[#:kind "функция" (внутренний-цвет [образец любой])
         любой]{
Преобразует @racket[образец] во внутреннее значение цвета Racket. Если @racket[образец] ---
@racket[цвет%], берётся его @racket[внутренний]; если символ --- ищется через
@racket[стандартные-цвета]; иначе @racket[образец] считается уже готовым внутренним цветом.
Используется при создании @racket[цвет%] и в API, ожидающем низкоуровневый цвет.}

@subsubsection[#:tag "ref-холст%"]{@racket[холст%]}

@defthing[#:kind "класс" холст% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Контекст рисования: линии, фигуры, текст и изображения. Холст картины даёт метод @racket[холст]; у
@racket[изображение%] --- тоже @racket[холст]. Модуль @racketidfont{рисование/холст} предоставляет
@racket[холст-адины]; объекты, которые она возвращает, принадлежат этому классу.
Координаты --- в единицах рисования холста. Углы --- в радианах: 0 направо, половина пи вверх, против
часовой стрелки.
}

@defproc[#:kind "функция" (холст-адины [внутренний любой])
         холст%]{
Оборачивает внутренний объект контекста рисования Racket в @racket[холст%]. Модуль
@racketidfont{рисование/холст}. @racket[внутренний] --- низкоуровневый указатель на объект (например,
из @racketidfont{make-dc} у @racket[изображение%] или @racketidfont{get-dc} у @racket[картина%]).}

@defproc[#:kind "метод" (залить-фоном)
         пусто?]{
заполняет область текущим цветом фона. См. также @racket[очистить].}

@defproc[#:kind "метод" (очистить)
         пусто?]{
для холста с каналом прозрачности обнуляет прозрачность; для прозрачной картины стирает рисунок, чтобы
был виден фон; иначе заливает белым.}

@defproc[#:kind "метод" (копировать [лево число?] [верх число?] [ширина число?] [высота число?]
              [лево-цели число?] [верх-цели число?])
         пусто?]{
копирует прямоугольник в точку (@racket[лево-цели], @racket[верх-цели]) на том же холсте. Области
могут пересекаться.}

@defproc[#:kind "метод" (нарисовать-дугу [лево число?] [верх число?] [ширина число?]
                   [высота число?] [начальный-угол число?]
                   [конечный-угол число?])
         пусто?]{
дуга эллипса, вписанного в прямоугольник лево--верх--ширина--высота. Углы: от начального к конечному
против часовой стрелки. Если углы равны, рисуется полный эллипс. Контур --- текущим пером; если кисть
не прозрачная, сектор заливается кистью.}

@defproc[#:kind "метод" (нарисовать-эллипс [лево число?] [верх число?] [ширина число?]
                     [высота число?])
         пусто?]{
эллипс в прямоугольнике лево--верх--ширина--высота. Перо --- контур, кисть --- заливка.}

@defproc[#:kind "метод" (нарисовать-прямоугольник [лево число?] [верх число?]
                            [ширина число?] [высота число?])
         пусто?]{
прямоугольник с тем же смыслом пера и кисти.}

@defproc[#:kind "метод" (нарисовать-закруглённый-прямоугольник [лево число?] [верх число?]
                             [ширина число?] [высота число?]
                             [радиус число? -0.25])
         пусто?]{
прямоугольник со скруглёнными углами. @racket[радиус] по умолчанию $-0.25$: положительный --- радиус
дуги угла, отрицательный --- доля меньшей стороны. Если величина слишком велика, берётся половина
ширины или высоты.}

@defproc[#:kind "метод" (нарисовать-линию [лево-начала число?] [верх-начала число?]
                  [лево-конца число?] [верх-конца число?])
         пусто?]{
отрезок от (@racket[лево-начала], @racket[верх-начала]) до (@racket[лево-конца],
@racket[верх-конца]).}

@defproc[#:kind "метод" (нарисовать-линии [список-линий список?]
                  [сместить-вправо число? 0] [сместить-вниз число? 0])
         пусто?]{
ломаная по списку точек.}

@defproc[#:kind "метод" (нарисовать-многоугольник [точки список?]
                          [сместить-вправо число? 0]
                          [сместить-вниз число? 0]
                          [стиль-заполнения символ? 'чёт-нечет])
         пусто?]{
многоугольник по списку точек. @racket[стиль-заполнения] по умолчанию @racket['чёт-нечет]; иное
значение --- правило ненулевой обмотки.}

@defproc[#:kind "метод" (нарисовать-контур [контур любой]
                   [сместить-вправо число? 0]
                   [сместить-вниз число? 0]
                   [стиль-заполнения символ? 'чёт-нечет])
         пусто?]{
рисует контур. Смещения и @racket[стиль-заполнения] как у многоугольника.}

@defproc[#:kind "метод" (нарисовать-точку [лево число?] [верх число?])
         пусто?]{
точка в (@racket[лево], @racket[верх]).}

@defproc[#:kind "метод" (нарисовать-лекало [начало любой] [контрольная-точка любой]
                   [конец любой])
         пусто?]{
сплайн от начала до конца через контрольную точку. Каждая точка --- список из лево и верх или объект с
полями @racket[лево] и @racket[верх].}

@defproc[#:kind "метод" (нарисовать-текст [текст строка?] [лево число?] [верх число?]
                  [комбинировать любой #,(elem (racketvalfont "ложь"))] [начальная-литера целое? 0]
                  [угол число? 0])
         пусто?]{
строка текущим шрифтом. Если @racket[комбинировать] --- @racket['графемы],
соседние литеры одного знака рисуются вместе; любое другое истинное значение может включать лигатуры,
кернинг и письмо справа налево.}

@defproc[#:kind "метод" (нарисовать-изображение [источник изображение%] [лево-цели число?]
                        [верх-цели число?] [стиль любой 'обычный]
                        [цвет любой 'чёрный] [маска любой #,(elem (racketvalfont "ложь"))])
         булево?]{
@racket[стиль] и @racket[цвет] учитываются только для чёрно-белого изображения: @racket['непрозрачный]
закрашивает белые пиксели фоном. @racket[маска] --- другое изображение того же размера или
@racketvalfont{ложь}. Возвращает @racketvalfont{истина}, если рисунок удался.}

@defproc[#:kind "метод" (нарисовать-часть-изображения [источник изображение%]
                               [лево-цели число?] [верх-цели число?]
                               [лево-части число?] [верх-части число?]
                               [ширина-части число?]
                               [высота-части число?]
                               [стиль любой 'обычный]
                               [цвет любой 'чёрный] [маска любой #,(elem (racketvalfont "ложь"))])
         булево?]{
как @racket[нарисовать-изображение], но копирует прямоугольник источника.}

@defproc[#:kind "метод" (начать-слой [прозрачность число?])
         пусто?]{
откладывает рисование до конца слоя, затем переносит результат с заданной прозрачностью (от 0 до 1),
умноженной на текущую. На время слоя текущая прозрачность становится 1. В отличие от
@racket[прозрачность], перекрывающиеся штрихи слоя тускнеют вместе, а не по отдельности.}

@defproc[#:kind "метод" (закончить-слой)
         пусто?]{
завершает слой, начатый @racket[начать-слой].}

@defproc[#:kind "метод" (отрисовать-немедленно)
         пусто?]{
сбрасывает отложенную отрисовку картины; для прочих холстов ничего не делает.}

@defproc[#:kind "метод" (начать-документ [сообщение строка?])
         пусто?]{
начало печати: @racket[сообщение] к началу документа.}

@defproc[#:kind "метод" (закончить-документ)
         пусто?]{
завершает документ печати.}

@defproc[#:kind "метод" (начать-страницу)
         пусто?]{
начало страницы при печати.}

@defproc[#:kind "метод" (закончить-страницу)
         пусто?]{
завершение страницы при печати.}

@defproc[#:kind "метод" (ключ-метрики-шрифта)
         целое?]{
целое число для кэша размеров текста: одинаковый ключ значит одинаковые метрики. Ноль --- кэшировать
нельзя.}

@defproc*[#:kind "метод"
          ([(прозрачность) число?]
           [(прозрачность [значение число?]) пусто?])]{
без аргумента читает непрозрачность рисования (от 0 до 1), с аргументом задаёт её.}

@defproc*[#:kind "метод"
          ([(фон) любой]
           [(фон [цвет любой]) пусто?])]{
цвет фона для @racket[залить-фоном] (чтение и запись).}

@defproc*[#:kind "метод"
          ([(кисть) любой]
           [(кисть [значение любой]) пусто?]
           [(кисть [цвет любой] [стиль любой]) пусто?])]{
без аргументов возвращает текущую кисть; с кистью задаёт её; с цветом и стилем создаёт кисть этого
цвета. Цвет может быть символом из @racket[стандартные-цвета]. Стиль @racket['непрозрачный] ---
непрозрачная кисть, иначе сплошная.}

@defproc*[#:kind "метод"
          ([(масштаб) любой]
           [(масштаб [по-ширине число?] [по-высоте число?]) пусто?])]{
без аргументов возвращает масштаб; с @racket[по-ширине] и @racket[по-высоте] задаёт его.}

@subsubsection[#:tag "ref-изображение%"]{@racket[изображение%]}

@defthing[#:kind "класс" изображение% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Растр: цветной, чёрно-белый или с каналом прозрачности. У изображения есть масштаб подложки: сколько
пикселей приходится на одну единицу рисования (у чёрно-белого всегда 1).
Нужно задать либо @racketidfont{файл}, либо оба @racket[ширина] и @racket[высота], либо
@racket[внутренний]. Иначе вызывается исключение с сообщением, что ширина и высота обязательны.
Пустой растр заданной ширины и высоты --- белый; с каналом прозрачности начальная прозрачность полная.
Если заданы байты, ширина и высота, создаётся чёрно-белый растр из этих битов.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (изображение% [ширина любой #,(elem (racketvalfont "ложь"))]
               [высота любой #,(elem (racketvalfont "ложь"))]
               [масштаб число? 1.0]
               [файл любой #,(elem (racketvalfont "ложь"))]
               [вид-файла любой 'unknown]
               [цвет-фона любой #,(elem (racketvalfont "ложь"))]
               [неудачная-загрузка-ошибка? булево? #,(elem (racketvalfont "ложь"))]
               [хранить-данные-файла? булево? #,(elem (racketvalfont "ложь"))]
               [байты любой #,(elem (racketvalfont "ложь"))]
               [чёрно-белое? любой #,(elem (racketvalfont "ложь"))]
               [прозрачность? булево? #,(elem (racketvalfont "ложь"))]
               [внутренний любой #,(elem (racketvalfont "ложь"))])
         изображение%]{
Создаётся формой @racket[объект]:
@racketblock[
объект изображение%
  ширина ширина
  высота высота
  масштаб масштаб
  файл файл
  вид-файла вид-файла
  цвет-фона цвет-фона
  неудачная-загрузка-ошибка? неудачная-загрузка-ошибка?
  хранить-данные-файла? хранить-данные-файла?
  байты байты
  чёрно-белое? чёрно-белое?
  прозрачность? прозрачность?
  внутренний внутренний
]
@racket[ширина] и @racket[высота] обязательны вместе, если нет файла и нет внутреннего объекта; по
умолчанию оба @racketvalfont{ложь}. @racket[масштаб] по умолчанию @racketvalfont{1.0}. @racket[файл],
@racket[цвет-фона], @racket[байты], @racket[чёрно-белое?] и @racket[внутренний] по умолчанию
@racketvalfont{ложь}. @racket[вид-файла] по умолчанию @racket['unknown].
@racket[неудачная-загрузка-ошибка?] и @racket[хранить-данные-файла?] по умолчанию
@racketvalfont{ложь}. @racket[прозрачность?] по умолчанию @racketvalfont{ложь}. @racket[байты] ---
чёрно-белый растр из битов (восемь пикселей в байте, 1 --- чёрный).}

@defproc[#:kind "метод" (ширина)
         целое-неотрицательное?]{
размер в единицах рисования.}

@defproc[#:kind "метод" (высота)
         целое-неотрицательное?]{
размер в единицах рисования.}

@defproc[#:kind "метод" #:link-target? #f (масштаб)
         число?]{
масштаб подложки.}

@defproc[#:kind "метод" (глубина-цвета)
         целое-неотрицательное?]{
1 для чёрно-белого, 32 для цветного.}

@defproc[#:kind "метод" (чёрно-белое?)
         булево?]{
@racketvalfont{истина}, если растр не цветной.}

@defproc[#:kind "метод" (есть-прозрачность?)
         булево?]{
есть ли канал прозрачности.}

@defproc[#:kind "метод" (работает?)
         булево?]{
@racketvalfont{истина}, если изображение удалось загрузить; иначе рисование в него и из него не
действует.}

@defproc[#:kind "метод" (холст)
         любой]{
холст для рисования по этому растру или @racketvalfont{ложь}.}

@defproc[#:kind "метод" (загрузить-файл)
         пусто?]{
читает файл. Аргументы: файл, вид-файла, цвет-фона, неудачная-загрузка-ошибка?,
@racket[хранить-данные-файла?]. Допустимо, только если масштаб подложки равен 1.}

@defproc[#:kind "метод" (записать-файл)
         пусто?]{
записывает файл. Аргументы: файл, вид-файла, @racket[качество] (75), @racket[без-масштаба?].}

@defproc[#:kind "метод" (данные-файла)
         любой]{
если при загрузке просили сохранить данные, возвращает содержимое файла; иначе @racketvalfont{ложь}.}

@defproc[#:kind "метод" (указатель)
         любой]{
низкоуровневый указатель на объект растра.}

@defproc*[#:kind "метод"
          ([(загруженная-маска) любой]
           [(загруженная-маска [значение любой]) пусто?])]{
маска прозрачности, полученная при загрузке, или @racketvalfont{ложь}. Чтение и запись. Сама по себе
при рисовании не применяется: её передают как @racketidfont{маска} в @racket[нарисовать-изображение].}

@defproc[#:kind "метод" (пиксели-в-байты [лево число?] [верх число?] [ширина число?]
                 [высота число?] [байты любой]
                 [только-прозрачность? булево? #,(elem (racketvalfont "ложь"))]
                 [учитывать-прозрачность? булево? #,(elem (racketvalfont "ложь"))]
                 [без-масштаба? булево? #,(elem (racketvalfont "ложь"))])
         пусто?]{
копирует прямоугольник в изменяемые байты (по четыре байта на пиксель: прозрачность, красный, зелёный,
синий). @racket[только-прозрачность?] --- только канал прозрачности; @racket[учитывать-прозрачность?]
--- значения уже умножены на прозрачность; @racket[без-масштаба?] --- координаты в пикселях, без
масштаба подложки.}

@defproc[#:kind "метод" (байты-в-пиксели [лево число?] [верх число?] [ширина число?]
                 [высота число?] [байты любой]
                 [только-прозрачность? булево? #,(elem (racketvalfont "ложь"))]
                 [учитывать-прозрачность? булево? #,(elem (racketvalfont "ложь"))]
                 [без-масштаба? булево? #,(elem (racketvalfont "ложь"))])
         пусто?]{
обратная запись из байтов в прямоугольник растра. Те же необязательные аргументы.}

@subsubsection[#:tag "draw-ref-icons"]{Значки}

Функции модуля @racketidfont{рисование/картинка} возвращают
@racket[изображение%]. Общие ключевые аргументы: @racket[цвет]
(символ из @racket[стандартные-цвета] или @racket[цвет%]; если цвет ---
символ, он ищется в справочнике), @racket[высота] и
@racketidfont{материал}. Высота и материал по умолчанию --- стандартные
для значков. Цвет по умолчанию свой у каждой функции.

@defproc[#:kind "функция" (переработка [#:цвет цвет любой]
                                       [#:высота высота число?]
                                       [#:материал материал любой])
         изображение%]{
Значок переработки: три загнутые стрелки.}

@defproc[#:kind "функция" (галочка [#:цвет цвет любой]
                                   [#:высота высота число?]
                                   [#:материал материал любой])
         изображение%]{
Значок галочки.}

@defproc[#:kind "функция" (лямбда [#:цвет цвет любой]
                                  [#:высота высота число?]
                                  [#:материал материал любой])
         изображение%]{
Значок буквы «лямбда».}

@defproc[#:kind "функция" (стоп [#:цвет цвет любой]
                                [#:высота высота число?]
                                [#:материал материал любой])
         изображение%]{
Значок остановки.}

@defproc[#:kind "функция" (текст [строка строка?]
                                 [шрифт шрифт%]
                                 [#:обрезать? обрезать булево?]
                                 [#:цвет цвет любой]
                                 [#:высота высота число?]
                                 [#:материал материал любой]
                                 [#:толщина-контура толщина-контура число?])
         изображение%]{
Значок из строки @racket[строка] шрифтом @racket[шрифт] (по умолчанию
новый объект @racket[шрифт%]). @racketidfont{обрезать?} (по умолчанию
@racketvalfont{истина}) убирает лишние
поля вокруг букв. Цвет по умолчанию @racket['белый].
@racketidfont{толщина-контура} по умолчанию --- высота, делённая на 32.}

@subsection[#:tag "gui-reference"]{Графический интерфейс}

Классы и интерфейсы окон, кнопок, панелей, меню и текстового редактора
доступны после

@racketblock[(используется графический-интерфейс)]

Отдельные файлы каталога подключаются по имени, например
@racketidfont{графический-интерфейс/кнопка} или
@racketidfont{графический-интерфейс/табличная-панель}.
Сводка @racketidfont{графический-интерфейс} не подключает
@racketidfont{графический-интерфейс/табличная-панель},
@racketidfont{графический-интерфейс/поле-даты},
@racketidfont{графический-интерфейс/стиль} и
@racketidfont{графический-интерфейс/набор-команд} --- их нужно указать
самим. Модуль сам подключает @racket[класс]. Объекты создаются формой
@racket[объект].

Общий предок виджетов --- @racket[объект-графического-интерфейса%].
Многие методы читают значение без аргументов и записывают его, если
аргумент передан: так устроены @racket[заголовок],
@racket[значение], @racket[минимальная-ширина],
@racket[выравнивание]. События --- методы с приставкой
@racketidfont{при-} или с вопросом (@racket[можно-закрыть?]);
в базовых классах они ничего не делают или возвращают значение по
умолчанию, их можно переопределить.

Класс реализует методы своих интерфейсов: у @racket[кнопка%] доступны
методы @racket[элемент-управления<%>] и всех предков этого интерфейса.

@subsubsection[#:tag "ref-объект-графического-интерфейса%"]{@racket[объект-графического-интерфейса%]}

@defthing[#:kind "класс" объект-графического-интерфейса% класс?]{
Общий предок окон, элементов, шрифтов, событий, кусков и стилей. Модуль
@racketidfont{графический-интерфейс/объект}.
}

@defproc[#:kind "конструктор" #:link-target? #f (объект-графического-интерфейса% [внутренний любой])
         объект-графического-интерфейса%]{
Создаётся формой @racket[объект]:
@racketblock[
объект объект-графического-интерфейса%
  внутренний внутренний
]
@racket[внутренний] --- уже созданный внутренний объект; если его нет, наследник создаёт свой.}

@defproc[#:kind "метод" (внутренний)
         любой]{
Возвращает внутренний объект, обёрнутый этим классом.}

@subsubsection[#:tag "ref-область<%>"]{@racket[область<%>]}

@defthing[#:kind "интерфейс" область<%> любой]{
Общие размеры и растягивание. Модуль
@racketidfont{графический-интерфейс/интерфейс-область}.

@itemlist[
 @item{@racket[родитель] --- вместилище, в котором лежит область,
  или @racketvalfont{ложь} у окна верхнего уровня;}
 @item{@racketidfont{главное-окно} --- окно верхнего уровня, которому
  принадлежит область, или @racketvalfont{ложь};}
 @item{@racket[минимальная-ширина] и @racket[минимальная-высота]
  --- чтение и запись минимальных размеров; @racketvalfont{ложь} ---
  размер по содержимому;}
 @item{@racketidfont{минимальные-размеры-вывода} --- два значения:
  графические минимумы ширины и высоты (с учётом рамки и полей);}
 @item{@racketidfont{растягивается-ширина} и
  @racketidfont{растягивается-высота} --- можно ли отдавать области
  лишнее место во вместилище.}
]

Аргументы инициализации у реализующих классов: @racket[родитель]
(обязателен, если не окно верхнего уровня), @racket[минимальная-ширина],
@racket[минимальная-высота] (по умолчанию @racketvalfont{ложь}),
@racketidfont{растягивается-ширина} и @racketidfont{растягивается-высота}
(по умолчанию @racketvalfont{истина}).}

@subsubsection[#:tag "ref-окно<%>"]{@racket[окно<%>]}

@defthing[#:kind "интерфейс" окно<%> интерфейс?]{
Расширяет: @racket[область<%>]. Показ, фокус, геометрия и события окна. Модуль
@racketidfont{графический-интерфейс/интерфейс-окно}.
События (в базовой реализации ничего не делают; клавиша и мышь элемента возвращают
@racketvalfont{ложь} --- событие можно обработать дальше).
Дополнительный аргумент инициализации: @racketidfont{включен} (по умолчанию @racketvalfont{истина}).
}

@defproc[#:kind "метод" (показать [состояние любой])
         пусто?]{
Показать или скрыть окно (аргумент не @racketvalfont{ложь} --- показать).}

@defproc[#:kind "метод" (показано?)
         булево?]{
Видно ли окно.}

@defproc*[#:kind "метод"
          ([(включить) булево?]
           [(включить [состояние любой]) пусто?])]{
Разрешить или запретить взаимодействие.}

@defproc[#:kind "метод" (включено?)
         булево?]{
Можно ли взаимодействовать.}

@defproc[#:kind "метод" (есть-фокус?)
         булево?]{
Есть ли клавиатурный фокус.}

@defproc[#:kind "метод" (сфокусировать)
         пусто?]{
Отдать окну фокус.}

@defproc[#:kind "метод" #:link-target? #f (ширина)
         целое-неотрицательное?]{
Внешний размер.}

@defproc[#:kind "метод" #:link-target? #f (высота)
         целое-неотрицательное?]{
Внешний размер.}

@defproc[#:kind "метод" (лево)
         число?]{
Положение относительно родителя или экрана у окна верхнего уровня.}

@defproc[#:kind "метод" (верх)
         число?]{
Положение относительно родителя или экрана у окна верхнего уровня.}

@defproc[#:kind "метод" (размеры)
         любой]{
Два значения: внешние ширина и высота.}

@defproc[#:kind "метод" (внутренние-размеры)
         любой]{
Два значения: ширина и высота клиентской области без рамки.}

@defproc[#:kind "метод" (окно->экран [лево число?] [верх число?])
         любой]{
Из координат окна в экранные.}

@defproc[#:kind "метод" (экран->окно [лево число?] [верх число?])
         любой]{
Из экранных в координаты окна.}

@defproc[#:kind "метод" (переместить-курсор [лево число?] [верх число?])
         пусто?]{
Поставить указатель мыши в точку окна.}

@defproc*[#:kind "метод"
          ([(заголовок) любой]
           [(заголовок [значение любой]) пусто?])]{
Подпись окна или элемента (чтение и запись).}

@defproc*[#:kind "метод"
          ([(курсор) любой]
           [(курсор [значение любой]) пусто?])]{
Вид указателя над окном.}

@defproc*[#:kind "метод"
          ([(принимать-файлы) булево?]
           [(принимать-файлы [значение любой]) пусто?])]{
Принимать ли перетаскивание файлов (чтение и запись).}

@defproc[#:kind "метод" (контекстное-меню [меню любой] [лево число?] [верх число?])
         пусто?]{
Показать @racket[всплывающее-меню%] в точке (меню, лево, верх).}

@defproc[#:kind "метод" #:link-target? #f (указатель)
         любой]{
Низкоуровневая ручка окна.}

@defproc[#:kind "метод" (обновить)
         пусто?]{
Запросить перерисовку.}

@defproc*[#:kind "метод"
          ([(режим-колеса-мыши) любой]
           [(режим-колеса-мыши [значение любой]) пусто?])]{
Без аргумента читает режим; с аргументом задаёт @racket['по-одному], @racket['по-целым] или
@racket['сразу].}

@defproc[#:kind "метод" (при-получении-файла [путь строка?])
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Перетащили файл; аргумент --- путь.}

@defproc[#:kind "метод" (при-изменении-фокуса [есть-фокус? булево?])
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Аргумент: появился ли фокус.}

@defproc[#:kind "метод" (при-перемещении [лево число?] [верх число?])
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Новое лево и верх.}

@defproc[#:kind "метод" (при-изменении-размера [ширина число?] [высота число?])
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Новая ширина и высота.}

@defproc[#:kind "метод" (при-получении-элементом-клавиши [элемент любой]
                               [событие событие-клавиши%])
         любой]{
Этот метод переопределяется формой @racket[переопределить].

Клавиша ушла потомку; аргументы: элемент и событие клавиши. В базовой реализации возвращает
@racketvalfont{ложь}.}

@defproc[#:kind "метод" (при-получении-элементом-события [элемент любой]
                                [событие событие-мыши%])
         любой]{
Этот метод переопределяется формой @racket[переопределить].

Мышь у потомка. В базовой реализации возвращает @racketvalfont{ложь}.}

@defproc[#:kind "метод" (при-изменении-фокуса-элемента [элемент любой]
                              [есть-фокус? булево?])
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Фокус потомка.}

@defproc[#:kind "метод" (при-активации-содержащего-окна [активно? булево?])
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Изменилось охватывающее окно.}

@defproc[#:kind "метод" (при-включении-содержащего-окна [включено? булево?])
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Изменилось охватывающее окно.}

@defproc[#:kind "метод" (при-показывании-содержащего-окна [показано? булево?])
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Изменилось охватывающее окно.}

@subsubsection[#:tag "ref-подобласть<%>"]{@racket[подобласть<%>]}

@defthing[#:kind "интерфейс" подобласть<%> интерфейс?]{
Расширяет: @racket[область<%>]. Отступы внутри вместилища. Модуль
@racketidfont{графический-интерфейс/интерфейс-подобласть}.
У элементов управления поля по умолчанию 2, у групп и панелей --- 0. Аргументы инициализации с теми же
именами.
}

@defproc*[#:kind "метод"
          ([(горизонтальные-поля) целое-неотрицательное?]
           [(горизонтальные-поля [значение любой]) пусто?])]{
Отступ слева и справа (чтение и запись).}

@defproc*[#:kind "метод"
          ([(вертикальные-поля) целое-неотрицательное?]
           [(вертикальные-поля [значение любой]) пусто?])]{
Отступ сверху и снизу (чтение и запись).}

@subsubsection[#:tag "ref-подокно<%>"]{@racket[подокно<%>]}

@defthing[#:kind "интерфейс" подокно<%> интерфейс?]{
Одновременно @racket[окно<%>] и @racket[подобласть<%>]. Модуль
@racketidfont{графический-интерфейс/интерфейс-подокно}.
Методы @racket[окно<%>] и @racket[подобласть<%>] тоже доступны.
}

@defproc*[#:kind "метод"
          ([(родитель) любой]
           [(родитель [новый-родитель любой]) пусто?])]{
Без аргумента читает вместилище; с новым родителем переносит окно в другое вместилище.}

@subsubsection[#:tag "ref-вместилище<%>"]{@racket[вместилище<%>]}

@defthing[#:kind "интерфейс" вместилище<%> интерфейс?]{
Расширяет: @racket[область<%>]. Контейнер дочерних элементов. Модуль
@racketidfont{графический-интерфейс/интерфейс-вместилище}.
Аргументы инициализации: @racket[граница], @racket[интервал], @racket[выравнивание] --- список из двух
символов (у каждого класса своё умолчание).
}

@defproc[#:kind "метод" (элементы)
         любой]{
Список детей (объекты Адины).}

@defproc[#:kind "метод" (добавить-элемент [элемент любой])
         пусто?]{
Показать уже созданного ребёнка (не уничтожая его).}

@defproc[#:kind "метод" (удалить-элемент [элемент любой])
         пусто?]{
Убрать уже созданного ребёнка (не уничтожая его).}

@defproc[#:kind "метод" (изменить-элементы [отбор функция?])
         пусто?]{
Функция от списка детей, возвращает новый список; порядок в списке --- порядок раскладки.}

@defproc[#:kind "метод" (начать-изменения)
         пусто?]{
Начало пакета обновлений раскладки: между ним и @racket[закончить-изменения] дети не пересчитываются.}

@defproc[#:kind "метод" (закончить-изменения)
         пусто?]{
Завершение пакета обновлений раскладки.}

@defproc[#:kind "метод" (пересчитать-положения)
         пусто?]{
Сразу переложить детей.}

@defproc[#:kind "метод" (положения-изменены)
         пусто?]{
Сообщить, что геометрия детей могла измениться, и нужна новая раскладка.}

@defproc*[#:kind "метод"
          ([(граница) целое-неотрицательное?]
           [(граница [значение любой]) пусто?])]{
Внутренний отступ у края контейнера (чтение и запись, по умолчанию 0).}

@defproc*[#:kind "метод"
          ([(интервал) целое-неотрицательное?]
           [(интервал [значение любой]) пусто?])]{
Промежуток между детьми (по умолчанию 0).}

@defproc*[#:kind "метод"
          ([(выравнивание) любой]
           [(выравнивание [горизонтально любой] [вертикально любой]) пусто?])]{
Без аргументов возвращает два символа (горизонталь и вертикаль), с двумя символами задаёт их.
Горизонталь: @racket['лево], @racket['центр], @racket['право]. Вертикаль: @racket['верх],
@racket['центр], @racket['низ].}

@defproc[#:kind "метод" (размер-вместилища [информация любой])
         любой]{
По сведениям о детях вернуть желаемые ширину и высоту контейнера; в базовой реализации --- расчёт
предка.}

@defproc[#:kind "метод" (разместить-элементы [информация любой] [ширина число?]
                     [высота число?])
         пусто?]{
По сведениям о детях и размеру контейнера вернуть положения детей; в базовой реализации --- расчёт
предка.}

@defproc[#:kind "метод" (после-добавления-элемента [элемент любой])
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Событие после появления нового ребёнка; аргумент --- этот элемент. В базовой реализации ничего не
делает.}

@subsubsection[#:tag "ref-вместилище-окно<%>"]{@racket[вместилище-окно<%>]}

@defthing[#:kind "интерфейс" вместилище-окно<%> интерфейс?]{
Одновременно @racket[вместилище<%>] и @racket[окно<%>]. Модуль
@racketidfont{графический-интерфейс/интерфейс-вместилище-окно}.
Отдельных методов нет: это сочетание контейнера и видимого окна (панель, главное окно, диалог).
}

@subsubsection[#:tag "ref-главное-окно<%>"]{@racket[главное-окно<%>]}

@defthing[#:kind "интерфейс" главное-окно<%> интерфейс?]{
Расширяет: @racket[вместилище-окно<%>]. Окно верхнего уровня. Модуль
@racketidfont{графический-интерфейс/интерфейс-главное-окно}.
Выравнивание по умолчанию @racket['(центр верх)], родитель необязателен.
}

@defproc[#:kind "метод" (в-центр [направление любой 'оба])
         пусто?]{
Поставить окно в центр экрана. Направление @racket['оба] (по умолчанию), @racket['по-горизонтали] или
@racket['по-вертикали].}

@defproc[#:kind "метод" (переместить [лево число?] [верх число?])
         пусто?]{
Лево и верх на экране.}

@defproc[#:kind "метод" (установить-размер [ширина число?] [высота число?])
         пусто?]{
Внешние ширина и высота.}

@defproc[#:kind "метод" (пространство-событий)
         любой]{
Очередь событий этого окна.}

@defproc[#:kind "метод" (объект-в-фокусе)
         любой]{
Элемент или редактор с фокусом либо @racketvalfont{ложь}.}

@defproc[#:kind "метод" (окно-в-фокусе)
         любой]{
Подокно с фокусом либо @racketvalfont{ложь}.}

@defproc[#:kind "метод" (объект-для-редактирования)
         любой]{
Куда идёт ввод, если фокус на картине редактора.}

@defproc[#:kind "метод" (окно-для-редактирования)
         любой]{
Окно этого объекта.}

@defproc[#:kind "метод" (установить-иконку [иконка любой]
                    [маска любой #,(elem (racketvalfont "ложь"))]
                    [какую любой 'обе])
         пусто?]{
Значок окна. Аргументы: изображение, необязательная маска (@racketvalfont{ложь}) и какой значок
сменить: @racket['маленькую], @racket['большую] или @racket['обе] (по умолчанию).}

@defproc[#:kind "метод" (можно-закрыть?)
         булево?]{
Разрешить ли закрытие; по умолчанию @racketvalfont{истина}.}

@defproc[#:kind "метод" (можно-выйти?)
         булево?]{
Разрешить ли выход из приложения; по умолчанию вызывает @racket[можно-закрыть?].}

@defproc[#:kind "метод" (при-активации [активно? булево?])
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Окно стало активным или нет.}

@defproc[#:kind "метод" (при-закрытии)
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Окно закрывают.}

@defproc[#:kind "метод" (при-выходе)
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Приложение завершают через это окно.}

@defproc[#:kind "метод" (при-получении-сообщения [сообщение любой])
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Системное сообщение.}

@defproc[#:kind "метод" (при-изменении-дисплея)
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Сменились экраны или масштаб (есть у @racket[главное-окно%]).}

@defproc[#:kind "метод" (при-получении-клавиши [событие событие-клавиши%])
         любой]{
Этот метод переопределяется формой @racket[переопределить].

Клавиша обхода фокуса; @racketvalfont{ложь} --- обработать как обычно, иначе клавиша уже обработана.}

@subsubsection[#:tag "ref-элемент-управления<%>"]{@racket[элемент-управления<%>]}

@defthing[#:kind "интерфейс" элемент-управления<%> интерфейс?]{
Расширяет: @racket[подокно<%>]. Кнопка, надпись, поле ввода. Модуль
@racketidfont{графический-интерфейс/интерфейс-элемент-управления}.
По умолчанию элемент не растягивается. Аргументы инициализации: @racketidfont{шрифт} (по умолчанию
@racket[шрифт-элемента-управления]), @racket[стиль] --- список символов; @racket['скрытая] --- элемент
создан, но не показан в родителе.
}

@defproc[#:kind "метод" (команда [событие любой])
         пусто?]{
Передаёт элементу событие управления так, словно его вызвал пользователь.}

@subsubsection[#:tag "ref-картина<%>"]{@racket[картина<%>]}

@defthing[#:kind "интерфейс" картина<%> интерфейс?]{
Расширяет: @racket[подокно<%>]. Холст или поле редактора. Модуль
@racketidfont{графический-интерфейс/интерфейс-картина}.
События: если вернуть не @racketvalfont{ложь}, стандартная обработка не выполняется. В базовой
реализации все четыре возвращают @racketvalfont{ложь}.
}

@defproc*[#:kind "метод"
          ([(принимать-фокус-табом) булево?]
           [(принимать-фокус-табом [значение любой]) пусто?])]{
Читает или задаёт, получает ли картина клавиатурный фокус при обходе табуляцией и стрелками. По
умолчанию @racketvalfont{ложь}.
Если фокус по табуляции включён, клавиши табуляции, стрелок, ввода и отмены обрабатываются
@racket[при-получении-клавиши] окна верхнего уровня, а не доходят до картины.}

@defproc[#:kind "метод" #:link-target? #f (отрисовать-немедленно)
         пусто?]{
Сбрасывает отложенную отрисовку картины на экран.}

@defproc*[#:kind "метод" #:link-target? #f
          ([(фон) любой]
           [(фон [цвет цвет%]) пусто?])]{
Без аргумента возвращает цвет, которым стирают картину перед @racket[при-отрисовке]. С аргументом
задаёт этот цвет.
Если картина создана с прозрачным стилем, чтение возвращает @racketvalfont{ложь}; запись в таком
случае ошибка.}

@defproc[#:kind "метод" #:link-target? #f (холст)
         любой]{
Возвращает @racket[холст%] для рисования на картине или @racketvalfont{ложь}, если внутренний холст
ещё не создан.}

@defproc*[#:kind "метод"
          ([(минимальная-ширина-холста) целое-неотрицательное?]
           [(минимальная-ширина-холста [ширина целое-неотрицательное?]) пусто?])]{
Читает или задаёт минимальную ширину клиентской области картины (без рамки и полос прокрутки) для
раскладки. Значение меньше графического минимума игнорируется.}

@defproc*[#:kind "метод"
          ([(минимальная-высота-холста) целое-неотрицательное?]
           [(минимальная-высота-холста [высота целое-неотрицательное?]) пусто?])]{
Читает или задаёт минимальную высоту клиентской области картины (без рамки и полос прокрутки) для
раскладки. Значение меньше графического минимума игнорируется.}

@defproc[#:kind "метод" (приостановить-отрисовку)
         пусто?]{
Временно отключает сброс буфера картины на экран. Вызовы можно вкладывать; сброс возобновляется после
стольких же вызовов @racket[продолжить-отрисовку].}

@defproc[#:kind "метод" (продолжить-отрисовку)
         пусто?]{
Возобновляет сброс буфера после @racket[приостановить-отрисовку]. См.
@racket[приостановить-отрисовку].}

@defproc[#:kind "метод" #:link-target? #f (при-получении-клавиши [событие событие-клавиши%])
         любой]{
Этот метод переопределяется формой @racket[переопределить].

Вызывается, когда картина получает событие клавиатуры. Если вернуть не @racketvalfont{ложь},
стандартная обработка не выполняется.
В базовом классе возвращает @racketvalfont{ложь}.}

@defproc[#:kind "метод" (при-получении-события [событие событие-мыши%])
         любой]{
Этот метод переопределяется формой @racket[переопределить].

Вызывается, когда картина получает событие мыши. Если вернуть не @racketvalfont{ложь}, стандартная
обработка не выполняется.
В базовом классе возвращает @racketvalfont{ложь}.}

@defproc[#:kind "метод" (при-отрисовке)
         любой]{
Этот метод переопределяется формой @racket[переопределить].

Вызывается, когда картину нужно перерисовать (показ, изменение размера, запрос @racket[обновить]).
Если вернуть не @racketvalfont{ложь}, стандартная отрисовка (в том числе @racketidfont{обработка}) не
выполняется.
В базовом классе возвращает @racketvalfont{ложь}; тогда вызывается @racketidfont{обработка}.}

@defproc[#:kind "метод" (при-входе-табом)
         любой]{
Этот метод переопределяется формой @racket[переопределить].

Вызывается, когда клавиатурный фокус входит на картину обходом табуляцией (событие
@racket[при-изменении-фокуса] тоже вызывается). Если вернуть не @racketvalfont{ложь}, стандартная
обработка не выполняется.
В базовом классе возвращает @racketvalfont{ложь}.}

@subsubsection[#:tag "ref-редактор<%>"]{@racket[редактор<%>]}

@defthing[#:kind "интерфейс" редактор<%> интерфейс?]{
Документ, который показывают через @racket[поле-редактора%] или другую картину. Его реализует
@racket[текст%]. Модуль @racketidfont{графический-интерфейс/интерфейс-редактор}.
}

@defproc[#:kind "метод" (добавить-картину [картина любой])
         пусто?]{
Добавляет картину в список тех, что показывают этот документ. Обычно вызывается самим полем редактора,
когда ему задают документ.}

@defproc[#:kind "метод" (добавить-отменятель [отменятель функция?])
         пусто?]{
Кладёт функцию отмены на стек отмен. Если сейчас выполняется отмена, функция попадает на стек
повторов. Система вызывает её, когда она первая на стеке.
Для встроенных действий (вставка, удаление, смена шрифта) записи отмены создаются сами. Свой
отменятель нужен, когда надо откатить действие, которого в документе нет (например, смену своей
подписи куска). После возврата функция снимается со стека; если её результат не @racketvalfont{ложь},
сразу выполняется следующий отменятель --- так несколько действий откатываются одним шагом. Имеет
смысл возвращать не @racketvalfont{ложь}, если отменяемое действие входило в последовательность
редактирования.}

@defproc[#:kind "метод" (начать-последовательность-редактирования)
         пусто?]{
Начало набора правок, чтобы показ обновился один раз в конце. Вызовы можно вкладывать. Это сильно
ускоряет перерисовку.
Если документ содержит вложенные документы, последовательность на внешнем частично охватывает и их, но
для вложенного эффективнее начать свою последовательность.}

@defproc[#:kind "метод" (закончить-последовательность-редактирования)
         пусто?]{
Завершение набора правок, начатого @racket[начать-последовательность-редактирования].}

@defproc*[#:kind "метод"
          ([(команды-редактора) любой]
           [(команды-редактора [набор любой]) пусто?])]{
Без аргументов возвращает текущий @racket[набор-команд%] или @racketvalfont{ложь}. С аргументом задаёт
набор; @racketvalfont{ложь} снимает все привязки клавиш.}

@defproc[#:kind "метод" (первый-кусок)
         любой]{
Первый кусок документа или @racketvalfont{ложь}, если документ пуст. В текстовом документе это кусок в
позиции 0. Остальные куски обходятся через @racket[следующий].}

@defproc[#:kind "метод" (вставить-из-буфера [время число? 0])
         пусто?]{
Вставляет текущее содержимое буфера обмена. Необязательный аргумент --- метка времени события (по
умолчанию 0). Система может вставить из буфера и без вызова этого метода.}

@defproc[#:kind "метод" (скопировать [дополнять? любой #,(elem (racketvalfont "ложь"))]
         [время число? 0])
         пусто?]{
Копирует текущее выделение в буфер обмена. Если @racket[дополнять?] не @racketvalfont{ложь},
содержимое буфера дополняется, а не заменяется. Необязательный аргумент @racket[время] --- метка
времени события (по умолчанию 0). Система может выполнить копирование и без вызова этого метода.}

@defproc[#:kind "метод" #:link-target? #f (размеры)
         любой]{
Возвращает два значения: ширину и высоту видимой области, в которую сейчас показывают документ.}

@defproc[#:kind "метод" (глобальные->местные [лево число?] [верх число?])
         любой]{
Переводит координаты @racket[лево] и @racket[верх] из системы показа (обычно картины) в координаты
документа. Возвращает два значения: местные лево и верх.}

@defproc*[#:kind "метод"
          ([(максимальная-ширина) число?]
           [(максимальная-ширина [значение любой]) пусто?])]{
Без аргумента читает ограничение размера содержимого при показе, с аргументом записывает. Ноль значит,
что ограничения нет. В текстовом документе отсутствие максимальной ширины отключает автоматический
перенос строк.}

@defproc*[#:kind "метод"
          ([(минимальная-ширина) число?]
           [(минимальная-ширина [значение любой]) пусто?])]{
Без аргумента читает ограничение размера содержимого при показе, с аргументом записывает. Ноль значит,
что ограничения нет.}

@defproc*[#:kind "метод"
          ([(максимальная-высота) число?]
           [(максимальная-высота [значение любой]) пусто?])]{
Без аргумента читает ограничение размера содержимого при показе, с аргументом записывает. Ноль значит,
что ограничения нет.}

@defproc*[#:kind "метод"
          ([(минимальная-высота) число?]
           [(минимальная-высота [значение любой]) пусто?])]{
Без аргумента читает ограничение размера содержимого при показе, с аргументом записывает. Ноль значит,
что ограничения нет.}

@defproc[#:kind "метод" (обработать-изменение-размера)
         пусто?]{
Вызывает @racket[при-изменении-размера], если документ не в середине последовательности редактирования
и не перерисовывается сейчас. Иначе вызов откладывается.}

@defproc[#:kind "метод" #:link-target? #f (при-получении-события [событие событие-мыши%])
         любой]{
Этот метод переопределяется формой @racket[переопределить].

Событие мыши по документу. Аргумент --- обёрнутое событие мыши. В базовом классе возвращает
@racketvalfont{истина}: тогда обработка события продолжается (клавиши, кусок с курсором, выбор куска).
Если переопределить и вернуть @racketvalfont{ложь}, дальнейшая обработка не выполняется.}

@defproc[#:kind "метод" (при-получении-необработанного-события [событие событие-мыши%])
         любой]{
Этот метод переопределяется формой @racket[переопределить].

Вызывается, когда событие мыши не обработал кусок с курсором и не обработал набор команд. Аргумент ---
обёрнутое событие мыши. В базовом классе возвращает @racketvalfont{истина}, и обработка события
продолжается. Значение @racketvalfont{ложь} её прерывает.}

@defproc[#:kind "метод" #:link-target? #f (при-изменении-размера)
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Показ изменил размер видимой области (как у @racket[размеры]). Вызывается не напрямую, а через
@racket[обработать-изменение-размера]. В базовом классе ничего не делает. Если включён автоматический
перенос, исходная реализация подстраивает максимальную ширину под картины документа.}

@defproc[#:kind "метод" (при-начале-последовательности-редактировании)
         пусто?]{
Этот метод переопределяется формой @racket[переопределить].

Внешняя (не вложенная) последовательность редактирования только что началась. Во время
последовательности остальные события вызываются как обычно, но долгие вычисления лучше отложить до её
конца. В базовом классе ничего не делает.}

@subsubsection[#:tag "ref-пункт-меню<%>"]{@racket[пункт-меню<%>]}

@defthing[#:kind "интерфейс" пункт-меню<%> интерфейс?]{
Пункт в строке меню, выпадающем или всплывающем меню. Определён в сводке
@racketidfont{графический-интерфейс}.
}

@defproc[#:kind "метод" #:link-target? #f (родитель)
         любой]{
Меню или строка меню, в которой лежит пункт.}

@defproc[#:kind "метод" (удалить)
         пусто?]{
Убрать пункт из меню, не уничтожая его.}

@defproc[#:kind "метод" (восстановить)
         пусто?]{
Вернуть ранее удалённый пункт на прежнее место.}

@defproc[#:kind "метод" (удалён?)
         булево?]{
Убран ли пункт.}

@subsubsection[#:tag "ref-пункт-меню-с-заголовком<%>"]{@racket[пункт-меню-с-заголовком<%>]}

@defthing[#:kind "интерфейс" пункт-меню-с-заголовком<%> интерфейс?]{
Расширяет: @racket[пункт-меню<%>]. Пункт с подписью.
}

@defproc*[#:kind "метод" #:link-target? #f
          ([(заголовок) любой]
           [(заголовок [значение любой]) пусто?])]{
Текст пункта (чтение и запись).}

@defproc[#:kind "метод" #:link-target? #f (включить [включён? любой])
         пусто?]{
Разрешить или запретить выбор.}

@defproc[#:kind "метод" (включён?)
         булево?]{
Можно ли выбрать пункт.}

@defproc*[#:kind "метод"
          ([(строка-помощи) любой]
           [(строка-помощи [значение любой]) пусто?])]{
Подсказка в строке состояния (чтение и запись; @racketvalfont{ложь} --- без подсказки).}

@subsubsection[#:tag "ref-главное-окно%"]{@racket[главное-окно%]}

@defthing[#:kind "класс" главное-окно% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%], реализует интерфейс @racket[главное-окно<%>].
Окно верхнего уровня с рамкой, заголовком и, при необходимости, строкой состояния. Модуль
@racketidfont{графический-интерфейс/главное-окно}.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (главное-окно% [заголовок строка? ""]
               [ширина любой #,(elem (racketvalfont "ложь"))]
               [высота любой #,(elem (racketvalfont "ложь"))]
               [родитель любой]
               [граница целое-неотрицательное? 0]
                                  [интервал целое-неотрицательное? 0]
                                  [выравнивание любой '(центр верх)]
               [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                                  [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
                                  [растягивается-ширина булево?
                                   #,(elem (racketvalfont "истина"))]
                                  [растягивается-высота булево?
                                   #,(elem (racketvalfont "истина"))]
               [горизонтальные-поля целое-неотрицательное? 0]
                                  [вертикальные-поля целое-неотрицательное? 0]
               [включен булево? #,(elem (racketvalfont "истина"))])
         главное-окно%]{
Создаётся формой @racket[объект]:
@racketblock[
объект главное-окно%
  заголовок заголовок
  ширина ширина
  высота высота
  родитель родитель
  граница граница
  интервал интервал
  выравнивание выравнивание
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  растягивается-ширина растягивается-ширина
  растягивается-высота растягивается-высота
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
  включен включен
]

@racket[заголовок] по умолчанию пустая строка. @racket[ширина] и @racket[высота] --- число или
@racketvalfont{ложь} (размер по содержимому). @racket[родитель] необязателен; выравнивание по
умолчанию @racket['(центр верх)]. @racket[минимальная-ширина] и @racket[минимальная-высота] по
умолчанию @racketvalfont{ложь} --- размер по содержимому.}

@defproc[#:kind "метод" (создать-строку-состояния)
         пусто?]{
Добавляет полосу состояния внизу окна.}

@defproc[#:kind "метод" (показать-состояние [состояние строка?])
         пусто?]{
Задаёт текст в полосе состояния.}

@subsubsection[#:tag "ref-диалог%"]{@racket[диалог%]}

@defthing[#:kind "класс" диалог% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%], реализует интерфейс @racket[главное-окно<%>].
Модальное или вспомогательное окно без строки состояния. Определён в сводке
@racketidfont{графический-интерфейс}.
Методы @racket[главное-окно<%>] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (диалог% [заголовок строка? ""]
          [ширина любой #,(elem (racketvalfont "ложь"))]
          [высота любой #,(elem (racketvalfont "ложь"))]
          [родитель любой]
          [граница целое-неотрицательное? 0]
                             [интервал целое-неотрицательное? 0]
                             [выравнивание любой '(центр верх)]
          [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                             [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
                             [растягивается-ширина булево?
                              #,(elem (racketvalfont "истина"))]
                             [растягивается-высота булево?
                              #,(elem (racketvalfont "истина"))]
          [горизонтальные-поля целое-неотрицательное? 0]
                             [вертикальные-поля целое-неотрицательное? 0]
          [включен булево? #,(elem (racketvalfont "истина"))])
         диалог%]{
Создаётся формой @racket[объект]:
@racketblock[
объект диалог%
  заголовок заголовок
  ширина ширина
  высота высота
  родитель родитель
  граница граница
  интервал интервал
  выравнивание выравнивание
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  растягивается-ширина растягивается-ширина
  растягивается-высота растягивается-высота
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
  включен включен
]

Те же аргументы, что у @racket[главное-окно%]. Методы --- как у @racket[главное-окно<%>], без
@racket[создать-строку-состояния] и @racket[показать-состояние].}

@defproc[#:kind "функция" (получить-файл
               [сообщение (один-из строка? #,(elem (racketvalfont "ложь")))
                #,(elem (racketvalfont "ложь"))]
               [родитель (один-из главное-окно% диалог% #,(elem (racketvalfont "ложь")))
                #,(elem (racketvalfont "ложь"))]
               [каталог (один-из строка? #,(elem (racketvalfont "ложь")))
                #,(elem (racketvalfont "ложь"))]
               [имя-файла (один-из строка? #,(elem (racketvalfont "ложь")))
                #,(elem (racketvalfont "ложь"))]
               [расширение (один-из строка? #,(elem (racketvalfont "ложь")))
                #,(elem (racketvalfont "ложь"))]
               [стиль список? пустой-список]
               [фильтры список? '((«Любые» «*.*»))])
         (один-из строка? #,(elem (racketvalfont "ложь")))]{
Показывает стандартный модальный диалог выбора файла. Возвращает путь к выбранному файлу или
@racketvalfont{ложь}, если пользователь отменил выбор. @racket[сообщение] --- текст в верхней части
диалога. @racket[родитель] --- окно-родитель. @racket[каталог] и @racket[имя-файла] задают начальный
каталог и имя. @racket[расширение] --- расширение по умолчанию. @racket[стиль] --- список флагов
(@racket['common], @racket['packages], @racket['enter-packages]). @racket[фильтры] --- список пар
(подпись, маска). Модуль @racketidfont{графический-интерфейс/диалог}.}

@subsubsection[#:tag "ref-группа%"]{@racket[группа%]}

@defthing[#:kind "класс" группа% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Реализует @racket[вместилище<%>] и @racket[подобласть<%>]. Контейнер без собственной рамки: только
раскладывает детей. Модуль @racketidfont{графический-интерфейс/группа}.
Методы @racket[вместилище<%>] и @racket[подобласть<%>] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (группа% [родитель любой]
          [граница целое-неотрицательное? 0]
                             [интервал целое-неотрицательное? 0]
                             [выравнивание любой '(центр центр)]
          [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                             [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
                             [растягивается-ширина булево?
                              #,(elem (racketvalfont "истина"))]
                             [растягивается-высота булево?
                              #,(elem (racketvalfont "истина"))]
          [горизонтальные-поля целое-неотрицательное? 0]
                             [вертикальные-поля целое-неотрицательное? 0])
         группа%]{
Создаётся формой @racket[объект]:
@racketblock[
объект группа%
  родитель родитель
  граница граница
  интервал интервал
  выравнивание выравнивание
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  растягивается-ширина растягивается-ширина
  растягивается-высота растягивается-высота
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
]

Выравнивание по умолчанию @racket['(центр центр)]. Поля по умолчанию 0. @racket[родитель] обязателен.
@racket[минимальная-ширина] и @racket[минимальная-высота] по умолчанию @racketvalfont{ложь} --- размер
по содержимому.}

@subsubsection[#:tag "ref-горизонтальная-группа%"]{@racket[горизонтальная-группа%]}

@defthing[#:kind "класс" горизонтальная-группа% класс?]{
Базовый класс: @racket[группа%].
Дети слева направо. Выравнивание по умолчанию @racket['(лево центр)].
Методы @racket[вместилище<%>] и @racket[подобласть<%>] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (горизонтальная-группа% [родитель любой]
          [граница целое-неотрицательное? 0]
                             [интервал целое-неотрицательное? 0]
                             [выравнивание любой '(лево центр)]
          [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                             [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
                             [растягивается-ширина булево?
                              #,(elem (racketvalfont "истина"))]
                             [растягивается-высота булево?
                              #,(elem (racketvalfont "истина"))]
          [горизонтальные-поля целое-неотрицательное? 0]
                             [вертикальные-поля целое-неотрицательное? 0])
         горизонтальная-группа%]{
Создаётся формой @racket[объект]:
@racketblock[
объект горизонтальная-группа%
  родитель родитель
  граница граница
  интервал интервал
  выравнивание выравнивание
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  растягивается-ширина растягивается-ширина
  растягивается-высота растягивается-высота
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
]

Выравнивание по умолчанию @racket['(лево центр)]. @racket[родитель] обязателен.
@racket[минимальная-ширина] и @racket[минимальная-высота] по умолчанию @racketvalfont{ложь} --- размер
по содержимому.}

@subsubsection[#:tag "ref-вертикальная-группа%"]{@racket[вертикальная-группа%]}

@defthing[#:kind "класс" вертикальная-группа% класс?]{
Базовый класс: @racket[группа%].
Дети сверху вниз. Выравнивание по умолчанию @racket['(центр верх)].
Методы @racket[вместилище<%>] и @racket[подобласть<%>] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (вертикальная-группа% [родитель любой]
          [граница целое-неотрицательное? 0]
                             [интервал целое-неотрицательное? 0]
                             [выравнивание любой '(центр верх)]
          [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                             [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
                             [растягивается-ширина булево?
                              #,(elem (racketvalfont "истина"))]
                             [растягивается-высота булево?
                              #,(elem (racketvalfont "истина"))]
          [горизонтальные-поля целое-неотрицательное? 0]
                             [вертикальные-поля целое-неотрицательное? 0])
         вертикальная-группа%]{
Создаётся формой @racket[объект]:
@racketblock[
объект вертикальная-группа%
  родитель родитель
  граница граница
  интервал интервал
  выравнивание выравнивание
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  растягивается-ширина растягивается-ширина
  растягивается-высота растягивается-высота
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
]

Выравнивание по умолчанию @racket['(центр верх)]. @racket[родитель] обязателен.
@racket[минимальная-ширина] и @racket[минимальная-высота] по умолчанию @racketvalfont{ложь} --- размер
по содержимому.}

@subsubsection[#:tag "ref-панель%"]{@racket[панель%]}

@defthing[#:kind "класс" панель% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Реализует @racket[вместилище-окно<%>] и @racket[подокно<%>]. Видимый контейнер с фоном. Модуль
@racketidfont{графический-интерфейс/панель}.
Методы @racket[вместилище-окно<%>] и @racket[подокно<%>] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (панель% [родитель любой]
          [стиль любой '()]
          [граница целое-неотрицательное? 0]
                             [интервал целое-неотрицательное? 0]
                             [выравнивание любой '(центр центр)]
          [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                             [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
                             [растягивается-ширина булево?
                              #,(elem (racketvalfont "истина"))]
                             [растягивается-высота булево?
                              #,(elem (racketvalfont "истина"))]
          [горизонтальные-поля целое-неотрицательное? 0]
                             [вертикальные-поля целое-неотрицательное? 0]
          [включен булево? #,(elem (racketvalfont "истина"))])
         панель%]{
Создаётся формой @racket[объект]:
@racketblock[
объект панель%
  родитель родитель
  стиль стиль
  граница граница
  интервал интервал
  выравнивание выравнивание
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  растягивается-ширина растягивается-ширина
  растягивается-высота растягивается-высота
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
  включен включен
]

@racket[стиль] --- список символов стиля (по умолчанию пустой). Выравнивание по умолчанию
@racket['(центр центр)]. @racket[родитель] обязателен. @racket[минимальная-ширина] и
@racket[минимальная-высота] по умолчанию @racketvalfont{ложь} --- размер по содержимому.}

@subsubsection[#:tag "ref-горизонтальная-панель%"]{@racket[горизонтальная-панель%]}

@defthing[#:kind "класс" горизонтальная-панель% класс?]{
Базовый класс: @racket[панель%].
Дети слева направо. Выравнивание по умолчанию @racket['(лево центр)].
Методы @racket[вместилище-окно<%>] и @racket[подокно<%>] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (горизонтальная-панель% [родитель любой]
          [стиль любой '()]
          [граница целое-неотрицательное? 0]
                             [интервал целое-неотрицательное? 0]
                             [выравнивание любой '(лево центр)]
          [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                             [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
                             [растягивается-ширина булево?
                              #,(elem (racketvalfont "истина"))]
                             [растягивается-высота булево?
                              #,(elem (racketvalfont "истина"))]
          [горизонтальные-поля целое-неотрицательное? 0]
                             [вертикальные-поля целое-неотрицательное? 0]
          [включен булево? #,(elem (racketvalfont "истина"))])
         горизонтальная-панель%]{
Создаётся формой @racket[объект]:
@racketblock[
объект горизонтальная-панель%
  родитель родитель
  стиль стиль
  граница граница
  интервал интервал
  выравнивание выравнивание
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  растягивается-ширина растягивается-ширина
  растягивается-высота растягивается-высота
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
  включен включен
]

Выравнивание по умолчанию @racket['(лево центр)]. @racket[родитель] обязателен.
@racket[минимальная-ширина] и @racket[минимальная-высота] по умолчанию @racketvalfont{ложь} --- размер
по содержимому.}

@defproc*[#:kind "метод"
          ([(ориентация) любой]
           [(ориентация [значение любой]) пусто?])]{
Без аргумента: не @racketvalfont{ложь}, если раскладка горизонтальная; с аргументом задаёт
горизонтальную (@racketvalfont{истина}) или вертикальную раскладку.}

@subsubsection[#:tag "ref-вертикальная-панель%"]{@racket[вертикальная-панель%]}

@defthing[#:kind "класс" вертикальная-панель% класс?]{
Базовый класс: @racket[панель%].
Дети сверху вниз. Выравнивание по умолчанию @racket['(центр верх)].
Методы @racket[вместилище-окно<%>] и @racket[подокно<%>] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (вертикальная-панель% [родитель любой]
          [стиль любой '()]
          [граница целое-неотрицательное? 0]
                             [интервал целое-неотрицательное? 0]
                             [выравнивание любой '(центр верх)]
          [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                             [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
                             [растягивается-ширина булево?
                              #,(elem (racketvalfont "истина"))]
                             [растягивается-высота булево?
                              #,(elem (racketvalfont "истина"))]
          [горизонтальные-поля целое-неотрицательное? 0]
                             [вертикальные-поля целое-неотрицательное? 0]
          [включен булево? #,(elem (racketvalfont "истина"))])
         вертикальная-панель%]{
Создаётся формой @racket[объект]:
@racketblock[
объект вертикальная-панель%
  родитель родитель
  стиль стиль
  граница граница
  интервал интервал
  выравнивание выравнивание
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  растягивается-ширина растягивается-ширина
  растягивается-высота растягивается-высота
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
  включен включен
]

Выравнивание по умолчанию @racket['(центр верх)]. @racket[родитель] обязателен.
@racket[минимальная-ширина] и @racket[минимальная-высота] по умолчанию @racketvalfont{ложь} --- размер
по содержимому.}

@defproc*[#:kind "метод" #:link-target? #f
          ([(ориентация) любой]
           [(ориентация [значение любой]) пусто?])]{
Без аргумента: не @racketvalfont{ложь}, если раскладка горизонтальная; с аргументом задаёт
горизонтальную (@racketvalfont{истина}) или вертикальную раскладку.}

@subsubsection[#:tag "ref-табличная-панель%"]{@racket[табличная-панель%]}

@defthing[#:kind "класс" табличная-панель% класс?]{
Базовый класс: @racket[панель%].
Подключается отдельно: @racketidfont{графический-интерфейс/табличная-панель}. Дети в сетке рядов и
колонок. Выравнивание по умолчанию @racket['(лево центр)].
Методы @racket[вместилище-окно<%>] и @racket[подокно<%>] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (табличная-панель% [родитель любой]
                    [стиль любой '()]
                    [измерения любой '(1 1)]
                    [заполнять символ? 'ряд]
                    [колонки-растягиваются символ? 'любая]
                    [ряды-растягиваются символ? 'любая]
                    [граница целое-неотрицательное? 0]
                                       [интервал целое-неотрицательное? 0]
                                       [выравнивание любой '(лево центр)]
                    [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                                       [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
                                       [растягивается-ширина булево?
                                        #,(elem (racketvalfont "истина"))]
                                       [растягивается-высота булево?
                                        #,(elem (racketvalfont "истина"))]
                    [горизонтальные-поля целое-неотрицательное? 0]
                                       [вертикальные-поля целое-неотрицательное? 0]
                    [включен булево? #,(elem (racketvalfont "истина"))])
         табличная-панель%]{
Создаётся формой @racket[объект]:
@racketblock[
объект табличная-панель%
  родитель родитель
  стиль стиль
  измерения измерения
  заполнять заполнять
  колонки-растягиваются колонки-растягиваются
  ряды-растягиваются ряды-растягиваются
  граница граница
  интервал интервал
  выравнивание выравнивание
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  растягивается-ширина растягивается-ширина
  растягивается-высота растягивается-высота
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
  включен включен
]

@racket[измерения] --- список из числа рядов и числа колонок, по умолчанию @racket['(1 1)].
@racket[заполнять] --- @racket['ряд] (по умолчанию) или @racket['колонку].
@racket[колонки-растягиваются] и @racket[ряды-растягиваются] --- @racket['любая] или @racket['каждая],
по умолчанию @racket['любая]. @racket[минимальная-ширина] и @racket[минимальная-высота] по умолчанию
@racketvalfont{ложь} --- размер по содержимому.}

@defproc*[#:kind "метод"
          ([(измерения) любой]
           [(измерения [ряды целое?] [колонки целое?]) пусто?])]{
Читает или задаёт число рядов и колонок.}

@defproc*[#:kind "метод"
          ([(заполнять) любой]
           [(заполнять [значение символ?]) пусто?])]{
Читает или задаёт, что заполнять в первую очередь: @racket['ряд] или @racket['колонку].}

@defproc*[#:kind "метод"
          ([(колонки-растягиваются) любой]
           [(колонки-растягиваются [значение символ?]) пусто?])]{
Читает или задаёт растягивание колонок: @racket['любая] или @racket['каждая].}

@defproc*[#:kind "метод"
          ([(ряды-растягиваются) любой]
           [(ряды-растягиваются [значение символ?]) пусто?])]{
Читает или задаёт растягивание рядов: @racket['любая] или @racket['каждая].}

@subsubsection[#:tag "ref-кнопка%"]{@racket[кнопка%]}

@defthing[#:kind "класс" кнопка% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%], реализует интерфейс
@racket[элемент-управления<%>].
Модуль @racketidfont{графический-интерфейс/кнопка}.
Методы @racket[элемент-управления<%>] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (кнопка% [заголовок любой]
          [родитель любой]
          [команда функция? (функция (кнопка событие-управления%) пусто)]
          [шрифт шрифт% шрифт-элемента-управления]
                             [стиль любой '()]
                             [растягивается-ширина булево? #,(elem (racketvalfont "ложь"))]
                             [растягивается-высота булево? #,(elem (racketvalfont "ложь"))]
          [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                             [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
          [горизонтальные-поля целое-неотрицательное? 0]
                             [вертикальные-поля целое-неотрицательное? 0]
          [включен булево? #,(elem (racketvalfont "истина"))])
         кнопка%]{
Создаётся формой @racket[объект]:
@racketblock[
объект кнопка%
  заголовок заголовок
  родитель родитель
  команда команда
  шрифт шрифт
  стиль стиль
  растягивается-ширина растягивается-ширина
  растягивается-высота растягивается-высота
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
  включен включен
]

@racket[заголовок] --- строка или изображение (обязателен). @racket[родитель] обязателен.
@racket[команда] вызывается при нажатии. @racket[стиль] --- @racket['граница],
@racket['многострочная], @racket['скрытая]. @racket[шрифт] по умолчанию
@racket[шрифт-элемента-управления]. @racket[минимальная-ширина] и @racket[минимальная-высота] по
умолчанию @racketvalfont{ложь} --- размер по содержимому.}

@subsubsection[#:tag "ref-флажок%"]{@racket[флажок%]}

@defthing[#:kind "класс" флажок% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%], реализует интерфейс
@racket[элемент-управления<%>].
Флажок (галочка). Модуль @racketidfont{графический-интерфейс/флажок}.
Методы @racket[элемент-управления<%>] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (флажок% [заголовок любой]
          [родитель любой]
          [значение булево? #,(elem (racketvalfont "ложь"))]
          [команда функция? (функция (флажок событие-управления%) пусто)]
          [шрифт шрифт% шрифт-элемента-управления]
                             [стиль любой '()]
                             [растягивается-ширина булево? #,(elem (racketvalfont "ложь"))]
                             [растягивается-высота булево? #,(elem (racketvalfont "ложь"))]
          [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                             [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
          [горизонтальные-поля целое-неотрицательное? 0]
                             [вертикальные-поля целое-неотрицательное? 0]
          [включен булево? #,(elem (racketvalfont "истина"))])
         флажок%]{
Создаётся формой @racket[объект]:
@racketblock[
объект флажок%
  заголовок заголовок
  родитель родитель
  значение значение
  команда команда
  шрифт шрифт
  стиль стиль
  растягивается-ширина растягивается-ширина
  растягивается-высота растягивается-высота
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
  включен включен
]

@racket[заголовок] --- строка или @racket[изображение%] (обязателен). @racket[родитель] обязателен.
@racket[значение] --- начальное состояние, по умолчанию @racketvalfont{ложь}. @racket[команда]
вызывается при переключении пользователем; тип события --- @racket['флажок]. @racket[стиль] ---
@racket['скрытая]. @racket[шрифт] по умолчанию @racket[шрифт-элемента-управления].
@racket[минимальная-ширина] и @racket[минимальная-высота] по умолчанию @racketvalfont{ложь} --- размер
по содержимому.}

@defproc*[#:kind "метод"
          ([(значение) булево?]
           [(значение [новое-значение булево?]) пусто?])]{
Читает или записывает состояние флажка (без вызова @racket[команда]).}

@subsubsection[#:tag "ref-надпись%"]{@racket[надпись%]}

@defthing[#:kind "класс" надпись% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%], реализует интерфейс
@racket[элемент-управления<%>].
Статический текст или картинка. Модуль @racketidfont{графический-интерфейс/надпись}.
Методы @racket[элемент-управления<%>] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (надпись% [заголовок любой]
           [родитель любой]
           [цвет любой #,(elem (racketvalfont "ложь"))]
           [размер-по-заголовку булево? #,(elem (racketvalfont "ложь"))]
           [шрифт шрифт% шрифт-элемента-управления]
                              [стиль любой '()]
                              [растягивается-ширина булево? #,(elem (racketvalfont "ложь"))]
                              [растягивается-высота булево? #,(elem (racketvalfont "ложь"))]
           [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                              [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
           [горизонтальные-поля целое-неотрицательное? 0]
                              [вертикальные-поля целое-неотрицательное? 0]
           [включен булево? #,(elem (racketvalfont "истина"))])
         надпись%]{
Создаётся формой @racket[объект]:
@racketblock[
объект надпись%
  заголовок заголовок
  родитель родитель
  цвет цвет
  размер-по-заголовку размер-по-заголовку
  шрифт шрифт
  стиль стиль
  растягивается-ширина растягивается-ширина
  растягивается-высота растягивается-высота
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
  включен включен
]

@racket[заголовок] --- строка или @racket[изображение%] (обязателен). @racket[цвет] по умолчанию
@racketvalfont{ложь}. @racket[размер-по-заголовку] по умолчанию @racketvalfont{ложь}. @racket[шрифт]
по умолчанию @racket[шрифт-элемента-управления]. @racket[минимальная-ширина] и
@racket[минимальная-высота] по умолчанию @racketvalfont{ложь} --- размер по содержимому.}

@defproc*[#:kind "метод"
          ([(цвет) любой]
           [(цвет [значение любой]) пусто?])]{
Цвет текста (чтение и запись).}

@defproc*[#:kind "метод"
          ([(размер-по-заголовку) булево?]
           [(размер-по-заголовку [значение булево?]) пусто?])]{
Подгонять ли размер под текущий заголовок (чтение и запись).}

@subsubsection[#:tag "ref-текстовое-поле%"]{@racket[текстовое-поле%]}

@defthing[#:kind "класс" текстовое-поле% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%], реализует интерфейс
@racket[элемент-управления<%>].
Однострочное или многострочное поле. Модуль @racketidfont{графический-интерфейс/текстовое-поле}.
Методы @racket[элемент-управления<%>] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (текстовое-поле% [родитель любой]
                  [заголовок любой #,(elem (racketvalfont "ложь"))]
                  [значение строка? ""]
                  [при-изменении функция? #,(elem (racketvalfont "ложь"))]
                  [стиль любой '(однострочное)]
                  [растягивается-ширина булево? #,(elem (racketvalfont "истина"))]
                  [шрифт шрифт% шрифт-элемента-управления]
                  [растягивается-высота булево? #,(elem (racketvalfont "ложь"))]
                  [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                                     [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
                  [горизонтальные-поля целое-неотрицательное? 0]
                                     [вертикальные-поля целое-неотрицательное? 0]
                  [включен булево? #,(elem (racketvalfont "истина"))])
         текстовое-поле%]{
Создаётся формой @racket[объект]:
@racketblock[
объект текстовое-поле%
  родитель родитель
  заголовок заголовок
  значение значение
  при-изменении при-изменении
  стиль стиль
  растягивается-ширина растягивается-ширина
  шрифт шрифт
  растягивается-высота растягивается-высота
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
  включен включен
]

@racket[заголовок] --- подпись, по умолчанию @racketvalfont{ложь}. @racket[стиль] ---
@racket['(однострочное)], @racket['многострочное] или @racket['скрытая]. @racket[шрифт] по умолчанию
@racket[шрифт-элемента-управления]. @racket[минимальная-ширина] и @racket[минимальная-высота] по
умолчанию @racketvalfont{ложь} --- размер по содержимому. @racket[растягивается-ширина] по умолчанию
@racketvalfont{истина}. При правке вызывается @racketidfont{при-изменении}; тип события ---
@racket['текстовое-поле] или @racket['ввод-в-текстовом-поле].}

@defproc*[#:kind "метод" #:link-target? #f
          ([(значение) строка?]
           [(значение [новое-значение строка?]) пусто?])]{
Читает или записывает текст поля.}

@subsubsection[#:tag "ref-поле-даты%"]{@racket[поле-даты%]}

@defthing[#:kind "класс" поле-даты% класс?]{
Базовый класс: @racket[текстовое-поле%].
Подключается отдельно: @racketidfont{графический-интерфейс/поле-даты}. Поле ввода даты.
Методы @racket[элемент-управления<%>] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (поле-даты% [родитель любой]
              [заголовок любой #,(elem (racketvalfont "ложь"))]
              [значение строка? ""]
              [при-изменении функция? #,(elem (racketvalfont "ложь"))]
              [стиль любой '(однострочное)]
              [растягивается-ширина булево? #,(elem (racketvalfont "истина"))]
              [шрифт шрифт% шрифт-элемента-управления]
                                [растягивается-высота булево? #,(elem (racketvalfont "ложь"))]
              [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                                 [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
              [горизонтальные-поля целое-неотрицательное? 0]
                                 [вертикальные-поля целое-неотрицательное? 0]
              [включен булево? #,(elem (racketvalfont "истина"))])
         поле-даты%]{
Создаётся формой @racket[объект]:
@racketblock[
объект поле-даты%
  родитель родитель
  заголовок заголовок
  значение значение
  при-изменении при-изменении
  стиль стиль
  растягивается-ширина растягивается-ширина
  шрифт шрифт
  растягивается-высота растягивается-высота
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
  включен включен
]

Те же аргументы и методы, что у @racket[текстовое-поле%].}

@subsubsection[#:tag "ref-строка-меню%"]{@racket[строка-меню%]}

@defthing[#:kind "класс" строка-меню% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Строка меню окна верхнего уровня. Определена в сводке @racketidfont{графический-интерфейс}.
}

@defproc[#:kind "конструктор" #:link-target? #f (строка-меню% [родитель любой])
         строка-меню%]{
Создаётся формой @racket[объект]:
@racketblock[
объект строка-меню%
  родитель родитель
]

@racket[родитель] обязателен: @racket[главное-окно%] или @racket[диалог%]. В строку кладут объекты
@racket[меню%].}

@subsubsection[#:tag "ref-меню%"]{@racket[меню%]}

@defthing[#:kind "класс" меню% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Выпадающее меню. Определено в сводке @racketidfont{графический-интерфейс}.
}

@defproc[#:kind "конструктор" #:link-target? #f (меню% [родитель любой] [заголовок любой])
         меню%]{
Создаётся формой @racket[объект]:
@racketblock[
объект меню%
  родитель родитель
  заголовок заголовок
]

@racket[родитель] и @racket[заголовок] обязательны.}

@subsubsection[#:tag "ref-всплывающее-меню%"]{@racket[всплывающее-меню%]}

@defthing[#:kind "класс" всплывающее-меню% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Контекстное меню. Определено в сводке @racketidfont{графический-интерфейс}.
}

@defproc[#:kind "конструктор" #:link-target? #f (всплывающее-меню% [заголовок строка? ""])
         всплывающее-меню%]{
Создаётся формой @racket[объект]:
@racketblock[
объект всплывающее-меню%
  заголовок заголовок
]

Показывают методом @racket[контекстное-меню] окна. Пункты --- @racket[пункт-меню%].}

@subsubsection[#:tag "ref-пункт-меню%"]{@racket[пункт-меню%]}

@defthing[#:kind "класс" пункт-меню% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Реализует @racket[пункт-меню<%>] и @racket[пункт-меню-с-заголовком<%>].
Методы @racket[пункт-меню<%>] и @racket[пункт-меню-с-заголовком<%>] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (пункт-меню% [родитель любой]
              [заголовок любой]
              [действие функция? (функция (пункт событие-управления%) пусто)])
         пункт-меню%]{
Создаётся формой @racket[объект]:
@racketblock[
объект пункт-меню%
  родитель родитель
  заголовок заголовок
  действие действие
]

Все три аргумента обязательны.}

@subsubsection[#:tag "ref-шрифт%"]{@racket[шрифт%]}

@defthing[#:kind "класс" шрифт% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Описание шрифта. Модуль @racketidfont{графический-интерфейс/шрифт}.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (шрифт% [размер число? 12]
         [семейство любой 'default]
         [имя любой #,(elem (racketvalfont "ложь"))]
         [стиль символ? 'нормальный]
         [насыщенность символ? 'нормальная]
         [подчёркнутый? булево? #,(elem (racketvalfont "ложь"))]
         [сглаживание символ? 'стандартное]
         [размер-в-пикселях? булево? #,(elem (racketvalfont "ложь"))]
         [округлять-метрики? булево? #,(elem (racketvalfont "истина"))]
         [свойства соответствие? #hash()])
         шрифт%]{
Создаётся формой @racket[объект]:
@racketblock[
объект шрифт%
  размер размер
  семейство семейство
  имя имя
  стиль стиль
  насыщенность насыщенность
  подчёркнутый? подчёркнутый?
  сглаживание сглаживание
  размер-в-пикселях? размер-в-пикселях?
  округлять-метрики? округлять-метрики?
  свойства свойства
]

@racket[имя] по умолчанию @racketvalfont{ложь}. @racket[стиль] --- @racket['нормальный],
@racket['наклонный] или @racket['курсив]. @racket[насыщенность] --- @racket['тонкая] …
@racket['сверхтяжёлая]. @racket[сглаживание] --- @racket['стандартное], @racket['частичное],
@racket['полное], @racket['отсутствует].}

@defproc[#:kind "метод" (размер [в-пикселях? булево?])
         число?]{
Кегль; необязательный аргумент: считать ли размер в пикселях.}

@defproc[#:kind "метод" (размер-в-пикселях?)
         булево?]{
Задан ли кегль в пикселях.}

@defproc[#:kind "метод" (семейство)
         любой]{
Семейство шрифта.}

@defproc[#:kind "метод" (имя)
         любой]{
Имя начертания.}

@defproc[#:kind "метод" (стиль)
         символ?]{
Стиль: @racket['нормальный], @racket['наклонный] или @racket['курсив].}

@defproc[#:kind "метод" (насыщенность)
         символ?]{
Насыщенность начертания.}

@defproc[#:kind "метод" (подчёркнутый?)
         булево?]{
Подчёркнут ли шрифт.}

@defproc[#:kind "метод" (сглаживание)
         символ?]{
Режим сглаживания.}

@defproc[#:kind "метод" (округлять-метрики?)
         булево?]{
Округлять ли метрики.}

@defproc[#:kind "метод" (свойства)
         соответствие?]{
Дополнительные свойства.}

@defproc[#:kind "метод" (идентификатор)
         целое?]{
Целое число начертания в системе.}

@defproc[#:kind "метод" (есть-литера? [литера любой]
         [для-заголовка? булево? #,(elem (racketvalfont "ложь"))])
         булево?]{
Есть ли знак для литеры; @racket[для-заголовка?] по умолчанию @racketvalfont{ложь}.}

@defthing[#:kind "значение" шрифт-элемента-управления шрифт%]{
Стандартный шрифт кнопок, полей и надписей.}

@defproc[#:kind "функция" (на-основе-шрифта [шрифт шрифт%]
                   [#:размер размер число?]
                   [#:семейство семейство любой]
                   [#:имя имя любой]
                   [#:стиль стиль любой]
                   [#:насыщенность насыщенность любой]
                   [#:подчёркнутый? подчёркнутый? булево?]
                   [#:сглаживание сглаживание любой]
                   [#:размер-в-пикселях? размер-в-пикселях? булево?]
                   [#:округлять-метрики? округлять-метрики? булево?]
                   [#:свойства свойства соответствие?])
         шрифт%]{
Новый шрифт, копирующий @racket[шрифт]. Не указанные ключевые аргументы берутся из исходного шрифта.}

@subsubsection[#:tag "ref-событие%"]{@racket[событие%]}

@defthing[#:kind "класс" событие% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Базовое событие. Модуль @racketidfont{графический-интерфейс/событие}.
}

@defproc[#:kind "конструктор" #:link-target? #f (событие% [время целое? 0])
         событие%]{
Создаётся формой @racket[объект]:
@racketblock[
объект событие%
  время время
]

Метка времени в миллисекундах (по умолчанию 0).}

@defproc*[#:kind "метод"
          ([(время) целое?]
           [(время [значение целое?]) пусто?])]{
Метка времени в миллисекундах (чтение и запись).}

@subsubsection[#:tag "ref-событие-мыши%"]{@racket[событие-мыши%]}

@defthing[#:kind "класс" событие-мыши% класс?]{
Базовый класс: @racket[событие%].
Событие мыши. Обычно получают через @racket[событие-мыши-адины].
Методы @racket[событие%] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (событие-мыши% [тип символ? 'вход]
                [время целое? 0]
                [внутренний любой])
         событие-мыши%]{
Создаётся формой @racket[объект]:
@racketblock[
объект событие-мыши%
  тип тип
  время время
  внутренний внутренний
]

@racket[тип] --- @racket['вход], @racket['выход], @racket['левая-нажата], @racket['движение] и др.}

@defproc*[#:kind "метод"
          ([(тип) символ?]
           [(тип [значение символ?]) пусто?])]{
Тип события мыши (чтение и запись).}

@defproc[#:kind "метод" #:link-target? #f (лево)
         число?]{
Координата лево в окне.}

@defproc[#:kind "метод" #:link-target? #f (верх)
         число?]{
Координата верх в окне.}

@defproc[#:kind "метод" (нажата-кнопка? [кнопка символ? 'любая])
         булево?]{
С аргументом @racket['левая], @racket['правая], @racket['средняя] или @racket['любая].}

@defproc[#:kind "метод" (отпущена-кнопка? [кнопка символ? 'любая])
         булево?]{
С аргументом @racket['левая], @racket['правая], @racket['средняя] или @racket['любая].}

@subsubsection[#:tag "ref-событие-клавиши%"]{@racket[событие-клавиши%]}

@defthing[#:kind "класс" событие-клавиши% класс?]{
Базовый класс: @racket[событие%].
Событие клавиатуры.
Методы @racket[событие%] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f (событие-клавиши% [внутренний любой])
         событие-клавиши%]{
Создаётся формой @racket[объект]:
@racketblock[
объект событие-клавиши%
  внутренний внутренний
]

Обычно получают через @racket[событие-клавиши-адины].}

@defproc[#:kind "метод" (клавиша)
         любой]{
Код нажатой клавиши (литера или символ специальной клавиши).}

@subsubsection[#:tag "ref-событие-управления%"]{@racket[событие-управления%]}

@defthing[#:kind "класс" событие-управления% класс?]{
Базовый класс: @racket[событие%].
Событие кнопки, поля, меню.
Методы @racket[событие%] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f (событие-управления% [внутренний любой])
         событие-управления%]{
Создаётся формой @racket[объект]:
@racketblock[
объект событие-управления%
  внутренний внутренний
]

Обычно получают через @racket[событие-управления-адины].}

@defproc*[#:kind "метод" #:link-target? #f
          ([(тип) символ?]
           [(тип [значение символ?]) пусто?])]{
Тип события (чтение и запись): @racket['кнопка], @racket['флажок], @racket['текстовое-поле],
@racket['меню] и др.}

@defproc[#:kind "функция" (событие-мыши-адины [событие любой])
         событие-мыши%]{
Оборачивает внутреннее событие мыши в @racket[событие-мыши%].}

@defproc[#:kind "функция" (событие-клавиши-адины [событие любой])
         событие-клавиши%]{
Оборачивает внутреннее событие клавиши в @racket[событие-клавиши%].}

@defproc[#:kind "функция" (событие-управления-адины [событие любой])
         событие-управления%]{
Оборачивает внутреннее событие управления в @racket[событие-управления%].}

@subsubsection[#:tag "ref-картина%"]{@racket[картина%]}

@defthing[#:kind "класс" картина% класс?]{

Базовый класс @racket[объект-графического-интерфейса%], реализует интерфейс @racket[картина<%>].

Холст для произвольного рисования и обработки событий мыши и клавиатуры.
Модуль @racketidfont{графический-интерфейс/картина}.

Рисование обычно делают в @racketidfont{обработка} или в
@racket[при-отрисовке], получая @racket[холст%] через
@racket[холст]. Система вызывает отрисовку при показе, изменении
размера и по запросу @racket[обновить]; несколько запросов
@racket[обновить] могут объединиться в один вызов
@racket[при-отрисовке].

Методы @racket[подокно<%>] и @racket[картина<%>] тоже доступны у объекта.}

@defproc[#:kind "конструктор" #:link-target? #f
         (картина% [родитель любой]
                   [заголовок любой #,(elem (racketvalfont "ложь"))]
                   [обработка функция? (функция (картина холст) пусто)]
                   [стиль любой '()]
                   [горизонтальные-поля целое-неотрицательное? 0]
                   [вертикальные-поля целое-неотрицательное? 0]
                   [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                                      [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
                                      [растягивается-ширина булево? #,(elem (racketvalfont "истина"))]
                                      [растягивается-высота булево? #,(elem (racketvalfont "истина"))]
                   [включен булево? #,(elem (racketvalfont "истина"))])
         картина%]{
Создаётся формой @racket[объект]:
@racketblock[
объект картина%
  родитель родитель
  заголовок заголовок
  обработка обработка
  стиль стиль
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  растягивается-ширина растягивается-ширина
  растягивается-высота растягивается-высота
  включен включен
]

@racket[родитель] --- вместилище, в котором создаётся картина (обязателен).

@racket[заголовок] --- имя картины для @racket[заголовок]; на экране
не показывается. По умолчанию @racketvalfont{ложь}.

@racket[стиль] --- список символов (по умолчанию пустой): @racket['рамка],
@racket['рамка-элемента-управления], @racket['комбинированное],
@racket['горизонтальная-прокрутка], @racket['вертикальная-прокрутка],
@racket['угол-изменения-размера], @racket['трёхмерная], @racket['без-автоочистки],
@racket['прозрачная], @racket['без-фокуса], @racket['скрытая].

@racket[обработка] --- функция от картины и холста; вызывается, когда
@racket[при-отрисовке] возвращает @racketvalfont{ложь}.
Холст уже обёрнут в @racket[холст%]. По умолчанию ничего не делает.

@racket[горизонтальные-поля] и @racket[вертикальные-поля] --- внутренние
отступы у краёв картины (по умолчанию 0). @racket[минимальная-ширина] и
@racket[минимальная-высота] по умолчанию @racketvalfont{ложь} --- размер по содержимому.
@racket[растягивается-ширина] и @racket[растягивается-высота] по умолчанию @racketvalfont{истина}.
@racket[включен] по умолчанию @racketvalfont{истина}.}

@defproc*[#:kind "метод" #:link-target? #f
          ([(принимать-фокус-табом) булево?]
           [(принимать-фокус-табом [значение любой]) пусто?])]{
Читает или задаёт, получает ли картина клавиатурный фокус при обходе
табуляцией и стрелками. По умолчанию @racketvalfont{ложь}.

Если фокус по табуляции включён, клавиши табуляции, стрелок, ввода и
отмены обрабатываются @racket[при-получении-клавиши] окна верхнего
уровня, а не доходят до картины.}

@defproc[#:kind "метод" #:link-target? #f (отрисовать-немедленно) пусто?]{
Сбрасывает отложенную отрисовку картины на экран.}

@defproc*[#:kind "метод" #:link-target? #f
          ([(фон) любой]
           [(фон [цвет цвет%]) пусто?])]{
Без аргумента возвращает цвет, которым стирают картину перед
@racket[при-отрисовке]. С аргументом задаёт этот цвет.

Если картина создана с прозрачным стилем, чтение возвращает
@racketvalfont{ложь}; запись в таком случае ошибка.}

@defproc[#:kind "метод" #:link-target? #f (холст) любой]{
Возвращает @racket[холст%] для рисования на картине или
@racketvalfont{ложь}, если внутренний холст ещё не создан.}

@defproc*[#:kind "метод" #:link-target? #f
          ([(минимальная-ширина-холста) целое-неотрицательное?]
           [(минимальная-ширина-холста [ширина целое-неотрицательное?])
            пусто?])]{
Читает или задаёт минимальную ширину клиентской области картины (без
рамки и полос прокрутки) для раскладки. Значение меньше графического
минимума игнорируется.}

@defproc*[#:kind "метод" #:link-target? #f
          ([(минимальная-высота-холста) целое-неотрицательное?]
           [(минимальная-высота-холста [высота целое-неотрицательное?])
            пусто?])]{
Читает или задаёт минимальную высоту клиентской области картины (без
рамки и полос прокрутки) для раскладки. Значение меньше графического
минимума игнорируется.}

@defproc[#:kind "метод" #:link-target? #f (приостановить-отрисовку) пусто?]{
Временно отключает сброс буфера картины на экран. Вызовы можно
вкладывать; сброс возобновляется после стольких же вызовов
@racket[продолжить-отрисовку].}

@defproc[#:kind "метод" #:link-target? #f (продолжить-отрисовку) пусто?]{
Возобновляет сброс буфера после @racket[приостановить-отрисовку].
См. @racket[приостановить-отрисовку].}

@defproc[#:kind "метод" #:link-target? #f
         (при-получении-клавиши [событие событие-клавиши%])
         любой]{
Этот метод переопределяется формой @racket[переопределить].

Вызывается, когда картина получает событие клавиатуры. Если вернуть не
@racketvalfont{ложь}, стандартная обработка не выполняется.

В базовом классе возвращает @racketvalfont{ложь}.}

@defproc[#:kind "метод" #:link-target? #f
         (при-получении-события [событие событие-мыши%])
         любой]{
Этот метод переопределяется формой @racket[переопределить].

Вызывается, когда картина получает событие мыши. Если вернуть не
@racketvalfont{ложь}, стандартная обработка не выполняется.

В базовом классе возвращает @racketvalfont{ложь}.}

@defproc[#:kind "метод" #:link-target? #f (при-отрисовке) любой]{
Этот метод переопределяется формой @racket[переопределить].

Вызывается, когда картину нужно перерисовать (показ, изменение размера,
запрос @racket[обновить]). Если вернуть не @racketvalfont{ложь},
стандартная отрисовка (в том числе @racketidfont{обработка}) не
выполняется.

В базовом классе возвращает @racketvalfont{ложь}; тогда вызывается
@racketidfont{обработка}.}

@defproc[#:kind "метод" #:link-target? #f (при-входе-табом) любой]{
Этот метод переопределяется формой @racket[переопределить].

Вызывается, когда клавиатурный фокус входит на картину обходом
табуляцией (событие @racket[при-изменении-фокуса] тоже
вызывается). Если вернуть не @racketvalfont{ложь}, стандартная
обработка не выполняется.

В базовом классе возвращает @racketvalfont{ложь}.}

@subsubsection[#:tag "ref-поле-редактора%"]{@racket[поле-редактора%]}

@defthing[#:kind "класс" поле-редактора% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%], реализует интерфейс @racket[картина<%>].
Холст, показывающий документ-редактор. Модуль @racketidfont{графический-интерфейс/поле-редактора}.
Методы @racket[картина<%>] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (поле-редактора% [родитель любой]
                   [заголовок любой #,(elem (racketvalfont "ложь"))]
                   [редактор любой #,(elem (racketvalfont "ложь"))]
                   [стиль любой '()]
                   [горизонтальные-поля целое-неотрицательное? 0]
                   [вертикальные-поля целое-неотрицательное? 0]
                   [минимальная-ширина любой #,(elem (racketvalfont "ложь"))]
                                      [минимальная-высота любой #,(elem (racketvalfont "ложь"))]
                                      [растягивается-ширина булево?
                                       #,(elem (racketvalfont "истина"))]
                                      [растягивается-высота булево?
                                       #,(elem (racketvalfont "истина"))]
                   [включен булево? #,(elem (racketvalfont "истина"))])
         поле-редактора%]{
Создаётся формой @racket[объект]:
@racketblock[
объект поле-редактора%
  родитель родитель
  заголовок заголовок
  редактор редактор
  стиль стиль
  горизонтальные-поля горизонтальные-поля
  вертикальные-поля вертикальные-поля
  минимальная-ширина минимальная-ширина
  минимальная-высота минимальная-высота
  растягивается-ширина растягивается-ширина
  растягивается-высота растягивается-высота
  включен включен
]

Аргументы как у @racket[картина%], вместо @racket[обработка] --- @racket[редактор] (@racket[текст%]
или другой @racket[редактор<%>], по умолчанию @racketvalfont{ложь} --- пустое поле).
@racket[стиль] --- список символов (по умолчанию пустой): @racket['без-рамки],
@racket['рамка-элемента-управления], @racket['комбинированное],
@racket['без-горизонтальной-прокрутки], @racket['без-вертикальной-прокрутки],
@racket['скрыть-горизонтальную-прокрутку], @racket['скрыть-вертикальную-прокрутку],
@racket['авто-горизонтальная-прокрутка], @racket['авто-вертикальная-прокрутка],
@racket['угол-изменения-размера], @racket['без-фокуса], @racket['скрытая],
@racket['прозрачная].
@racket[минимальная-ширина] и @racket[минимальная-высота] по умолчанию @racketvalfont{ложь} --- размер
по содержимому.}

@defproc*[#:kind "метод" #:link-target? #f
          ([(редактор) любой]
           [(редактор [новое-значение любой]) пусто?])]{
Читает или задаёт документ поля. Без документа --- @racketvalfont{ложь}.}

@subsubsection[#:tag "ref-текст%"]{@racket[текст%]}

@defthing[#:kind "класс" текст% класс?]{

Базовый класс @racket[объект-графического-интерфейса%], 
реализует интерфейс @racket[редактор<%>].

Обычный текстовый документ. На экране его показывают через
@racket[поле-редактора%] или другую картину. Модуль
@racketidfont{графический-интерфейс/текст}.

Методы @racket[редактор<%>] тоже доступны у объекта.}

@defproc[#:kind "конструктор" #:link-target? #f
         (текст% [межстрочный-промежуток число? 1.0]
                 [автоматический-перенос любой #,(elem (racketvalfont "ложь"))])
         текст%]{
Создаётся формой @racket[объект]:
@racketblock[
объект текст%
  межстрочный-промежуток межстрочный-промежуток
  автоматический-перенос автоматический-перенос
]

@racket[межстрочный-промежуток] --- дополнительный промежуток между
строками при показе (входит в сообщаемую высоту строки).

Если @racket[автоматический-перенос] не @racketvalfont{ложь}, включается
перенос длинных строк; по умолчанию @racketvalfont{ложь}.

Для нового документа создаются свой @racket[набор-команд%] и свой
@racket[список-стилей%].}

@defproc[#:kind "метод"
         (после-вставки [начало целое-неотрицательное?]
                        [длина целое-неотрицательное?])
         пусто?]{
Этот метод дополняется формой @racket[дополнить].

Вызывается после вставки элементов (и после обновления показа).
@racket[начало] --- позиция начала вставки, @racket[длина] --- число
вставленных элементов.

В базовом классе ничего не делает. Чтобы при правке в этом методе не
было лишних перерисовок, используйте
@racket[начать-последовательность-редактирования].
Внутренние замки при вызове не ставятся.

См. также @racket[можно-менять-стиль?] и
@racket[при-начале-последовательности-редактировании].}

@defproc[#:kind "метод"
         (после-объединения-кусков [позиция целое-неотрицательное?])
         пусто?]{
Этот метод дополняется формой @racket[дополнить].

Вызывается после слияния соседних кусков в один. @racket[позиция] ---
место, где слились куски: один был сразу перед ней, другой сразу после,
новый охватывает её.

В базовом классе ничего не делает.}

@defproc[#:kind "метод"
         (можно-менять-стиль? [начало целое-неотрицательное?]
                              [длина целое-неотрицательное?])
         булево?]{
Этот метод дополняется формой @racket[дополнить].

Вызывается перед сменой стиля на участке от @racket[начало] длиной
@racket[длина]. Если вернуть @racketvalfont{ложь}, смена отменяется.

Во время вызова документ заперт на запись; правки, если нужны, делайте
после смены стиля или через последовательность редактирования.

В базовом классе возвращает @racketvalfont{истина}.

См. также @racket[изменить-стиль] и
@racket[при-начале-последовательности-редактировании].}

@defproc[#:kind "метод"
         (изменить-стиль [стиль-или-отклонение любой]
                         [начало любой]
                         [конец любой]
                         [считать-изменением любой])
         пусто?]{
Меняет стиль участка, применяя @racket[отклонение-стиля%] или
устанавливая @racket[стиль%]. @racket[стиль-или-отклонение] может быть
@racketvalfont{ложь}.

Если @racket[начало] и @racket[конец] не заданы, меняется текущее
выделение. Если задано только @racket[начало], стиль меняется от него
до конца выделения.

Если @racket[считать-изменением] --- @racketvalfont{ложь}, документ не
помечается изменённым. По умолчанию считается изменением.

Чтобы сменить стиль у многих кусков сразу, лучше передать готовый
@racket[стиль%], а не отклонение: иначе отклонение приходится переводить
в стиль для каждого куска.}

@defproc[#:kind "метод" #:link-target? #f (очистить) пусто?]{
Удаляет всё содержимое документа.}

@defproc[#:kind "метод"
         (найти-позицию [лево число?] [верх число?])
         целое-неотрицательное?]{
По координатам @racket[лево] и @racket[верх] в документе возвращает
позицию в этом месте.}

@defproc[#:kind "метод"
         (найти-кусок [позиция целое-неотрицательное?]
                      [направление символ?])
         любой]{
Кусок в заданной @racket[позиция] или @racketvalfont{ложь}, если
подходящего куска нет.

Если позиция лежит между кусками, @racket[направление] выбирает, какой
вернуть:

@itemlist[
 @item{@racket['перед-или-пусто] --- кусок перед позицией или
  @racketvalfont{ложь} в начале документа;}
 @item{@racket['перед] --- кусок перед позицией или первый кусок
  в начале документа;}
 @item{@racket['после] --- кусок после позиции или последний кусок
  в конце документа;}
 @item{@racket['после-или-пусто] --- кусок после позиции или
  @racketvalfont{ложь} в конце документа и дальше.}
]}

@defproc*[#:kind "метод"
          ([(данные-куска [кусок кусок%]) любой]
           [(данные-куска [кусок кусок%] [данные любой]) пусто?])]{
Без @racket[данные] возвращает дополнительные данные куска или
@racketvalfont{ложь}, если данных нет. С куском и данными записывает
эти данные (например, положение куска).}

@defproc[#:kind "метод" (позиция-куска [кусок кусок%]) любой]{
Позиция начала @racket[кусок] в документе или @racketvalfont{ложь},
если куска в этом документе нет.}

@defproc*[#:kind "метод"
          ([(вставить [элемент любой]) пусто?]
           [(вставить [элемент любой]
                      [начало целое-неотрицательное?]) пусто?]
           [(вставить [элемент любой]
                      [начало целое-неотрицательное?]
                      [конец любой]) пусто?]
           [(вставить [элемент любой]
                      [начало целое-неотрицательное?]
                      [конец любой]
                      [прокрутка любой]) пусто?]
           [(вставить [элемент любой]
                      [начало целое-неотрицательное?]
                      [конец любой]
                      [прокрутка любой]
                      [объединять любой]) пусто?]
           [(вставить [количество целое-неотрицательное?]
                      [элемент любой]
                      [начало целое-неотрицательное?]
                      [конец любой]
                      [прокрутка любой]
                      [объединять любой]) пусто?])]{
Вставляет строку, кусок или литеру в документ в позицию @racket[начало].
Если задано @racket[количество], вставляются только первые столько литер
строки.

Кусок нельзя вставить в несколько документов или дважды в один: при
вставке его стиль переводится в список стилей документа.

Если @racket[начало] не указано, берётся начало текущего выделения.
Если выделена область, вставляемое значение заменяет её; начало и конец
выделения сдвигаются к концу вставки.

Если @racket[конец] задан и не совпадает с началом, вставка заменяет
область от начала до конца, и выделение остаётся в конце вставленного.
Иначе, если позиция вставки не позже начала или конца выделения, эти
границы сдвигаются на длину вставленного.

Если @racket[прокрутка] не @racketvalfont{ложь} и начало совпадает с
началом текущего выделения, показ прокручивается к новой позиции
выделения.

Если @racket[объединять] не @racketvalfont{ложь} (или вставляется литера),
соседние литеры, которые должны рисоваться как один знак, включаются во
вставку, чтобы знак не разорвался по разным кускам.}

@defproc[#:kind "метод" (последняя-позиция) целое-неотрицательное?]{
Последняя позиция выделения в документе; она же равна числу элементов
документа.}

@defproc[#:kind "метод" (начало-выделения) целое-неотрицательное?]{
Начальная позиция текущего выделения в документе.}

@defproc[#:kind "метод" (конец-выделения) целое-неотрицательное?]{
Конечная позиция текущего выделения в документе.}

@defproc[#:kind "метод" #:link-target? #f
         (текст [начало целое-неотрицательное? 0]
                [конец (или/c целое-неотрицательное? 'конец-файла) 'конец-файла]
                [сплющить? любой #,(elem (racketvalfont "ложь"))]
                [принудительный-перенос? любой #,(elem (racketvalfont "ложь"))])
         строка?]{
Возвращает текст документа от @racket[начало] до @racket[конец]. Если
@racket[конец] --- @racket['конец-файла], берётся фрагмент от
@racket[начало] до конца документа.

Если @racket[сплющить?] не @racketvalfont{ложь}, возвращается
«сплющенный» текст (см. Flattened Text в документации Racket). Если
@racket[принудительный-перенос?] не @racketvalfont{ложь}, в конец
каждого абзаца добавляется символ перевода строки.}

@defproc[#:kind "метод"
         (установить-выделение [начало целое-неотрицательное?]
                               [конец (или/c целое-неотрицательное? 'начало) 'начало]
                               [в-конце-строки? любой #,(elem (racketvalfont "ложь"))]
                               [прокрутить? любой #,(elem (racketvalfont "истина"))]
                               [тип-выделения (или/c 'по-умолчанию 'x 'локально)
                                               'по-умолчанию])
         пусто?]{
Задаёт текущее выделение в документе (в Racket --- @racketidfont{set-position}).

Если @racket[конец] --- @racket['начало] или не больше позиции
@racket[начало], и начало, и конец выделения становятся равны
@racket[начало] (курсор без выделения). Иначе выделяется диапазон от
@racket[начало] до @racket[конец].

Если @racket[прокрутить?] не @racketvalfont{ложь}, показ прокручивается
так, чтобы выделение стало видимым. @racket[тип-выделения] влияет на
механизм X selection (@racket['по-умолчанию], @racket['x],
@racket['локально]).}

@defproc[#:kind "метод" (только-просмотр [значение любой]) пусто?]{
Запирает документ от любых изменений, если @racket[значение] не
@racketvalfont{ложь}, и снимает запрет, если @racketvalfont{ложь}.
Запираются не только правки пользователя, но и программные.

На внутренние замки показа и пересчёта раскладки этот метод не влияет.}

@defproc[#:kind "метод"
         (конец-абзаца [абзац целое?]
                       [видимый? любой])
         целое-неотрицательное?]{
Конечная позиция абзаца с номером @racket[абзац]. Абзацы нумеруются с
нуля. @racket[видимый?] по умолчанию @racketvalfont{истина}.

Если абзацев меньше, чем номер плюс один, возвращается конец последнего
абзаца. Если номер меньше нуля --- конец первого абзаца.

Если абзац заканчивается невидимыми элементами (например, переносом
строки) и @racket[видимый?] не @racketvalfont{ложь}, возвращается первая
позиция перед этими элементами.}

@defproc[#:kind "метод"
         (прокрутить-к-позиции [позиция целое-неотрицательное?])
         булево?]{
Прокручивает показ так, чтобы @racket[позиция] стала видна. Возвращает
@racketvalfont{истина}, если документ прокрутился, иначе
@racketvalfont{ложь}.

Если обновление отложено, запрос запоминается до конца отложения.
Прокрутка запрещена, пока документ заперт на пересчёт раскладки.

Показ сам может прокручивать документ, не вызывая этот метод
(например, полоса прокрутки картины).}

@defproc[#:kind "метод"
         (обработчик-щелчка [начало целое-неотрицательное?]
                            [конец целое-неотрицательное?]
                            [функция функция?]
                            [отметка любой]
                            [при-нажатии? любой])
         пусто?]{
Ставит обработчик на участок от @racket[начало] до @racket[конец].
Если на пересекающийся участок уже был обработчик, новый имеет
преимущество.

@racket[функция] вызывается, когда пользователь выбирает участок: ей
передаются документ и границы участка.

@racket[отметка] --- @racket[отклонение-стиля%], которое применяется,
пока кнопка мыши удерживается над участком; по умолчанию
@racketvalfont{ложь} --- начертание при выборе не меняется.

Если @racket[при-нажатии?] не @racketvalfont{ложь} (по умолчанию
@racketvalfont{ложь}), функция вызывается сразу при нажатии, а не при
отпускании; отметка тогда не используется.}

@defproc[#:kind "функция" (для-каждого-куска [текст текст%] [функция функция?])
         пусто?]{
Вызывает @racket[функция] для каждого куска документа по порядку.}

@subsubsection[#:tag "ref-кусок%"]{@racket[кусок%]}

@defthing[#:kind "класс" кусок% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Фрагмент документа. Модуль @racketidfont{графический-интерфейс/текст}. Создаётся без обязательных
аргументов.
}

@defproc[#:kind "конструктор" #:link-target? #f (кусок% [внутренний любой])
         кусок%]{
Создаётся формой @racket[объект]:
@racketblock[
объект кусок%
  внутренний внутренний
]

Обычно куски делают наследниками или получают из документа.}

@defproc[#:kind "метод" (следующий)
         любой]{
Соседний кусок или @racketvalfont{ложь}.}

@defproc[#:kind "метод" (предыдущий)
         любой]{
Соседний кусок или @racketvalfont{ложь}.}

@defproc[#:kind "метод" #:link-target? #f (текст [смещение целое?] [количество целое?])
         строка?]{
Строка содержимого; аргументы: смещение и количество литер.}

@defproc[#:kind "метод" #:link-target? #f (скопировать)
         любой]{
Копия куска, ещё не вставленная в документ.}

@defproc*[#:kind "метод"
          ([(количество) целое?]
           [(количество [новое-значение целое?]) пусто?])]{
Число позиций, которые занимает кусок (чтение и запись).}

@defproc*[#:kind "метод" #:link-target? #f
          ([(стиль) стиль%]
           [(стиль [новое-значение стиль%]) пусто?])]{
@racket[стиль%] куска (чтение и запись).}

@defproc*[#:kind "метод"
          ([(параметры) список?]
           [(параметры [новое-значение список?]) пусто?])]{
Список символов: @racket['текст], @racket['можно-объединять], @racket['невидимый], @racket['перенос] и
др.}

@defproc[#:kind "метод" #:link-target? #f
         (при-получении-клавиши [холст холст%] [лево число?] [верх число?]
                        [лево-редактора число?] [верх-редактора число?]
                        [событие событие-клавиши%])
         пусто?]{
В базовом классе ничего не делает.}

@defproc[#:kind "метод" #:link-target? #f
         (при-получении-события [холст холст%] [лево число?] [верх число?]
                        [лево-редактора число?] [верх-редактора число?]
                        [событие событие-мыши%])
         пусто?]{
В базовом классе ничего не делает.}

@defproc[#:kind "метод" (изменён-размер [ширина число?] [высота число?])
         любой]{
Этот метод переопределяется формой @racket[переопределить].

Если вернуть не @racketvalfont{ложь}, стандартное изменение размера не выполняется. В базовом классе
возвращает @racketvalfont{ложь}.}

@defproc[#:kind "функция" (кусок-адины [внутренний любой])
         любой]{
Оборачивает внутренний кусок в подходящий класс Адины. Если @racket[внутренний] ---
@racketvalfont{ложь}, возвращает @racketvalfont{ложь}.}

@subsubsection[#:tag "ref-кусок-со-строкой%"]{@racket[кусок-со-строкой%]}

@defthing[#:kind "класс" кусок-со-строкой% класс?]{
Базовый класс: @racket[кусок%].
Текстовый фрагмент.
Методы @racket[кусок%] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f (кусок-со-строкой% [содержимое любой 0])
         кусок-со-строкой%]{
Создаётся формой @racket[объект]:
@racketblock[
объект кусок-со-строкой%
  содержимое содержимое
]

@racket[содержимое] --- строка или целое число (запас мест под литеры), по умолчанию 0.}

@subsubsection[#:tag "ref-кусок-с-изображением%"]{@racket[кусок-с-изображением%]}

@defthing[#:kind "класс" кусок-с-изображением% класс?]{
Базовый класс: @racket[кусок%].
Рисунок в документе.
Методы @racket[кусок%] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (кусок-с-изображением% [содержимое любой #,(elem (racketvalfont "ложь"))])
         кусок-с-изображением%]{
Создаётся формой @racket[объект]:
@racketblock[
объект кусок-с-изображением%
  содержимое содержимое
]

@racket[содержимое] --- @racket[изображение%] или @racketvalfont{ложь}.}

@defproc*[#:kind "метод"
          ([(изображение) изображение%]
           [(изображение [значение изображение%]) пусто?])]{
Читает или задаёт растр (@racket[изображение%]).}

@subsubsection[#:tag "ref-кусок-с-редактором%"]{@racket[кусок-с-редактором%]}

@defthing[#:kind "класс" кусок-с-редактором% класс?]{
Базовый класс: @racket[кусок%].
Вложенный редактор в ячейке документа.
Методы @racket[кусок%] тоже доступны.
}

@defproc[#:kind "конструктор" #:link-target? #f
         (кусок-с-редактором% [редактор любой #,(elem (racketvalfont "ложь"))]
                       [есть-граница булево? #,(elem (racketvalfont "истина"))])
         кусок-с-редактором%]{
Создаётся формой @racket[объект]:
@racketblock[
объект кусок-с-редактором%
  редактор редактор
  есть-граница есть-граница
]

@racket[редактор] --- @racket[редактор<%>] или @racketvalfont{ложь}. @racket[есть-граница] по
умолчанию @racketvalfont{истина}.}

@defproc[#:kind "метод" (редактор)
         любой]{
Вложенный документ.}

@defproc*[#:kind "метод" #:link-target? #f
          ([(максимальная-ширина) число?]
           [(максимальная-ширина [новое-значение число?] [редактор? булево?]) пусто?])]{
Ограничение размера ячейки (чтение и запись). При записи второй аргумент --- менять ли те же пределы у
вложенного редактора.}

@defproc*[#:kind "метод" #:link-target? #f
          ([(минимальная-ширина) число?]
           [(минимальная-ширина [новое-значение число?] [редактор? булево?]) пусто?])]{
Ограничение размера ячейки (чтение и запись). При записи второй аргумент --- менять ли те же пределы у
вложенного редактора.}

@defproc*[#:kind "метод" #:link-target? #f
          ([(максимальная-высота) число?]
           [(максимальная-высота [новое-значение число?] [редактор? булево?]) пусто?])]{
Ограничение размера ячейки (чтение и запись). При записи второй аргумент --- менять ли те же пределы у
вложенного редактора.}

@defproc*[#:kind "метод" #:link-target? #f
          ([(минимальная-высота) число?]
           [(минимальная-высота [новое-значение число?] [редактор? булево?]) пусто?])]{
Ограничение размера ячейки (чтение и запись). При записи второй аргумент --- менять ли те же пределы у
вложенного редактора.}

@subsubsection[#:tag "ref-список-стилей%"]{@racket[список-стилей%]}

@defthing[#:kind "класс" список-стилей% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Список именованных стилей документа. Модуль @racketidfont{графический-интерфейс/стиль}. Создаётся без
аргументов.
}

@defproc[#:kind "конструктор" #:link-target? #f (список-стилей%)
         список-стилей%]{
Создаётся формой @racket[объект]: @racketblock[объект список-стилей%]}

@defproc[#:kind "метод" (базовый-стиль)
         стиль%]{
Корневой стиль списка.}

@defproc[#:kind "метод" (найти-по-имени [имя символ?])
         стиль%]{
Стиль с этим именем или @racketvalfont{ложь}.}

@defproc[#:kind "метод" (новый-с-именем [имя символ?] [стиль стиль%])
         стиль%]{
Зарегистрировать копию стиля под именем.}

@defproc[#:kind "метод" (заменить-с-именем [имя символ?] [стиль стиль%])
         стиль%]{
Подменить стиль с этим именем или создать новый.}

@subsubsection[#:tag "ref-стиль%"]{@racket[стиль%]}

@defthing[#:kind "класс" стиль% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Стиль начертания куска. Тот же модуль. Обычно получают из списка стилей, а не создают пустым.
}

@defproc[#:kind "конструктор" #:link-target? #f (стиль% [внутренний любой])
         стиль%]{
Создаётся формой @racket[объект]:
@racketblock[
объект стиль%
  внутренний внутренний
]

Обычно получают из @racket[список-стилей%].}

@defproc[#:kind "метод" #:link-target? #f (базовый-стиль)
         стиль%]{
Стиль, от которого унаследован этот, или @racketvalfont{ложь}.}

@defproc[#:kind "метод" (установить-отклонение [отклонение отклонение-стиля%])
         пусто?]{
Применить @racket[отклонение-стиля%] к этому стилю.}

@defproc[#:kind "функция" (стиль-адины [внутренний любой])
         стиль%]{
Оборачивает внутренний стиль в @racket[стиль%].}

@subsubsection[#:tag "ref-отклонение-стиля%"]{@racket[отклонение-стиля%]}

@defthing[#:kind "класс" отклонение-стиля% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Отличие стиля от базового при @racket[изменить-стиль]. Создаётся без аргументов (пустое отклонение).
}

@defproc[#:kind "конструктор" #:link-target? #f (отклонение-стиля%)
         отклонение-стиля%]{
Создаётся формой @racket[объект]: @racketblock[объект отклонение-стиля%]}

@defproc[#:kind "метод" (установить-цвет-текста [цвет любой])
         пусто?]{
Цвет букв (@racket[цвет%] или символ из @racket[стандартные-цвета]).}

@defproc[#:kind "метод" (установить-цвет-фона [цвет любой])
         пусто?]{
Цвет фона участка.}

@subsubsection[#:tag "ref-набор-команд%"]{@racket[набор-команд%]}

@defthing[#:kind "класс" набор-команд% класс?]{
Базовый класс: @racket[объект-графического-интерфейса%].
Привязки клавиш редактора. Модуль @racketidfont{графический-интерфейс/набор-команд}.
}

@defproc[#:kind "конструктор" #:link-target? #f (набор-команд% [внутренний любой])
         набор-команд%]{
Создаётся формой @racket[объект]:
@racketblock[
объект набор-команд%
  внутренний внутренний
]

@racket[внутренний] --- готовый набор; иначе создаётся пустой.}

@defproc[#:kind "метод" (добавить-функцию [имя символ?] [функция функция?])
         пусто?]{
Регистрирует имя и функцию; функция получает источник и событие.}

@defproc[#:kind "метод" (есть-функция? [имя символ?])
         булево?]{
Зарегистрировано ли имя.}

@defproc[#:kind "метод" (вызвать-функцию [имя символ?] [источник любой] [событие любой])
         пусто?]{
Вызывает функцию по имени.}

@defproc[#:kind "метод" (функция-команды [команда строка?] [функция символ?])
         пусто?]{
Привязывает последовательность клавиш к имени функции.}

@defproc[#:kind "метод" (отменить-последовательность)
         пусто?]{
Сбрасывает частично введённую последовательность клавиш.}

@defproc[#:kind "функция" (набор-команд-адины [внутренний любой])
         набор-команд%]{
Оборачивает внутренний набор команд в @racket[набор-команд%].}
@subsection[#:tag "operation-system"]{Операционная система}

@defform[#:kind "синтаксис" (замерить-время команда ... выражение)]{Выполняет переданные
команды, возвращает результат выражения. После выполнения выводит в текущий порт вывода строку
«время процессора: {т1} реальное: {т2} сборки мусора: {т3}» c значениями времени выполнения переданных
команд и выражения в полях {т1}..{т3}}.

@defproc[#:kind "функция" (применить-замеряя-время [функция функция?] [список список?])
         (values список? точное-целое? точное-целое? точное-целое?)]{Вызывает переданную функцию
 с аргументами из переданного списка. Возвращает четыре значения: список результатов выполнения
 функции, время процессора, время реальное и время сборки мусора.}

@defproc[#:kind "функция" (открыть-страницу-Интернета [строка-адреса строка?])
         пусто?]{
Открывает @racket[строка-адреса] в браузере по умолчанию. Модуль
@racketidfont{операционная-система}.}

@defproc[#:kind "функция" (воспроизвести-звук [файл (один-из строка? путь?)]
                               [асинхронно? любой #,(elem (racketvalfont "ложь"))])
         булево?]{
Воспроизводит звуковой файл (@racketidfont{play-sound}). Если
@racket[асинхронно?] --- @racketvalfont{ложь}, не возвращает управление, пока
звук не закончится. Возвращает @racketvalfont{истина}, если воспроизведение
удалось, иначе @racketvalfont{ложь}.}

@defproc[#:kind "функция" (завершить-работу
         [значение любой #,(elem (racketvalfont "истина"))])
         любой]{
Передаёт @racket[значение] обработчику завершения работы. Если
обработчик не завершает процесс и не выходит из потока, возвращается
@racketvalfont{ложь}.

Если @racket[значение] --- точное целое от 1 до 255, оно становится
кодом завершения процесса (обычно неуспех); иначе код равен 0
(обычно успех). По умолчанию @racket[значение] ---
@racketvalfont{истина}, то есть успешное завершение.
Модуль @racketidfont{операционная-система}.}

@subsection[#:tag "hypertext-server"]{Сервер гипертекста}

Модуль @racketidfont{сервер-гипертекста/сервер}.

@defproc[#:kind "функция" (сервер/единственный
         [обработчик функция?]
         [#:командная-строка? командная-строка? любой
          #,(elem (racketvalfont "ложь"))]
         [#:закрывать-соединение? закрывать-соединение? любой
          #,(elem (racketvalfont "ложь"))]
         [#:открывать-браузер? открывать-браузер? любой
          (не командная-строка?)]
         [#:выход? выход? любой (не командная-строка?)]
         [#:выводить-сведения? выводить-сведения? любой
          (не командная-строка?)]
         [#:слушать-адрес слушать-адрес
          (один-из строка? #,(elem (racketvalfont "ложь")))
          «127.0.0.1»]
         [#:порт порт целое-неотрицательное? 8000]
         [#:наибольшая-очередь наибольшая-очередь
          целое-неотрицательное? 511]
         [#:пределы-безопасности пределы-безопасности любой]
         [#:управляющий управляющий любой]
         [#:путь-сервлета путь-сервлета строка?
          «/servlets/standalone.rkt»]
         [#:регулярное-сервлета регулярное-сервлета любой]
         [#:без-состояния? без-состояния? любой
          #,(elem (racketvalfont "ложь"))]
         [#:упаковщик упаковщик любой]
         [#:пространство-имён-сервлета пространство-имён-сервлета
          список? пустой-список]
         [#:корень-сервера корень-сервера любой]
         [#:дополнительные-файлы дополнительные-файлы список?]
         [#:корень-сервлетов корень-сервлетов любой]
         [#:текущий-каталог-сервлета текущий-каталог-сервлета любой]
         [#:ответ-файл-не-найден ответ-файл-не-найден функция?]
         [#:ответ-загрузки-сервлета ответ-загрузки-сервлета функция?]
         [#:ответ-ошибки-сервлета ответ-ошибки-сервлета функция?]
         [#:путь-типов-содержимого путь-типов-содержимого любой]
         [#:защищённый? защищённый? любой
          #,(elem (racketvalfont "ложь"))]
         [#:сертификат сертификат любой]
         [#:ключ ключ любой]
         [#:журнал журнал
          (один-из строка? путь? порт-вывода?
           #,(elem (racketvalfont "ложь")))
          #,(elem (racketvalfont "ложь"))]
         [#:формат-журнала формат-журнала любой 'апач-умолчание])
         любой]{
Запускает сервер с одним обработчиком запросов. @racket[обработчик]
получает запрос и возвращает ответ. Не возвращает управление, пока
сервер не остановлен. Не указанные ключевые аргументы необязательны
и имеют умолчания.

Если @racket[командная-строка?] --- @racketvalfont{истина}, то по
умолчанию браузер не открывается, адрес остановки не добавляется и
сведения о запуске не выводятся.

@racket[слушать-адрес] --- адрес, на котором слушать; по умолчанию
только местный. @racketvalfont{ложь} --- все интерфейсы.
@racket[порт] по умолчанию 8000.

Если @racket[открывать-браузер?] --- @racketvalfont{истина}, открывает
@racket[путь-сервлета] в браузере. Если @racket[выход?] ---
@racketvalfont{истина}, адрес «/quit» останавливает
сервер. Если @racket[выводить-сведения?] --- @racketvalfont{истина},
выводит адрес сервера.

@racket[защищённый?] включает шифрование соединения; тогда
@racket[сертификат] и @racket[ключ] --- пути к файлам, либо
умолчание из @racket[корень-сервера].

@racket[журнал] --- файл или порт журнала; @racketvalfont{ложь} ---
не вести журнал. @racket[формат-журнала] --- @racket['апач-умолчание],
@racket['скобочное-умолчание], @racket['расширенный],
@racket['совмещённый] или своя функция.

Модуль @racketidfont{сервер-гипертекста/сервер}.}

@defproc[#:kind "функция" (ответ [код целое-неотрицательное?]
         [сообщение байты?]
         [секунды число?]
         [тип-содержимого (один-из байты? #,(elem (racketvalfont "ложь")))]
         [заголовки список?]
         [вывод (порт-вывода? . -> . любой)])
         любой]{
Собирает ответ: @racket[код] (от 100 до 999), краткое @racket[сообщение]
в байтах, время @racket[секунды], @racket[тип-содержимого] (байты или
@racketvalfont{ложь}), @racket[заголовки] и @racket[вывод] --- функция,
которая пишет тело в порт. Модуль
@racketidfont{сервер-гипертекста/обработчик}.}

@defproc[#:kind "функция" (данные-запроса-отправки/необработанные
         [запрос любой])
         (один-из байты? #,(elem (racketvalfont "ложь")))]{
Необработанные данные тела отправки запроса или @racketvalfont{ложь}.
Модуль @racketidfont{сервер-гипертекста/обработчик}.}

@defproc[#:kind "функция" (отправить/приостановить/маршрутизировать
         [генератор функция?])
         любой]{
Вызывает @racket[генератор] с функцией, которая по обработчику запроса
строит адрес. Отправляет полученный ответ; при обращении по адресу
вызывается этот обработчик, а его результат возвращается из
@racket[отправить/приостановить/маршрутизировать]. Модуль
@racketidfont{сервер-гипертекста/обработчик}.}

@defproc[#:kind "функция" (ответ/выражение-разметки
         [выражение-разметки любой]
         [#:код код целое-неотрицательное? 200]
         [#:сообщение сообщение
          (один-из байты? #,(elem (racketvalfont "ложь")))
          #,(elem (racketvalfont "ложь"))]
         [#:секунды секунды число? (сейчас)]
         [#:тип-содержимого тип-содержимого
          (один-из байты? #,(elem (racketvalfont "ложь")))]
         [#:куки куки список? пустой-список]
         [#:заголовки заголовки список? пустой-список]
         [#:преамбула преамбула байты? (новые-байты 0)])
         любой]{
Ответ из выражения разметки. По умолчанию код 200, время ---
@racket[сейчас], тип содержимого --- гипертекст. Модуль
@racketidfont{сервер-гипертекста/обработчик}.}

@defproc[#:kind "функция" (пустой-запрос
         [#:заголовки заголовки список? пустой-список])
         любой]{
Перенаправляет браузер на адрес продолжения методом получения и
возвращает следующий запрос. Модуль
@racketidfont{сервер-гипертекста/обработчик}.}

@defproc[#:kind "функция" (параметры-запроса [запрос любой])
         список?]{
Список пар имя--значение из полей запроса. Имя --- символ. Модуль
@racketidfont{сервер-гипертекста/обработчик}.}

@defproc[#:kind "функция" (извлечь-параметр/единственный
         [имя символ?] [параметры список?])
         любой]{
Значение единственного параметра с @racket[имя] в @racket[параметры].
Если такого нет или их несколько, вызывается исключение. Модуль
@racketidfont{сервер-гипертекста/обработчик}.}

@defproc[#:kind "функция" (параметры-запроса/необработанные [запрос любой])
         список?]{
Список необработанных параметров запроса. Модуль
@racketidfont{сервер-гипертекста/обработчик}.}

@subsection[#:tag "priorities"]{Приоритет операторов}

Чем больше число, тем выше приоритет. Если не указано иное, операторы
с одинаковым приоритетом группируются слева направо.

@;(оператор! '* 8)
@;(оператор! '/ 8)
@;(оператор! '// 8)
@;(оператор! '% 8)
@;(оператор! '+ 7)
@;(оператор! '- 7)
@;(оператор! '++ 6)
@;(оператор! '== 5)
@;(оператор! '/= 5)
@;(оператор! '< 5)
@;(оператор! '> 5)
@;(оператор! '<= 5)
@;(оператор! '>= 5)
@;(оператор! '&& 4)
@;(оператор! '|| 3)
@;(оператор! '? 2)
@;(оператор! ': 2 'право)
@;(оператор! '==> 1.5)
@;(оператор! ':= 1 'право)
@;(оператор! '= 0)

@tabular[#:sep @hspace[1]
         (list (list @bold{Оператор} @bold{Приоритет})
               (list @racket[*]       "8")
               (list @racket[/]       "8")
               (list @racket[//]      "8")
               (list @racket[%]       "8")
               (list @racket[+]       "7")
               (list @racket[-]       "7")
               (list @racket[++]      "6")
               (list @racket[==]      "5")
               (list @racket[/=]      "5")
               (list @racket[<]       "5")
               (list @racket[>]       "5")
               (list @racket[<=]      "5")
               (list @racket[>=]      "5")
               (list @racket[&&]      "4")
               (list @racket[||]      "3")
               (list @racket[?]       "2")
               (list @racket[:]       "2, группировка справа")
               (list @racket[==>]     "1,5")
               (list @racket[:=]      "1, группировка справа")
               (list @racket[=]       "0"))]
 