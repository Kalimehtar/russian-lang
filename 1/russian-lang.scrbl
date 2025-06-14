#lang scribble/manual

@(require 1/lang scribble/example scribble/core scribble/racket
          (for-label (except-in 1/all-base ложь)))

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
@codeblock|{
  english()
}|

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
писать кывычки внутри кавычек. Если парность нарушается, то также можно поставить обратную черту
перед кывычкой: @litchar{\«} и @litchar{\»}.

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
то можно функция писать в виде «функция(аргумент1 аргумент2 ...)».
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

Также есть ещё две особые синтаксических конструкции: «список 1 2 3 4 список(5 6)» можно
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
@racket[(+ 2 (2 * 2))], затем в @racket[(+ 2 (* 2 2))],и затем вычислится как @racket[(+ 2 4) = 6].

@subsection[#:tag "basic definitions"]{Основы определений}

При описании синтаксиса «...» обозначает, что предыдущий элемент может повторяться 0 и более раз,
«...+» --- 1 и более раз. В угловых скобках указываются синтаксические переменные. Например, вместо
<идентификатор> может быть подставлен любой допустимый имдентификатор языка.

Определение в форме
@racketblock[
(<идентификатор> = <выражение>)
]
cвязывает <идентификатор> с результатом вычисления выражения, а в форме
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

И, на самом деле, определение функции, также как и определение не функции, всего лишь связывает
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
объединяются в одно выражение, а слова справа остаются как есть, также как в определении функции.

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

В случае, если при выполнеии условия необходимо не только вернуть результат, но и выполнить
какие-либо действия, есть вариант синтаксиса с ключевыми словами:

@codeblock|{
(если <выражение-условия> ... тогда <выражения-если-истина> ... иначе <выражения-если-ложь> ...)
(если <выражение-условия> ... тогда <выражения-если-истина> ...)
}|

Выражение условия может состоять из нескольких слов: всё, что находится между ключевыми словами
«если» и «тогда» объединяется в одно выражение. После «тогда» и после «иначе» может быть несколько
выражений. Также как в функции они вычисляются все и возвращает значение последнего выражения.
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
называется вычисления по короткой схеме.

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

Синтаксически, первый элемент списка может быть любым значением, но при выполнеии будет ошибка:

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

Если Вы случайно пропустите имя функции или поставите лишние скобки вокруг выржения, то
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
то жаль писать целое определение. В Адине можно использовать выраюение @racket[функция],
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

Так вышеприведённый пример может быть переписан как:
@examples[#:label #f
 (eval:alts
  (unsyntax (elem (racket дважды)
                  (linebreak) (hspace 4) (racket функция (строка) $ строка ++ "!")
                  (linebreak) (hspace 4) (racket "Привет")))
  "Привет!!")
 (eval:alts
  (unsyntax (elem (racket дважды)
                  (linebreak) (hspace 4) (racket функция (строка) $ строка ++ "?!")
                  (linebreak) (hspace 4) (racket "Привет")))
  "Привет?!?!")]

Другое применение выражения @racket[функция] --- результат для функции принимающе функции.

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
определить функцию. Эти два определения эквивалентны:

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

Как можно видеть, при выводе список выводится как апостроф, после котрого идёт открывающая скобка,
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

Разные функции обработки элементов по разному комбинируют результаты обработки.
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

Такое поведении при вычислении иногда называют «оптимизацией хвостовых вызовов», так как в более
примитивных языках программирования каждый вызов всё равно тратит кадр памяти, даже если результат
вызова сразу должен стать результатом вызывающей функции. Но на самом деле это не оптимизация,
а гарантия того, как будут производится вычисления. Есди точнее, то выражение в хвостовой позиции
всегда не требует дополнительного места.

В случае @racket[моё-отобразить] место для результатирующего списка и место для временных данных
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

Также как форма @racket[буквально] для списков автоматически применяется для вложенных списков,
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
выводится, так как апостроф перед списком уже указывает, что все имена в списке являются символвами.

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

Неточные числа также возвращаются такими функциям как @racket[корень], @racket[логарифм]
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

Литеры Адины соответствуют @italic{кодам Юникода}. Код Юникола можно
трактовать как беззнаковое целое число, которое можно отобразить в 21 бит и которое
соответствует символу естественного языка или части символа. Технически, код является
более простым понятием, чем то, что в стандарте Юникода называется символом, но его достаточно
в большинстве случаев. Например, любую акцентированную латинскую букву, любую кириллическую букву или
любой обычный китайский иероглиф можно представить в виде кода.

Несмотря на то, что каждая литера Адины соответствует числу, литеральный тип отделён от числового.
Функции @racket[литера->число] и @racket[число->литера] позволяют преобразовывать целые числа
и соответствующие литеры друг в друга.

Печатные литеры обычно выводятся как @litchar{#\} и отображаемая литера. Непечатные обычно выводятся
как @litchar{#\u} и код литеры в виде шестнадцатиричного числа. Некоторые литеры печатаются особым
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
выводятся как литеры с этими номерами, но перед кавычкой и обратной чертой как ипри выводе строк
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
   
Используются ключевые слова при работе с именоваными параметрами функций и макросов.

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
                    (racketcommentfont "- именованый аргумент #:если-существует ")
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
Вывод этих функция отличается только тем, как они выводят элементы списков.

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

Соответствие позволяет сопоставить произвольным значения-ключам произвольные значения.
Ключи сравниваются либо при помощи @racket[==], если соответствие создано при помощи
@racket[соответствие] или @racket[новое-соответствие], либо при помощи @racket[==],
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
структуры разных типов, даже если у них одинаковые поля. Объявление типа осуществядется формой
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

Из имени создаётся конструктор: функция, которая создаёт новую сруктуру с заданными значениями полей.
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
унаследованных от родительского объекта. Даже если этот родительсткий
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
@racket[вызвать-цепочку-методов] или @racket[для-объекта]. Или при помощи синтакиса
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
   Первая форма связывает идентификатор с результатом вычисления выражжения.
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
              ]]{Описывает тип стурктуры.
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

@defform[#:kind "синтаксис" (буквально элемент ...+)]{
Если вызвана с одним элементом, то возвращает постоянное значение,
 соответствующее переданному элементу (то есть фрагменту программы).
 Если с несколькими, то формирует из них список. Возвращаемое значение всегда
 неизменяемое.}

@defthing[#:kind "значение" пусто пусто?]{Если значением функции является @racket[пусто],
тогда результат не выводится}

@defproc[#:kind "функция" (пусто? [аргумент любой])
         булево?]{Возвращает @racketvalfont{истина}, если @racket[аргумент] равен @racket[пусто]}

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
         (одно-из число? ложь)]{Возвращает число из строкового представления числа или ложь,
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
         (одно-из пара? ложь)]{Считает, что @racket[список] начинается со
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
                                         [литера-ошибки (одно-из литера? ложь) ложь]
                                         [начало целое-неотрицательное? 0]
                                         [конец целое-неотрицательное? (длина-байтов байты)])
         строка?]{Преобразует отрезок байтов в строку, трактуя байты в кодировке UTF-8. Если
@racket[литера-ошибки] не ложь, то она подставляется вместо байтов, не являющихся частью
коректной последовательности, иначе вызывается исключение.}

@defproc[#:kind "функция" (байты->строка/местные
                           [байты байты?]
                           [литера-ошибки (одно-из литера? ложь) ложь]
                           [начало целое-неотрицательное? 0]
                           [конец целое-неотрицательное? (длина-байтов байты)])
         строка?]{Преобразует отрезок байтов в строку, трактуя байты в кодировке
региональных настроек. Если @racket[литера-ошибки] не ложь, то она подставляется
вместо байтов, не являющихся частью коректной последовательности, иначе вызывается исключение.}

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

@defproc*[#:kind "параметр" ([(текущее-место) (одно-из строка? ложь)]
                             [(текущее-место [место (один-из строка? ложь)]) пусто?])]{
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

@subsection[#:tag "functions"]{Функции}

@defproc[#:kind "функция" (функция? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является функцией.}

@subsection[#:tag "parameters"]{Параметры}

@defproc[#:kind "функция" (параметр? [аргумент любой])
         булево?]{Возвращает истину, если @racket[аргумент] является параметром.}

@defproc[#:kind "функция" (параметр [аргумент любой]
                                    [охрана (один-из (любой . -> . любой) ложь) ложь]
                                    [имя символ? 'функция-параметра])
         параметр?]{Возвращает параметр с начальным значением @racket[аргумент].
Если @racket[охрана] не ложь, то когда функция параметра вызывается с аргументом,
аргумент передаётся в функцию @racket[охрана], а уже результат этой функции записывается
в параметр. Также @racket[охрана] может вызывать исключение, если значение непримемлемо.
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

@subsection[#:tag "modules"]{Модули}

@defform[#:kind "синтаксис" (используется выражение-модуля ...)]{Подключает
указанные модули. Выражение модуля может быть строкой с именем файла относительно текущего
каталога, символом с именем модуля или конструкцией использования.}

@subsection[#:tag "operation-system"]{Операционная система}

@defform[#:kind "синтаксис" (замерить-время команда ... выражение)]{Выполняет переданные
команды, возвращает результат выражения. После выполнения выводит в текущий порт вывода строку
«время процессора: {т1} реальное: {т2} сборки мусора: {т3}» c значениями времени выполнения переданных
команд и выражения в полях {т1}..{т3}}.

@defproc[#:kind "функция" (применить-замеряя-время [функция функция?] [список список?])
         (values список? точное-целое? точное-целое? точное-целое?)]{Вызывает переданную функцию
 с аргументами из переданного списка. Возвращает четыре значения: список результатов выполнения
 функции, время процессора, время реальное и время сборки мусора.}

@subsection[#:tag "priorities"]{Приоритет операторов}


@;(оператор! '* 7)
@;(оператор! '/ 7)
@;(оператор! '// 7)
@;(оператор! '% 7)
@;(оператор! '+ 6)
@;(оператор! '- 6)
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
@;(оператор! ':= 1 'право)
@;(оператор! '= 0)

@tabular[#:sep @hspace[1]
         (list (list @bold{Оператор} @bold{Приоритет})
               (list @racket[++]      "6")
               (list @racket[==]      "5")
               (list @racket[&&]      "4")
               (list @racket[||]      "3")
               (list @racket[?]       "2")
               (list @racket[:]       "2, группировка справа")
               (list @racket[:=]      "1, группировка справа")
               (list @racket[=]       "0"))]
 