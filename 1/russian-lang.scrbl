#lang scribble/manual

@(require 1/lang scribble/example (for-label 1/all-base))

@title{Русский язык программирования Ади́на}
@author[(author+email "Клочков Роман" "kalimehtar@mail.ru")]

Документация основана на @other-doc['(lib "scribblings/guide/guide.scrbl")].

@defmodulelang["1" #:module-path 1/all-base #:packages ("russian-lang")]

Это руководство описывает русскоязычный язык программирования, основанный
на идеях из расширения синтаксиса Scheme @hyperlink["http://www.dwheeler.com/readable/"]{readable}.

Название Ади́на взято из названия симпатичного
@hyperlink["https://www.plantarium.ru/page/view/item/67917.html"]{кустарника}.

Семантика языка на данный момент полностью унаследована от Racket,
вплоть до полной обратной совместимости: из этого
языка можно вызывать любые функции и синтаксические конструкции Racket,
а из Racket можно вызывать модули Адины.

Для включения синтаксиса данного языка просто укажите в модуле Racket в первой строке

@nested[#:style 'code-inset]{
  #lang 1
}

или

@codeblock|{
  #!1
}|

Второй вариант рекомендуется при использовании русского языка для написания программы.

@section[#:tag "like Racket"]{Отличия от Racket}

Эта глава предназаначена для тех, кто умеет программировать на Scheme и/или Racket. Остальные
могут её пропустить и перейти к @seclink["essentials" "следующей"].

На Адине можно писать как на Racket с упрощённым синтаксисом. Обратная совместимость
поддерживается почти полностью,
за исключением строчных комментариев и квадратных и фигурных скобок. Если в Racket
использовалась «;», то здесь для строчных комментариев необходимо использовать
«--», так как «;» используется в других синтаксических конструкциях, которые будут описаны ниже.
Квадратные и фигурные скобки также нельзя использовать вместо круглых,
так как они несут другой синтаксический смысл.

То есть, например, программа

@codeblock|{
  #!1
  (letrec ((is-even? (lambda (n)
                       (or (zero? n)
                           (is-odd? (sub1 n)))))
           (is-odd? (lambda (n)
                      (and (not (zero? n))
                           (is-even? (sub1 n))))))
    (is-odd? 11))
}|

Будет также, как и в Racket, возвращать #t. Но этот язык позволяет сделать списки, из которых
состоит прорамма на Racket, читабельней.
В нём структуру списка можно обозначить не скобками, а отступами. Список можно записать как
@codeblock{
  #!1
  список 1 2 3 4 5 6
}
или как
@codeblock{
 #!1
 список 1 2 3 4
    5
    6
}
Функция @racket[список] синоним функции @racket[list].

Если на одной строке есть несколько элементов, разделённых пробельными символами, то это список.
Если следующая строка начинается с большего отступа, чем текущая, то это элемент ---
продолжение списка, если отступ текущей строки равен отступу предыдущей,
которая является элементом списка, то эта строка также элемент того же списка.

Для иллюстрации запишу @racket[(список 1 2 (список 3 4) 5 (список 6 (список 7 8)))]
@codeblock{
  #!1
  список 1 2
    список 3 4
    5
    список 6
      список 7 8
}

Также есть специальная конструкция для списков, первым элементом которых тоже является список.
В этом случае длядополнительного отступа можно использовать «;». Либо её же можно использовать
для разделения списка на подсписки.

Например
@codeblock{
  (let ((x 1) (y 2))
    (f x y))
}
можно записать как
@codeblock{
  #!1
  let
    ;
      x 1
      y 2
    f x y
}
или как
@codeblock{
  #!1
  let (x 1; y 2)
    f x y
}

Синтаксическое правило выглядит так: если в списке встречается «;», то список разделяется на
подсписки, как если бы
вместо «;» был перенос строки с сохранением отступа.

Таким образом, последовательности элементов «x 1» и «y 2» становятся вложенными списками.

Запишем предыдущий пример, описывая структуру программы отступами
@codeblock|{
  #!1
  letrec
    ;
      is-even?
        lambda (n)
          or
            zero? n
            is-odd?
              sub1 n
      is-odd?
        lambda (n)
          and
            not
              zero? n
            is-even?
              sub1 n
    is-odd? 11
}|

Есть ещё одна синтаксическая конструкция, заимствованная из Haskell, позволяющая сократить
количество строк не добавляя скобок. Символ «$» показывает, что
элементы справа от него являются списком, который должен быть подставлен на место этого символа.

@racket[(список 1 2 (список 3 4) 5 (список 6 (список 7 8)))] теперь можно записать как

@codeblock|{                              
  #!1
  список 1 2
    список 3 4; 5
    список 6 $ список 7 8
}|

А пример из Racket как
@codeblock|{
  #!1
  letrec
    ;
      is-even? $ lambda (n)
        or
          zero? n
          is-odd? $ sub1 n
      is-odd? $ lambda (n)
        and
          not $ zero? n
          is-even? $ sub1 n
    is-odd? 11
}|

Таким образом получаем наглядное представление программы, которое не перегружено скобками.

Для упрощения чтения программы также добавлено ещё несколько синтаксических конструкций,
которые позволяют сделать текст программы
более похожим на широко распространённые языки программирования.

Если перед скобкой нет пробела, то включается особый алгоритм обработки. Для круглой скобки
элемент перед скобкой добавляется в голову списка.
Элементы внутри спиcка можно (но не обязательно) разделять при помощи «;».

@racket[(список 1 2 (список 3 4) 5 (список 6 (список 7 8)))] можно выразить как
@codeblock|{
#!1
список(1; 2; список 3 4; 5; список 6 $ список 7 8)
}|

Так можно записывать в одну строку вызовы функций с аргументами, которые являются вызовами
функций. Кроме того, таким образом удобно вызывать каррированные функции
Вместо @racket[(((f 5) 6) 7)] будет
@racket[#,(elem (racket f) (racket (5)) (racket (6)) (racket (7)))].

Для квадратной скобки конструкция преобразуется в инструкция
доступа к коллекции (массиву/списку/хэшу).

Вместо @racket[(vector-ref a 5)] можно просто писать @racket[#,(elem (racket a) (racket [5]))].
А вместо @racket[(vector-ref (vector-ref a 5) 6)] ---
@racket[#,(elem (hspace 1) (racket a) (racket [5]) (racket [6]))].

При помощи фигурных скобок есть возможность вызвать методы объекта.

@racket[(send window show #t)] можно записать как @racket[window{show #t}]. также можно использовать
несколько вызовов как в @racket[send+].

@codeblock{
(send* (new point%)
 (move-x 5)
 (move-y 7)
 (move-x 12))
}

преобразуется в
@codeblock|{
#!1
new(point%){move-x 5; move-y 7; move-x 11}
}|
или
@codeblock|{
#!1
new(point%){move-x(5) move-y(7) move-x(11)}
}|

Для удобства работы с арифметикой реализованы приоритеты бинарных операций.
Если в списке обнаружена бинарная операция, то она становится в голову списка и получает элементы
до и после неё как два аргумента-списка. Операцией считается любой индентификатор,
который состоит только из !#$%&⋆+./<=>?@"@"^~:*- и не равен «...». Любой другой идентификатор можно
сделать оператором добавив перед и после него символы «^». Например, @racket[(2 ^cons^ 3)]
то же самое, что @racket[(cons 2 3)].

Если надо, чтобы операция оставалась аргументом, то пишите её как список из точки и операции.
Например, если надо написать @racket[(list + 3)], то можно написать
@codeblock{
#!1
list (. +) 3
}

Оператор равенства реализован как == (вместо @racket[equal?]) и === (вместо @racket[eqv?]),
также реализованы // (как @racket[quotient]), /= (неравно), ||, &&, % (как @racket[remainder]).

@racket[(+ (vector-ref a 5) (* 2 (hash-ref h 'key)))] можно написать как
@codeblock{
#!1
a[5] + 2 * h['key]
}

Внимание: пробелы вокруг операций обязательны, так как @racket[2*h],
например, является нормальным именем переменной.

При помощи операций вышеупомянутый пример можно записать так:
@codeblock|{
#!1           
letrec
  ;
    is-even? $ lambda (n)
      n === 0 || is-odd? (n - 1)
    is-odd? $ lambda (n)
      n /= 0 && is-even? (n - 1)
  is-odd? 11
}|

@section[#:tag "essentials"]{Основы языка}

Программа состоит из команд. Команда может быть вызовом функции, синтаксической конструкцией или
определением переменной.
Первая строка в программе определяет используемый язык программирования и является строкой «#!1».

Комментарий начинается с символов «--» и заканчивается концом строки.

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
вещественными числами двойной точности и хранят только 15-17 знаков.

@defterm{Логические значения} — это @racketvalfont{истина} и @racketvalfont{ложь}. При проверках
в логических операциях любое значение, не равное
@racketvalfont{ложь} трактуется как истина.

@defterm{Строчные значения} записываются между двойными кавычками. Для записи кавычк используется
последовательность символов «\"», для записи символа «\» --- «\\». Все остальные символы
Юникода можно писать как есть.

@racketblock[
"Привет!"
"Автомобиль \"Москвич\""
"你好"
]

Когда константа выводится в окне интерпретатора, как правило, она имеет тот же вид, в котором она
была введена, но иногда при выводе происходит нормализация.
В окне интерпретатора и в документации результат вычислений выводится синим, а не зелёным, чтобы
было видно, где результат, а где введённое значение.

@examples[#:label "Примеры:"
 (eval:alts (unsyntax (racketvalfont "1.0000")) 1.0)
 (eval:alts (unsyntax (racketvalfont "\"\\u0022ok\\u0022\"")) "\u0022ok\u0022")]

@subsection[#:tag "expressions"]{Выражения}

Выражение --- это команда языка Адина, возвращающая значение.

Выражения записываются в виде последовательности слов, разделённых пробельными символами.
Слово может быть оператором, если состоит только из символов «!#$%&⋆+./<=>?@"@"^~:*-» кроме «...»
или начинается и заканчивается на «^». Примеры операторов: +, -, ^пара^.
Если оператор начинается и заканчивается на «^», то он вызывает функцию по имени между «^»
со своими аргументами.
Например, @racket[(2 ^пара^ 3)] то же самое, что @racket[(пара 2 3)].

Также операторы (кроме @racket[=] и @racket[?] описанных ниже) автоматически объединяют слова
перед и после оператора в команды. Например @racket[(список 2 3 ++ список 4 5 6)] то же самое,
что @racket[(++ (список 2 3) (список 4 5 6))].

Если в выражении нет операторов, то первое слово определеяет синтаксис выражения.
Если первое слово --- функция, то остальные слова --- аргументы этой функции.
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
                           (racket список) (racket "(5 6) 7")))
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
и тогда внутри скобок переносы и отступы игнорируются. Если внутри скобок 

Если строка очень длинная, то можно перед переносом вставить символ «\», тогда перенос
не будет нести синтаксического смысла.

Выбор способа написания определяется удобством чтения. При вводе в окно интерпретатора
ввод заканчивается после пустой строки, так как до этого возможно продолжение команды.

Также есть ещё две особые синтаксических конструкции: «список 1 2 3 4 список(5 6)» можно
записать как «список 1 2 3 4 $ список 5 6», то есть оператор «$» позволяет
слова после неё выделить в отдельную команду. Чтобы объединить несколько коротких команд
или значений в одну строку в одну, можно использовать оператор «;».
@examples[#:label "Пример:"
(eval:alts (unsyntax (elem (racket список) (racketvalfont " 1 2 3 4")
                           (linebreak) (hspace 4) (racket список) (racketvalfont " 5 6")
                           (linebreak) (hspace 4) (racketvalfont "7") (racketparenfont ";")
                           (hspace 1) (racket список)
                           (racketvalfont " 8") (racketparenfont ";") (racketvalfont " 9")))
           '(1 2 3 4 (5 6) 7 (8) 9))
]
Можно заметить, что перед «;» пробел не обязателен.

Операторы «$» и «;» работают также и в скобках, но
«;» разбивает выражение на подвыражения
равного уровня, то есть
@examples[#:label "Пример:"
(eval:alts (unsyntax (elem (racketparenfont "(") (racket список) (racketparenfont ";")
                           (hspace 1) (racket список 1 2)
                           (racketparenfont ";") (hspace 1) (racket список 3 4)
                           (racketparenfont ")")))
           '((1 2) (3 4)))
]

Аналогичная конструкция для стандартного синтаксиса требует одинакового отступа для подвыражений,
поэтому её корень будет пустым и замещаться «;»:
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
(eval:alts (unsyntax (racketvalfont "кусок \"три символа\"")) "три")
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

Синтакисис для идентификаторов масимально свободный. В них могут быть использованы любые символы
кроме пробелов, скобок, кавычек, апострофов, точки с запятой, запятой, решётки, вертикальной черты
и обратной косой черты за исключением последовательностей, являющихся числовыми константами.
Более того, можно вводить идентификатор между вертикальным чертами, тогда
допустимы вообще любые символы кроме вертикальной черты.

Примеры идентификаторов:
@codeblock|{
не-печётся
++
=
Проверка
проверка/вывод
а+б
с+1
|идентификатор со спецсимволами ( ) [ ] { } " , ' ` ; # \|
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
                  (racket строка-начинается-с?) (racket запрос) (hspace 1) (hspace 1)
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
Возвращается результат последнего вычисленного выражения.

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
взято в скобки.

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
  (unsyntax (elem (racket преобразовать "Привет мир!")))
  "Привет!")
 (eval:alts
  (unsyntax (elem (racket преобразовать "Приветствую Земля!")))
  "Чего?")
 (eval:alts
  (unsyntax (elem (racket преобразовать "Пока друзья.")))
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
связываются с соответсвующими им именами. Форму @racket[пусть] можно использовать,
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
   Первая форма связывает идентификатор с результатом вычисления выражжения.
 Вторая позволяет одновременно связать несколько идентификаторов с значениями
(выражение должно в этом случае возвращать необходимое количество значений).
 Третья связывает идентификатор с функцией, здесь особым образом обрабатывается оператор: каждый
 элемент после @racket[=] считается отдельной командой. То есть, если надо сделать
 функцию из одного выражения, выражение должно быть одним элементом или
 обязательно после @racket[=] делать перенос и отступ. Если @racket[заголовок] является списком,
 то создаётся функция, возвращающая функцию с аргументами, указанными после первого элемента
 заголовка.}

@defform*[#:kind "синтаксис" #:id функция
   ((функция (аргументы) = команда ... выражение))
   #:grammar [(аргументы (code:line аргумент ...)
                         (code:line аргумент ... @#,racketparenfont{.} аргумент-оставшихся))
              (аргумент идентификатор [идентификатор выражение] (code:line ключ идентификатор)
                        (code:line [ключ идентификатор выражение]))
              ]]{
   Возвращает функцию с указанными аргументами и телом.}

@subsection[#:tag "logicals"]{Логические выражения}

@defproc[#:kind "функция" (логический? [аргумент любой])
         логический?]{Возвращает @racketvalfont{истина}, если @racket[аргумент]
 @racketvalfont{истина} или @racketvalfont{ложь}, в противном случае возвращает
 @racketvalfont{ложь}.}

@defproc[#:kind "функция" (== [аргумент любой] ...+)
         логический?]{Возвращает @racketvalfont{истина}, если @racket[аргумент]ы
 равны. Списки и массивы считаются равными, если равны их элементы.}

@defproc[#:kind "функция" (=== [аргумент любой] ...+)
         логический?]{Возвращает @racketvalfont{истина}, если @racket[аргумент]ы
 равны. Списки и массивы считаются равными, если являются одним и тем же объектом,
 а не просто состоят из одинаковых элементов.}

@defproc[#:kind "функция" (/= [аргумент любой] ...+)
         логический?]{Возвращает @racketvalfont{ложь}, если @racket[аргумент]ы
 равны в смысле @racket[==].}

@subsection[#:tag "conditionals"]{Условия}

@defform[#:kind "синтаксис" (? условие выражение-если-истина выражение-если-ложь)
         #:contracts ([условие логический?])]{Если @racket[условие] истинно,
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
                              (условие => выражение))]]{
 Выполняет выражения по условиям. Если команда создаёт переменную, то эта переменная
имеет область видимости только внутри блока с условием. В конструкции с @racket[=>]
выражение должно возвращать функцию от одного аргумента, в эту функцию будет передан
результат вычисления условия.}

@subsection[#:tag "numbers"]{Числа}

@defproc[#:kind "функция" (число? [аргумент любой])
         логический?]{Возвращает истину, если @racket[аргумент] является числом.}

@defproc[#:kind "функция" (корень [число число?])
         число?]{Возвращает главный (для положительных вещественных совпадает с арифметическим)
 квадратный корень из значения аргумента @racket[число].
 Результат точный, если @racket[число] точное и квадратный корень из него рациональный.}

@subsection[#:tag "lists"]{Списки}

@defproc[#:kind "функция" (список? [аргумент любой])
         логический?]{Возвращает истину, если @racket[аргумент] является списком.}

@defproc[#:kind "функция" (список [аргумент любой] ...)
         список?]{Возвращает список из произвольных значений.}

@defproc[#:kind "функция" (пара? [аргумент любой])
         логический?]{Возвращает истину, если @racket[аргумент] является парой.}

@defproc[#:kind "функция" (пара [аргумент1 любой] [аргумент2 любой])
         пара?]{Возвращает пару из переданных аргументов. Если второй аргумент список,
 то возвращаемое значение тоже список.}

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

@defproc[#:kind "функция" (++ [аргумент (один-из список? строка? массив?)] ...)
         (один-из список? строка? массив?)]{Возвращает сцепку переданных аргументов.
Создаётся новая изменяемая коллекция достаточного размера для всех элементов аргументов,
затем все элементы всех аргументов последовательно копируются в новую коллекцию.
 Тип аргументов должен быть одинаковый.}

@subsection[#:tag "strings"]{Строки}

@defproc[#:kind "функция" (строка? [аргумент любой])
         логический?]{Возвращает истину, если @racket[аргумент] является строкой.}

@defproc[#:kind "функция" (длина-строки [строка строка?])
         целое-неотрицательное?]{Возвращает длину строки в символах.}

@defproc[#:kind "функция" (подстрока [строка строка?] [начало целое-неотрицательное?]
                                     [конец целое-неотрицательное? (длина-строки строка)])
         строка?]{Возвращает подстроку из аргумента @racket[строка] с позиции @racket[начало]
 по позицию @racket[конец].}

@defproc[#:kind "функция" (добавить-строки [строка строка?] ...)
         строка?]{Возвращает сцепку строк.
Создаётся новая изменяемая строка достаточного размера,
затем все знаки всех строк последовательно копируются в новую.}

Следующие функции досутпны только при использовании модуля строка.

@defproc[#:kind "функция" (строка-начинается-с? [строка строка?] [подстрока строка?])
         логический?]{Возвращает истину, если @racket[строка] начинается с знаков
 в аргументе @racket[подстрока].}

@defproc[#:kind "функция" (строка-заканчивается-на? [строка строка?] [подстрока строка?])
         логический?]{Возвращает истину, если @racket[строка] заканчивается на знаки
 в аргументе @racket[подстрока].}

@subsection[#:tag "inout"]{Ввод-вывод}

@defproc[#:kind "функция" (вывести [аргумент любой] [вывод порт? (текущий-порт-вывода)])
         пусто?]{Выводит значение @racket[аргумент]а в @racket[вывод].}

@defproc[#:kind "функция" (прочитать-строку)
         строка?]{Читает строку из стандартного ввода.}

@subsection[#:tag "functions"]{Функции}

@defproc[#:kind "функция" (функция? [аргумент любой])
         логический?]{Возвращает истину, если @racket[аргумент] является функцией.}

@subsection[#:tag "modules"]{Модули}

@defform[#:kind "синтаксис" (используется выражение-модуля ...)]{Подключает
указанные модули. Выражение модуля может быть строкой с именем файла относительно текущего
каталога, символом с именем модуля или конструкцией использования.}

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
               (list @racket[++]       "6")
               (list @racket[==]       "5")
               (list @racket[&&]       "4")
               (list @racket[||]       "3")
               (list @racket[?]       "2")
               (list @racket[=]       "0"))]

