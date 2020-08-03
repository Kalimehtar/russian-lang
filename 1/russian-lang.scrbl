#lang scribble/manual

@(require (for-label racket))

@title{Русский язык программирования}
@author[(author+email "Клочков Роман" "kalimehtar@mail.ru")]

Это руководство описывает русскоязычный язык программирования, основанный
на идеях из расширения синтаксиса Scheme @hyperlink["http://www.dwheeler.com/readable/"]{readable}.

Семантика языка на данный момент полностью унаследована от Racket,
вплоть до полной обратной совместимости: из этого
языка можно вызывать любые функции и синтаксические конструкци Racket,
а из Racket можно вызывать модули на этом языке.

Для включения синтаксиса данного языка просто укажите в модуле Racket в первой строке

@nested[#:style 'code-inset]{
  #lang 1
}

или

@codeblock|{
  #!1
}|

Второй вариант рекомендуется при использовании русского языка для написания программы.

@section{Отличия от Racket}

Эта глава предназаначена для тех, кто умеет программировать на Scheme и/или Racket. Остальные
могут её пропустить и перейти к следующей.

На этом языке можно писать как на Racket с упрощённым синтаксисом. Обратная совместимость
поддерживается почти полностью,
за исключением строчных комментариев и квадратных и фигурных скобок. Если в Racket
использовалась ";", то здесь для строчных комментариев необходимо использовать
"--", так как ";" используется в других синтаксических конструкциях, которые будут описаны ниже.
Квадратные иф фигурные скобки также нельзя использовать вместо круглых,
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

Для иллюстрации запишу (список 1 2 (список 3 4) 5 (список 6 (список 7 8)))
@codeblock{
  #!1
  список 1 2
    список 3 4
    5
    список 6
      список 7 8
}

Также есть специальная конструкция для списков, первым элементом которых тоже является список.
В этом случае длядополнительного отступа можно использовать ";". Либо её же можно использовать
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

Синтаксическое правило выглядит так: если в списке встречается ";", то список разделяется на
подсписки, как если бы
вместо ";" был перенос строки с сохранением отступа.

Таким образом, последовательности элементов "x 1" и "y 2" становятся вложенными списками.

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
количество строк не добавляя скобок. Символ "$" показывает, что
элементы справа от него являются списком, который должен быть подставлен на место этого символа.

(список 1 2 (список 3 4) 5 (список 6 (список 7 8))) теперь можно записать как
@nested[#:style 'code-inset]{@verbatim{
  #!1
  список 1 2
    список 3 4; 5
    список 6 $ список 7 8
}}

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
элемент перд скобкой добавляется в голову списка.
Элементы внутри спиcка можно (но не обязательно) разделять при помощи ";".

(список 1 2 (список 3 4) 5 (список 6 (список 7 8))) можно выразить как
@nested[#:style 'code-inset]{@verbatim{
#!1
список(1; 2; список 3 4; 5; список 6 $ список 7 8)
}}

Так можно записывать в одну строку вызовы функций с аргументами, которые являются вызовами
функций. Кроме того такитм образом удобно вызывать каррированные функции
Вместо (((f 5) 6) 7) будет f(5)(6)(7)

Для квадратной скобки конструкция преобразуется в инструкция
доступа к коллекции (массиву/списку/хэшу).

Вместо (vector-ref a 5) можно просто писать a[5].
А вместо (vector-ref (vector-ref a 5) 6) --- a[5][6]

Для фигурной скобки конструкция даёт возможность вызвать методы объекта.

(send window show #t) можно записать как window{show #t}. также можно использовать несколько
вызовов как в @racket[send+].

@codeblock{
(send+ (new point%)
         (move-x 5)
         (move-y 7)
         (move-x 12))
}

преобразуется в
@nested[#:style 'code-inset]{@verbatim{
#!1
new(point%){move-x 5; move-y 7; move-x 11}
}}
или
@nested[#:style 'code-inset]{@verbatim{
#!1
new(point%){move-x(5) move-y(7) move-x(11)}
}}

Для удобства работы с арифметикой реализованы приоритеты бинарных операций.
Если в списке обнаружена бинарная операция, то она становится в голову списка и получает элементы
до и после неё как два аргумента-списка. Операцией считается любой индентификатор,
который состояит только из !#$%&⋆+./<=>?@"@"^~:*-

Оператор равенства реализован как == (вместо =), также реализованы // (как quotient), /= (неравно),
||, &&, % (как remainder).

(+ (vector-ref a 5) (* 2 (hash-ref h 'key)) можно написать как
@codeblock{
#!1
a[5] + 2 * h['key]
}

Внимание: пробелы вокруг операций обязательны, так как @racket[2*h],
например, является нормальным именем переменной.

