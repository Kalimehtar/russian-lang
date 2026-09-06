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

@include-section{doc/sections/essentials.scrbl}

@include-section{doc/sections/builtin.scrbl}

@include-section{doc/sections/gui.scrbl}

@include-section{doc/sections/reference.scrbl}

