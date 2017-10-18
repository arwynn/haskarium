Классы типов
============


Смысл и реализация
------------------

Класс типов выражает некоторое свойство типа.

Реализуется через набор функций с одним именем и типом,
но разными реализациями для разных типов.


Объявление класса
-----------------

> class Interactive a where
>     onTick :: Time -> a -> a
>     onEvent :: Event -> a -> a

Класс `Interactive` выражает свойство объекта изменяться по ходу игры,
под воздействием времени и внешних событий.

Функции, определённые в классе, называются также методами.


Объявление экземпляра
---------------------

Такой синтаксис объявляет тип экземпляром класса, то есть объявляет,
что данный тип обладает указанным свойством.

> data Stone = Stone
>
> instance Interactive Stone where
>     onTick _ = id -- сегодня ничего не произошло
>     onEvent _ = id

Чтобы определить полноценный экземпляр,
необходимо предоставить доказательства -- реализовать все методы класса.


Использование
-------------

Чтобы использовать свойство типа,
то есть чтобы вызывать методы класса с известным типом, необходимо,
чтобы соответствующий экземпляр
был объявлен в текущем модуле или в любом модуле,
импортируемом в текущий.


Типы с ограничениями
--------------------

Чтобы вызывать методы класса с неизвестным типом, то есть типом,
заданным переменной в сигнатуре,
необходимо добавить добавить ограничение на эту переменную в сигнатуру
через толстую стрелку (=>):

> generateMany :: Generate a => Natural -> [a]

Ограничение `Generate a` означает, что тип `a` может быть любой,
но обязательно принадлежащий классу `Generate`.

Если посмотреть на тип метода в GHCi, мы увидим,
что компилятор автоматически добавляет ограничение на соответствующий класс
в его тип.

< λ> :info onTick
< class Interactive a where
<   onTick :: Time -> a -> a
<   ...
<
< λ> :type onTick
< onTick :: Interactive a => Time -> a -> a


Общая реализация метода
-----------------------

Классы позволяют предоставить разную реализацию для разных типов,
но иногда надо сделать наоборот — для одних типов реализовать разное поведение,
а для других — одинаковое.
В таких случаях пишем обычную функцию
и используем её в соответствующих экземплярах.

Пример:

> instance Interactive Stone where
>     onTick = stayStill
>
> instance Interactive Tree where
>     onTick = stayStill
>
> instance Interactive Ant where
>     onTick = run 20
>
> stayStill :: Time -> a -> a
> stayStill _ = id