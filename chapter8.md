# 自定义Types 和 Typesclasses
## Algebraic Data Types 入门
使用`data`关键字。  
比如Bool的定义：
```Haskell
data Bool=False|True
```
等号左端表示类型的名称，右端为值构造子，明确了该类型可能的值。

### 自定义类型：
- Point点类型，包含x与y坐标
- Circle类型，包含圆心坐标 x y，和半径r
- Rectangle 类型，包含左上角与右下角的两个坐标
- Shape类型，包含Circle类型和Rectangle类型
```Haskell
--如果定义类型的话，在等号后面也要加上类型名
Prelude>  data Point=Point  Float Float 
Prelude> data Circle=Circle Float Float Float 


--使用已定义的点类型定义圆
Prelude> data Circle=Circle Point Float deriving(Show)
--非法调用
--Prelude> Circle 1 2 3

--第一种调用方法
Prelude> let a=Point 1 2
Prelude> Circle a 1
Circle (Point 1.0 2.0) 1.0

--第二种调用方法
Prelude> Circle (Point 1 2) 3
Circle (Point 1.0 2.0) 3.0

--定义Recangle
data Rectangle=Rectangle Point Point

--定义Shape类型

--错误的定义方法，如果在上面已经定义了Circle和Rectangle，那么就会报错提示多次定义
-- Multiple declarations of xxx
-- data Shape=Circle|Rectangle
data Shape =Circle  Point Float|Rectangle Point Point

```


### 在.hs文件中完成自定义类型
```Haskell
data Point =Point Float Float
--此处在定义的时候Circle和Rectangle都是可见的，可以直接使用，不需要再单独定义
data Shape =Circle  Point Float|Rectangle Point Point
surface::Shape->Float
surface (Circle _ r)=pi*r^2
surface (Rectangle (Point x1 y1) (Point x2 y2))=(abs $ x2-x1)*(abs $y2-y1)


*Main> let a=Circle (Point 1 2) 3
*Main> surface a
28.274334
*Main> let b =Rectangle (Point 0 0) (Point 3 3)
*Main> surface b
9.0
```

### 将上面写的内容定义为模组(Module)
```Haskell
--先写好接口
--数据成员用 Type(..)，函数成员用 Funcname 表示
--之间使用逗号隔开，在结尾的部分后使用where关键字后面加上定义就可以了
module Shapes(
    Point(..),
    Shape(..),
    surface
)where
data Point =Point Float Float
data Shape =Circle Point Float|Rectangle Point Point
surface::Shape->Float
surface (Circle _ r)=pi*r^2
surface (Rectangle (Point x1 y1) (Point x2 y2))=(abs $ x2-x1)*(abs $y2-y1)

--装载之后运行的过程：
E:\Github repository\Haskell\Code>ghci a
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Shapes           ( a.hs, interpreted )
Ok, one module loaded.
*Shapes> let a=Circle (Point 1 2) 3
*Shapes> surface a
28.274334
```