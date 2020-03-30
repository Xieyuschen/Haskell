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

## Record Syntax
简单而言就是给自定义的类型都起上名字，方便使用
```Haskell
data Person=Person{
    firstName::String,
    lastName::String,
    age::Int,
    hetght::Float,
    phoneNumber::String
}deriving(Show)

--对自定义的类初始化
Person{firstName="Xie",lastName="Yuchen",age=20,hetght=1.8,phoneNumber="xxx"}
```
起个名字，那么在使用的时候就会很方便.

## Type parameters
类型构造子可以取类型做参数，产生新的类型。与C++的模板有相似的地方，但有一些不一样的地方。

### Maybe 类型
**这个类型很重要，以后经常碰到它，记住啦！**。
```Haskell
--此处的a为类型参数
data Maybe a =Nothing|Just a

ghci>:t Just "haha"
Just "haha"::Maybe [Char]
ghci>:t Just 84
Just 84::(Num t)=>Maybe t
ghci>:t Nothing
Nothing::Maybe a
```
以上的例子说明了类型构造的用法，根据提供给它的值来自动确定该为什么类型，也即不需要显式的指定类型，只用把对应类型的值给出就可以了，它可以自动推断出来。

### 特别的Nothing
Nothing的类型是`Maybe a`，也就是说我不知道它的具体类型。这个类型是多态的，如果有`Maybe Int`类型的，就可以传递给他一个Nothing，因为Nothing里面不包含任何值。`Maybe a`类型可以有`Maybe Int`的行为，也可以是`Maybe String`的行为。
- List的额外理解
list的`[]`就是一个`Nothing`,不需要指定类型，所以他可以对所有的list进行操作。比如说：  
```Haskell
[1,2,3]++[]
"hello"++[]
```
以上的代码都是正确的，这要取决于`[]`具有多态性。

## Derived instances
### `类型类`与C++等语言中`类`的区别
- 类：类像是蓝图，可以根据它来创建对象，保存状态并执行操作。
- 类型类：更像是界面，**不是靠它构造数据，而是给既有的数据描述行为**。

### 使用`deriving`关键字生成类型类`Eq`的`instance`
```Haskell
data Person=Person{
    firstName::String,
    lastName::String,
    age::Int
}deriving(Eq)
```
如上代码，Person类型derive了Eq的instance，就可以直接使用`==`,`/=`来判断他们的相等性了。首先检查两个值的值构造子是否一致，在检查其中的所有数据（都必须是Eq的成员）。

## Type synonyms
使用`type`关键字给一个类型起一个别名。  
`type String = [Char]`  

## Recursive data structure
### 构造一棵Binary Search Tree
```Haskell
data Tree a=EmptyTree|Node a (Tree a) (Tree a) deriving(Eq,Read,Show)
singleton::a->Tree a
singleton x =Node x EmptyTree EmptyTree

--不在原有的树上进行操作，而是重新创建了一棵新树
treeInsert::(Ord a)->Tree a->Tree a
treeInsert x EmptyTree=singleton x
treeInsert x (Node a left right)
--相等就不需要加了，直接返回原树即可
    |x==a =Node x left right
    |x<a =Node a (treeInsert x left) right
    |x>a =Node a left (treeInsert x right)

treeElem::(Ord a)=>a->Tree a->Bool
treeElem x EmptyTree=False
treeElem x (Node a left right)
    |x==a = True
    |x<a =treeElem x left
    |x>a =treeElem x right

```

## 再叙`Typeclass`
Typeclass和Java或Python里面的class一点关系都没有。  
Typeclass 就像一个interface，一个typeclass定义了一些行为（比如说比较相不相等，比较大小顺序能否穷举）。而我们会把希望满足这些性质的类型定义成这些typeclass的instance。简单而言，**instance简单来说就是实现某个Typeclass的类型**。

### instance 关键字
instance关键字用来说明我们要定义某个typeclass得instance。
#### 创建Eq的instance
```Haskell
data TrafficLight =Red|Yellow|Green
instance Eq TrafficLight where
    Red == Red =True
    Yellow == Yellow =True
    Green == Green =True
    _ == _ =False

*Main> Red == Red
True
*Main> Green == Red
False
```

#### 创建Show的instance
```Haskell
--如果简单的derive来自动生成Eq的话，效果是一样的，但是derive生成的show会把值构造子转换为字符串。
data TrafficLight =Red|Yellow|Green
instance Show TrafficLight where
    show Red = "Red light"
    show Green = "Green light"
    show Yellow = "Yellow light"

*Main> Red
Red light
*Main> Green
Green light
```

## yes-no typeclass
在弱类型程序语言中可以在if语句中摆上任何东西。
```Haskell
class YesNo a where
    yesno::a->Bool
instance YesNo Int where
    yesno 0 = False
    yesno _ = True

*Main> yesno $ length []
False
```

## Funcroe typeclass
一个重要的typeclass，叫做`Functor`。基本可以代表被map over的事物。  
list就属于Functor这个typeclass。  

[注]：剩下的两小节，p161~170，以后再来看一看。