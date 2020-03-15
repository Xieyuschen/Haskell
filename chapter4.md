# 函数的语法

## 模式匹配
模式匹配通过检查数据的特定结构来检查其是否匹配，并按匹配模式从中取得数据。在定义函数的时候可以给不同的模式分别定义函数本身，让源代码更加简洁易读。

```Haskell
sayMe::(Integral a) => a-> String
sayMe 1="Monday"
sayMe 2="Tuesdat"
sayMe y="I don't know"
```
上面这个函数`sayMe`被调用的时候，函数会从上至下进行匹配，一旦有匹配对应的函数体就被应用。如果1就是第一个2就是第二个，最后其他类型全部被接受。
在定义模式时，一定要留一个**万能匹配的模式**（相当于default来处理），这样就不会因为不可预料的输入而崩溃。



## 模式匹配的一些练习：

### 三元组取值
```Haskell
first::(a,b,c)->a
first (x,_,_)=x
second ::(a,b,c)->b
second (_,x,_)->x
```

`_`表示我们不关心这一部分内容。忽略掉次要的，只关注主要的。


### 求list长度的函数
```Haskell
--这里的(x:m)说的为[x,m] 括号表示x:m是一个整体
--不要理解为Tuple
--这里两种写法都一样，推荐第二种写法

--1.
len::[a]->Int
len [x]=1
len (x:m) = len(m)+1
--2.
len::[a]->Int
len []=0
--(_,m)表示不关注第一个位置的值是多少
len (_:m) = len(m)+1
*Main> len [1,2,3]
3
*Main> len [1,2,3,4,5,6]
6
```

### 求和
```Haskell
-- 这里的类型必须支持+运算子
addNum::(Num a)=>[a]->a
addNum []=0
addNum (m:n)=m+addNum n

*Main> addNum [1,2,3,4]
10
```

### 引用as的使用
```Haskell
word::String->String
word all@(x:y)="the first letter in word "++all++" is "++[x]

*Main> word "Helloworld"
"the first letter in word Helloworld is H"
```


## Guard的使用
模式用来检查一个值是否合适兵从中取值，guard检查一个值属性是否为真。类似于if语句

```Haskell
func::(RealFloat a)=>a->String
func bmi
    |bmi <=18.5= "you're underweight"
    |bmi <=25.0="Supposedly normal"
    |bmi<=30.0="Fat"
    |otherwise="Bingo"

*Main> func 20
"Supposedly normal"
*Main> func 90
"Bingo"
```
guard 由跟在函数名及参数后面的竖线标志组成。一个guard 就是一个**bool表达式**。如果真就使用对应的函数体，为假则下一个guard。

### 比较大小的函数
```Haskell
myCompare::(Ord a)=>a->a->Bool
a `myCompare` b 
    |a>b =True
    |otherwise= False

*Main> 1 `myCompare` 5
False
```
通过反单引号，不仅可以通过中缀形式调用函数，也可以在定义函数的时候使用。   


## 关键字Where
where绑定是在函数底部定义名字，对包含所有的guard在内的真个函数可见。
### demo
```Haskell
func::(RealFloat a)=>a->a->String
func weight height
    |bmi <=18.5= "you're underweight"
    |bmi <=25.0="Supposedly normal"
    |bmi<=30.0="Fat"
    |otherwise="Bingo"
    where bmi=weight/height^2
```
换句话说也就是`Where`语句可以定义一些变量，然后在这个语句块内使用。
where里面还可以i当以函数

### 计算多个bmi值
```Haskell
calbmis::(RealFloat a)=>[(a,a)]->[a]
calbmis xs=[bmi w h|(w,h)<-xs]
    where bmi weight height=weight/height^2

*Main> calbmis [(70,1.8)]
[21.604938271604937]
*Main> calbmis [(70,1.80),(49,165)]
[21.604938271604937,1.7998163452708907e-3]
```


## Let关键字
let绑定表达式，在任何位置定义局部变量，对不同的guard表空间。
let的格式为**let [bingdings] in [expression]**.对let绑定的名字仅**in部分可见**。
### 求圆柱的表面积
```Haskell
cylinder::(RealFloat a)=>a->a->a
cylinder r h=
    let sideArea=2*pi*r*h
        topArea=pi*r^2
    in sideArea+2*topArea
*Main> cylinder 10 10
1256.6370614359173
```

### 求bmi的let版本
```Haskell
calbmis::(RealFloat a)=>[(a,a)]->[a]
calbmis xs=[bmi|(v,h)<-xs,let bmi =v/h^2]
```

## case 表达式
```Haskell
--case 表达式语法：
case expression of pattern->result
                   pattern->result
                   ...                
```

### 两种等价的表达方式
```Haskell
meHead::[a]->a
meHead []=error "Cannot do this!"
meHead (a:_)=a

