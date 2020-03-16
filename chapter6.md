# 高阶函数 
- 一个简单的高阶函数可以玩出很多花样，命令式语句使用**for，while，赋值，状态检测**来实现功能，再包起来留个界面使之象个函数一样调用。而函数式编程使用高阶函数来抽象出常见模式。
Haskell中的函数可以作为参数和返回值传来传去，这样的函数称为高阶函数。  
**要拒绝循环与状态的改变而通过定义问题“是什么”来解决问题，高阶函数必不可少**。

## Curried functions
本质上，**所有的函数只有一个参数**。
举一个Demo，max函数看起来取两个参数返回较大值。实际上，执行`max 4 5`时，会首先返回一个**取一个参数的函数**，其返回值不是4就是该参数，取决于谁大。然后再用5为参数来调用他。

```Haskell
-- 两种相同的表示方法
max 4 5
(max 4)5
```

- 空格放在两个东西间，叫做**函数调用**。
```Haskell
--等价的表示方法
max::(Ord a)=>a->a->a
max::(Ord a)=>a->(a->a)
```

也就是说，如果以不全的参数调用某函数，就可以得到一个不全调用的函数。如下函数所示
```Haskell
multThree::(Num a)=>a->a->a->a
multThree x y z=x*y*z

```
如果执行` multThree 3 5 9`是如何运行的呢？  
- 根据空格分隔，把3交给`multThree`，此时返回一个返回函数的函数，再把5交给他。返回一个去一个参数并使之x15的函数，最后把9交给函数返回135.
- 就像这样`multThree::(Num a)=>a->(a->(a->a))`,->前面的就是参数


### Demos
```Haskell
*Main> let mulTwo =multThree 9
*Main> mulTwo 2 3
54
*Main> let mul=mulTwo 2   
*Main> mul 1
18 
```


## 高阶函数的实例
```Haskell
--第一个参数为一个函数，类型限制为a->a,即接受一个a类型参数并返回一个a类型的值
--第二个参数为一个a类型参数
--这里的括号不能省
apply::(a->a)->a->a
--这里的运算是干什么呢？首先f接受一个参数为x，然后返回a类型的运算结果再被f所调用
apply f x=f(f x)

*Main> apply (+3) 10
16
*Main> apply (++"HAHA") "Hey"
"HeyHAHAHAHA"
*Main> apply (3:) [1]
[3,3,1]
```

### 重写zipwith
```Haskell
zipwithMe::(a->b->c)->[a]->[b]->[c]
zipwithMe _ [] _=[]
zipwithMe _ _ []=[]
zipwithMe f (a:ax) (b:bx)=f a b:zipwithMe f ax bx

*Main> zipwithMe (+) [1,2,3,4] [1,2,3]
[2,4,6]
```

### 重写flip
```Haskell
flipMe::(a->b->c)->b->a->c
--这种写法就有点厉害了，我怎么能知道g是一个什么样子的根据where的限定条件
flipMe f=g
  where g y x=f x y

flipMe::(a->b->c)->b->a->c
flipMe f x y=f y x
```

## map与filter
- map取一个函数和list做参数，遍历list的每个元素调用该函数产生一个新的list.  
- filter去一个限制条件和一个list，返回所有符合条件的元素。  

```Haskell
-- map
Prelude> map (succ) [1,2,3,4]
[2,3,4,5]
*Main> map (+3) [1,2,3,4,5]
[4,5,6,7,8]

--filter

```
### 重写map
```Haskell
-- 函数的映射正好是自变量，因变量的映射
mapMe::(a->b)->[a]->[b]
mapMe _ []=[]
mapMe f (x:xs)=f x:mapMe f xs
*Main> mapMe (succ) [1,2,3,4,5]
[2,3,4,5,6]
```

### 重写filter
```Haskell
filterMe::(a->Bool)->[a]->[a]
filterMe _ []=[]
filterMe f (x:xs)
  |f x=x:p
  |otherwise =p
  where p=filterMe f xs

*Main> filterMe (>=5) [1,3,5,7,9]
[5,7,9]
```

### 求小于100000中3829倍数
```Haskell
getNum::(Integral a)=>[a]
getNum = take 2 (filter p [100000,99999..])
 where p x=x `mod`3829==0 
```

### 找到所有小于10000的奇数平方和
- takeWhile函数：取一个限制条件和list做参数，从头遍历list并返回合法元素，直到限制条件不被满足则停止.
```Haskell
Prelude> sum(takeWhile (<10000) [m|m<-[n^2|n<-[1..]],odd m])
166650

--注意map的表达式要加括号,表示这个整体的返回值是一个参数
sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
166650
```

map与filter起到的作用和list comprehension是一样的


## lambda表达式
匿名函数,这个函数只会用到一次.编写lambda就在前面加`\`,后面是空格分隔的参数,`->`后面是函数体.
```Haskell
Prelude> zipWith (\a b->a*b) [1,2,3,4,5] [1,2,3,4]
[1,4,9,16]
```

### 使用lambda重写flip
```Haskell
filpMe::(a->b->c)->b->a->c
--这个lambda表示产生了新函数
flipMe f=\x y->f y x
```

## fold
- 一个fold取一个二元函数,一个初始值(可以叫做累加值)和一个需要折叠的list,二元函数有两个参数,即累加值和list的首项,返回值是新的累加值.然后,新的累加值和新的list首项调用该函数,直到list遍历完.
- 如何理解**折叠思路**?  
假设二元函数f,起始值z,从左右折叠`[3,4,5,6]`.那么实际上执行的就是:  
`f 3 (f 4 (f 5 (f 6 z)))`这样一个式子.z会在每轮调用之后被更新然后继续使用.

### 使用fold求list的和
```Haskell
sumMe::(Num a)=>[a]->a
-- 0为起始的累加值,lambda表示如何进行计算
sumMe xs= foldl (\acc x->acc+x) 0 xs

*Main> sumMe [1,2,3,4]
10
```
### 使用fold重写elem
```Haskell
elemMe::(Eq a)=>a->[a]->Bool
-- current为累加值,y为list的首项,返回值是新的累加值,y始终为list的首项
elemMe x xs=foldl (\current y-> if x==y then True else current) False xs
```
### 对list求积
```Haskell
mul::(Num a)=>[a]->a
mul xs=foldl (\curr fir->curr*fir) 1 xs

*Main> mul [1,2,3,4,5]
120
```

### 使用fold重写map
```Haskell
mapMe::(a->b)->[a]->[b]
--这样写是倒着的,所以不能这么写
mapMe f xs=foldl (\acc y->f y:acc) [] xs
*Main> mapMe (+3) [1,2,3,4,5]
[8,7,6,5,4]

--正确的写法:使用++运算子或者使用foldr,从右边开始遍历
mapMe f xs=foldl (\acc y->acc++[f y]) [] xs
mapMe f xs=foldr (\acc y->f y:acc) [] xs

```

### 使用foldr1与foldl1
与之前的函数类似,但无需提供初始值,假定首个(末尾)元素为初始值,并从旁边的元素开始折叠.

```Haskell
maxMe::(Ord a)=>[a]->a
--第一个为累加值,第二个参数为旁边的值
maxMe= foldl1(\acc y-> if acc<y then y else acc)
minMe::(Ord a)=>[a]->a
minMe=foldl1(\acc x->if acc>x then x else acc) 

*Main> let a =[1,2,3,4,5]
*Main> minMe a
1
*Main> maxMe a
5
```
### scanl与sacnr
与foldl,foldr类似,但返回会返回每部叠加的结果.

### 复合函数运算符 `.`
```Haskell
f::(Num a)=>a->a
g::(Num a)=>a->a
f x=x^3
g x=x^2 +2*x

*Main> (f . g) 3
3375
*Main> g 3
15
*Main> f 15
3375
```