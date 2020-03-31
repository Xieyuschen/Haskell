# 递回
**otherwise 后面的等号不要漏掉了！！**。
## 重写一些运算子
### 最大值
```Haskell
myMax::(Ord t)=>[t]->t
myMax []=error "Error"
myMax [x]=x
myMax (x:xs)
     | x > maxTail = x 
     |otherwise= maxTail 
     where maxTail= myMax xs
*Main> myMax [1,2,3,4,4]
4
```

### 重写replicate
```Haskell
rep::(Ord i,Num i)=>i->a->[a]
rep n x
   |n<=0 =[]
   |otherwise =x:rep (n-1) x
```

### 重写take函数
```Haskell
takeMe::(Num i,Ord i)=>i->[a]->[a]
takeMe x _
 |x<=0 =[]
takeMe n (x:xs)=x:takeMe (n-1) xs
```

### 重写reverse
```Haskell
reverseMe::[a]->[a]
reverseMe []=[]
--不要写reverseMe xs :x
reverseMe (x:xs)=reverseMe xs ++[x]
```

### 重写zip
```Haskell
zipMe::[a]->[b]->[(a,b)]
zipMe _ []=[]
zipMe [] _=[]
zipMe (a:ax) (b:bx)=(a,b):zipMe ax bx
```
### 重写elem
```Haskell
elemMe::(Eq a)=>a->[a]->Bool
elemMe a []= False
elemMe a (x:xs)
 |a==x =True
 --不想用中缀函数就这样写
 -- |otherwise = elemMe a xs
 |otherwise = a `elemMe` xs
```

## 快排
```Haskell
quickSort::(Ord a)=>[a]->[a]
quickSort []=[]
quickSort (x:xs)=
 let smallpart=quickSort [a|a<-xs,a<=x]
     bigpart=quickSort [a|a<-xs,a>x]
 in smallpart++[x]++bigpart

 *Main> quickSort "sadhiahwdowoqqwruiqhwoqwobmlckamz"
"aaabcddhhhiiklmmooooqqqqrsuwwwwwz"
```

## 使用递回来思考
1. 先定义一个边界条件
2. 在定义一个函数，让函数从一堆元素取一个元素做点事情之后，把剩下的元素继续交给这个函数
