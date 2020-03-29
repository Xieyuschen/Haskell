# Functor
被定义为Functor的instance可以使用famp，但在声明instance的环节要把fmap如何工作写清楚。
 <img src="./Images/1.jpg" width = "300" height = "400" alt="图片名称"/>
 <img src="./Images/2.jpg" width = "300" height = "400" alt="图片名称"/>
 <img src="./Images/3.jpg" width = "300" height = "400" alt="图片名称"/>
 <img src="./Images/4.jpg" width = "300" height = "400" alt="图片名称"/>

## functor与IO action的互动
### 将字符翻转1
```Haskell
import System.IO
main = do 
    line<-getLine
    let line' =reverse line
    putStrLn $ "you said" ++ line' ++" backwards"
    putStrLn $ "Yes , you really said "++line'++ "backwards"
```

### 将字符翻转2
主要举出Functor的使用方法。
```Haskell
--这个程序是不能通过编译的，因为IO是不允许重写的？
--a.hs:1:10: error:
--Duplicate instance declarations:
--instance Functor IO -- Defined at a.hs:1:10
--instance Functor IO -- Defined in ‘GHC.Base’
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
main = do
--思考为什么本行可以运行？——因为getLine是Functor的instance，所以就可以使用fmap
    line <- fmap reverse getLine
    putStrLn $ "you said " ++ line ++" backwards"
    putStrLn $ "Yes , you really said "++line++ "backwards"
```

### fmap的使用：
在库里，已有的instace都有Functor这个typeclass，所以可以直接使用：
```Haskell
Prelude> fmap (*2) [1,2,3]
[2,4,6]  
Prelude> fmap reverse getLine
hello
"olleh"  
```
下面写了一个函数，同样也是直接使用了fmap，因为getLine已经是Functor的instance了。
```Haskell
import Data.Char
import Data.List
main = do
    line <- fmap(intersperse '-' . reverse . map toUpper) getLine
    putStrLn line
hello
O-L-L-E-H
hello world
D-L-R-O-W- -O-L-L-E-H
```

## Functor与(->) r
简单而言函数也可以进行运算
```Haskell
ghci>fmap (*3) (+100) 1
300
```