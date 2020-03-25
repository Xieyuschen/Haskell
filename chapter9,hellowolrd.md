# 输入与输出
Haskell是一个纯粹的函数式语言。  
- 命令式语言使用定义东西的方法执行。
- Haskell中，一个函数不能改变状态，如改变一个变量的内容是不可能的。

## Helloworld
```Haskell
main = putStrLn "helloworld"
--输入命令：
ghc --make a
--运行结果
E:\Github repository\Haskell\Code>a
helloworld
```

## putStrLn 与 getLine
```Haskell
Prelude> :t putStrLn
putStrLn :: String -> IO ()

--getLine返回String的I/O action，等待用户的输入
Prelude> :t getLine
getLine :: IO String
```
- putStrLn接受一个字符串并返回一个**I/O action**,I/o action 包含了`()`的类型（空的tuple或者是unit类型）。一个I/O action 是一个会造成副作用的动作，也会返回某些值。因为在屏幕输出字符串并没有什么有意义的返回值，所以用一个`()`表示。  
一个I/O action会在把他绑定到`main`这个名字并执行程序的时候触发。

```Haskell
main = do
    putStrLn "Hello,what's your name?"
    --执行一个I/O action getLine并将它的结果绑定到name这个名字，所以说name的类型是String
    name<-getLine
    putStrLn ("Hey"++name++", you rock!")

Hello,what's your name?
xieyuchen
Heyxieyuchen, you rock!
```

### 把名字转为大写
```Haskell
import Data.Char
main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine

    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $"Hey "++bigFirstName++" "++bigLastName++" ,how are you"

What's your first name?
Xie
What's your last name?
Yuchen
Hey XIE YUCHEN ,how are you
```


### 把输入的字符串倒过来
```Haskell
main = do 
    line<-getLine
    --输入一个空白行为真，null是一个函数判断是否为空    
    if null line
    --利用某个pure value造出I/O action
        then return ()
        else do
            putStrLn $reverseWords line
            --递回调用main函数
            main

--比如现在输入的为“hello world"
--首先调用word来产生一个字符串行["hello","world"]
--然后使用reverse来map整行，得到["olleh","dlrow"]
--然后unwords得到最终的结"olleh dlrow"
reverseWords::String->String
reverseWords = unwords . map reverse . words
```

### return的使用
嗯这里的return和命令式语句的return一点关系都没有，就像巴基斯坦和巴勒斯坦有个**关系（逃。  
- return将value包装成I/O actions，如果不绑定名称则结果会被忽略，可以使用`<-`与return结合来达到绑定名称的目的。
```Haskell
main = do
    a <- return "Hello"
    b <- return "world"
    putStrLn $ a++" "++b
```

return 与`<-`作用相反，return把value装进盒子(I/O action)里，而`<-`把value从盒子里拿出来并绑定一个名词。

#### 一个小问题：那么let bindings 也可以达到目的，为什么还需要return？
1. 需要一个什么都不做的I/O action
2. 不希望这个do block形参的I/O action 的结果是这个block中的最后一个I/O action。



## 一些实用的I/O函数
### putStr
与putStrLn基本一致，只是前者会换行，而后者不会换行
### putChar
接受一个字符，然后输出出来
### print
接受任何是`Show typeclass`的instance类型的值。

### getChar
从输入读入一个字符的I/O action。只有当enter被按下的时候才会触发读取字符的行为。

### when函数：对if xxx then xxx else return ()的封装
使用when函数要导入`import Control.Monad`.  
接受Bool值与一个I/O action，如果True则返回传给它的I/O action，法则就 `return ()`。
```Haskell
--两个等价的程序：
main = do 
    c<-getChar
    if c/=' '
        then do
            putChar c
            main
        else return ()

--使用when
import Control.Monad
main = do
    c<-getChar
    when(c/=' ') $ do
        putChar c
        main

```

### sequance
sequence接受遗传I/O action，并返回一个会依序执行它们的I/Oaction。运算的结果是包在一个I/O action的一连串I/O action的运算结果。
