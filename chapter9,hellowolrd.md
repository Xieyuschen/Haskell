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
sequence接受一串I/O action，并返回一个会依序执行它们的I/Oaction。运算的结果是包在一个I/O action的一连串I/O action的运算结果。  
seqaunce的typeclass是`sequacne::[IO a]->IO [a]`，简单来说就是上面所说的内容。
```Haskell
--下面的这两个程序是一样的，第二段用到了sequance
main = do
        a<-getLine
        b<-getLine
        c<-getLine
        print [a,b,c]

main = do
    rs<-sequance[getLine,getLine,getLine]
    print rs
```

### sequence与map的互动：
关注样例的输出：`[(),(),(),()]`,这个是从哪里来的呢？  
——在ghci中运行IOaction的时候，会执行并把结果打印出来，唯一异常是当结果为`()`时候不会被印出。所以`putStrLn "Suprising!"`只输出`Suprising!`，因为`putStrLn "Suprising!"`的结果是`()`,而getLine的结果我IO String，所以结果会被印出来。
```Haskell
sequence (map print [1,2,3,4])
--输出的结果：
1
2
3
4
[(),(),(),()]
```
#### mapM与mapM_
因为`sequence map ....`这个用法太经常了，所以就封装了一下。
```Haskell
mapM print [1,2,3]
1
2
3

mapM_ print [1,2,3]
1
2
3
[(),(),()]
```


### forever
forever接受一个I/O action并返回一个永远作用同一件事的I/Oaction。
```Haskell
import Control.Monad
import Data.Char
--do里面是一个I/O action，会永远的运行这个I/Oaction :)
main = forever $do
    putStrLn "Give my some input: "
    j<-getLine
    putStrLn $map toUpper j

Give my some input: 
hello
HELLO
Give my some input:
gutentag
GUTENTAG
Give my some input:

```


## 关于I/O action
I/O action跟其他Haskell中的value没有两样，我们能够把它当参数传给函数，或是函数返回 I/O action。特别之处在于当他们是写在main里面或ghci里面的时候，他们会被执行也就是实际输出的时候，妹妹个I/O action也能抱着一个从真实世界拿回来的值。   
不要把上面I/O函数认为是接受字符并输出到荧幕。要想成**一个函数接受字符串并返回一个I/O action**。当I/Oaction被执行的时候，会输出想要的东西。

# 文件和字符流
## getContents
### 在运行exe时指定输入

```Haskell
--hs 文件，编译后不指明名字的话就是a.exe
import Control.Monad
import Data.Char
main = forever $do
    putStrLn "Give my some input: "
    j<-getLine
    putStrLn $map toUpper j

--作为输入的文件名叫1.txt
E:\Github repository\Haskell\Code>type 1.txt
what are you wanna to lose
you cover your wounds but underneath them
a million voices in your mind that wisper stop now

--将1.txt的内容作为输入
E:\Github repository\Haskell\Code>type 1.txt |a
Give my some input:
WHAT ARE YOU WANNA TO LOSE
Give my some input:
YOU COVER YOUR WOUNDS BUT UNDERNEATH THEM
Give my some input:
A MILLION VOICES IN YOUR MIND THAT WISPER STOP NOW
Give my some input:
a: <stdin>: hGetLine: end of file
```

### 使用getContents
```Haskell
import Control.Monad
import Data.Char
main = do
    contents <-getContents
    putStr $map toUpper contents
```


## interact
### 读取输入，印出少于十个的字符行
```Haskell
main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly::String->String
shortLinesOnly input=
    let allLines = lines input
        shortLines =filter(\line->length line<10) allLines
        result =unlines shortLines
    in result
```

因为从输入那里的字符串经过一些转换然后输出这样的模式实在太常用了，所以建立一个函数叫`interact`.接受一个String->String的函数，并返回一个I/Oaction。这个I/Oaction会读取一些输入，调用提供的函数然后把函数的结果印出来。

### 判断是否为回文数：
```Haskell
func::String->String
func contents=unlines(map(\xs->
    if isPalindrome xs then "Palindrome" else "not Palindrome") (lines contents))
    where isPalindrome xs=xs ==reverse xs
--interact去读取输入，把输入内容作为参数给func调用，返回IO String
main = interact func

--如果不使用文件IO的话就可以直接运行命令runhaskell
E:\Github repository\Haskell\Code>runhaskell a.hs
hello
not Palindrome
hh
Palindrome
goog
Palindrome
```

## 读写文件
### 读文件
前面介绍的读文件类似于把一个文件当作标准输入，而不是直接读文件。这里是程序打开文件进行一些列的操作。  
**do 把若干个I/O action绑在一起**。
```Haskell
import System.IO 
main = do
--openFile::FilePath->IOMode->IO Handle
    handle<-openFile "1.txt" ReadMode
    contents<-hGetContents handle
    putStr contents
--hClose用来关闭文件
    hClose handle

```
- IOMode：
`ReadMode|WriteMode|AppendMode|ReadWriteMode`.