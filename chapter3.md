# Types and Typeclasses
## `:t`命令
`:t`命令后跟任何可用的表达式，得到表达式的类型（C#中的nameof也可以这样）。
```Haskell
Prelude> :t "hello"
"hello" :: [Char]
Prelude> :t 13
13 :: Num p => p
Prelude> :t 3.1
3.1 :: Fractional p => p
Prelude> :t (1,"qe")
(1,"qe") :: Num a => (a, [Char])
```