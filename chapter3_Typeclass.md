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

## 类型
**凡是类型首字母必大写~**。
- Int 整数，有界 -214748364~214748364
- Interger 无界整数。
- Float 单精度浮点数
- Double 双精度浮点数
- Bool
- Char

## Typeclasses
类型定义行为的界面，如果一个类型属于某Typeclass，那么它必实现了该Typeclass描述的行为（就是接口嘛）。

### 基本的Typeclass
- Eq 包含可判断相等的类型
- Ord 包含可比较大小的类型
- Show 可用字符串表示的成员，目前为止除函数外所有的类型都是Show成员。
- Read 可以将一个字符串转换为Read的某成员类型
- Enum 成员都是连续类型，可枚举
- Bounded 成员取上下限
- Num 表示数字的Typeclass，成员类型都具有数字的特征。所有的数字都是堕胎常量