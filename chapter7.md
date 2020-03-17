# 模组 Modules
Haskell中的模组是含有一组相关的函数，类型和类型类的组合。而Haskell程序的本质是从**主模块中引用其他模组并调用其中的函数来执行操作**。  
——>这样可以把源代码分成多块，一个模块祖国独立就可以被重用，提高健壮性（高内聚低耦合/xyx）。  
标准库就是一组模组，每个模组都含有功能相近或相关的函数和类型。
>处理list的模组
>处理并发的魔族
>处理复数的模组
>...

比如之前用到的所有函数，类型以及类型类都是`Prelude`模组的一部分。  
本章学习几个**常用的模组**。

## 装载模组
装载的语法为`import`，在函数定义之前，一般放在源代码的顶部。  
——>比如装载一个Data.List，里面有很多处理List的函数。

```Haskell
import Data.List

--在ghci里装载一个/多个模组
ghci>:m Data.List
ghci>:m Data.List Data.Map Data.Set

--只使用模组中的部分函数，比如只使用sort
import Data.List (sort)

--包含除去某函数之外的其他函数，可以避免命名冲突
import Data.List hiding (sort)


--避免命名冲突，但还都要使用
--使用import qualified 加入的包，如果使用必须指明
--比如说调用Data.Map中的filter，就要Data.Map.filter
import qualified Data.Map

--取别名
import qualified Data.Map as M
```
## Data.List
**常用函数**：  
### intersperse 取一个元素与list，将此元素置于list每个元素之间
```Haskell
Prelude> :m Data.List
Prelude Data.List>  intersperse '.' "HelloWolrd"
"H.e.l.l.o.W.o.l.r.d"
```
### intercalate 取两个list，将第一个list交叉插入第二个元素中间.
```Haskell
intercalate :: [a] -> [[a]] -> [a]

Prelude Data.List> intercalate " " ["h","t","w"]
"h t w"
--这个操作是非法的
--Prelude Data.List> intercalate [0,0,0] [1,2,3,4]

--正确操作
Prelude Data.List> intercalate [0,0,0] [[1,2],[3,4],[5,6]]
[1,2,0,0,0,3,4,0,0,0,5,6]
```
### tanspose 将矩阵转秩
```Haskell
Prelude Data.List> transpose [[1,2,3],[4,5,6],[7,8,9]]
[[1,4,7],[2,5,8],[3,6,9]]

Prelude Data.List> transpose [[3,2,1,0],[0,1,1,2],[1,0,3,5]]
[[3,0,1],[2,1,0],[1,1,3],[0,2,5]]
```
### concat 把一组List连接成为一个List
```Haskell
Prelude Data.List> concat [[1,2,3],[1,3,4]]
[1,2,3,1,3,4]
```
### concatMap
与map一个list再concat它等价.    
```Haskell
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
--下面的两种运算方式等价
Prelude Data.List> concatMap (replicate 4) [1..3]
[1,1,1,1,2,2,2,2,3,3,3,3]

Prelude Data.List> map (replicate 4) [1..3] 
[[1,1,1,1],[2,2,2,2],[3,3,3,3]]

Prelude Data.List> concat $ map (replicate 4) [1..3] 
[1,1,1,1,2,2,2,2,3,3,3,3]
```
### and,or,any ,all
- and取一组Bool值的List做参数，其中值全为真返回.
- or：有一个真就返回真.
- any/all 取一个限制条件判断是否存在/所有都符合条件.
```Haskell
Prelude Data.List> and $ map (>4) [1,2,3,4,5,6]
False
Prelude Data.List> or $ map (>4) [1,2,3,4,5,6]
True
Prelude Data.List> all (>4) [1,2,3,4,5,6]
False
Prelude Data.List> any (>4) [1,2,3,4,5,6]
True
```

### iterate
取一个函数和一个值做参数，用该值调用该函数并用所得结果再次调用该函数，产生一个无限的List。
```Haskell
Prelude Data.List> take 10 $ iterate (*2) 1
[1,2,4,8,16,32,64,128,256,512]
```
### splitAt 取一个list和数值做参数，返回一个二元组
数值表示第几位，索引从0开始
```Haskell
Prelude Data.List> splitAt 3 [1,2,3,4,5]
([1,2,3],[4,5])

Prelude Data.List> let (a,b) =splitAt 3 "HeyHello" in b++a
"HelloHey"
```
### dropWhile 与 takeWhile
takeWhile 遇到不符合条件的元素停止，dropWhile相反，扔掉符合条件的元素一旦限制条件返回Fasle就返回剩下的部分
```Haskell
Prelude Data.List> takeWhile (<4) [1,1,2,3,3,4,5,6,7]
[1,1,2,3,3]
Prelude Data.List> dropWhile (<4) [1,1,2,3,3,4,5,6,7]
[4,5,6,7]
```

### span 与 break
span首次条件为False断开list，break首次为True时断开list
`break p <=> span (not . p)`
```Haskell
Prelude Data.List> break (==4) [1,2,3,4,5,6,7]
([1,2,3],[4,5,6,7])
Prelude Data.List> span (/=4) [1,2,3,4,5,6,7]
([1,2,3],[4,5,6,7])
```

### group
将**相邻并相等**的元素各自归类.
```Haskell
Prelude Data.List> group [1,1,1,2,2,2,2,3,4,5]
[[1,1,1],[2,2,2,2],[3],[4],[5]]
Prelude Data.List> group [1,2,3,3,2,2,1,1,5]
[[1],[2],[3,3],[2,2],[1,1],[5]]
```

### sort 
对一个list排序.
### inits tails 
与init，tail相似，会递归调用自身直到什么都不剩，返回每次调用的结果
```Haskell
Prelude Data.List> inits [1,2,3,4,5,6]
[[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[1,2,3,4,5,6]]
```

### /与union
集合的差集与∪

### 注
一些函数需要使用的自己查文档，不列全了。
### 求多个多项式的和
求 3x^3+2x^2+x、x^2+x+2、x^3+3x+5的和
```Haskell
Prelude Data.List> map sum $transpose [[3,2,1,0],[0,1,1,2],[1,0,3,5]]
[4,3,5,7]
```

### foldl'与foldl1'
以上是它们各自惰性实现的严格版本，在只用fold处理较大的List的时候，经常会遇到**堆栈溢出**的问题。引起问题的原因就是fold的惰性。  
——>如何理解惰性？   
执行fold时，**累加器的值不会被立即更新**，而是做一个“在必要时会取得所需的结果”的承诺。美国一边累加器这一行为就重复一次，最后这堆承诺就会塞满堆栈。而严格的fold不会做承诺，而是直接计算中间值的结果并继续执行下去。

