# 基本语法
## 函数
执行运算的都是函数，并不区分运算符与函数（在类里面的运算符重载本质也是函数）。比如*运算符就是一个函数，关于函数有更细的分类：前缀、中缀、后缀函数。

### 常见函数
- `succ`:递增  
- `max/min`:接受两个参数，求出最大/小的。  
- `/=`：不等于运算符，中缀函数
调用函数并不需要使用括号，比如`max 2 3`结果为3.  


## 自定义函数：
```haskell
doubleMe x=x+x
```
**Haskell的函数名首字母不允许大写**。  
函数的声明先函数名，然后跟**空格**分隔的参数表，然后使用=隔开参数列表与函数的具体行为。

## if-else语句
```
if condition
then operation1
else operation2

//Demo:
if x>100
then x
else x+100
```
注意else部分是不可以省略的。
**Haskell严格遵守类型，~~非零即真~~这句话不适用于此处。**。  

## List
list是一种单类型数据类型，存储多个**类型相同**的元素。（感觉和模板类差不多）。

### list的一些运算
#### `++`运算子：
实现两个list的连接。
```
ghci>[1,2,3]++[7,8,9]
[1,2,3,7,8,9]
gchi>"hello"++"world"
"helloworld"
```
运行++运算子遍历运算子左边的list，如果想要在一个较长的list上追加元素，向list前端插入元素会是更好的选择。

#### `:`运算子
`:`运算子可以连接**一个元素**到一个list中
```
ghci> 5:[1,2,3,4,5]
[5,1,2,3,4,5]
```
#### `!!`运算子
根据索引求list中值的运算子，下标开始为0.
```haskell
ghci>[0,1,2,3,4,5,6]!!3
3
```
如果越界就会报错。

#### `>与>=`运算子
比较两个list中的大小，返回Bool值。  
先比较第一个，相同在比较第二个。。
```
ghci> [3,2]>[2,1]
True
```

#### 一些常用操作
- head返回第一个元素
- tail返回出第一个元素剩下的部分
- init返回除最后一个的其他元素
- last返回最后一个元素。  

不能对一个空列表进行上述操作

- length 返回list的长度
- null 判断list是否为空，若为空返回True
- reverse 将list翻转
```
ghci> reverse [1,2]
[2,1]
```
- take 返回前几个元素，参数列表为 take index list
```
ghci> take 3 [6,5,4,3,2]
[6,5,4]
```
若take的索引超过列表数目，则返回原列表。

- drop 操作与take类似
- sum 返回和（对不能加的类型操作会报错）
- elem 判断list是否包含元素，中缀函数

### Range初始化list

```
ghci>[1..5]
[1,2,3,4,5]
gchi>[2,4..10]
[2,4,6,8,10]

//取24个13的倍数
gchi>take 24 [13,26..]
```
Haskell是惰性的，斌会不会对无限长度的list求值，只会在需要的时候取合适的数量以返回。

#### cycle、repeat与replicate
- cycle：用于获得一个循环特征的若干项 比如获得`[1,2,3,1,2,3,1,2]`
- repeat 用于获得连续若干个相等的list，如`[1,1,1,1,1,1]`
- replicate 类似于repeat

```
gchi> take 4 (cycle [1,2,3])
[1,2,3,1]
gchi> take 4 (repeat 1)
[1,1,1,1]
gchi> replicate 4 1
[1,1,1,1]
```


## List Comprehension
写一个集合

```
ghci> [x*2|x<-[1..10]]
[2,4,6,8,10]
```
在括号内部通过竖线隔开，左边为对对应集合的元素进行运算，右边对元素的范围进行了设定。

筛选大小写字母：
```Haskell
ghci> removeNonUpper x=[c|c<-x,c `elem` ['A'..'Z']]
ghci> removeUpper x=[c|c<-x,c `elem` ['a'..'z']]
ghci> c="HELLOworld"
gchi> removeNonUpper c
HELLO
gchi> removeUpper c
world
```

### 嵌套使用
求列表中所有的偶数
```Haskell
ghci>  let xxs=[[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
//错误示例：
ghci> [x|x<-xxs,[c|c<-x,even c]]

//正确示例：
ghci> [[x|x<-xs,even x]|xs<-xxs]
[[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
```

分析我要的是什么，首先我要的仍是一个包含list的list，所以说要的东西应该是`xs<-xxs`这样的东西，然后再在前面进一步筛选。


## Tuple
Tuple要求对需要组合的数据的数目非常明确，类型取决于其中项的数目和各自的类型。Tuple中的项由括号括起，逗号隔开。  
Tuple没有**单元素**。
### 常见操作
- fst 返回一个序对的首项。 
- snd 返回尾项
- zip 将两个list进行配对
```Haskell
Prelude> zip [1,2,3,4,5] [5,5,5,5,5]
[(1,5),(2,5),(3,5),(4,5),(5,5)]

Prelude> zip [1..] [2,3,4]
[(1,2),(2,3),(3,4)]
```
较长的会在中间断开匹配较短的那个，因为惰性所以可以处理有限或无限的list。

### 一个Demo
求边长小于10，周长为24的直角三角形
```Haskell
Prelude> [(a,b,c)|a<-[1..10],b<-[1..a],c<-[1..b],a+b+c==24,b^2+c^2==a^2]
[(10,8,6)]
```