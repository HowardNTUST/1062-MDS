
#### 加減乘除####
1+2
1-2
1*2
1/2

#### 平方 ####
2^5

#### 變數運算 ####
howard = 100
peter = 99
howard - peter
howard = howard + peter

#### 檢視變數性質####
a=T
class(a)
str(a)

a=1
class(a)
str(a)

#### 變數應用####

# c() 向量 = 代表裡面可以一次儲存不同的物件
# '' 代表字串
# 1,2,3 代表數值，電腦可以拿來計算
days = c('Mon','Tue','Wed','Thu','Fri','Sat','Sun') 
temp = c(1,2,3,4,5,6,7) 
class(temp)
str(temp)

# 替 temp 命名，以days為之
names(temp) = days
class(temp)
str(temp)

# 挑選temp數值向量中的第二個
temp[2]

# 挑選temp數值向量>3的
temp>3
# Mon   Tue   Wed   Thu   Fri   Sat   Sun 
# FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE 

# 向量的條件選擇
temp[temp>3]
temp[temp<3 | temp>5] # | 代表or: 聯集
temp[temp>3 & temp>5] # | 代表and: 交集
temp[temp==3] # == 代表：是否符合，那why不是 單純 = 呢？


# 變數運算
sum(temp)
var(temp)
sd(temp)
max(temp)
min(temp)
prod(temp)

#### if else ####
a = temp[3]

if (a>=3) {
  print(names(a))
}else if(a>=4){
  print('好棒棒')
}


# exercise 1 
'
1.將temp裡面<2的數值挑選出來後加總，存成變數temp1
2.將temp裡面>6的數值挑選出來後加總，存成變數temp2
3.將temp1 與 temp2 相乘
'
temp1 = sum(temp[temp<2])
temp2 = sum(temp[temp>6])
temp1 * temp2


# exercise 2
'
一行指令將<2 或者 >6的數值挑選出來後加總
'

temp1 = sum(temp[temp<2 | temp>6])  
