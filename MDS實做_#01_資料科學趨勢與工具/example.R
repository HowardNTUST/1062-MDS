## 問題二
exampleV <- c ( 1 , "2"  ,5  , "JAON" , 0.5 , " f " )
exampleF  <- factor ( c ( 1 , " B " , 5 , " B " , 0.5 , " B " ) )

ex.vactor[4]
str(ex.factor)

# c() 向量 = 代表裡面可以一次儲存不同的物件
# '' 代表字串
# 1,2,3 代表數值，電腦可以拿來計算
days = c('Mon','Tue','Wed','Thu','Fri','Sat','Sun') 
temp = c(1,2,3,4,5,6,7) 


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



## 問題三

a <- 10
if (a >15){ 
  print("a 沒有大於15")
}else{
  print("a 大於15")
}

# for 列舉i從1到7 
for (  i  in  1:7 ){ 
  print( i )
} 


Vactors <- c()
for ( x in 1:5 ) {
  # 將 x 變數加到 vactors 最後面
  vactors <-  append (  vactors , x )
} 


# for 列舉i從1到7 
for ( x  in  vactors  ) { 
  print( x )
} 

# 自訂
costmerSUM <- function(A ,B ) {
  return (A+B)
}

costmerSUM(99,100)

