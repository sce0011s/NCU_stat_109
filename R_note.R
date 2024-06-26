###R範例:
#數值解  
f <- function(x){-(x^2+20)}
f <- function(x){(x+1)*exp(x)}
curve(f,xlim = c(-1000,1000))
optimize(f,interval = c(-10,10),maximum = T)
optimize(f,interval = c(-1000,1000),maximum = F) #tol:精度容忍度

#數值積分
f <- function(x){(x+1)*exp(x)}
integrate(f,0,10)

#畫圖
data("cars")
plot(cars)
#3d散佈圖
library(scatterplot3d)
scatterplot3d(mtcars$wt,mtcars$disp,mtcars$mpg,main = "Basic 3D Scatter Plot")
#地形圖
x.volc<-10*(1:nrow(volcano))
y.volc<-10*(1:ncol(volcano))
contour(x.volc, y.volc, volcano, main="contour()")
image(x.volc, y.volc, volcano, main="image()")
persp(x.volc, y.volc, volcano, main="persp()")
#地圖
library(maps)
map <- map_data("world")
wmap1 <- ggplot(map, aes(x = long, y = lat, group = group)) + geom_polygon(colour = "white")
wmap1
map <- map_data("world", region = c("Japan","Laos","Cambodia", "Vietnam", "Malaysia", "Thailand", "Taiwan", "China","South Korea","North Korea","Singapore","Indonesia", "Mongolia"))
wmap1 <- ggplot(map, aes(x = long, y = lat, group = group)) + geom_polygon(colour = "white")
wmap1
#文字雲圖
library(wordcloud2)
wordcloud2(demoFreq, size = 2, fontFamily = "微軟雅黑", color = "random-light", backgroundColor = "grey")
#dashboard(互動式圖表)
library(shiny)
runGitHub("ballr", "toddwschneider")

###R特性:
#1.命名規則:物件.函數可包含底線.句號,不能以數字或句點+數字做開頭(需利用``處理)
1a <- 1 #unexpected symbol in "1a"
.1a <- "s" #unexpected symbol in ".1a"
`1a` <- 1
中央統計所 <- "NCUSTAT" #不建議使用中文

#2.賦值符號 =,<-,->
a = 1 #特殊狀況下會造成錯誤
b <- 2 #在函數中給變數設值使用=
3 -> c #較少使用
assign(paste0("a","1"),1) #進階:eval()+substitute()
x1=x2=x3=123 #可用多個賦值符號做賦值
x1 <- 124 ->x2

#3.NA(Not a valiable),NaN(Not a Number),NULL(空物件)
a <- NA
b <- NULL
c <- sqrt(-3) #NaN(Not a Number)
class(c) #"numeric"
#packages:mice

#4.double(雙倍精度浮點運算)
x <- seq(0,1,0.2)
y <- seq(0,1,0.2)
x[3]
1-x[3]
y[4]
1-x[3] == y[4]
# .Machine:查看目前配備下數值精度相關資訊

#5.向量第一個指標為1而非0(python,c)

#6.跳脫字元\
read.csv("C:\Users\steven\Desktop\test.csv") #在起始 ""C:\U" 的字串中使用了 '\U' 卻無十六進位數字
read.csv("C:\\Users\\steven\\Desktop\\test.csv")

#7.自動重複循環運算
c(1,2,3,4)+2
c(1,2,3,4)+c(2,2,2,2)
c(1,2,3,4)+c(1,2)
#Q? test:c(1,2,3,4)+c(1,2,3)

#8.預設左尾機率
pnorm(1.96) #0.9750021
pchisq(3.84,1) #0.9499565

#9.RSS即SSE

#10.Rstudio下善用tab鍵

#11.對大小寫敏感(SAS)

#12.產生物件前可不需要指定物件類別


###?,??,help(),apropos()
?mean #適用查詢函數
??dplyr #適用查詢套件
help(mean) #適用查詢函數或套件
mean #顯示函數中的演算方式
apropos("norm") #查詢包含該字串的函數
#善用google
#https://rstudio.com/resources/cheatsheets/
#library(Rcmdr) 

###套件(package):library(),require(),install.packages(),devtools::install-github(),update.packages()
library(devtools)
require(dplyr)
install.packages("dplyr")
devtools::install_github("tidyverse/dplyr")
update.packages("dplyr")

#options()
?option
options(encoding = "UTF-8") #資料裡有中文,出現亂碼時請先確認編碼標準
getwd()
setwd("C:\\Users\\steven\\Desktop")
read.csv("test.csv")
rm(list = ls())
#memory.limit():可查詢電腦記憶體上限或限制記憶體最大使用量


###運算子(Operator)
#一般計算
1+1 #和
1-1 #差
2*3 #積
6/2 #商 
7%/%3 #商
7%%3 #餘
2^3 #平方
2**3 #平方
2^(1/2) 
sqrt(2) #根號
sign(-8) #判斷正負號
sign(100)
abs(-9) #絕對值
#log(x,base=exp(1))
log(2.718)
log10(100)
log2(16)
exp(1) #自然數
sin(pi/2) #cos(),tan(),asin(),sinh(),asinh()
factorial(4) #階乘,gamma(5)
prod(c(1,2,3,4))
sum(c(1,2,3,4))
cumsum(c(1,2,3,4)) #cummax(),cummin(),cumprod()
# 矩陣運算之後再講

#邏輯判斷式
# &,| 適用向量,陣列.資料框架的元素或子集合
# &&,|| 適用單一元素
# !,!=,>,<,>=,<=
# %in% : 在...之內


###資料類型(屬性)
#數值(numeric):又分為double.integer
class(2.2)
class(1L)
is.double(2.2)
class(Inf) #-Inf

#文字(character,string)
class("abc")
LETTERS
letters
LETTERS[LETTERS<"T"]
LETTERS[LETTERS<2]
"中">"央" #ASCII編碼
#nchar(),toupper(),tolower(),substr(),substring(),strtrim(),strsplit(),paste(),paste0(),match(),grep(),sub(),gsub()
#packages:stringr(http://blog.fens.me/r-stringr/),stringi
#進階:正規表達式(regular expression)

#邏輯值(logical)
T
F
TRUE
FALSE
2+3==5
T==1
T < 2
F==0
F>1
class(NA)
#na,fail(),na.pass(),na.omit()

#複數(complex)
2+1i
(2+1i)^2
(2+1i)*(2-1i)

#日期(POSIXlt):利用文字轉為日期
(d <- Sys.Date())
class(d)
class("2021-01-13")
(t <- Sys.time())
class(t)
as.Date("2021-01-13")
as.Date("2021/01/13")
as.Date("01-03-2021") #"0001-03-20"
as.Date("01-03-2021","%m-%d-%y")
as.Date("01-03-2021","%m/%d/%y") #NA
#%d,%a,%A,%m,%b,%B,%y,%Y
#packages:lubridate,hms

#因子(factor):
f <- c("台北","台中","彰化")
(f <- as.factor(f))
class(f)
(x <- factor(c("台北","台中","彰化"),levels=c("彰化","台中","台北"),ordered=T)) #ordered:依照levels順序做排序
為甚麼要設LEVEL?
(y <- factor(c("台北","台北","台中","彰化"),levels=c("彰化","台中","台北"),labels=c("A","B","C"))) #labels順序與levels相同
levels(y)
table(y) #列聯表(也可用在字串向量)
(z <- factor(c(1,2,2,3,3,3))) #數字亦可轉為factor
ordered(z)
(z <- ordered(z,levels=c(2,1,3)))
#gl(n,k,length,labels,ordered):n分類數目,k各分類重複次數,levels內含各分類名稱的文字向量,ordered是否為順序factor
#packages:forcats

#raw:二進位資料(幾乎不會用到)


###物件類型
#向量(vector):最常用
v1 <- c("a","b","c")
v2 <- c(1,2,3)
v3 <- c(第一項=1,第二項=2,第三項=3) #定名輸入法
#Q? test:class(c(v1,v2))
v <- c(1,2,3,4,5,6,7,8,9)
seq(1,9,1) #seq(from,to,by)
seq(1,9,0.5)
seq(0,1,length=101) #產生長度為101,min=0,max=1的向量
diff(seq(1,9,0.5)) #計算(後-前)
v <- 1:9
v <- rep(1,5) #rep(vector,times,each)
v <- rep(1:5,time=3)
v <- rep(1:5,each=3)
#Q? test:v <- rep(c(letters[1:2],1:2),time = 2 , each=3)
v <- c("a","b","c","d")
v <- append(v,values = c("N","C","U"),after = 3) #增加元素(values)在指定位置(after)之後
v <- replace(v,list = c(1,3,5),values = c("1","2","3")) #取代在指定位置(list)上的元素(values)
school <- c("N","C","U","S","T","A","T")
names(school) <- letters[1:7]
school
school[c(1,4,5)]
school['c']
number <- 1:9
sum(number)
number>5
number[number>5] #vector[condition]
(number <- replace(number,5,NA))
number>5
number[number>5]
sum(number,na.rm = T) #Q如何解決NA?
sort(number,decreasing = T)
!is.na(number)
round(c(1.5,2.3,4.8),digits = 0) #四捨五入.ceiling(),floor(),trunc()
v <- 1:100
cut(v,breaks = seq(0,100,20),labels = 1:5)
#sum(),max(),min(),range(),median(),var(),sd(),cov(),cor(),sort(),which(condition),which.min(),which.max(),rev(),rank(),match(),pmatch(),all(),any(),prod(),#split()
#R中沒有眾數mode:但可以table()找出

#矩陣(matrix):
m <- matrix(1:9)
m <- matrix(1:12,ncol = 3)
dim(m)
m <- matrix(1:9,ncol = 3,nrow = 3,byrow = T)
m <- matrix(letters[1:9])
x <- 1:4
dim(x)=c(2,2) #可以利用dim()將向量轉為矩陣或陣列
x
x[2,1]
dim(c(1:4))=c(2,2) #賦值目標擴充到非語言的物件
det(x)
t(x) #轉置
solve(x) #反矩陣
diag(3)
diag(1:3)
x %*% x #+,-

r <- list() #建立list包含10個30*30矩陣並做矩陣相加,利用Reduce
for(i in 1:10){
  r[[i]] <- matrix(rnorm(30*30),30,30)
} 
sum(sapply(1:10, function(x){r[[x]][1,1]})) 
Reduce(`+`,r)[1,1] #類似do.call效果

#crossprod(x,x)==t(x) %*% x
#strucchange::solveCrossprod()==solve(t(x) %*% x)
eigen(x) #特徵值,特徵向量
#常用函數:qr(),lower.tri(),upper.tri(),rowSums(),rowMeans(),colSums(),colMeans()

#陣列(array):多維矩陣,很少用
(array(1:9,c(3,3,3)))
ary3 <- array(1:27,c(3,3,3))
ary4 <- array(1:8,c(2,2,2,2))
x <- 1:27
dim(x) <- c(3,3,3)
x

#資料框架(data.frame)
df <- data.frame("name" = c("陳奕儒","林宜興","藺禹筑"),
                 "weight" = c(65,60,55),
                 "height" = c(170,179,155),
                 "birth" = c("1997-01-20","1997-08-30","1996-02-21")) #建議用定名表示法建立資料框架
View(df)
df[2,3]
df$height
df[["height"]]
df[[3]]
df[df$name=="林宜興",]
df[df$height>160,]
is.vector(df$name) #資料框架的每個column為vector,可進行vector的操作
df$BMI <- df$weight/(df$height/100)^2 #在資料框架中建立新變數
View(df)
#ftable()

#Q? test:讀取sale_data.xlsx,並轉換成各季度資料
#Ans
library(xlsx)
sale <- read.xlsx("C:\\Users\\steven\\Desktop\\sale_data.xlsx",1)
library(readxl)
sale <- readxl::read_xlsx("C:\\Users\\steven\\Desktop\\sale_data.xlsx")
sale$Q1 <- sale$month1+sale$month2+sale$month3
sale$Q2 <- sale$month4+sale$month5+sale$month6
sale$Q3 <- sale$month7+sale$month8+sale$month9
sale$Q4 <- sale$month10 + sale$month11 + sale$month12

#edit(),fix(),names(),row.names(),expand.grid(),stack()寬表格轉長表格,unstack(),reshape()
#packages:dplyr,data.table,reshape(melt,cast:長寬表格轉換),tidyr


#串列(list):可以放所有物件
v1 <- c("a","b","c")
m <- matrix(1:12,ncol = 3)
ary <- array(1:18,c(3,3,2))
ls <- list(v1,m,ary,df)
ls[[4]][1]
View(ls)
#unlist(ls)

#時間數列(ts)
#ts(x,start,end,freq)
z = ts(1:10,start = c(1959,2),freq=4) #freq:4(季),12(月),365(天)

#物件常用函數
attributes(df)
mode(v) #適用所有物件
class(df$name) #適用所有物件
names(df) #適用所有物件
length(df) #對df作用效果與ncol相同
ncol(df) #適用矩陣,陣列,資料框架
nrow(df) #適用矩陣,陣列,資料框架
attach(df) #綁定物件
head(df,2) #tail()
str(df)
df$name <- as.vector(df$name) #is.,as.
print() #cat(),sprintf():與C的sprintf相同
#packages:descriptr

###資料合併:c(),union(),rbind(),cbind(),merge()
#c()
comb <- c(df,ary) #list,vector

#union()
df1 <- data.frame("name" = c("陳奕儒","林宜興","藺禹筑"),
                 "weight" = c(65,60,55),
                 "height" = c(170,179,157),
                 "birth" = c("1997-01-20","1997-08-30","1996-02-21"))
View(df1)
df2 <- data.frame(name = "黃雅若",
                  weight = 48,
                  birth = "199?-08-08",
                  height = 161)
View(df2)
df3 <- union(df1,df2,df2)

#rbind()
rbind(df1,df2,df2)

#cbind()
id <- c("108225016","109225016","109225015","109225022")
name <- c("陳奕儒","林宜興","藺禹筑","黃雅若")
weight <- c(65,60,55,48)
height = c(170,179,157,161)
birth = c("1997-01-20","1997-08-30","1996-02-21","199?-08-08")
df3 <- cbind(id,name,weight,height,birth) #matrix,元素為character

#merge:資料庫結構(主鍵,外來鍵)
df4 <- data.frame(id = c("陳奕儒","林宜興","黃雅若","藺禹筑"),
                  number = c("108225016","109225016","109225022","109225015"),
                  gender = c("M","M","F","F"))
merge(df3,df4,by.x = c("id","name") , by.y = c("number","id")) #兩個data.frame鍵值名稱相同時可用by
#Q? test:合併dplyr套件中的 band_instruments,band_members資料集,並列出所有資料
#Ans
merge(dplyr::band_instruments,dplyr::band_members,by="name",all = T)
#packages:dplyr,data.table
#http://blog.fens.me/r-tibble/
#http://blog.fens.me/r-data-table/


###外部資料
#利用base R匯入外部資料
file <- file.choose() #找出檔案路徑
list.files("C:\\Users\\steven\\Desktop\\筆記") #找出資料夾下的所有檔案與子資料夾名稱
readLines(file,n = 1L)
scan(file,what = "character") #不指定路徑則為手動輸入資料(參考readline()),sep:指定分隔符號
a <- scan()
read.table("C:\\Users\\steven\\Desktop\\R_csv.txt",fileEncoding = "UTF-8",header = T,sep = ",") #未指定sep,header.colClasses可指定變數屬性
read.csv(file = "C:\\Users\\steven\\Desktop\\R_csv.txt" ,fileEncoding = "UTF-8",header = T) #sep=","
read.csv2() #sep=";"
read.delim(file = ) #sep="\t"
#load() 讀取R資料(.rda)

#利用packages匯入外部資料
library(readr) #較快速
library(readxl) #讀取excel(.xls,.xlsx)
#packages:xlsx,openxlsx,XLConnect(需要處理Java問題),gdata,foreign(S,SAS,SPSSS,Stata),RODBC,mongolite,jsonlite
#進階:讀取圖片packages:png,jpeg,magick,biOps

#匯出資料
write()
write.csv(a,"C:\\Users\\steven\\Desktop\\R_scan11.csv")
save() #將指定物件儲存為.rda檔
save.image() #將目前工作空間中的所有物件儲存為.rda檔
#Q? test:匯出iris data到你的電腦中
#Ans:
write.csv(iris,"C:\\Users\\steven\\Desktop\\iris.csv")
#路徑盡量不要有中文


###條件語法:搭配return使用
# if(condition){
#   statements
# }else if(condiiton){ #可以有多個else if
#     statements
# }else if(condition){
#     statements
# }else{
#     
#   }
#例1:多個statement
x=1
if(x >= 2){
  y=4
  z=5 #可以不只一個statement
}
y
z
#例2
x=5
y=8
if(x>y){
  z=x-y
}else{
  z=y-x
}
z

#例3:多個condition
x=5
y=6
if(x > 3 & y > 4){
  cat(x,"大於3",y,"大於4") #cat():直接output,類似print()+paste()的效果
}else if(x>3 & y <4){
  cat(x,"大於3",y,"小於4")
}else if(x < 3 & y > 4){
  cat(x,"小於3",y,"大於4")
}else{
  cat(x,"小於3",y,"小於4")
  }

# print(paste0(x,"大於3;",y,"大於4"))

#例4:if-else if-else只能對一個元素使用(無法對vector使用)
x = 1:9
if(x>3){x^2} #條件的長度 > 1，因此只能用其第一元素

#ifelse(condition,yes statement,no statement):只能有單一statement
#例1:ifelse可以對vector使用
x=1:9
ifelse(x>3,x^2,sqrt(x))

#例2:多個condition搭配ifelse使用
x=1:9
ifelse(x<3,x^2,
       ifelse(x<5,x^3,
              ifelse(x<9,x^4,sqrt(x))))
#例3
ifelse(letters %in% c("a","f","p"),paste0("NCU",letters),letters) #!(x %in% y )

#switch():少用
#例1
x=3
switch(x,2+2,mean(1:10),rnorm(5)) #x=3,執行switch函數中x後第三個statement

#例2:可用文字替代
x="身高"
switch(x,姓名="藺禹筑",身高=157,體重=55)


###迴圈:可搭配break(跳出迴圈),next(跳過剩下的statements)使用
#for (variable in vector){statements} vector中元素個數即迴圈次數
#迴圈的vector通常會被設定為序列
#單迴圈
#例1.1
total = 0
for(i in 1:9){
  total = total + i #必須在迴圈前先設定total才能在迴圈內使用
  print(paste0("第",i,"次迴圈"))
  print(total)
}

#例1.2
norm <- rnorm(15)
for(i in 1:length(norm)){
  if(norm[i]>0){print(paste0("第",i,"個抽樣大於0:",norm[i]))} #可將i當作index控制
}

#例1.3:利用物件個數做迴圈
x <- c()
for(i in 1:30){
  x[i] <- i^2
}
x
#x <- (c(1:30))^2

#例1.4:vector可以是文字向量
for(i in c("A","B","AB","O")){
  print(paste0("血型為:",i,"型"))
}

x <- c(4,9,7,21)
for(i in x){
  print(i^2)
}
for(i in 1:length(x)){
  print(x[i]^2)
}

#Q? test:make a scatter plot of 256 successive pairs from x_i = a*x_(i-1) mod m 
#, i.e. (x_0,x_1),(x_1,x_2) , ... , (x_255,x_256),staring with any x_0 , but a=3 , m=256
#Ans:
fun <- function(x_0,a,m){
  x <- c()
  y <- list()
  x[1] <- x_0
  for(i in 2:257){
    x[i] <- (a*x[i-1]) %% m
    y[[i-1]] <- c(x[i-1],x[i])
  }
  y <- as.data.frame(do.call(rbind,y))
  names(y) <- c("x_i","x_(i+1)")
  return(y)
}

dot <- fun(3,3,256)
plot(dot)

dot <- fun(3,7^5,2^31-1)
plot(dot)

#例1.4
#多迴圈
m <- matrix(0,nrow = 3,ncol = 5)
for(i in 1:nrow(m)){
  for(j in 1:ncol(m)){
    m[i,j] <- i+j
  }
}
m

#Q? test:利用雙重迴圈做九九乘法表(paste0)
#Ans
m <- matrix(rep(0,81),ncol = 9)
for(i in 1:9){
  for(j in 1:9){
    # m[i,j] <- i*j
    m[i,j] <- paste(i,"x",j,"=",i*j)
  }
}
m

matrix(rep(1:9)*rep(1:9,each=9),byrow = F,ncol=9)

#while(condition for execute){statements} #符合condition則繼續執行迴圈
#例1
total = 0
i = 1
while(i <= 100){
  total = total + i
  print(paste0("i=",i,",total=",total))
  i = i+1 #若無此statement,則i永遠為1,會不斷重複迴圈(不會停止)
}

#Q? test:編寫小於1000的Fibaonacci序列
#Ans:
fibonacci <- c()
fibonacci[1] <- fibonacci[2] <- 1
i <- 1
while(fibonacci[i]+fibonacci[i+1]<1000){
  fibonacci[i+2] <- fibonacci[i]+fibonacci[i+1]
  i <- i+1
}
fibonacci

#repeat:類似while,較少用 
repeat{
  statements
  if(condition){break}
  statements
  }

#Q? test:利用repeat生成小於1000的fibonacci序列
#Ans:
fibonacci <- c()
fibonacci[1] <- fibonacci[2] <- 1
i <- 1
repeat{
  fibonacci[i+2] <- fibonacci[i]+fibonacci[i+1]
  i <- i+1
  if(fibonacci[i]+fibonacci[i+1]>=1000){break}
}
fibonacci

#packages:foreach,doParallel

#Q? test:利用iris data
# 若Species為 setosa 則 Sepal.Width+0.2
# 若Species為 versicolor 則 Petal.Width+0.3
# 其他狀況則 Sepal.Length+0.4 , Petal.Length+0.4
# 計算Sepal.Length,Sepal.Width,Petal.Length,Petal.Width的平均值
#Ans:
df <- iris
for (i in 1:nrow(df)) {
  if(df$Species[i]=="setosa"){
    df$Sepal.Width[i] <-df$Sepal.Width[i]+0.2
  }else if(df$Species[i]=="versicolor"){
    df$Petal.Width[i] <- df$Petal.Width[i]+0.3
  }else{
    df$Sepal.Length[i] <- df$Sepal.Length[i]+0.4
    df$Petal.Length[i] <- df$Petal.Length[i]+0.4
  }
}

summary(df) #5.977,3.124,3.891,1.299
summary(iris) #5.843,3.057,3.758,1.199

#Q? test:The dice game craps is played as follows. The player throws two dice, and if
        # the sum is seven or eleven, then he wins. If the sum is two, three, or twelve,
        # then he loses. If the sum is anything else, then he continues throwing until
        # he either throws that number again (in which case he wins) or he throws a
        # seven (in which case he loses).

#Ans
craps <- function() {
  #returns TRUE if you win, FALSE otherwise
  initial.roll <- sum(sample(1:6,2,replace=T))
  if (initial.roll == 7 || initial.roll == 11) return(TRUE)
  while (TRUE) {
    current.roll <- sum(sample(1:6,2,replace=T))
    if (current.roll == 7 || current.roll == 11) {
      return(FALSE)
    } else if (current.roll == initial.roll) {
      return(TRUE)
    }
  }
}
mean(replicate(10000, craps()))


###function
# function(parameters){
#   statements
#   return()}
#例1:

BMI_function <- function(height,weight){
  BMI = weight/(height/100)^2
  return(BMI) #也可以只寫BMI,但沒有這行則不會有output
}
BMI <- BMI_function(157,55)
BMI_function(c(157,170),c(55,60))

#例2:如果要回傳多個值?
BMI_function <- function(height,weight){
  BMI = weight/(height/100)^2
  return(list(height,weight,BMI))
}
BMI_function(157,55)
#Q? test:BMI_function(157,55) #不允許多引數回傳 如何解決?

#例3:具有預設值的函數
BMI_function <- function(height=157,weight=55){
  BMI = weight/(height/100)^2
  return(BMI)
}
BMI_function()
BMI_function(height = 170)

#例4:function具有遞迴性質
f1 <- function(x){
  if(x>0){
    y=x-1
    return(x*f1(y))
  }else{
    return(1)
  }
}
f1(4) #4*3*2*1

#eval(parse(text="")):將字串轉為指令執行
sample_fun <- function(pdf,n,...){
  y <- eval(parse(text = paste0("r",pdf,"(",n,")")))
  return(y)
}
sample_fun("norm",100) #從標準常態分配抽100個樣本
sample_fun("unif",100) #從U(0,1)分配抽100個樣本
#packages:lazyeval

#args():顯示函數的參數與預設值,body():顯示函數內容,formals(),invisible()
#進階技巧:
    #全域變數(可用 <<- 做指定)
    #...:將function裡面所用到的其他function需要的參數傳入
        # f1 <- function(x,...){
        #   y = mean(...)+x
        #   return(y)
        # }
        # z = rnorm(100)
        # f1(5,z)
    #參數可以是其他函數的名稱
    #自定義運算子"%anything%"
        # "%!%" <- function(a,b){return(a + b + 10)}
        # 5%!%10

#計算過程是否用到遞迴?------------否:apply
#                     |
#                     ------------是--------是否知道迴圈次數?---------------是:for
#                                                            |
#                                                            ---------------否:while,repeat

###apply向量運算:不需要做遞迴公式時使用
#向量化運算:
f <- function(x){
  sqrt(x)*10
}
x
x <- sample(1:100,12,T)
sqrt(x)*10
f(x=x) #並非所有函數都可以對向量物件使用
for(i in 1:10){
  print(sqrt(x[i])*10)
}
#與for迴圈比較效率:比較執行10000次抽5000個樣本計算平均數與標準差的時間
#如何知道程式執行時間?system.time(),Sys.time(),proc.time()
#方法1:使用迴圈Time difference of 7.908095 secs
n=10000
start <- Sys.time()
a <- c()
b <- c()

  for(i in 1:n){
    a[i] <- mean(rnorm(5000,mean = i))
    b[i] <- var(rnorm(5000,mean = i))}
  df <- data.frame(i = 1:n,a,b)
  
end <- Sys.time()
end-start
#方法2:使用apply執行Time difference of 7.59842 secs
start <- Sys.time()
ls <- lapply(c(1:n),FUN = function(x){
  c(
    a <- mean(rnorm(5000,mean = x)),
    b <- var(rnorm(5000,mean = x))
  )
})
df <- as.data.frame(do.call(rbind,ls))
end <- Sys.time()
end-start

#apply(x,MARGIN,FUN,...):x為矩陣或陣列,MARGIN=1依row執行=2依column執行,FUN為作用的函數名稱
x=matrix(x,4,3)
x
f(x=x)
apply(x,1,function(x){mean(sqrt(x)*10)})
apply(x,2,function(x){mean(sqrt(x)*10)})

#lapply(list, function),sapply(list, function):x為向量或list
#例1
x <- list(a = 1:10,beta = exp(-3:3),logic = c(T,F,F,T))
x
lapply(x,mean) #回傳list
sapply(x,mean) #回傳向量或矩陣(特殊情況回傳list)
sapply(1:5,seq) #sapply回傳list情況:長度不同
y <- data.frame(cbind(x1=3, x2=c(2:1,4:5)))
sapply(y, cumsum)
#例2:make a scatter plot of 256 successive pairs from x_i = a*x_(i-1) mod m,
#i.e. (x_0,x_1),(x_1,x_2) , ... , (x_255,x_256),staring with any x_0 , but a=3 , m=256
fun <- function(x_0,a,m){
  x <- c()
  y <- list()
  x[1] <- x_0
  for(i in 2:257){
    x[i] <- (a*x[i-1]) %% m
    y[[i-1]] <- c(x[i-1],x[i])
  }
  y <- as.data.frame(do.call(rbind,y))
  names(y) <- c("x_i","x_(i+1)")
  return(y)
}

dot <- fun(3,3,256);plot(dot)
dot <- fun(3,7^5,2^31-1);plot(dot)

parameter <- list(c(3,3,256)
                  ,c(3,7^5,2^31-1))
ls <- lapply(c(1,2), FUN = function(x){fun(parameter[[x]][1],
                                           parameter[[x]][2],
                                           parameter[[x]][3])})
plot(ls[[1]])
plot(ls[[2]])
#lapply常搭配do.call(rbind,.),do.call(cbind,.)使用

#vapply(list, function, FUN.VALUE , ...)類似sapply,但可用FUN.VALUE控制output的名稱
x <- data.frame(cbind(x1=3, x2=c(2:1,4:5)))
vapply(x,cumsum,FUN.VALUE=c('a'=0,'b'=0,'c'=0,'d'=0))
sapply(x,cumsum) #可用row.names指定名稱

#tapply(vector, index, function):利用index將vector分類後做運算
# presidents #1945Q1-1974Q4 美國總統支持度ts資料
tapply(presidents,cycle(presidents),mean,na.rm=TRUE)
#例1
x<-y<-1:10;x;y
t<-round(runif(10,1,100)%%2);t
tapply(x,t,sum)
tapply(x,t,sum,y) #錯誤用法
tapply(x,t,sum)+tapply(y,t,sum)

#mapply(function, ...):多變量形sapply,...表示多個數據
#例1
x <- 1:10
y <- 5:-4
z <- round(runif(10,-5,5))
x;y;z
mapply(max,x,y,z)
#例2
set.seed(1)
n<-rep(4,4)
m<-v<-c(1,10,100,1000)
mapply(rnorm,n,m,v)
set.seed(1)
rnorm(4,1,1)
rnorm(4,10,10)

#rapply(list,f,classes = "ANY",deflt = NULL,how = c("unlist","replace","list"))
#遞迴版本的lapply,只處理list
#例1:排序
x=list(a=12,b=4:1,c=c('b','a'))
y=pi
z=data.frame(a=rnorm(10),b=10:1)
a <- list(x=x,y=y,z=z)
a
rapply(a , sort,how='replace')
#例2:文字類型加入"++++",非文字設為NA
rapply(a,function(x) paste(x,'++++'),classes="character",deflt=NA, how = "list")

#參考:http://blog.fens.me/r-apply/
#http://blog.fens.me/wp-content/uploads/2016/04/apply.png

#進階:CPU平行化運算package:doParallel,parallel,foreach,pbapply,snow
# library(parallel)
# library(snow)
# cpu.cores <- detectCores() #取得cpu核心數
# cl = makeCluster(cpu.cores,type='SOCK') #開啟指定核心數
# #clusterExport(cl,c("rnorm","mean","var")) #若平行運算中須使用非base R的物件或函數須先進行指定
# start <- Sys.time()
# ls_snow <- parLapply(cl,c(1:10000),fun = function(x){
#   c(
#     a <- mean(rnorm(5000,mean = x)),
#     b <- var(rnorm(5000,mean = x))
#   )
# })
# end <- Sys.time()
# stopCluster(cl)
# df_snow <- do.call(rbind,ls_snow)
# end-start

#平行化迴圈
#foreach+doParallel:
library(foreach)
library(doParallel)
foreach(..,.combine) #.combine:output格式,預設為list

#%do%
#基本用法同for loop
x <- foreach ( i = 1:3) %do% sqrt(i)
#可多個variable同時來迭代
x <- foreach(a = 1:3, b=rep(10, 3)) %do% ( a + b )
#可以使用大括號, 裡頭可以包更複雜的運算
x <- foreach(a= 1:3, b= rep(10 ,3)) %do% { a + b }

#使用.combine 選項，可以將運算的複雜度提高
x <- foreach(i = 1:3, .combine='c') %do% exp(i)
x <- foreach( i = 1:3, .combine='cbind') %do% rnorm(4) #cfun為使用自訂函數
x <- foreach( i = 1:3, .combine='cfun', .multicombine=TRUE) %do% rnorm(4)

#%dopar%:需在程式頭尾加入開啟與關閉平行化的核心,並將%do%用法改為%dopar%,若要加入其他套件使用.packages參數
cpu.cores <- detectCores() #取得cpu最大核心數
cl = makeCluster(cpu.cores) # 開啟平行核心數
doParallel::registerDoParallel(cl) #建立平行運算連結
x <- foreach(i = 1:3, .combine='c') %dopar% exp(i)
stopCluster(cl) #關閉平行核心數

#平行化apply:snow,pbapply
library(pbapply)
library(parallel)
#pb*apply(cl):語法和apply基本相同,可加上cl指定平行化運算
cpu.cores <- detectCores() #取得cpu最大核心數
cl = makeCluster(cpu.cores,type='SOCK') #開啟指定核心數
x=matrix(x,4,3)
x
clusterExport(cl, c("x")) #建立平行運算連結
pbapply(x,1,function(x){mean(sqrt(x)*10)},cl = cl)
pbapply(x,2,function(x){mean(sqrt(x)*10)},cl = cl)
stopCluster(cl) #關閉平行核心數

#google:R 平行化 平行運算 parallel
#GPU加速packages:gputools,cudaBayesreg,HiPLARM,gmatrix,OpenCL,gpuR
#在R上寫C++語法package:Rcpp

###模擬simulation:
#set.seed():使隨機化函數可以產生固定的結果,set.seed(NULL)
rnorm(10)
rnorm(10) #沒有設定seed,則抽樣結果不同
set.seed(109225004)
rnorm(10)
set.seed(109225004)
rnorm(10) #設定seed下抽樣結果會相同
set.seed(109225004)
rnorm(1) #與rnorm(10)第一個抽樣結果相同
rnorm(1) #與rnorm(10)第二個抽樣結果相同

#sample(x,size,replace=F):replace抽取後是否放回
x <- 1:10
sample(x,5,replace = F)
x <- list(a = 1:10,beta = exp(-3:3),logic = c(T,F,F,T))
sample(x,6,replace = T)

#機率分配函數
rnorm(n=5,mean = 5,sd=3) #p表示隨機模擬或隨機亂數生成函式,從N(5,3^2)隨機抽5個樣本
qnorm(p = 0.95) #q表示分位數,符合 u ≤ P(X <= x) 的最小 x
pnorm(q = 1.96) #p表示累積機率分配函數CDF
dnorm(x = 0) #d表示機率密度函數pdf
#預設lower.tail=F表示左尾計算,=T則右尾計算
#其他機率分配:https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Distributions.html
#geometric distribution的x為失敗次數(非實驗次數)
#Q? test:建立一個函數dcunif(n,i,j)可隨機抽取n個discreteU(i,j)
















#Ans
dcunif <- function(n,i,j){
  i+floor((j-i+1)*runif(n))
}
dcunif(5,11,20)

f <- function(n,i,j){sample(i:j,n,T)}
f(5,11,20)

#Inverse Transform Method
#Q? test:generate 10000 Poisson(10.5)
#Ans:
library(arm)
seed <- 0309
set.seed(seed)
n <- 10000
x <- rpois(n,10.5)
mean(x) #10.4646
var(x) #10.29778
discrete.hist(x,freq = F,prob.col="black")

poi <- function(x,lambda){ #建立poisson分配的累積機率函數
  if(is.infinite(x)==T){
    y=1
  }else if(x<0){
    y=0
  }else{
    y=sum(lambda^(seq(0,x))/factorial(c(0:x)))*exp(-lambda)
  }
  return(y)
}
ppois(0,lambda = 10.5)
poi(0,10.5)

#1
set.seed(seed)
y <- c()
u <- runif(n)
for(i in 1:n){
  I=0
  if(u[i]<poi(I,10.5)){
    y[i] <- I
  }else{
    while(poi(I,10.5) <= u[i]){
      y[i] <- I+1
      I <- I+1
    }
  }
}
mean(y) #10.4904
var(y) #10.19633
discrete.hist(y,freq = F,prob.col="black")

#2
set.seed(seed)
y <- c()
u <- runif(n)
for(i in 1:n){
  I=-1
  while(poi(I,10.5) <= u[i]){
    y[i] <- I+1
    I <- I+1
  }
}
mean(y) #10.4904
var(y) #10.19633
discrete.hist(y,freq = F,prob.col="black")

#Q? test:generate 100 Bin(10,0.2)
#Ans:
library(arm)
seed <- 0309
set.seed(seed)
n <- 100
y <- rbinom(100,10,0.2)
mean(y) #2.15
var(y) #1.643939
discrete.hist(y,freq = F)

set.seed(seed)
y <- c()
u <- runif(n)
for(i in 1:n){
  I=-1
  while(pbinom(I,10,0.2) <= u[i]){
    y[i] <- I+1
    I <- I+1
  }
}
mean(y) #1.89
var(y) #1.917071
discrete.hist(y,freq = F,prob.col="black")

#Q? test:generate 100 Geo(0.2)
#Ans:
seed <- 108225023
set.seed(seed)
n <- 10000
x <- rgeom(n,0.2)
mean(x) #4.0113
var(x) #19.70134
discrete.hist(x,freq = F,prob.col="black")

set.seed(seed)
y <- c()
u <- runif(n)
for(i in 1:n){
  I=-1
  while(pgeom(I,0.2) <= u[i]){
    y[i] <- I+1
    I <- I+1
  }
}
mean(y) #3.9917
var(y) #19.49658
discrete.hist(y,freq = F,prob.col="black")

#Accept-Rejection method
#Q? test:generate 1000 r.v. whose pdf is beta(2,2)

f <- function(x){dbeta(x,2,2)} #要抽樣的目標函數
g <- function(x){dunif(x)} #已知的分配,g的定義域要包含f的定義域
c <- optimize(function(x){f(x)/g(x)},interval = c(0,1),maximum = T)$object
curve(f,0,1)
curve(f(x)/c*g(x),0,1,add = T,col="green")
x <- c()
TF <- c()
for(i in 1:20){
  u <- runif(1)
  y <- runif(1)
  if(u < f(y)/(c*g(y))){
    x[i] <- y
    TF[i] <- T
    text(y,u,labels = i,col = "red")
  }else{
    x[i] <- y
    TF[i] <- F
    text(y,u,labels = i,col = "black")
  }
  Sys.sleep(1)
}





#Ans:
x <- c()
TF <- c()
while(sum(TF)<10000){
  u <- runif(1)
  y <- runif(1)
  if(u < f(y)/(c*g(y))){
    x <- c(x,y)
    TF <- c(TF,T)
  }else{
    x <- c(x,y)
    TF <- c(TF,F)
  }
}
hist(x[TF],freq = F)
curve(f,0,1,add=T)

#計算mle或極值:
#stats4::mle(minuslogl,start,method):求MLE
        #minuslogl:-(log-likelihood)的函數
        #start:儲存參數起始值的list變數
        #method:預設為"BFGS".其他有"L-BFGS-B","CG","Nelder-Mead","SANN"

#optimize(f,interval,maximum),optimise(),uniroot:求一維函數極值
        #f:函數名稱
        #interval=c(from,to):x下界,x上界
        #maximum=T:計算最大值或最小值

#polyiroot():多項式求根(含複數根)

#optim(par,fn,gr,method,lower=-Inf,upper=Inf,control,hessian)
        #par:變數起始值
        #fn:函數名稱
        #gr:梯度函數
        #lower,upper:使用"L-BFGS-B"時可設定上下界搜尋範圍
        #method:BFGS","L-BFGS-B","CG","Nelder-Mead","SANN"
        #control=list(fnscale=-1):改為求最大值,預設為求最小值

#constrOptim():線性不等式限制區域求最小值

#packages:sampling,ConvergenceConcepts(檢驗隨機變數收斂狀況),fitdistrplus(檢驗資料在特定分配下的參數估計值,log-likelihood)


###baseR繪圖
#library(help = "graphics")
#?plot.default
#plot(x),plot(x,y),plot(xy):若x.y皆為vector,或xy為一具有兩個column的矩陣或資料框架,則畫出X-Y散布圖
plot(cars)

#plot(x):若x為時間序列變數,則畫出時間數列圖.若x為數值向量則以各元素值為y軸,出現順序為x軸
z = ts(sample(1:10),start = c(1959,2),freq=4)
plot(z)

#plot(f):若f為factor變數,畫出f的長條圖
f <- as.factor(sample(1:6,1000,T)) #模擬骰子
plot(f)

#plot(f,v):若f為factor變數,y為相同長度的數字變數,畫出以f各分類為x軸,v值為y軸的box-plot
df <- cars
df$speed <- as.factor(df$speed)
plot(df)

#plot(X),pairs(X):X為多於兩個column的資料框架或矩陣,畫出所有行向量配對的X-Y散布圖
#plot(~x1+x2+...+xK):畫出x1,..,xk配對而成的x-y散布圖矩陣
#plot(y~x1+...+xk):逐一畫出y對每個x的X-Y散布圖
#coplot(x~y|z):畫出z分類下的X-Y散布圖
plot(iris)
plot(~iris$Sepal.Length+iris$Sepal.Width+iris$Petal.Width)
coplot(iris$Sepal.Length~iris$Sepal.Width|iris$Species,rows = 1)

#curve(expr,from,to,n=101,add=F):畫出函數曲線圖.expr為函數,from為起始點,to為終點,n為預設產生的點數量,add為是否加在前一張圖中
curve(dnorm(x),-5,5)
curve(dnorm(x,mean = 3),col=2,lty=5,add = T)

#qqnorm(x),qqplot(x,y),qqline(x):常態機率圖,常態機率線
x <- rnorm(1000)
qqnorm(x)
qqline(x)

#hist(x,breaks,nclass):x為數值vector,畫出直方圖.breaks為分隔點,nclass為分隔數
hist(rnorm(10000),freq = F) #freq=F:y軸改為各群的機率
curve(dnorm(x),-5,5,add=T)

#barplot(height,horiz,names.arg,col,density):長條圖
#height為數值向量.矩陣.list,或table()產生的結果
#horiz:是否畫為水平長條圖
#names.arg:文字向量,設定每個長條圖的說明文字
#col:顏色代號向量,設定每個長條的顏色
#density:設定每個bar的斜線密度
barplot(table(sample(1:6,100,T)),horiz=T,col = c(1:6),density = c(1:6))

#boxplot(x,horizontal,names,col):盒鬚圖
#x可為向量.矩陣.資料框架
#horizontal:是否畫出水平盒鬚圖
#names:文字向量,設定各box的說明文字
#col:顏色向量
boxplot(iris[,1:4],names = c("SL","SW","PL","PW"),col = c("red",3,"#7B7B00","#FFFF00"))

#pie(x,label):圓餅圖
#x為數值向量
#label為各分類敘述文字向量
pie <- c(10,20,30,40)
names <- paste0("Q",seq(1:4),":",pie)
pie(pie,labels=names)

#3D圖:x,y,z為三個數值向量
#image(x,y,z)
#contour(x,y,z)
#persp(x,y,z,theta,phi,box=TURE):z為矩陣dim(z)==c(x,y)
#packages:misc3d

#基本設定參數:可用在par()或其他繪圖函數中例如:plot()
        #col:顏色.可用color(),colors(),rainbow(5)得到顏色名稱
        #lty:線的種類 http://www.sthda.com/english/wiki/line-types-in-r-lty
        #pch:用指定的符號取代圓點
        #font:線寬
        #cex:字型大小.(cex.axis,cex.lab,cex.main,cex.sub)

#輔助參數:加在繪圖函數中,但部分參數可能不適用
        #add:強制目前的圖形覆蓋在前一張圖形上
        #axes:是否畫出座標軸
        #log:log="x",log="y",log="xy",畫圖前先將x或y或兩者取log
        #type:圖點長相.
              #"p":實心圓
              #"l":實線
              #"b":實心圓和實線
              #"o":空心圓和實線
              #"s":階梯狀,頂端
              #"S":階梯狀,底端
              #"h":垂直線
              #"n":只畫座標軸
        #xlab,ylab:x軸,y軸輔助說明文字
        #xlim=c(from,to),ylim(from,to):x,y軸的最大最小值
        #xaxt="n",yaxt="n":不畫出座標格線
        #main,sub:主標題,次標題

#附加圖形:輔助高階繪圖函數,在已經畫好的高階圖形中,加入各種圖.點.線或說明文字
        #points(x,y):在指定位置(x,y)上畫點
        #lines(x,y):在指定位置(x,y)上畫線
                  #segments()
                  #arrows()
        #text(x,y,labels):在指定位置(x,y)上寫出指定文字
        #abline(a,b):加入y=a+bx的直線
                  #abline(h=y):加入水平線
                  #abline(v=x):加入垂直線
                  #abline(lm(y~x)):加入迴歸線
        #polygon(x,y,density,angle,col):畫出封閉多邊形
        #legend(x,y,legend,pch,lty,col):在指定位置加入說明方塊 https://blog.csdn.net/glodon_mr_chen/article/details/79496403
        #title(main,sub):加入主標題與次標題
        #axis(side,at,labels,pos,line):加入軸線

#dev.new(height,width):彈出繪圖視窗

#matplot(x,y)在同一張圖上畫出多條線
library(faraway)
aatemp.lm <- lm(temp~year,aatemp)
aatemp.gls <- gls(temp~year,correlation = corAR1(form=~year),method="ML",data=aatemp)
aatemp.polylm <- lm(temp~poly(year,5),data=aatemp)
br <- function(x){ifelse(x<1930,0,x-1930)};bl <- function(x){ifelse(x<1930,1930-x,0)}
aatemp.seglm <- lm(temp~bl(year)+br(year),data=aatemp)
bx <- bs(aatemp$year,knots=c(1900,1950),intercept=T)
aatemp.bs <- lm(temp~bx-1,aatemp)
matplot(aatemp$year,cbind(aatemp$temp,aatemp.lm$fit,aatemp.gls$fit,aatemp.polylm$fit,aatemp.seglm$fit,aatemp.bs$fit),type="plllll",ylab = "temp",pch = 16,col = c(1:6))
legend("topleft",legend = c("aatemp.lm","aatemp.gls","aatemp.polylm","aatemp.seglm","aatemp.bs"),col=c(2:6), bty="n", xjust=1, lwd=2, cex=0.85)
#x,y可以都是矩陣

#圖形設定函數:
#par(mai,mfcol,mfrow,fig):
        #mai=c(bottom,left,top,right):與四個邊界的距離
        #mfcol=c(3,2):將下3*2張圖依照column放入.
        #mfrow=c(3,2):將下3*2張圖依照row放入.
        #fig=c(x1,x2,y1,y2):將下一張圖的左下角放在(x1,y1),右下角放在(x2,y2).設定不對稱的合併圖表位置
#layout(M,widths,heights,respect=F):設定之後出現的圖為不對稱的合併圖表位置
        #M:設定圖形分布狀況的矩陣變數
        #heights,weights:設定M矩陣所有區域各列與各行長度比例與寬度比例,其向量長度需與M維度相同.設定的基準圓點為左下角
        #respect:x.y軸所用長度是否一致,預設為否
layout(matrix(c(2,0,1,3),2,2,byrow=T),widths=c(3,1),heights=c(3,1))
# |==============================|==========|
# |                              |          |
# |             2                |          |
# |                              |          |
# |                              |          |
# |==============================|==========|
# |                              |          |
# |                              |          |
# |                              |          |
# |                              |          |
# |                              |          |
# |                              |          |
# |              1               |    3     |
# |                              |          |
# |                              |          |
# |                              |          |
# |                              |          |
# |                              |          |
# |==============================|==========|
layout(matrix(c(1,1,2,3),2,2,byrow=T))
# |========================================|
# |                                        |
# |                                        |
# |                                        |
# |                   1                    |
# |                                        |
# |                                        |
# |                                        |
# |                                        |
# |===================|====================|
# |                   |                    |
# |                   |                    |
# |                   |                    |
# |         2         |         3          |
# |                   |                    |
# |                   |                    |
# |                   |                    |
# |                   |                    |
# =========================================|
#例:
attach(iris)
x.hist <- hist(Sepal.Length,breaks = 10,plot = F)
y.hist <- hist(Sepal.Width,breaks = 10,plot = F)
top <- max(c(x.hist$counts,y.hist$counts))
layout(matrix(c(2,0,1,3),2,2,byrow=T),widths=c(3,1),heights=c(1,1))
plot(Sepal.Length,Sepal.Width,main = "X-Y散布圖")
barplot(x.hist$counts,axes = F,ylim = c(0,top),space = 0,main="x軸長條圖") #space為bar的間距
barplot(y.hist$counts,axes = F,xlim = c(0,top),space = 0,horiz = T,main = "y軸長條圖")

#互動式圖形函數
locator() #傳回位置的(x,y)座標
identify() #查詢圖點在資料中的辨識資料

#使用數學符號
?plotmath
expression(plotmath表達式)
quote()
# https://astrostatistics.psu.edu/su07/R/html/grDevices/html/plotmath.html

#匯出圖片
#??grDevices
#jpeg(),png(),bmp(),tiff(),postscript(),pdf()
png("C:\\Users\\steven\\Desktop\\test1.png")
dev.off() #將device關閉


#packages:ggplot2,epiDisplay,xtable(產生對齊的表格),leaflet(畫地圖https://blog.gtwang.org/r/r-leaflet-interactive-map-package-tutorial/)
library(leaflet)
x=121.19248
y=24.97088
map <- leaflet() %>%
  addTiles() %>%  # 加上預設的地圖資料
  addMarkers(lng=x, lat=y, popup=paste0("中央大學鴻經館:(",x,",",y,")"))
map  # 繪製地圖


###crawler
#packages for crawler:httr,rvest,rjson,jsonlite,downloader,RSelenium
#packages for data parser:dplyr,stringr,lubridate
#https://github.com/datasci-info/crawler101-yotta/tree/master/R
#https://rpubs.com/jack60810/TKURCrawler?fbclid=IwAR1m3BmH8oUORgeEBZCrmbJXqUvAksBNd55MNEyR-3BASOq4tD9Xf7fVksw
library(httr)
library(rjson)
library(magrittr)
library(rvest)
library(jsonlite)
#http method(GET,POST):https://developer.mozilla.org/zh-TW/docs/Web/HTTP/Methods
#網頁架構
#SelectorGadget


#json API(json Viewer)
url <- "https://ecshweb.pchome.com.tw/search/v3.3/all/results?q=sony&page=1&sort=rnk/dc"
pchome <- jsonlite::fromJSON(url) %>% .$prods %>% do.call(cbind,.) %>% as.data.frame()
GET(url) %>% content() %>% .$prods %>% do.call(rbind,.) %>% as.data.frame() %>% View()

#cookies
url <- "https://www.ptt.cc/bbs/Gossiping/index.html"
GET(url) %>% content(as = "text")
GET(url,set_cookies(over18=1)) %>% content(as = "text")

#useragent:使用者身分
url <- "http://buy.yungching.com.tw/region/台北市-_c/pricereduction_filter/"
GET(url) %>% content(as = "text")
GET(url,add_headers("User-Agent"="Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.76 Safari/537.36")) %>% content(as = "text")

#referer:使用者從哪裡連到網站?
url <- "http://httpbin.org/get"
res = url %>% GET %>% content(as = "text")
res = url %>% GET(add_headers("Referer"="http://httpbin.org/get")) %>% content(as = "text")

#URLencoding:網址上的中文編碼
"URL?v1=今天&v2=明天" %>% URLencode
"URL?v1=今天&v2=明天" %>% URLencode(reserved = T)

#POST(url,body)
#https://emap.pcsc.com.tw/
url <- "https://emap.pcsc.com.tw/EMapSDK.aspx" #利用F12找到資料
POST(url)
postData  <- list("commandid" = "SearchStore",
                  "city"="台北市",
                  "town"="松山區",
                  "roadname"="八德路二段",
                  "ID"="",
                  "StoreName"="",
                  "SpecialStore_Kind"="")
POST(url,body = postData,encode = "form") %>% content()

#CSS

#X-PATH

#https://rpubs.com/
#http://www.cookbook-r.com/
#https://rc2e.com/


##data.table
library(data.table)
#https://cran.r-project.org/web/packages/data.table/data.table.pdf
#?"special-symbols"
dt <- data.table(iris) #"Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species"
class(dt)

#設置索引(鍵值):與join有關 setkey(dt,colname1,colname2,...),setkey(dt,NULL)
setkey(dt,Species) # setosa , versicolor , virginica 
dt[list("setosa")] #dt[dt$Species=="setosa"]
dt["setosa",mult="first"] #dt[dt$Species=="setosa"][1,]
dt["setosa",mult="last"] #dt[dt$Species=="setosa"][nrow(dt[dt$Species=="setosa"]),]
key(dt) #查詢key haskey(dt)

#選擇欄
dt$Species
dt[,5]
dt[,.(Species)]
dt[,"Species"]
dt[c("setosa","versicolor")] #dt[dt$Species %in% c("setosa","versicolor"),]

#新增欄 data.table[,colname:=var1]
dt[,SP:=Sepal.Length+Sepal.Width]
length(dt) #6

#刪除欄 data.table[,colname:=NULL]
dt[,SP:=NULL] #dt[,c(col1,col2):=NULL] 刪除多欄
dt[1:=NULL,] #無法做列刪除
length(dt) #5

#選擇列
dt["setosa",on="Species"] #dt[dt$Species=="setosa",]
dt["setosa",on=.(Species)]
dt[Species=="setosa"]
dt[.(c("setosa","versicolor"),1.5),on=c("Species","Petal.Length")]

#改變欄順序 setcolorder(dt,c("colname1","colname3","colname2",...))
setcolorder(dt,c("Sepal.Width","Species","Sepal.Length","Petal.Length","Petal.Width"))
dt

#條件語法
dt[Species=="setosa",Sepal.Length := Sepal.Length + 0.2] #ifelse(dt$Species=="setosa",dt$Sepal.Length = dt$Sepal.Length +0.2,Sepal.Length)
dt[Species=="versicolor",Sepal.Width := Sepal.Width + 0.3]
dt[Species=="virginica",c(Petal.Length := Petal.Length + 0.4,Petal.Width := Petal.Width + 0.4)] #與ifelse相同,無法同時做兩件事

#分組計算 by
dt[,mean(Sepal.Length),by=Species] #以Species分組做Sepal.Length的平均

#JOIN: dt1[dt2],依照setkey的順序做join
id <- c("108225016","109225016","109225015","109225022")
name <- c("陳奕儒","林宜興","藺禹筑","黃雅若")
weight <- c(65,60,55,48)
height = c(170,179,157,161)
birth = c("1997-01-20","1997-08-30","1996-02-21","199?-08-08")
df3 <- as.data.table(id <- c("108225016","109225016","109225015","109225022"),
                     name,
                     weight,
                     height,
                     birth)
df4 <- data.table(id = c("陳奕儒","林宜興","黃雅若","藺禹筑"),
                  number = c("108225016","109225016","109225022","109225015"),
                  gender = c("M","M","F","F"))
merge(df3,df4,by.x = c("id","name") , by.y = c("number","id"))

setkey(df3,id,name)
setkey(df4,number,id)
df3[df4]

df3[df4,on=c("id","name")]

#分群計算
dt[,list(avg_SL = mean(Sepal.Length),avg_PW = mean(Petal.Width)),by=Species] #只能用=,不能用 <-

#排序:setorder(data.table,colname) 會直接改變資料的排列
setorder(dt,Petal.Width) #-colname為倒序
dt


