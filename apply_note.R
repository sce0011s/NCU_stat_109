#計算過程是否用到遞迴?------------否:apply
#                     |
#                     ------------是--------是否知道迴圈次數?---------------是:for
#                                                            |
#                                                            ---------------否:while,repeat

#HW1
z<-c()
for(i in 1:6){
    y<-runif(10^(i),0,1)
    x<-1/(5+(10-5)*y)
  x
  z[i]<-mean(x)*(10-5)
}
z
w<-c(10,100,1000,10000,100000,1000000)
# z <- sapply(w, FUN = function(x){(mean(1/runif(x,5,10)))*(10-5)})
plot(log10(w),z,xlab="log10(n)",ylab="theta",type="b")
abline(h=log(10)-log(5),lty=2)

#HW2
#I(a)
set.seed(123)
x<-c()
y<-c()
theta<-c()
p<-c()
for(i in 1:6){
  x<-runif(10^i,0,1)
  y<-runif(10^i,0,1)
  z<-c()
  z<-x^2+y^2<=1
  theta[i]<-mean(z)
  p[i]<-4*theta[i]
}
theta
p
w<-c(10,100,1000,10000,100000,1000000)
plot(log10(w),p,xlab="log10(n)",ylab="pi",type="b")
abline(h=pi,lty=2)
var<-c()
for(i in 1:6){
  var[i]<-(theta[i])*(1-theta[i])/((10^i))
}
var
# library(magrittr)
# set.seed(109225004)
# f <- function(n){
#   x <- runif(n)
#   y <- runif(n)
#   z <- ifelse(x^2+y^2<=1,1,0)
#   return(c("n" = n,
#            "pi_hat" = 4*mean(z),
#            "var" = (mean(z)*(1-mean(z)))/n))}
# data <- sapply(10^(1:6), f) %>% t()


#13
v<-c()
for(i in 1:1000000){
  product<-1
  n<-0
  while(product>exp(-3)){
    product<-product*runif(1,0,1)
    n<-n+1
  }
  v[i]<-n-1
}
# f <- function(){
#   u <- 1
#   i = 0
#   repeat{
#     if(u>=exp(-3)){
#       u <- u * runif(1)
#       i = i+1
#     }else{
#       break
#     }
#   }
#   return(i-1)
# }
# count <- replicate(10^6,f())
e<-mean(v)
e
x<-table(v)
y<-c()
for(j in 1:7){
  y[j]<-x[j]/1000000
  print(paste0("P(N=",j-1,")=",y[j]))
}
# x/10^6


#HW3
#1(ii) Bin(20,0.2)
set.seed(1234)
x<-rbinom(1000,20,0.2)
discrete.histogram(x,freq = F,main="Histogram of X")
mean(x);var(x)

set.seed(1234)
y<-c()
for(i in 1:1000){
  z<-c()
  for(j in 1:20){
    a<-runif(1,0,1)
    if(a<0.2){
      z[j]=1
    }else{
      z[j]=0
    }
  }
  y[i]<-sum(z)
}
y
# f <- function(n,p){
#   sum(replicate(n,ifelse(runif(1)<p,1,0)))
# }
# Iii <- replicate(10^3,f(n=20,p=0.2))
discrete.histogram(y,xlab="y",freq = F,main="Histogram of Y")
mean(y);var(y)

#1(iii) R package Poi(10)
set.seed(1234)
y<-c()
for(i in 1:1000){
  f<-exp(-10)
  p<-f
  n<-0
  u<-runif(1)
  while(u>f){
    n<-n+1
    p<-(p*10)/n
    f<-f+p
  }
  y[i]<-n
}
y
# f <- function(n){
#   y <- c()
#   u <- runif(n)
#   for(i in 1:n){
#     I=0
#     if(u[i]<ppois(I,10)){
#       y[i] <- I
#     }else{
#       while(ppois(I,10) <= u[i]){
#         y[i] <- I+1
#         I <- I+1
#       }
#     }
#   }
#   return(y)
# }
# Iiii <- f(10^3)
discrete.histogram(y,xlab="y",freq = F,main="Histogram of Y")
mean(y);var(y)

#4.3
x<-c(0.3,0.2,0.35,0.15)
y<-c()
for(i in 1:1000){
  z<-x[1]
  n<-1
  u<-runif(1)
  while(u>z){
    n<-n+1
    z<-z+x[n]
  }
  y[i]<-n
}
# f <- function(){
#   u <- runif(1)
#   if(u<0.3){
#     1
#   }else if(u<0.5){
#     2
#   }else if(u<0.85){
#     3
#   }else if(u<1){
#     4
#   }
# }
# II_4_3 <- replicate(10^6,f())
y
mean(y);var(y)

#4.10(a)
y<-c()
for(i in 1:1000){
  x<-c()
  for(j in 1:10){ 
    u<-runif(1)
    x[j]<-floor(log(u)/log(0.2))+1
  }
  y[i]<-sum(x)
}
y
# f <- function(r,p){
#   sum(replicate(r,floor(log(runif(1))/log(1-p))+1))
# }
# ch4_10_a <- replicate(10^6,f(r=10,p=0.8))
mean(y);var(y)

#4.10(c)
y<-c()
for(i in 1:1000){
  f<-(0.8)^10
  p<-f
  j<-10
  u<-runif(1)
  while(u>f){
    p<-(j*0.2/(j-9))*p
    f<-f+p 
    j<-j+1
  }
  y[i]<-j
}
y
# f <- function(r,p){
#   u <- runif(1)
#   x = 10
#   cdf = pdf = p^r
#   while(u>=cdf){
#     pdf = (x*(1-p)/(x-r+1)) * pdf
#     cdf = cdf + pdf
#     x = x+1
#   }
#   return(x)
# }
# ch4_10_c <- replicate(10^6,f(r=10,p=0.8))
mean(y);var(y)

#4.10(d)
f <- function(r,p){
  x=0
  y=0
  while(x<=r){
    if(x==r){break}
    u<-runif(1)
    if(u<p){x=x+1}
    y=y+ 1
  }
  return(y)
}
z<- replicate(10^6,f(r=10,p=0.8))
mean(z);var(z)

#HW4

#Box-Muller transformation
set.seed(123)
start <- Sys.time()
x<-c()
y<-c()
for(i in 1:10^4){
  u1<-runif(1)
  u2<-runif(1,0,2*pi)
  x[i]<-sqrt((-2*log(u1)))*cos(u2)
  y[i]<-sqrt((-2*log(u1)))*sin(u2)
}

# f <- function(){
#   x=sqrt(rexp(1,1/2))*cos(runif(1,0,2*pi))
#   y=sqrt(rexp(1,1/2))*sin(runif(1,0,2*pi))
#   return(c(x,y))
# }
# hw1 <- as.data.frame(t(replicate(10^4,f())))


#HW7

#8.14
set.seed(123)
f_14<-c(1,3)
s_square<-var(f_14)
v_14<-c()
g_14<-function(l,n){
  while(length(v_14)<l){
    s_14<-c()
    for(i in 1:n){ 
      u<-runif(n,0,1)
      I<-floor(n*u)+1
      s_14[i]=f_14[(I[i])]
    }
    v_14[length(v_14)+1]<-var(s_14)
  }
  return((1/l)*(sum((v_14-s_square)^2)))
}
g_14(l=1000,n=2)


# set.seed(109225004)
# data <- replicate(1000,sample(c(1,3),size = 2,replace = T))
# s <- apply(data,2,var)
# mean((s-var(c(1,3)))^2)


#8.15
set.seed(123)
f_15<-c(5,4,9,6,21,17,11,20,7,10,21,15,13,16,8)
s_square_15<-var(f_15)
v_15<-c()
g_15<-function(l,n){
  while(length(v_15)<l){
    s_15<-c()
    for(i in 1:n){ 
      u<-runif(n,0,1)
      I<-floor(n*u)+1
      s_15[i]=f_15[(I[i])]
    }
    v_15[length(v_15)+1]<-var(s_15)
  }
  return((1/l)*(sum((v_15-s_square_15)^2)))
}
g_15(l=1000,n=15)


# set.seed(109225004)
# x <- c(5,4,9,6,21,17,11,20,7,10,21,15,13,16,8)
# data <- replicate(1000,sample(x,15,replace = T))
# s <- apply(data,2,var)
# mean((s-var(x))^2)

#2(b)
set.seed(123)
xbar_b<-c()
f_2_b<-function(l,n){
  while(length(xbar_b)<l){
    x_2_b<-c()
    for(i in 1:n){ 
      u<-runif(n,0,1)
      I<-floor(n*u)+1
      x_2_b[i]=x[(I[i])]
    }
    xbar_b[length(xbar_b)+1]<-mean(x_2_b)
  }
  return(xbar_b)
}
xbar_star<-f_2_b(l=1000,n=100)
MSE_b<-(1/1000)*(sum((xbar_star-x_bar)^2))
MSE_b
xbar_star_b<-sort(xbar_star)
CI_b<-c((xbar_star_b[50]+xbar_star_b[51])/2,(xbar_star_b[950]+xbar_star_b[951])/2)
CI_b

# set.seed(109225004)
# b <- replicate(1000,sample(x,length(x),replace = T))
# x_bar_b <- apply(b,2,mean)
# x_bar_b <- sort(x_bar_b)
# c((x_bar_b[50]+x_bar_b[51])/2,(x_bar_b[950]+x_bar_b[951])/2)
# mean((x_bar_b-mean(x))^2)

#2(c)
set.seed(123)
xbar_c<-c()
f_2_c<-function(l,n){
  while(length(xbar_c)<l){
    x_2_c<-rnorm(100,mean(x),sqrt(var(x)))
    xbar_c[length(xbar_c)+1]<-mean(x_2_c)
  }
  return(xbar_c)
}
xbar_starc<-f_2_c(l=1000,n=100)
MSE_c<-(1/1000)*(sum((xbar_starc-x_bar)^2))
MSE_c
xbar_star_c<-sort(xbar_starc)
CI_c<-c((xbar_star_c[50]+xbar_star_c[51])/2,(xbar_star_c[950]+xbar_star_c[951])/2)
CI_c

# set.seed(109225004)
# c <- replicate(1000,mean(rnorm(100,x_bar,sd = sqrt(s2))))
# c <- sort(c)
# c((c[50]+c[51])/2,(c[950]+c[951])/2)
# mean((c-mean(x))^2)

#HW9

#3 
library(stats)
set.seed(123)
f<-function(t){
  (1/((1/2)-t))+(1/((1/3)-t))+(1/((1/4)-t))+(1/((1/5)-t))-62
}
t_star<-uniroot(f,c(0, 0.2),tol = 0.000001)
t_star$root;t_star$f.root+62
St_star<-c()
for(i in 1:1000){
  x<-c()
  for(j in 1:4){
    x[j]<-rexp(1,(1/(j+1))-0.1745968)
  }
  St_star[i]<-sum(x) 
}
indicator<-c()
for(i in 1:1000){
  if(St_star[i]>62){
    indicator[i]=1
  }else{
    indicator[i]=0
  }
}
Nhat_IS<-sum(St_star*indicator*exp(-0.1745968*St_star))/1000
Dhat_IS<-sum(indicator*exp(-0.1745968*St_star))/1000
thetahat_IS<-Nhat_IS/Dhat_IS
thetahat_IS

# f1 <- function(x){1/(x+1)}
# f2 <- function(x){
#   sum(1/(f1(1:4)-x))-62
# }
# t <- uniroot(f2,c(0,0.2),tol=0.00001)$root
# t
# f2(t)+62
# set.seed(19225004)
# data <- mapply(function(x){rexp(1000,x)},f1(1:4)-t)
# s <- rowSums(data)
# N <- mean(s*(s>62)*exp(-t*s))
# D <- mean((s>62)*exp(-t*s))
# theta_IS <- N/D
# theta_IS
# set.seed(19225004)
# data <- mapply(function(x){rexp(10^7,x)},f1(1:4))
# total <- rowSums(data)
# mean(total[total>=62])







#apply:
#http://blog.fens.me/r-apply/
# http://blog.fens.me/wp-content/uploads/2016/04/apply.png
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

#apply(x,MARGIN,FUN,...):x為矩陣或陣列,MARGIN=1依row執行=2依column執行,FUN為作用的函數名稱
x=matrix(x,4,3)
x
f(x=x)
apply(x,1,function(x){mean(sqrt(x)*10)})
apply(x,2,function(x){mean(sqrt(x)*10)})

#lapply(list, function),sapply(list, function):x為向量或list
x <- list(a = 1:10,beta = exp(-3:3),logic = c(T,F,F,T))
x
lapply(x,mean) #回傳list
sapply(x,mean) #回傳向量或矩陣(特殊情況回傳list)
sapply(1:5,seq) #sapply回傳list情況:長度不同
y <- data.frame(cbind(x1=3, x2=c(2:1,4:5)))
sapply(y, cumsum)
#lapply常搭配do.call(rbind,.),do.call(cbind,.)使用

#vapply(list, function, FUN.VALUE , ...)類似sapply,但可用FUN.VALUE控制output的名稱
x <- data.frame(cbind(x1=3, x2=c(2:1,4:5)))
vapply(x,cumsum,FUN.VALUE=c('a'=0,'b'=0,'c'=0,'d'=0))
sapply(x,cumsum) #可用row.names指定名稱

#tapply(vector, index, function):利用index將vector分類後做運算
x<-y<-1:10;x;y
t<-round(runif(10,1,100)%%2);t
tapply(x,t,sum)
tapply(x,t,sum,y) #錯誤用法
tapply(x,t,sum)+tapply(y,t,sum)

#mapply(function, ...):多變量型sapply,...表示多個數據
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
x=list(b=4:1,a=12,c=c('b','a'))
y=pi
z=data.frame(a=rnorm(10),b=10:1)
a <- list(x=x,y=y,z=z)
a
rapply(a , sort,how='replace')
#例2:文字類型加入"++++",非文字設為NA
rapply(a,function(x) paste0(x,'++++'),classes="character",deflt=NA, how = "list")

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
x <- foreach( i = 1:3, .combine='cbind') %do% rnorm(4) 
x <- foreach( i = 1:3, .combine='cfun', .multicombine=TRUE) %do% rnorm(4) #cfun為使用自訂函數

#%dopar%:需在程式頭尾加入開啟與關閉平行化的核心,並將%do%用法改為%dopar%,若要加入其他套件使用.packages參數
cpu.cores <- detectCores() #取得cpu最大核心數
cl = makeCluster(cpu.cores) # 開啟平行核心數
registerDoParallel(cl)
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
clusterExport(cl, c("x")) 
pbapply(x,1,function(x){mean(sqrt(x)*10)},cl = cl)
pbapply(x,2,function(x){mean(sqrt(x)*10)},cl = cl)
stopCluster(cl) #關閉平行核心數
