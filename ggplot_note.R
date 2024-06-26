#EDA (Exploratory Data Analysis):探索性資料分析
#EDA 4-plot:趨勢圖,散布圖,直方圖,QQ plot
#Data Visualization

library(graphics)
demo(graphics) #常見圖形
demo(Hershey) #各種符號
demo(image) #image,contours
demo(Japanese) #日文字
demo(persp) #曲面圖
demo(plotmath) #數學符號

#============================================================================================
###baseR繪圖:R的內建繪圖函數可以分為base系統與lattice系統

#library(help = "graphics")
#?plot.default
#plot(x),plot(x,y),plot(xy):若x.y皆為vector,或xy為一具有兩個column的矩陣或資料框架,則畫出X-Y散布圖
plot(cars)

#plot(x):若x為時間序列變數,則畫出時間數列圖.若x為數值向量則以各元素值為y軸,出現順序為x軸
z = ts(sample(1:10),start = c(1959,2),freq=4)
plot(z)

#plot(f):若f為分類變數,畫出f的長條圖
f <- as.factor(sample(1:6,1000,T)) #模擬骰子
plot(f)

#plot(f,v):若f為分類變數,y為相同長度的數字變數,畫出以f各分類為x軸,v值為y軸的box-plot
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

#http://www.hmwu.idv.tw/web/R/A05-hmwu_R-Graphics&Visualization.pdf
#============================================================================================

###ggplot2繪圖系統
library(tidyverse)
library(gridExtra)
library(grid)
library(ggrepel)
library(ggthemes)
library(ggmap)
library(ggcharts)
library(rgl) #3D
library(plotly) #3D
library(gganimate) #animation
library(fmsb) #radar
library(treemap)
library(ggraph) #dendrogram樹狀圖
library(tweenr)
library(geosphere)
#install_github('arcdiagram', username='gastonstat')
library(arcdiagram) #library(devtools);install_github('arcdiagram', username='gastonstat')
library(networkD3)
library(igraph) #Network
library(ggraph)
library(gcookbook)
library(extrafont)
library(showtext)

#plot = data + aesthetics + geometry
#ggplot()+layer(mapping=aes(),data,geom,stat,position)
#ggplot(data, aes(x,y,label,...)) + geom_*(data,aes(...)) + stat_*(geom) + coord_*() + facet_*() + scale_*() + theme_*()
#geom_*()通常會有預設對應的stat_*()

#https://ggplot2-book.org/
#https://r-graphics.org/
#https://www.r-graph-gallery.com/index.html
#https://exts.ggplot2.tidyverse.org/gallery/
#https://ggplot2.tidyverse.org/reference/index.html
#https://www.rstudio.com/resources/cheatsheets/
#http://www.hmwu.idv.tw/index.php/r-software

#一些資料處理函數
transform()
ave(x,..., FUN = mean) #計算分組(依...做分組)平均(FUN=mean)
prop.table() #算比例
tidyr::gather() #寬表格轉長表格
tidyr::spread() #長表格轉寬表格

##ggplot圖形由以下要素構成
#data數據:ggplot()
  #data以data.frame格式為主
  ?ggplot()
#mapping映射圖形屬性aesthetics
  #aes(x,y,color顏色,alpha透明度,fill填色,shape形狀,size大小,stroke筆觸粗細,lintype線類別,lower, upper, middle, ymin, ymax,group)
  ?aes
#scale尺度:以顏色、大小、形狀等來表示不同值scale_*_*
  #scale_color_manual,scale_color_distiller,scale_color_brewer,scale_fill_manual,scale_x_continuous...
  #第二部分為屬性名稱:color,fill,x,y,linetype,shape,size等
  #第三部分為尺度名稱:continuous,discrete,brewer等
#layer圖層:由geom幾何,stat統計組成,layer(mapping,data,geom,stat,position)
?layer()
  #geom圖形:geom_line,geom_boxplot
  #stat:數據轉換
  #position:position_*()調整位置 "stack","dodge","fill","jitter","nudge"
  #annotation:特殊圖層,添加註解訊息annotate(geom=c("text","segement","rect"))

ggplot()+layer(data = mpg , mapping = aes(displ,hwy) , geom = "point" , stat = "identity" , position = "identity")
ggplot(data=mpg , mapping = aes(displ,hwy))+
  geom_point()
ggplot()+
  geom_point(data=mpg , mapping = aes(displ,hwy))
ggplot(mpg)+
  geom_point(aes(class,cty))+
  geom_boxplot(aes(trans,hwy))
#coord座標:圖形平面座標
#facet分面:將圖形應用至subset,facet_grid(),facet_wrap()
#theme主題:包含字體大小、背景顏色
  ?theme()

#============================================================================================
#https://www.twblogs.net/a/5b817c882b71772165acd40e
##基本圖形
a <- ggplot(economics,aes(date,unemploy))
seals$z <- with(seals,sqrt(delta_long^2+delta_lat^2))
b <- ggplot(seals,aes(x=long,y=lat))

#--------------------------------------------------------------------------------------------
#geom_blank()
a+geom_blank()+
  ggtitle("這是標題","這是副標題")

plot(economics$date,economics$unemploy,type="n")

#--------------------------------------------------------------------------------------------
#geom_curve(x,xend,y,yend,angle,curvature)
b+geom_curve(aes(yend=lat+0.5,xend=long+0.5),curvature = 0.5)+ #x,y已經在ggplot()中指定
  geom_curve(aes(y=30,yend=45,x=-160,xend=-140),curvature = -0.7 , color="red")+
  labs(title = "這還是標題", x="這是x軸",y="這是y軸",subtitle="這是次標題",caption="這是啥我不知道")

#--------------------------------------------------------------------------------------------
#geom_path(lineend = c("round", "butt", "square"),linejoin = c("round", "mitre", "bevel"))
a+geom_path(lineend = "butt",linejoin = "round",linemitre = 1)

year <- function(x){as.POSIXlt(x)$year+1900}
ggplot(economics,aes(unemploy/pop,uempmed))+
  geom_path(color="grey50")+ 
  geom_point(aes(color=year(date)))+
  labs(color="New color \n lengend title") #更改圖例標題

df <- data.frame(x = c(1,3,5,2,4,6),
                 y = c(2,4,6,1,3,5),
                 label = as.character(1:6))
ggplot(df,aes(x=x,y=y))+
  geom_path()+
  geom_text(aes(label=label))

df_arrange <- df %>% arrange(x)
df_arrange$label <- as.character(1:6)
ggplot(df_arrange,aes(x=x,y=y))+
  geom_path()+
  geom_text(aes(label=label),color="red",size=5)

#--------------------------------------------------------------------------------------------
#geom_polygon()
a+geom_polygon()

ggplot(df,aes(x=x,y=y))+
  geom_polygon(fill="green",alpha=0.3)+
  geom_text(aes(label=label),color="red") #逐點連線後填滿空間

ggplot(df_arrange,aes(x=x,y=y))+
  geom_polygon(fill="pink",alpha=0.7)+
  geom_text(aes(label=label),color="red",size=5)

#--------------------------------------------------------------------------------------------
#geom_rect(aes(xmin,xmax,ymin,ymax))
b+geom_rect(aes(xmin=long,ymin=lat,xmax=long+0.5,ymax=lat+0.5))

#--------------------------------------------------------------------------------------------
#geom_ribbon(aes(ymin,ymax))
a+geom_ribbon(aes(ymin=unemploy-900,ymax=unemploy+900),alpha=0.2)+ 
  geom_line(aes(y=unemploy-900),color="grey50",linetype="dotted")+ #下邊界虛線
  geom_line(aes(y=unemploy+900),color="grey50",linetype="dotted")+ #上邊界虛線
  geom_line() #中心實線
  
#--------------------------------------------------------------------------------------------
#geom_abline(aes(intercept,slope)),geom_hline(aes(yintercept)),geom_vline(aes(xintercept))
b+geom_abline(aes(intercept=110,slope=1/2))+
  geom_hline(aes(yintercept=lat),color="red")+
  geom_vline(aes(xintercept=long),color="blue")

#--------------------------------------------------------------------------------------------
#geom_segment():加入線段或箭頭(arrow)
b+geom_segment(aes(xend=long+0.5,yend=lat+0.5))+
  geom_segment(aes(x=-160, y=30, xend=-140, yend=40), color="blue",arrow = arrow(length = unit(0.5, "cm"),type = "open",ends = "last")) #arrow箭頭

#--------------------------------------------------------------------------------------------
#geom_spoke()
b+geom_spoke(aes(angle=1:1155,radius=0.5))

#--------------------------------------------------------------------------------------------
##one variable
c <- ggplot(mpg,aes(hwy))
c2 <- ggplot(mpg)
d <- ggplot(mpg,aes(fl))

#--------------------------------------------------------------------------------------------
#geom_area()
c+geom_area(stat="bin")+
  theme_bw() #背景改為白底黑邊

#--------------------------------------------------------------------------------------------
#geom_density(kernal=c("gaussian", "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine"))
c+geom_density(kernel="gaussian",color="darkblue", fill="lightblue", linetype="dashed" , alpha=0.5)+
  geom_vline(aes(xintercept=mean(hwy)),color="blue", linetype="dashed", size=1)+ #加入平均線
  xlim(c(0,50))

c+stat_density(geom="area",kernel="gaussian",color="darkblue", fill="lightblue", linetype="dashed" , alpha=0.5)+ #改用stat_*作圖
  xlim(c(0,50))

c+geom_density(aes(fill=class),alpha=0.4)+
  xlim(c(0,50))

c+stat_density(aes(fill=class),alpha=0.4,position=position_dodge(1))+ #改用stat_*作圖
  xlim(c(0,50))
  
c+stat_ecdf(geom="step")+ #empirical cumulative density function,geom=c("step","point","line")
  labs(title="Empirical Cumulative Density Function",y = "F(hwy)", x="hwy")

ggplot(faithful,aes(x=waiting))+
  geom_density()

#--------------------------------------------------------------------------------------------
#geom_function函數曲線圖: https://ggplot2.tidyverse.org/reference/geom_function.html
data <- data.frame(x = rnorm(100))
ggplot(data, aes(x)) +
  geom_histogram(aes(y=..density..),bins = 12,color=1,fill="grey")+ #將直方圖y軸改用density
  geom_density(color=2) +
  geom_function(fun = dnorm, colour = 1) #函數曲線圖

#如何在函數曲線下某區域加入陰影?需定義新函數,範圍外設為NA
#https://steemit.com/programming/@dkmathstats/creating-normal-distribution-plots-with-r-programming
dnorm_limit <- function(x){
  y <- dnorm(x)
  y[ x < 0 | x > 2 ] <- NA
  return(y)
}
p <- ggplot(data.frame(x=c(-3,3)),aes(x=x))
p+stat_function(fun=dnorm_limit,geom="area",fill="blue",alpha=0.2)+
  stat_function(fun=dnorm)
#https://www.coder.work/article/6346313

hist(data$x,freq = F)
lines(density(data$x),col="red")
curve(dnorm(x),add=T)

x <- seq(from = -5, to = +5, length.out = 100)
y <- dnorm(x)
plot(x, y, main = "Standard Normal Distribution", type = "l", ylab = "Density", xlab = "Quantile")
abline(h = 0)
region.x <- x[0 <= x & x <= 4]
region.y <- y[0 <= x & x <= 4]
region.x <- c(region.x[1], region.x, tail(region.x, 1))
region.y <- c(0, region.y, 0)
polygon(region.x, region.y, density = -1,col="blue") #預設density=10， 表示以傾斜45度的細線来填充

#--------------------------------------------------------------------------------------------
#geom_dotplot()
c+geom_dotplot(binwidth = 0.5)

#--------------------------------------------------------------------------------------------
#geom_freqpoly()
c+geom_freqpoly(binwidth=1) #頻率多邊圖(y為次數)

#--------------------------------------------------------------------------------------------
#geom_histogram(binwidth,bins,position=c("dodge","fill","stack"重疊))
c+geom_histogram(binwidth = 1) #組距=1

c+geom_histogram(aes(fill=drv),binwidth = 1,color="black",bins = 10) #color只改變邊框顏色,fill改變長條顏色

c+geom_histogram(aes(y=..density..),binwidth = 1,color="black",fill="blue",bins = 10) #不同stat會產生不同生成變量

c+geom_histogram(aes(fill=drv),binwidth = 2,position = "dodge")+ #binwidth調整寬度
  # facet_wrap(~drv,ncol=1)+ #分面
  coord_flip()+ #x,y軸對調
  # scale_fill_manual(values = c("#73BF00","#8C8C00","#D26900"))+
  scale_fill_brewer(palette = "Spectral")+
  scale_x_reverse() #反轉x軸(hwy)

p1 <- ggplot(mpg,aes(hwy,fill=drv))+geom_histogram(binwidth = 2,position = "dodge")
p2 <- ggplot(mpg,aes(hwy,fill=drv))+geom_histogram(binwidth = 2,position = "fill")
p3 <- ggplot(mpg,aes(hwy,fill=drv))+geom_histogram(binwidth = 2,position = "stack")
grid.arrange(p1,p2,p3,ncol = 3)

hist(mpg$hwy,breaks = 30)

#--------------------------------------------------------------------------------------------
#geom_qq()
c2+geom_qq(aes(sample=hwy))+geom_qq_line(aes(sample=hwy))

qqnorm(mpg$hwy);qqline(mpg$hwy)

#--------------------------------------------------------------------------------------------
#geom_bar()
d+geom_bar()+
  geom_text(aes(label=paste(..count..,"個"),y=..count..+10),stat="count",vjust=1.5,color="black",size=3)+ #列出長條圖高度
  ylab("count")+
  scale_y_continuous(breaks=NULL)

d+geom_bar(aes(fill=class),color="black") #"stack",position_stack(reverse=T),可配合guide_legend(reverse=T)

d+geom_bar(aes(fill=class),position="dodge") #position_dodge()

d+geom_bar(aes(fill=class),position="fill") #position_fill(),百分比堆疊圖

barplot(table(mpg$fl))

#--------------------------------------------------------------------------------------------
##two variables
e <- ggplot(mpg,aes(cty,hwy))
f <- ggplot(mpg,aes(class,hwy))
g <- ggplot(diamonds,aes(cut,color))
h <- ggplot(diamonds,aes(carat,price))
economics$group <- sample(c("1","2"),size = nrow(economics),replace = T)
i <- ggplot(economics,aes(date,unemploy))
df <- data.frame(grp = c("A","B"),fit=4:5,se=1:2)
j <- ggplot(df,aes(grp,fit,ymin=fit-se,ymax=fit+se))
BOD1 <- BOD
BOD1$Time <- factor(BOD1$Time)

#geom_label()
e+geom_label(aes(label=cty),nudge_x = 1,nudge_y = 1)

#--------------------------------------------------------------------------------------------
#geom_text(x,y,label,alpha,angle,colour,family=c("sans","serif","mono"),fontface,group,hjust,lineheight,size,vjust):在(x,y)加入文字標籤label(通常用rownames)
e+geom_text(aes(label=cty),nudge_x = 1,nudge_y = 1,check_overlap = T)

plot(mpg$cty,mpg$hwy,type="n")
text(mpg$cty,mpg$hwy,labels = mpg$cty)

#--------------------------------------------------------------------------------------------
#geom_point()
e+geom_point()+
  stat_ellipse(level=0.95,segments = 51,type="t")+ #畫橢圓
  scale_y_continuous(breaks=c(10,20,30,40),labels=c("低","中低","中高","高")) #將y軸設定標籤

e+geom_point(aes(color=class))+
  scale_color_manual(values=c(1,"blue","green","tomato","tomato","#e86b97","blue")) #利用scale指定color
  
e+geom_point(aes(colour="blue")) #warning

e+geom_point(aes(shape=drv))+ #調整點的形狀
  coord_fixed(ratio=3/4,xlim = c(0,40),ylim = c(0,45)) #調整坐標軸

e+geom_point(aes(size=cyl)) #調整點的大小
#調整size,shape,color等可視為資料分組視覺化

library(car)
scatterplot(hwy~cty,data=mpg,ellipse=TRUE, regLine=FALSE, smooth=FALSE,boxplots=F)
plot(mpg$cty,mpg$hwy,col=factor(mpg$class,labels = 1:7),pch=16)
plot(mpg$cty,mpg$hwy,col="blue",pch=16)
plot(mpg$cty,mpg$hwy,pch=as.numeric(factor(mpg$drv,labels = 1:3)))
plot(mpg$cty,mpg$hwy,cex=mpg$cyl-3,pch=16)

ggplot(mpg,aes(displ,hwy))+
  geom_point()+
  facet_wrap(~class,scales = "free") #分面:依class分為多個散布圖

#--------------------------------------------------------------------------------------------
#geom_jitter(height,width)
e+geom_jitter()

#--------------------------------------------------------------------------------------------
#geom_quantile()
e+geom_quantile()

#--------------------------------------------------------------------------------------------
#geom_rug(sides="trbl") top,right,bottom,left
e+geom_rug(sides="bl")

#--------------------------------------------------------------------------------------------
#geom_smooth(method=c("lm", "glm", "gam", "loess"))
e+geom_smooth(method="lm",span=0.5)+ #method曲線配適方法，span:曲線平滑程度
  geom_point()+
  geom_rug(sides = "bl")+
  annotate("text",x=c(20,30,10),y=c(30,20,45),label=c("a","b","R^2==0.9138"),color="red",parse=T) #在圖形上加入註解文字,parse可使label用類似latex文字方式撰寫

plot(mpg$cty,mpg$hwy,type = "p");abline(lm(mpg$hwy~mpg$cty))

#--------------------------------------------------------------------------------------------
#geom_col();geom_bar(stat="identity")
f+geom_col(aes(fill=trans),position = position_dodge(1.2),width=0.5) #width改變長條圖寬度,position_dodge改變長條圖間隔寬度

f+geom_bar(stat="identity",aes(x=reorder(class,-hwy),y=hwy))

f_group <- mpg %>% group_by(class) %>% summarise(total_hwy = sum(hwy)) #若要將長條圖大小排序需先做分組計算
ggplot(f_group,aes(x=reorder(class,-total_hwy),y=total_hwy))+ #利用reorder進行大小排序,-為遞減排序
  geom_bar(stat="identity",width = 0.3) 

f_group_order <- f_group %>% arrange(desc(total_hwy)) #重新排序
barplot(height = f_group_order$total_hwy,names.arg = f_group_order$class) #baesR畫長條圖需用分組資料

#------------------------------------------------------------------------------------------------------------------------
#geom_boxplot(),geom_violin()
f+geom_boxplot()+ #fill分組填滿顏色
  stat_summary(fun.y = "mean",geom = "point",shape=23,size=3,fill="white") #加入平均值點

f+geom_boxplot(aes(fill=drv))+ #fill分組填滿顏色
  scale_x_discrete(limits=c("2seater","midsize","subcompact","compact","suv","minivan","pickup")) #改變盒狀圖順序

boxplot(hwy~class+drv,data=mpg,col=rainbow(3))

#--------------------------------------------------------------------------------------------
#geom_dotplot(binaxis=c("x","y"),stackdir=c("up","down","center"))點狀圖
f+geom_dotplot(binaxis = "y",stackdir = "down",binwidth=0.5,dotsize = 0.8) #binaxis以y變數計算作為點的數量

#--------------------------------------------------------------------------------------------
#geom_count()
g+geom_count()

#--------------------------------------------------------------------------------------------
#geom_bin2d(binwidth,bins)
h+geom_bin2d(binwidth=c(0.25,500))

h+geom_bin2d(bins = 100)

#--------------------------------------------------------------------------------------------
#geom_density2d():https://ggplot2.tidyverse.org/reference/geom_density_2d.html
h+geom_density2d()

#--------------------------------------------------------------------------------------------
#geom_hex()
h+geom_hex()

#--------------------------------------------------------------------------------------------
#geom_area()
i+geom_area()

#--------------------------------------------------------------------------------------------
#geom_line()
i+geom_line()+
  scale_x_date(date_labels=("%Y/%m%/%d"))+ #顯示x軸年月日資訊 https://www.stat.berkeley.edu/~s133/dates.html
  theme(axis.text.x = element_text(angle=45))+ #將x軸文字傾斜45度
  annotate("segment",x=as.Date("2005/01/01"),xend=as.Date("2009/01/01"),y=14000,yend=15000,arrow=arrow(),color="red",size=2)+ #加入箭頭註解
  annotate("rect",xmin = as.Date("1990/01/01"),xmax = as.Date("2000/01/01"),ymin=0,ymax=16000,alpha=0.1,fill="blue") #加入陰影區塊

i+geom_line(aes(color=group,linetype=group))+ #group決定哪些資料是相連的
  scale_y_continuous(breaks = c(5000,7000,9000,11000)) #改變y軸標籤

ggplot(BOD,aes(Time,demand))+
  geom_line()+
  ylim(0,max(BOD$demand)) #將y軸起始點調整為0
#比較與上圖差異
ggplot(BOD1,aes(Time,demand,group=1))+ #若x軸為類別型需指定group
  geom_line()+
  expand_limits(y=0) #將y軸起始點調整為0

ggplot(worldpop,aes(x=Year,y=Population))+
  geom_line()+
  geom_point()#+ 
  scale_y_log10() #將y軸做log計算
  
ggplot(tg,aes(x=dose,y=length,fill=supp,shape=supp))+
  geom_line(position = position_dodge(0.2))+ #將連接線左右移動0.2避免重疊
  geom_point(position = position_dodge(0.2),size=3)+ #將點左右移動0.2避免重疊,shape=21為空心圓點
  scale_shape_manual(values = c(21,22))+ #shape21-25為空心圖形,才可配合fill使用
  scale_fill_manual(values = c("black","white"))
  
plot(economics$date,economics$unemploy,type="l")
plot(economics$date[economics$group==1],economics$unemploy[economics$group==1],type="l",col="red",lty=1)
lines(economics$date[economics$group==2],economics$unemploy[economics$group==2],type="l",col="blue",lty=2)

#--------------------------------------------------------------------------------------------
#geom_step(direction=c("vh","hv","mid")),vertical,horizontal
i+geom_step(direction = "hv")

#--------------------------------------------------------------------------------------------
#geom_cross_bar()
j+geom_crossbar(fatten = 2)

#--------------------------------------------------------------------------------------------
#geom_errorbar()
j+geom_errorbar()

#--------------------------------------------------------------------------------------------
#geom_linerange()
j+geom_linerange()

#--------------------------------------------------------------------------------------------
#geom_pointrange()
j+geom_pointrange()

#--------------------------------------------------------------------------------------------
#geom_map():https://rstudio-pubs-static.s3.amazonaws.com/153254_cee58f2dd5d54d339c61099be47d2293.html
data <- data.frame(murder=USArrests$Murder,
                   state=tolower(rownames(USArrests)))
map <- map_data("state")
k <- ggplot(data,aes(fill=murder))
k+geom_map(aes(map_id=state),map=map)+ #data$state
  expand_limits(x=map$long,y=map$lat) 

#--------------------------------------------------------------------------------------------
##three variables
seals$z <- with(seals,sqrt(delta_long^2+delta_lat^2))
l <- ggplot(seals,aes(long,lat))

#--------------------------------------------------------------------------------------------
#geom_contour()
l+geom_contour(aes(z=z),bins = 25)

l+geom_contour(aes(z=z,color=after_stat(level)))

l+geom_contour_filled(aes(z=z),bins = 25)

l+geom_raster(aes(fill=z))+
  geom_contour(aes(z=z),color="white") #調換geom後的結果不同

#--------------------------------------------------------------------------------------------
#geom_raster()
l+geom_raster(aes(fill=z),hjust = 0.5,vjust = 0.5,interpolate = F)

#--------------------------------------------------------------------------------------------
#geom_tile()
l+geom_tile(aes(fill=z))

#--------------------------------------------------------------------------------------------
#圓餅圖:沒有函數可以直接畫,須將bar plot轉為極座標coord_polar()
cyl.df <- data.frame(table(mtcars$cyl))
names(cyl.df) <- c("cyl", "Freq")
cyl.df$Prop <- prop.table(cyl.df$Freq)
cyl.df
ggplot(cyl.df, aes(x=cyl, y=Freq, fill=cyl)) +
  geom_bar(width=1, stat="identity") +
  labs(x="", title="mtcars$cyl", fill="cyl")

ggplot(cyl.df, aes(x="", y=Freq, fill=cyl)) +
  geom_bar(width=1, stat="identity") +
  labs(x="", title="mtcars$cyl", fill="cyl")+
  coord_polar(theta = "y", start=0) + #轉為極座標
  theme_void() + #
  geom_text(aes(label = paste0(round(Prop*100), "%")),position = position_stack(vjust = 0.5))

pie(cyl.df$Freq,labels = as.character(cyl.df$cyl))

#--------------------------------------------------------------------------------------------
#相關矩陣圖
library(corrplot)
mcor <- cor(mtcars)
round(mcor,digits = 2)

corrplot(mcor)
corrplot(mcor,method="shade",shade.col = NA , tl.col = "black", tl.srt = 45,addCoef.col = "black",cl.pos = "n",order="AOE")

#--------------------------------------------------------------------------------------------
#網路圖
library(igraph)
gd <- graph(c(1,2,2,3,2,4,1,4,5,5,3,6))
plot(gd) #plot.igraph(gd)

g <- graph.data.frame(madmen2,directed = T) #生成有向
plot(g,layout = layout.fruchterman.reingold,vertex.size=8,edge.arrow.size=0.5,vertex.label=NA)

g <- graph.data.frame(madmen2,directed = F) #生成無向
plot(g,layout = layout.circle,vertex.size=8,vertex.label=NA)

#--------------------------------------------------------------------------------------------
#============================================================================================
#--------------------------------------------------------------------------------------------
#如何使用R內建外的字體
library(extrafont)
font_import()
fonts() #列出字體
loadfonts("win") #每次重啟R都要執行

library(showtext)
font_add("msjh", "msjh.ttc") #微軟正黑體
showtext_auto(enable=TRUE)

#--------------------------------------------------------------------------------------------
#多張圖排列:https://blog.chipcui.top/archives/easy-way-to-mix-multiple-graphs-on-the-same-page
p1 <- ggplot(mpg,aes(hwy,fill=drv))+geom_histogram(binwidth = 2,position = "dodge")+ggtitle("p1")
p2 <- ggplot(mpg,aes(hwy,fill=drv))+geom_histogram(binwidth = 2,position = "fill")+ggtitle("p2")
p3 <- ggplot(mpg,aes(hwy,fill=drv))+geom_histogram(binwidth = 2,position = "stack")+ggtitle("p3")
p4 <- ggplot(mpg,aes(hwy,fill=drv))+geom_histogram(binwidth = 2,position = "stack")+ggtitle("p4")

  
library(gridExtra)
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=1, ncol=4)

library(ggpubr)
ggarrange(p1,p2,p3,p4,ncol=2,nrow=2)

library(patchwork) #https://ggplot2-book.org/arranging-plots.html
p1+p2+p3+p4 + plot_layout(ncol=4)
p3 | (p2 / (p1 | p4))

#--------------------------------------------------------------------------------------------
#同時畫多張圖:利用apply
iris.hist <- function(x){
  ggplot(iris, aes(x=iris[,x], fill=Species)) +
    geom_histogram(binwidth = 0.1) +
    xlab(names(iris)[x])
}

hist.list <- lapply(1:4, iris.hist)
library(grid)
marrangeGrob(hist.list, nrow=2, ncol=2, top="")

#--------------------------------------------------------------------------------------------
#存圖片
#ggsave(filename,plot,device,path)將圖存至指定路徑
p <- ggplot(mpg,aes(cty,hwy))+
  geom_point(alpha=1/3)

print(p)
#檔案格式.eps,.pdf,.svg,.wmf,.png,.jpg,.bmp,.tiff等,預設尺吋為英吋
ggsave("C:\\Users\\steven\\Desktop\\mtcars.BMP", width = 20, height = 20, units = "cm") 

#============================================================================================
###ggplot語法:
##geom_*:每個圖形geom會有不同屬性aes
#基本圖形
geom_blank()
geom_curve()
geom_path()
geom_polygon()
geom_rect()
geom_ribbon()
geom_hline()
geom_vline()
geom_abline()
geom_segment()
geom_spoke()

#one variable
geom_area(stat="bin")
geom_density()
geom_dotplot()
geom_freqpoly()
geom_histogram()
geom_qq()
geom_bar()

#two variable
geom_label() #ggrepel::geom_label_repel文字不重疊
geom_text() #ggrepel::geom_text_repel文字不重疊
geom_point()
geom_jitter()
geom_quantile()
geom_rug()
geom_smooth()
geom_col() #geom_bar(stat="identity")
geom_boxplot()
geom_violin()
geom_count()
geom_bin2d()
geom_density2d()
geom_hex()
geom_area()
geom_line()
geom_step()
geom_crossbar()
geom_errorbar()
geom_linerange()
geom_pointrange()
geom_map() #geom_polygon()繪製地圖數據的快速版本

#three variable
geom_contour()
geom_tile()
geom_raster() #geom_tile的快速版本

#--------------------------------------------------------------------------------------------
##stat_*:有預設對應的geom.使aes可調用不同生成變量(Computed variables,以..*..使用),需從help查看
#變量:x, y, n, violinwidth, width, quantile, se, ymin, ymax, sample, theoretical, prop, count, ncount, density, ndensity, scaled, level, value, lower, middle, upper

#已有geom_*可使用
stat_bin() #geom_bar(),geom_freqpoly(),geom_histogram()
stat_bin2d() #geom_bin2d()
stat_bindot() #geom_dotplot()
stat_binhex() #geom_hex()
stat_boxplot() #geom_boxplot()
stat_contour() #geom_contour()
stat_quantile() #geom_quantile()
stat_smooth() #geom_smooth()
stat_sum() #geom_count
stat_function() #geom_function()
stat_qq() #geom_qq()
stat_spoke() #geom_spoke()

#不能用geom_*轉換
stat_ecdf() #經驗累積分布圖
stat_summary() #y|x
stat_summary2d()
stat_unique() #去除重複資料
stat_identity()
stat_ellipse(type=c("t","norm","euclid")) #橢圓

#stat_summary(geom = "point" , fun.y="mean")
#geom_point(stat="summary" , fun.y="mean")

#--------------------------------------------------------------------------------------------
##scale_*_*
#x,y
expand_limits(x=c(50, 100), y=c(0, 25)) #xlim(50,100)+ylim(0,25) 調整x,y軸座標刻度
scale_x_continuous(trans="log2") #對x軸座標做轉換,scale_x_log10,先做轉換才畫圖
scale_x_continuous(breaks,labels = c(percent,dollar,scientific),limits) #labels加入x軸單位
scale_x_discrete(position = "top") #更改座標軸刻度位置
scale_x_sqrt() #對x軸座標做根號轉換,scale_y_sqrt()
scale_x_reverse() #反轉x軸座標,scale_y_reverse()
scale_
scale_size(range) #縮放大小,scale_radius(range)

#color,fill:?discrete_scale,?continuous_scale
scale_color_manual(values,breaks) #指定分組顏色,離散型
scale_color_hue(h,c,l) #改變色相(h),飽和度(c),亮度(l)來調整顏色,離散型
scale_color_brewer() #使用ColorBrewer顏色,離散型.library(RColorBrewer);display.brewer.all()
scale_color_viridis() #使用viridis調色
scale_color_grey() #使用不同程度的灰色,離散型
scale_color_gradient(low,high) #漸層顏色,scale_color_gradient2,scale_color_gradientn,連續型
scale_color_distiller() #使用colorbrewer顏色,連續型
scale_color_identity(guide = "legend") #離散型,連續型,guide="legend"將圖例加回圖中

#第一個*可以是x,y,color,shape,size,alpha,linetype等
#packages:scales
#色碼表:https://www.ifreesite.com/color/

#--------------------------------------------------------------------------------------------
##coord_*
coord_cartesian(xlim,ylim,expand) #預設,expand=T預留邊界空隙
coord_fixed(ratio,xlim,ylim) #改變x,y軸刻度及比例,ratio=y軸/x軸實際長度比例
coord_trans(x="log2", y="log2") #對x,y軸做函數轉換,在做圖時才做轉換
coord_flip() #x,y對調
coord_polar(theta,start,direction) #轉為極座標,start圓餅圖起始點,direction各元素排列順序1為順時針-1為逆時針
coord_map() #地圖轉為球狀

#--------------------------------------------------------------------------------------------
##theme:legend,label
#https://zhuanlan.zhihu.com/p/29656775
theme(axis.title, #兩軸標籤外觀 
      axis.title.x, #x軸標籤外觀
      axis.title.y, #y軸標籤外觀
      axis.ticks, #兩軸刻度標籤外觀
      axis.ticks.x, #x軸刻度標籤外觀
      axis.ticks.y, #y軸刻度標籤外觀
      legend.title, #圖例標題外觀
      legend.text, #圖例文字項外觀
      legend.position, #圖例位置
      panel.*, #背景面板外觀
      plot.title) #圖例整體標題外觀
#其他可修改要素:https://r-graphics.org/recipe-appearance-theme-modify
theme_gray() #預設灰色主題
theme_bw() #白色背景,灰色網格,有邊框
theme_linedraw()
theme_light()
theme_dark()
theme_minimal() #只保留網格線
theme_classic() #只保留坐標軸
theme_void() #去除邊框及網格線
theme(plot.title=element_text(color="red", size=20, face="bold.italic"), #element_text改變plot.title標題文字設定
      axis.title.x=element_text(color="blue", size=14, face="bold"), #element_text改變 axis.title.x x軸文字設定
      axis.title.y=element_text(color="darkgreen", size=14, face="bold")) #element_text改變 axis.title.y y軸文字設定
    #element_text(family, face, colour, size) 改變文字設定
        #family=c("Helvetica","Times","Courier")字體
        #face=c("plain","bold","italic","bold.italic")粗斜體
        #color顏色
        #size字體大小
        #hjust橫向對齊:0左對齊,0.5居中,1右對齊
        #vjust縱向對齊:0底部對齊,0.5居中,1頂部對齊
        #angle:旋轉角度(度)
        #lineheight行間距倍數
    #element_blank() 清除設定
    #element_line() 
    #element_rect()
theme_set() #設定全局背景theme_set(theme_bw())
labs(x,y,title,size,color,...) #加入標籤,legend文字說明(size,color,shape,)
xlab() #x軸標籤,ylab()
ggtitle("主標題","副標題")
annotate(geom=c("text","rect","segment","pointrange")) #加入標註:text文字,rect矩形,segement線段

hw_plot <- ggplot(heightweight,aes(x=ageYear,y=heightIn))+geom_point(aes(color=sex))

hw_plot+guides(color="none")+ #三種移除圖例的方式
  scale_color_discrete(guide="none")+
  theme(legend.position = "none")

hw_plot+
  ggtitle("Age and Height \n of Schoolchildren")+
  theme(axis.title.x = element_text(size = 16,lineheight = 0.9,family = "Georgia",face = "bold.italic",color = "red",angle = 30), #改變x軸標籤設定,windowsFonts()查看windows字體
        plot.title = element_text(size=rel(1.5),lineheight = 0.9,family = "Magneto",face = "bold",color = "blue"), #改變標題設定,rel(1.5)字體大小定為當前主題基準字體大小的1.5倍
        panel.grid = element_blank(), #去除網格線
        legend.position = c(0.05,0.9), #修改圖例位置 (0,0)左下角 (1,1)右上角
        legend.background = element_blank(), #移除圖例整體邊框
        legend.key = element_blank(), #移除圖例項目周圍邊框
        legend.margin = margin(40,15,10,15), #按照上右下左的順序，指定圖例的四个方向空隙
        legend.direction = "horizontal", #圖例項目的放置方式水平(horizontal)或垂直(vertical),legend.box(),legend.box.just()
        legend.title = element_text(colour = "green") #圖例標題改為綠色
        )+  
  guides(color=guide_legend(title = "gender"))+ #將圖例標題改為gender
  scale_x_continuous(expand = c(0,0)) #去掉x軸的預留邊界

#xlab,ylab,xlim,ylim調整坐標軸
ggplot(mpg,aes(cty,hwy))+
  geom_point(alpha=1/3)+
  xlab(NULL)+ #x軸文字
  ylab(NULL)+ #y軸文字
  # labs(x=NULL,y=NULL)
  xlim(0,35)+ #x軸範圍
  ylim(0,50)  #y軸範圍

#packages:ggthemes,ggthemr,ggtech,ggsci,extrafont,showtext
#https://zhuanlan.zhihu.com/p/29662314

#--------------------------------------------------------------------------------------------
##legend
theme(legend.position = c("bottom","top","left","right")) #改變說明框位置
theme(legend.box = c("horizontal","vertical")) #改變說明框內容擺放方式
guides(color = guide_colourbar(order = 1), #改變說明框內容順序
       alpha = guide_legend(order = 2),
       size = guide_legend(order = 3), 
       shape = guide_legend(order = 4))


#--------------------------------------------------------------------------------------------
##position:位置調整
position_stack() #長條圖堆疊
position_fill() #長條圖堆疊並改為百分比計算
position_dodge() #長條圖並排
position_nudge() #點圖做固定偏移
position_jitter() #點圖做隨機偏移
position_jitterdodge() #點圖避開組內點做隨機偏移
position_identity()

ggplot(mpg,aes(displ,hwy))+
  geom_point()
ggplot(mpg,aes(displ,hwy))+
  geom_point(position = "jitter")
ggplot(mpg,aes(displ,hwy))+
  geom_point(position = position_jitter())

#--------------------------------------------------------------------------------------------
#============================================================================================
#qplot:ggplot的簡化
#qplot(x,y,data,geom,...)
qplot(displ,hwy,data=mpg,geom="point",main = "qplot範例")

