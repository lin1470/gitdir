#5.1
x=c(1,2,3,4)
 x
 y=c("a","b","c")
 y
 x = scan()

data(package="SemiPar")
data(copper,package=”SemiPar”)

 library(SemiPar)
 data()
 data(fossil)

s1=read.table("student.txt")
s1

s2=read.table("student.txt",header=T)
s2
S2=read.csv(file="student.csv")
S2

install.packages(“foreign”)
library(foreign)

#5.3 创建新变量
cons<-c(5000,5800,6000,10200,8500)
pop<-c(2000,3600,3500,5020,6100)
gnp<-c(6000,7200,7400,11000,9200)

pgnp<-gnp/pop
psave<-(gnp-cons)/pop

data<-data.frame(gnp,cons,pop)
transform(data,pgnp=gnp/pop,psave=(gnp-cons)/pop)
(pgnp<-with(data,gnp/pop))
(psave<-with(data,(gnp-cons)/pop))

#5.3.2 变量重编码
data(Cars93)
head(Cars93)
dat<-data.frame(manu=Cars93$Manufacturer,price=Cars93$Price)

dat$pricegrade<-NA
dat$pricegrade[dat$price>=20]<-"expensive"
dat$pricegrade[dat$price>=12&dat$price<20]<-"okay"
dat$pricegrade[dat$price<12]<-"cheap"
head(dat)

dat<-within(dat,{
    pricegrade<-NA
    pricegrade[price>=20]<-"expensive"
    pricegrade[price>=12&dat$price<20]<-"okay"
    pricegrade[price<12]<-"cheap"
})
head(dat)

dat$pricegrade<-cut(dat$price,c(0,12,20,max(dat$price)))  #将价格按照(0,12 ) ,(12,20 ),(20,max(price))分成 “cheap”, ”okay”,  ”expensive”三种类型。
levels(dat$pricegrade)<-c("cheap","okay","expensive")
head(dat)

recode(dat$pricegrade,"'cheap'='A';'okay'='B';'expensive'='C'")

#5.3.3 变量重命名
dat1 <- rename(dat, c(pricegrade="grade"))
head(dat1)

names(dat)
names(dat)[3] <- "grade"
head(dat)

#5.3.4变量类型的转换
a <- c(1,2,3);a
is.numeric(a)
is.vector(a)

a <- as.character(a);a
is.numeric(a)
is.vector(a)
is.character(a)

#5.4  缺失数据的探索与检验
y <- c(1,2,3,NA)
is.na(y)

data(sleep, package= "VIM")
sleep[!complete.cases(sleep),]
sum(!complete.cases(sleep))
mean(complete.cases(sleep))

newsleep <- na.omit(sleep)

#  缺失数据的处理
library(mice)
data(sleep, package="VIM")
imp <- mice(sleep, seed=6666)
fit <- with(imp,lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)

#5.5  数据框的合并与拆分
data(PlantGrowth)
PlantGrowth     
data(PlantGrowth)
unPG <- unstack(PlantGrowth)
unPG
sPG=stack(unPG)
sPG

# 数据集的抽取
newdata1 <- sleep[1:20, ]  #选择前20个观测

attach(sleep)
newdata2 <- sleep[which(Sleep>=3 & Sleep <6),]  #选择睡眠时间在3~6小时之间的观测
detach(sleep)

newdata3 <- subset(sleep, sleep>=3 & sleep<6, select=c(BodyWgt, Dream, Sleep,Span,	Pred, Exp,	Danger))

newdata4<- subset(sleep, Pred=3 & sleep<6, select=BodyWgt:  Pred)


install.packages('VIM')
install.packages('mice')
data(sleep,package='VIM')
summary(sleep)
attach(sleep)
summary(NonD)
sleep$NonD[sleep$NonD==NA] <- mean(NonD,na.rm=T)