install.packages("lmtest")
install.packages("bstats")
##multicolinarity
dat<-read.csv(file="11-1.csv")
lm3=lm(revenue~industry+agriculture+construction+consumption+pop+disaster,data=dat)
summary(lm3)
cor(dat[,-c(1,8)])#delete t and revenue
x<- cbind(rep(1,length(dat[,1])),dat[,-c(1,8)])
x<-as.matrix(x)
eigen(t(x)%*%x)
CI<-eigen(t(x)%*%x)$values[1]/eigen(t(x)%*%x)$values[7]
CI
library(bstats) #VIF test
vif(lm3)
## step wise
step(lm3,direction="forward")
step(lm3,direction="backward")
step(lm3,direction="both")
##### ridge regression
lm.r<-lm.ridge(revenue~industry+agriculture+construction+consumption+pop+disaster,data=dat)
lm.r
plot(lm.ridge(revenue~industry+agriculture+construction+consumption+pop+disaster,data=dat,lambda=seq(0,0.3,0.001)),lwd=3)
select(lm.ridge(revenue~industry+agriculture+construction+consumption+pop+disaster,data=dat,lambda=seq(0,0.3,0.001)))
lm.ridge(revenue~industry+agriculture+construction+consumption+pop+disaster,data=dat,lambda=0.004)
############################################
####hetersdascity
 agricul<-read.csv(file="11-2.csv")
 y=agricul[,2]
 x=agricul[,1]
 plot(x,y)         ###ɢ��ͼ�����췽��
 lm.a=lm(y~x)
 summary(lm.a)
plot(x,resid(lm.a)) #�в�ͼ
##    ###G-Q����
 g_qtest=function(x,y){  #��дG-Q���麯��
 n=length(x)
 m=round(n/4)
 xs=sort(x)
 xs1=xs[1:ceiling((n-m)/2)]
 xs2=xs[floor(n-(n-m)/2+1):n]
 n1=length(xs1)
 n2=length(xs2)
 ii=1:length(xs1)
    for(i in 1:length(xs1)){
 ii[i]=which(x==xs1[i])
 }
 y1=y[ii]
 ii2=1:length(xs2)
 for(i in 1:length(xs2)){
 ii2[i]=which(x==xs2[i])
 }
 y2=y[ii2]
 lm1=lm(y1~xs1)
 lm2=lm(y2~xs2)
 res1=lm1$resid
 res2=lm2$resid
 FF=(sum(res2^2)/length(xs2))/(sum(res1^2)/length(xs1))
 p=1-pf(FF,n1,n2)
 v=c("Fvalues:",FF,"pvalues:" ,p)
 return(v)
}


g_qtest(x,y)
library(lmtest)           ####��Ҫ�ع�����
 #####Gljzer����
re=resid(lm.a)
abre=abs(re)
summary(lm(abre~I(sqrt(x))))
summary(lm(abre~-1+I(sqrt(x))))
#### White test
library(bstas)
white.test(lm.a)
#######�췽��˷�
##������С���˷�
ys<-y/(0.2576*sqrt(x))
xs<-x/(0.2576*sqrt(x))
plot(xs,ys)
lm.sa<-lm(ys~xs)
summary(lm.sa)
plot(xs,resid(lm.sa)) #�в�ͼ
white.test(lm.sa) #white ����

###ȡ����
lny<-log(y)
lnx<-log(x)
plot(lnx,lny)
lm.lna<-lm(lny~lnx)
summary(lm.lna)
plot(lnx,resid(lm.lna)) #�в�ͼ
white.test(lm.lna) #white ����


###################   �������

  dat=read.csv("11-3.csv")
  y=dat$expend
  x=dat$income
  p=dat$cpi
  yp=y/p*100         #####�����۸�����
  xp=x/p*100         ##### �����۸�����
       #####������ؼ���
lm.in=lm(yp~xp)
summary(lm.in)
e<-resid(lm.in)
plot(e,type="l")   ###�Ӳв�ͼ�ж��������
abline(h=0,col="red")         ###��Ӻ�ɫˮƽ��
plot(e[-1],e[-10])
library(lmtest)
dw=dwtest(lm.in)                 ###DW����
####������ؿ˷�
library(orcutt)
cochrane.orcutt(lm.in)
pp1=1-dw/2              ###��DW�����ϵ��
                        ###corc-r ��
                        ###������

 