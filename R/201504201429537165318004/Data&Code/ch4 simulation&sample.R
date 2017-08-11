runif(5,0,1)
runif(3,1,3)
runif(5)
set.seed(1)
runif(5)
set.seed(1)
runif(5)
set.seed(2)
runif(5)
#simulation 100 uniform distribution
par(mfrow=c(1,1))
x=runif(100)
hist(x,prob=T,col=gray(0.5),main="uniform on [0,1]")
curve(dunif(x,0,1),add=T,col="red")
##check the uniform number iid
Nsim=10^3
x=runif(Nsim)
x1=x[-Nsim]
x2=x[-1]
par(mfrow=c(1,3))
hist(x,prob=T,col=gray(0.3),main="uniform on [0,1]")
curve(dunif(x,0,1),add=T,col="red")
plot(x1,x2,col="red")
acf(x)
 ###normal 
rnorm(5,10,5) #产生 5 个均值为 10 标准差为 5 的正态分布随机数
rnorm(5)
par(mfrow=c(1,1))
x=rnorm(1000)
hist(x,prob=T,main="normal mu=0,sigma=1")
curve(dnorm(x),add=T,col="red")
##exponential 
x=rexp(100,1/10) # 生成 100 个均值为 10 的指数分布随机数 
hist(x,prob=T,col=gray(0.9),main="exp(1/10)") 
curve(dexp(x,1/10),add=T,col="red") #添加指数分布密度�?
#compare inverse transform and rexp
 Nsim=10^4
 U=runif(Nsim)
 X=-log(U)
 Y=rexp(Nsim)
 par(mfrow=c(1,2))
 hist(X,freq=F,main="Exp from Uniform")
 curve(dexp(x,1),add=T,col="red")
 hist(Y,freq=F,main="Exp from R")
 curve(dexp(x,1),add=T,col="red")

##binomial
n=1; p=0.5
rbinom(10,n,p)
n=10; p=0.5
rbinom(5,n,p)

 par(mfrow=c(2,2))
 p=0.25
 for( n in c(10,20,50,500))
{ x=rbinom(100,n,p)
  hist(x,prob=T,main=paste("n =",n))
  xvals=0:n 
  points(xvals,dbinom(xvals,n,p),type="h",lwd=3)
}
 par(mfrow=c(1,1))
##poisson
rpois(10,7)
###r d p q
pnorm(2)
pnorm(1.96)
pnorm(0)
dnorm(0)
qnorm(0.95)
plot(function(x) dnorm(x),ylab="",-5,5)
plot(function(x) dnorm(x, log = TRUE), ylab="",-60, 60,
     main = "log { Normal density }")
curve(log(dnorm(x)), add = TRUE, col = "red", lwd = 2)
plot(function(x) dnorm(x,1,2),ylab="",-10,10)
plot(function(x) dt(x,3),ylab="",-5,5)
curve(dnorm(x),add=T,col=2)
plot(function(x) df(x,1,1),ylab="",-10,10)
curve(df(x,1,5),add=T,col=2)
curve(df(x,1,50),add=T,col=3)
curve(df(x,5,5),add=T,col=4)
###########################
##mulitnormal random number
# MASS mvrnorm
library(MASS)
Sigma <- matrix(c(10,3,3,2),2,2)
Sigma
x=mvrnorm(n=1000, rep(0, 2), Sigma)
head(x)
var(x)

##Mvnorm package
install.packages("mvtnorm")
library(mvtnorm)
sigma <- matrix(c(10,3,3,2), ncol=2)
x <- rmvnorm(n=500, mean=c(1,2), sigma=sigma)
head(x)
colMeans(x)
var(x)
plot(x)
par(mfrow=c(1,1))
x <- rmvnorm(n=500, mean=c(1,2), sigma=sigma, method="chol")
colMeans(x)
var(x)

plot(x)
######pmvnorm
(mean <- rep(0, 5))
(lower <- rep(-1, 5))
(upper <- rep(3, 5))
(corr <- diag(5))
corr[lower.tri(corr)] <- 0.5
corr[upper.tri(corr)] <- 0.5
corr
(prob <- pmvnorm(lower, upper, mean, corr))
#####rmvt
x <- rmvt(n=100, sigma = diag(2), df = 3)
plot(x)
sigma=diag(2)+1
sigma
x2<-rmvt(n=1000,df=5,sigma=sigma)
head(x2)
plot(x2)






################################
###sample
sample(c("H","T"),10,rep=T)
sample(1:6,10,rep=T)
sample(100,10)

dice=as.vector(outer(1:6,1:6,paste))
sample(dice,10,replace=T)

data(faithful)
faithful
head(faithful)
attach(faithful)
sample(eruptions,10,rep=T)     

Sample=sample(eruptions,272,rep=T) 
par(mfrow=c(1,2))
hist(eruptions,breaks=25)
#par(new=T);plot(density(eruptions))  
hist(Sample,breaks=25);
#par(new=T);plot(density(Sample)) 
par(mfrow=c(1,1))
detach()

n=10;p=0.25
x=rbinom(1,n,p)
y=(x-n*p)/sqrt(n*p*(1-p))

m =1000                     
n=1000; p=0.25
b=rbinom(m,n,p)            
x=(b-n*p)/sqrt(n*p*(1-p))  
hist(x,prob=T,main=paste("n =",n))
curve(dnorm(x),add=T,col=2)

m =100                         
n = 10; p = 0.25
z = rbinom(m,n,p)                 
x = (z-n*p)/sqrt(n*p*(1-p))          
hist(x,prob=T,breaks=20,main=paste("n =",n))
curve(dnorm(x),add=T)             

sim.clt <- function (m=100,n=10,p=0.25)
{ z = rbinom(m,n,p)               
  x = (z-n*p)/sqrt(n*p*(1-p))        
  hist(x,prob=T,breaks=20,main=paste("n =",n,"p =",p))
  curve(dnorm(x),-4,4,add=T,col=2)             
}

par(mfrow=c(2,2))
sim.clt()            
sim.clt(1000,100)        
sim.clt(1000,1000)      
sim.clt(1000,10000)      
par(mfrow=c(1,1))

par(mfrow=c(2,2))
x=rnorm(100,0,1)
qqnorm(x,main="N(0,1)")
qqline(x)
x=rnorm(100,10,25);qqnorm(x,main="N(10,25)");qqline(x)
x=rexp(100,1/10);qqnorm(x,main="exp(0.1)");qqline(x)
x=runif(100,0,1);qqnorm(x,main="U(0,1)");qqline(x)
par(mfrow=c(1,1))

sim.fun <- function (m,f,...)  
{
  sample <- 1:m
  for (i in 1:m) {
    sample[i] <- f(...)
  }
  sample
}

f <- function (n=10,p=0.5) {
  S = rbinom(1,10,p)
  (S- n*p)/sqrt(n*p*(1-p) )
}
x=sim.fun(1000,f(n=1000))
hist(x,prob=T)

f1=function(n=10) (mean(runif(n))-1/2)/(1/sqrt(12*n))
x=sim.fun(1000,f1(n=1000))   
hist(x,prob=T,main="n=1000")
curve(dnorm(x),-4,4,add=T,col=2)
#lines(dnorm(x))   

f <- function(n=100,mu=10) (mean(rexp(n,1/mu))-mu)/(mu/sqrt(n))
x=sim.fun(10,f)

f2 <- function(n=10,mu=0,sigma=1){
  r=rnorm(n,mu,sigma)
  (mean(r)-mu)/(sigma/sqrt(n))
}
x = sim.fun(1000,f2)      
hist(x,breaks=10,prob=T)

x = sim.fun(1000,f,30,5,2)   
hist(x,breaks=10,prob=T)

f <- function(n,mu=10)(mean(rexp(n,1/mu)-mu))/(mu/sqrt(n))
x=seq(-3,3,0.01)
par(mfrow=c(2,2))
hist(sim.fun(100,f,1,10),prob=T,main="n=1")
points(x,dnorm(x,0,1),type="l")
hist(sim.fun(100,f,5,10),prob=T,main="n=5")
points(x,dnorm(x,0,1),type="l")
hist(sim.fun(100,f,30,10),prob=T,main="n=30")
points(x,dnorm(x,0,1),type="l")
hist(sim.fun(100,f,100,10),prob=T,main="n=100")
points(x,dnorm(x,0,1),type="l")
par(mfrow=c(1,1)

