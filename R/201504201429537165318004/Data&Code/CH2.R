setwd("~/Kupan.localized/è®¡ç®—æœºåœ¨ç»Ÿè®¡ä¸­çš„åº”ç”¨/Rä¹?")
#########CH2
## 
x <- seq(0, 1, by = 0.2) 
y <- seq(0, 1, by = 0.2) 
y[4] 
x[3] 
1 - x[3] 
y[4] > 1 - x[3] 
##
seq(0, 1, 0.1) 
seq(0, 1, 0.1) == c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1) 
0.3 - 0.7 + 0.4 == 0 
sqrt(2)^2 == 2 
##vector
x <-c(1,3,5,7,9)
x
c(1,3,5,7,9) -> y
y
z <- c("male","female","female","male","male")
z
##
assign("w",c(1,3,5,7,9))
w
##
x * y
x / y
x^2
y^x
5%/%3
5%%3
##
length(x)
mode(w)
min(x)
max(x)
range(x)
which.min(x)
which.max(x)
##
(t <- 1:10)
(r <- 5:1)
2*1:5
##
seq(1,10,2)
seq(1,10,by=0.1)
seq(10,1,-1)
seq(from=1,by=2,length=10)
##
rep(c(1,3),4) 
rep(c(1,3),each=4)
rep(1:3,rep(2,3))
##
sum(t)
mean(t)
median(t)
var(t)
sd(t)
##
y <- c(2,6,7,3,5)
sort(y)
sort(y,decreasing=T)
rev(y)
rank(y)
prod(y)
append(y,10:15,after=3)
append(y,10:15)
##
x <- c(1,3,5)
x[2]
(c(1,2,3)+4)[2]
x[2] <- 10
x
x[c(1,3)] <- c(9,11)
x
##
x <- c(1,3,5)
x < 4
x[x<4]
z <- c(-1,1:3,NA)
z[is.na(z)] <- 0
z
z <- c(-1,1:3,NA)
y <- z[!is.na(z)]
y
##
y <- numeric(length(x))
y[x<0] <- 1-x[x<0]
y[x>=0] <- 1+x[x>=0]
##
x <- 1:10
x[-(1:5)]
x[-c(1,3)]
##metrix
matrix(1:12,nrow=4,ncol=3)
matrix(1:12,nrow=4,ncol=3,byrow=T)
##
(A <- matrix(1:12,nrow=3,ncol=4))
t(A)
##
A <- B <- matrix(1:12,nrow=3)
A+B
A-B
##
3*A
##
B <- t(A)
A%*%B
##
(A <- matrix(1:16,nrow=4))
diag(A)
diag(diag(A))
diag(3)
##
(A <- matrix(rnorm(16),4,4))
solve(A)
solve(A) %*% A
##
(A <- diag(4)+1)
(A.eigen <- eigen(A,symmetric=T))
A.eigen$vectors %*% diag(A.eigen$values) %*% t(A.eigen$vectors)
##
chol(A)
t(chol(A)) %*% chol(A)
crossprod(chol(A),chol(A))
##
prod(diag(chol(A))^2)
det(A)
chol2inv(chol(A))
solve(A)
##
(A <- matrix(1:18,3,6))
(A.svd <- svd(A))
A.svd$u %*% diag(A.svd$d) %*% t(A.svd$v)
##
A <- matrix(1:16,4,4)
qr(A)
qr.R(qr(A))
qr.Q(qr(A))
qr.Q(qr(A)) %*% qr.R(qr(A))
##
(A <- matrix(1:4,2,2))
(B <- matrix(rep(1,4),2,2))
kronecker(A,B)
##
(A <- matrix(1:12,3))
dim(A)
nrow(A)
ncol(A)
rowSums(A)
rowMeans(A)
colSums(A)
colMeans(A)
##
library("strucchange")
A <- matrix(rnorm(16),4)
solveCrossprod(A,method="qr")
solveCrossprod(A,method="chol")
solveCrossprod(A,method="solve")
solve(crossprod(A,A))
##
A <- matrix(1:16,4)
lower.tri(A)
lower.tri(A,diag=T)
upper.tri(A)
upper.tri(A,diag=T)
A[lower.tri(A)]=0
A
A[upper.tri(A)]=0
A
##
A <- matrix(1:16,4)
row(A)
col(A)
A[row(A)<col(A)]=0
A
##
det(A)
##
vec <- function(x){
  t(t(as.vector(x))) 
}
(A <- matrix(1:12,3,4))
vec(A)
c(A)
##
library("fMultivar")
x <- 1:20
tslag(x,1:4,trim=F)
tslag(x,1:4,trim=T)
##
apply(A,2,sum)
apply(A,2,mean)
apply(A,2,var)
apply(A,2,sd)
rbind(A,A)
cbind(A,A)
####
A=matrix(1:12,3,4)
A
A[2,3]
A[2,]
A[,3]
A[1:3,2]

##
sex <- factor(c("ç”?","å¥?","ç”?","ç”?","å¥?"))
h <- c(165,175,180,170,165)
tapply(h,sex,mean)
##
rbind(B,B)
cbind(B,B)
##array
(xx <- array(1:24,c(3,4,2)))    # äº§ç”Ÿç»´æ•°ä¸?(3,4,2)
xx[2,3,2]
xx[2,1:3,2]
xx[,2,]
dim(xx)
##factor
y <- c("female","male","male","female","female","female","male") 
(f <- factor(y))
levels(f)
##
score <- c("B","C","D","B","A","D","A")
(score_o <- ordered(score,levels=c("D","C","B","A")))
##list
x<-c(1,1,2,2,3,3,3)
y<-c("Å®","ÄÐ","ÄÐ","Å®","Å®","Å®","ÄÐ")
z<-c(80,85,92,76,61,95,83)

(LST <- list(class=x,sex=y,score=z))
LST[3]
LST[[3]]
LST[[2]][1:3]
LST$score
LST$sc
##data.frame
(student <- data.frame(x,y,z))

(student <- data.frame(class=x,sex=y,score=z))
row.names(student) <- c("Íõx","ÕÅx","ÕÔx","Áõx","»Æx","Ëïx","Àîx")
student
student[1:3,2]
student[,3]
student[["sex"]]
student$sex
student$score

attach(student)
score
class
detach(student)
score

data(PlantGrowth)
unPG<-unstack(PlantGrowth)
sPG=stack(unPG)
###
x=scan()
x=scan(file="student.txt")
##Data operation
ls()
rm(LST)
data(package="SemiPar")
data(cars)

##
s1=read.table("student.txt")
s1=read.table("student.txt",header=T)
S2=read.csv(file="student.csv")
S3=read.xlsx(file="student.xls")
write.table(S2,file="SS2.txt",3)
write.csv(S2,file="SS2.csv",3)
S2
S2[,-1]
S2[,c(1,3)]
##
std <- function(x) {
  sqrt(var(x))
}
##
welcome <- function() {
  print("welcome to use R")
}
welcome()
##
welcome.sb <- function(names="Mr fang"){
  print(paste("welcome", names,"to use R"))
}
welcome.sb()
##
plot.f <- function(f,a,b,...){
  xvals=seq(a,b,length=100)
  plot(xvals,f(xvals),type="l",...)
}
plot.f(sin,0,2*pi)  
##
my.average <- function(x) {
  sum(x)/length(x)
}
my.average(c(1,2,3))


