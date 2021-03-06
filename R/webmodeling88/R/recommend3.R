rm(list = ls())
# data <- read.table('C:/Users/45543/Desktop/汕头大学/data.csv', sep = ',', header = TRUE,
#                    stringsAsFactors = FALSE)
data<- hdata
###筛选浏览记录2次以上的IP
h <- table(data$realIP)
totalIP <- names(h[h >2])

###将数据划分为测试集与训练集
# index <- 1:round(length(totalIP)*0.8)
# trainIP <- totalIP[index]
# testIP <- totalIP[-index]
# train <- data[data$realIP %in% trainIP,]
# test <- data[data$realIP %in% testIP,]
# write.csv(train, 'traindata.csv')
# write.csv(test, 'testdata.csv')
train <- read.csv('traindata.csv')
test <- read.csv('testdata.csv')
###构建用户浏览矩阵
myip <- unique(train$realIP)
myurl <- unique(train$fullURL)
user <- matrix(0, nrow = length(myip), ncol = length(myurl))
rownames(user) <- myip
colnames(user) <- myurl
for(i in 1:nrow(train)){
  user[as.character(train[i,1]),train[i,2]] <- 1
}

###根据杰卡德系数公式构造物品相似度矩阵
rela <- matrix(0, nrow = length(myurl), ncol = length(myurl))
for(i in 1:length(myurl)){
  for(j in 1:length(myurl)){
    a <- sum(rowSums(user[,c(i,j)])==2)
    b <- sum(rowSums(user[,c(i,j)])!=0)
    rela[i,j] <- a/b
  }
}
rownames(rela) <- myurl
colnames(rela) <- myurl
for(i in 1:length(myurl)){
  rela[i,i] <- 0
}

###构建列表：每个IP的浏览记录为一个字表
temp <- list()
tempip <- unique(test[,1])
for(i in tempip){
  temp[[as.character(i)]] <- test[test$realIP %in% i,2]
}

###利用测试集，根据用户的第一条浏览记录进行推荐
ma <- matrix(0,nrow = nrow(test),ncol = 2)
rem <- cbind(test,ma) #推荐算法评判矩阵
for(i in 1:nrow(test)){
  if(test[i,2] %in% colnames(rela)){ # 判断浏览的网页在不在物品相似度矩阵中
    re <- which.max(rela[test[i,2],]) # 下标
    re <- names(re) # URL全称
    rem[i,3] <- re # 推荐的网页
    rem[i,4] <- re %in% temp[[as.character(test[i,1])]] # 推荐的网页是否在IP用户的浏览记录中
  }
}

###计算推荐正确率
sum(rem[,3]==0) #浏览的网页不在物品相似度矩阵中的总数
p <- sum(rem[,4])/(nrow(rem)-sum(rem[,3]==0)) #推荐正确的概率
p
