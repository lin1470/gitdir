EDA <- function (x)
  { 
    par(mfrow=c(2,2))              # 同时做4个图
    hist(x)                        # 直方图 
    dotchart(x)                    # 点图
    boxplot(x,horizontal=T)        # 箱式图
    qqnorm(x);qqline(x)            # 正态概率图
    par(mfrow=c(1,1))              # 恢复单图
  }
