EDA <- function (x)
  { 
    par(mfrow=c(2,2))              # ͬʱ��4��ͼ
    hist(x)                        # ֱ��ͼ 
    dotchart(x)                    # ��ͼ
    boxplot(x,horizontal=T)        # ��ʽͼ
    qqnorm(x);qqline(x)            # ��̬����ͼ
    par(mfrow=c(1,1))              # �ָ���ͼ
  }