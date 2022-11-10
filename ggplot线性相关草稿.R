setwd("C:/Users/a/Desktop/数据分析/皮肤油脂/xlsx")

library(ggplot2)
library(ggpubr)
#oilcl <- read.csv('测量三次.csv', sep = ',', header = TRUE , encoding = "UTF-8")
btest <- openxlsx::read.xlsx("测量三次.xlsx", sheet="总", rowNames=T)

atest <- openxlsx::read.xlsx("测量三次.xlsx", sheet="油脂恢复水平-测三次", rowNames=F)

#搞线性相关的
#ctest <- craw[,c("","")]
#dtest <- draw[,c("","")]
#data <- merge(ctest,dtest)
adata <- btest[,c("前额正中部第一次测试值","前额正中部第二次测试值","前额正中部第三次测试值","前额正中部三次测试值总和")]
p <- ggplot(data = adata,aes(x=前额正中部第一次测试值,y=前额正中部三次测试值总和))+
  geom_point(pch=16, color="blue",size=1)+
  geom_smooth(method="lm",color="red",linetype=2,level=.95)+       
  labs(title ="Automobile Data", x="前额正中部第一次测试值", y="前额正中部三次测试值总和")

p+theme(text=element_text(size=20,  family="mono", face = "bold"))
#times new roma#sans/宋体#mono/雅黑             

  +panel.grid.major=element_blank(),panel.grid.minor=element_blank())
  axis.title.x =element_text(size=20,  family="mono"
          
          
          
          )
  
  #scale_x_continuous()+
  #scale_y_continuous()+
  


ggplot(data = mtcars,aes(x=rank,y=salary))+
  geom_boxplot(fill="cornflowerblue",color="black",notch=TRUE)+
  geom_point(position="dodge", color="blue",size=2,alpha=.5)+
  geom_rug(side="1", color="black")+
  labs(title ="Automobile Data", x="Weight", y="Miles Per Gallon")

#热图
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

#相关性分析及pearson分析

mydata <- atest[,c("","","","")]
cormat <- round(cor(mydata),2)
head(cormat)

ggplot(data = cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()








