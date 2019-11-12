## ---R version 3.6.1
## ---zmr
setwd("C:/Users/zengmingrong/Desktop/11.1脱敏处理")
{
library(tidyverse)
library(dplyr)
library(dslabs)
library(ggplot2)
library(gganimate)
library(gifski)
library(png)
}
# 数据准备
data_pre<-data.frame(read.csv("Sales amount.csv",header = T))
# 更改列名为英文名称，R语言对中文列名不友好
colnames(data_pre)<-c("time_interval","department","order_num","amount")
# 以一级部门为准，半点的时间跨度，统计成交金额
data_pre2 <- data_pre%>% 
  mutate(minu=time_interval*30)%>% 
  mutate(clock=paste(minu%/%60,ifelse(minu%% 60==0,"00",minu%% 60),sep=":"))%>%  
  group_by(clock,department)%>% 
  summarise(amount_v1=sum(amount,na.rm = TRUE))
# clock格式为char,先转化为时间格式，然后按照%H:%M输出，让后续动图有序输出
data_pre2$clock<-format(strptime(data_pre2$clock,"%H:%M"),"%H:%M")
# 数据清理，一级部门因子排序：只对数据排序不会改变图表排序
data_pre2=subset(data_pre2,data_pre2$department=="GG"|
                   data_pre2$department=="JJ"|
                   data_pre2$department=="MM"|
                   data_pre2$department=="SS")
data_pre2$department<-factor(data_pre2$department,levels = c("GG","MM","JJ","SS"))
# 处理好的文件存在本地备份
write.table(data_pre2,"data_pre2.csv",sep=",",quote = F,row.names = F)
# make plot
theme_set(theme_gray())
p <- data_pre2%>%
  ggplot(aes(department, y=amount_v1/10000,col = department,fill=department)) +
  transition_manual(clock)+       
  geom_bar(stat="identity", position="dodge",alpha = 0.8) +
  geom_hline(yintercept = 0, size = 1, colour="#333333")+
  coord_cartesian(ylim = c(0, 100000)) + 
  coord_flip()+
  geom_label(aes(x = department, y = amount_v1/10000+2000, label = round(amount_v1/10000, 0)),
             colour = "white", 
             fill = NA, label.size = NA, 
             family="Helvetica", 
             size = 6)+
  
  xlab("department") +
  ylab("amount/'0000'") + 
  geom_text(aes(x=1, y=20000, label=clock), cex=20, color="grey") +
  labs(title = "Business group real-time transaction amount",
       subtitle = "Based on the first department erery half hour")+
  theme(plot.margin = margin(9, 0, 9, 0),
        plot.title=element_text(family="Helvetica",size=18,face = "bold",color="#222222"),
        plot.subtitle=element_text(family="Helvetica",size=16,color="#222222"),
        axis.title=element_text(family = "Helvetica", size = 12,face = "bold",color = "#222222"),
        axis.text=element_text(family = "Helvetica", size = 12,color = "#222222"),
        legend.text=element_text(family = "Helvetica", size = 12,color = "#222222"))
# 调整动画播放速度，播放速度=duration/fps,fps多少帧,duration时间长度
# animate 帮助见animate.gganim
g=animate(p,fps = 50,duration=50)
# 保存,end
anim_save("Sales amount .gif",g)
