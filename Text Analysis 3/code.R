## 测试：《破冰行动》同名小说分析
## 姓名：张笑竹 
## 学号：201618070114


## -----------------------------------------
## -----------------------------------------
## 清除工作环境
cat("\014");rm(list=ls())

## 设置工作目录
setwd("C:\\Users\\小竹子\\Desktop\\新建文件夹\\测试\\破冰行动 测试")
## 读入package
# 载入需要的包 
library(stringr)         # 数据清洗tidyverse包 
library(ggplot2)         # 绘图ggplot2包 
library(jiebaR)          # 文本分词jiebaR包 
library(wordcloud2)      # 词云wordcloud2包 
library(igraph)          # 网络图igraph包
library(stringr)


## -----------------------------------------
## -----------------------------------------
## 任务一
pobing <- readLines("破冰行动.txt")
## 删去段落中的空行
pobing1 <- c()
for(i in 1:length(pobing))
{
  if(pobing[i]!="")
  {
    pobing1 <- c(pobing1,pobing[i])
  }
}
## 删去段落中的标题行
title_index=grep("正文",pobing1)
pobing1 <- pobing1[-title_index]
pobing1 <- pobing1[-1]
## 删去段落中的空格
for(i in 1:length(pobing1))
{
  pobing1[i]=str_trim(pobing1[i],'left')
}

## 展示处理后数据的前6条
head(pobing1)




## -----------------------------------------
## -----------------------------------------
## 任务二
## 给分词器设置用户词典，并对文章给分词
## 导入词典
dictpath <- "./pbxd_user.txt" 
## 导入停用词词库 
stoppath <- "./stopword.txt" 
## 设置分词器 
cutter <- worker(bylines = TRUE, user = dictpath, stop_word=stoppath,"tag")
## 进行分词
pobing_fenci <- cutter[pobing1]
## 将pobing_fenci从list转换为matrix格式
pobing_fenci <- lapply(pobing_fenci,as.matrix)



## 统计人物出现的频率
## 读入主要人物列表 
roles <- c("(李维民)|(维民)","(马云波)|(云波)","(李飞)","(陈文泽)|(文泽)","(赵嘉良)|(嘉良)",
           "(马雯)","(蔡永强)|(永强)","(陈光荣)|(光荣)","(林耀东)|(耀东)","(林水伯)|(水伯)",
           "(陈珂)","(宋杨)","(蔡杰)","(蔡军)","(林宗辉)|(宗辉)","(林耀华)|(耀华)",
           "(林胜武)|(胜武)","(林胜文)|(胜文)","(周琳林)","(方天逸)|(天逸)","(陆童)","(何瑞龙)|(瑞龙)",
           "(苏建国)|(建国)","(苏康)","(左兰)","(陈岩)","(王志雄)|(志雄)")
## 找到人物出现的段落 
role_para <- sapply(roles, grepl, pobing1)
## role_para列名的更改
role_name = c("李维民","马云波","李飞","陈文泽","赵嘉良",
              "马雯","蔡永强","陈光荣","林耀东","林水伯",
              "陈珂","宋杨","蔡杰","蔡军","林宗辉","林耀华",
              "林胜武","林胜文","周琳林","方天逸","陆童","何瑞龙",
              "苏建国","苏康","左兰","陈岩","王志雄")
colnames(role_para) <- role_name
## 构建数据框，最后一列统计主要地名在全书出现的次数 
role_count = data.frame(role = factor(colnames(role_para), 
                                      levels = role_name), 
                        count = colSums(role_para))
head(role_count)
#          role count
# 李维民 李维民  1159
# 马云波 马云波   596
# 李飞     李飞  2029
# 陈文泽 陈文泽   127
# 赵嘉良 赵嘉良   763
# 马雯     马雯   521



## 对出现频率不小于100的人物绘制词频柱状图
role_count_100 <- role_count[which(role_count$count>=100),]

ggplot(role_count_100, aes(x = role, y = count, fill = role)) + #aes参数说明对x和y进行图形映射，映射方式为fill 
  geom_bar(stat = "identity", width = 0.75) +                   #geom_bar指绘制条形图。stat = "identity"表示绘制原始数据，不进行统计变换。 
  xlab("") + #x轴标题为空 ylab("频数") +                        #y轴标题为频数 #theme调整不与数据有关的图的元素的函数 
  theme(axis.text=element_text(size=17),                        #调整文本字体大小 
        axis.title=element_text(size=20,face="bold"),           #调整标题字体
        axis.text.x = element_text(angle = 90, hjust = 1),       #vjust 调整标签竖直位置 
        legend.position="none")                             #图例的位置 设置为无图例





## -----------------------------------------
## -----------------------------------------
## 任务三：人物关系网络分析
## 提取每一句中的人名
NET=c()
for(i in 1: nrow(role_para))
{
  rr=c()
  for(j in 1:ncol(role_para))
  {
    if(role_para[i,j]==TRUE)
    {
      rr=c(rr,role_name[j])
    }
  }
  k=length(rr)
  if(k==2)
  {
    NET=rbind(NET,rr)
  }
  if(k==3)
  {
    NET=rbind(NET,c(rr[1],rr[2]))
    NET=rbind(NET,c(rr[1],rr[3]))
    NET=rbind(NET,c(rr[2],rr[3]))
  }
}
colnames(NET) <- c("from", "to") 

## 进行频率统计
NET.freq = unique(NET)
freq=rep(0,nrow(NET.freq))
for(i in 1:nrow(NET))
{
  for(j in 1:nrow(NET.freq))
  {
    if(NET[i,1]==NET.freq[j,1] && NET[i,2]==NET.freq[j,2])
    freq[j]=freq[j]+1
  }
}
NET.freq = cbind(NET.freq,freq)

g <- graph_from_data_frame(NET.freq, directed=FALSE, vertices=role_name)
plot(g)





## -----------------------------------------
## -----------------------------------------
## 任务四：绘制李飞、李维民、赵嘉良三人全文出现密度曲线
## 找出对应角色出现的段落序号（role_para即可）
para <- 1:nrow(role_para)

para.lifei <- para[role_para[,3]]
n1 <- length(para.lifei)

para.liweimin <- para[role_para[,1]]
n2 <- length(para.liweimin)

para.zhaojialiang <- para[role_para[,5]]
n3 <- length(para.zhaojialiang)

d <- c(para.lifei, para.liweimin, para.zhaojialiang)
id <- c(rep("李飞",n1),rep("李维民",n2),rep("赵嘉良",n3))
id <- as.factor(id)

dd <- data.frame(d,id)
ggplot(dd, aes(x = d, fill = id,alpha = 0.1)) + 
  geom_density()




## -----------------------------------------
## -----------------------------------------
## 任务五：词云
Ciyun=c()
j1=1
j2=50
while(j2<=nrow(role_para))
{
  if(sum(role_para[j1:j2,'林耀东'])!=0 && sum(role_para[j1:j2,'林宗辉'])!=0)
  {
    for(k in j1:j2)
    {
      Ciyun=c(Ciyun,as.vector(pobing_fenci[[k]]))
    }
  }
  j1=j1+50
  j2=j2+50
}

freq = as.data.frame(table(Ciyun),stringsAsFactors = F) 
freq = freq[order(-freq[,2]),] 

freq <- subset(freq,str_detect(freq$Ciyun,"一|么|有|以|来|去|这|那|我|们|不|是|在|的|开|关|为|然|几乎|最|里|外|上|下|了|只")==F)
freq <- subset(freq,str_detect(freq$Ciyun,"己|已|着|个|道|刘|完|看|于|事|见|马|出|进|能|觉|定|今|天|之|前|后|时|如|说|声|眼睛|应|目|大|瞬间|刚|希望|就|连忙|到")==F)
freq <- freq[nchar(freq$Ciyun)>=2,]
head(freq,30)

wordcloud2(freq[1:200,],size = 1, minRotation = -pi/3, maxRotation = pi/3,
           rotateRatio = 0.8,fontFamily = "微软雅黑", 
           color = "random-light")

## 去掉人物
freq1 <- freq[20:200,]
wordcloud2(freq1, size = 0.4, minRotation = -pi/3, maxRotation = pi/3,
           rotateRatio = 0.8,fontFamily = "微软雅黑", 
           color = "random-light") 
