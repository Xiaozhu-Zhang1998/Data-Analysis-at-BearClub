## 清除工作环境
cat("\014");rm(list=ls())

## 设置工作目录
setwd("C:\\Users\\小竹子\\Desktop\\新建文件夹\\第二天")
## 读入数据

## -------------------
## -------------------
## 任务一
langya <- readLines("琅琊榜.txt")
## 搜索出现冒号（注意是中文环境下的冒号）的段落
## 搜索中文冒号出现的段落
para_word=grep("：",langya)
## 提取提取说话的段落
word <- langya[para_word]
## 展现数据的前10行
word[1:5]


## -------------------
## -------------------
## 任务二
## 载入R包：jiebaRD 
library(jiebaRD) 
## 载入R包：jiebaR 
library(jiebaR)
## 导入词典
dictpath <- "./langya_words_sogou.txt" 
## 导入停用词词库 
stoppath <- "./stopwords.dat" 
## 设置分词器 
cutter <- worker(bylines = TRUE,user = dictpath,stop_word=stoppath,"tag")
## 读入词性列表 
choose_tags <- read.csv("tags.csv", header = F, stringsAsFactors = F, fileEncoding = 'utf-8')
## 找到词性所在列
choose_tags <- as.matrix(choose_tags[,1])

## 搜索说话中有梅长苏的段落(可以将梅长苏替换为相应的人)
meichangsu_word_index <- grep("梅长苏|宗主", word)
## 取出梅长苏说的话
meichangsu_word <- word[meichangsu_word_index]
## 设置分词环境,按行分词，自定义词典，规定停用词，标注词性
meichangsu <- cutter[meichangsu_word]
## 提取每个词的词性
meichangsu_tag <- lapply(meichangsu,names)

## 按照词性进行筛选
## 将meichangsu从list转换为matrix格式
text <- lapply(meichangsu,as.matrix)
## 将meichangsu_tag从list转换为matrix
meichangsu_tag <- lapply(meichangsu_tag,as.matrix)
## 将每行文本的分词结果逐一排列起来
text_all <- as.data.frame(do.call(rbind, text), stringsAsFactors = F)
## 将标注的词性结果逐一排列起来 
tag_all <- as.data.frame(do.call(rbind, meichangsu_tag), stringsAsFactors = F)
## 找出tag中属于choose_tags中任意一个词性的小标
tnum <- apply(choose_tags,1,function(x) {return(which(tag_all[,1]==x))})
## 将tnum从list变为vector 
tnum <- unlist(tnum)
## 得到筛选后的tag 
tag_all2 <- tag_all[tnum,1]
## 得到筛选后的text
text_all2 <- text_all[tnum,1]
## 将删选后的tag和text合并到一起 
tmp <- cbind(text_all2,tag_all2)
## 将tmp数据集变为data.frame形式 
tmp <- as.data.frame(tmp)
##转换为字符串
tmp$text_all2 <- as.character(tmp$text_all2)
## 转换为字符串 
tmp$tag_all2 <- as.character(tmp$tag_all2)
## 去重，词频与词性一一对应 
tmp <- unique(tmp)
## 进行词频统计 
freq <- as.data.frame(table(text_all2),stringsAsFactors = F)
#将词频与词性merge到一起 
test <- merge(freq, tmp, by ="text_all2", all.x = T)
## 按词频个数降序排列 
test <- test[order(-test[,2]),]
## 展现前6行 
head(test)


## -------------------
## -------------------
## 任务三
##载入R包：ggplot2 
library(ggplot2) 
## 读入主要地点列表 
roles <- readLines("主要地点.txt") 
## 出场频次
roles1 <- paste0(" (", gsub(" ", ") | (", roles), ") ")
## 找到地点列表前7的地点出现的段落 
role_para <- sapply(roles1[1:7], grepl, langya)
## 选取"金陵","地道","悬镜司","苏宅","天牢","琅琊阁","靖王府"做为主要的地名 
main_roles <- c("金陵","地道","悬镜司","苏宅","天牢","琅琊阁","靖王府")
## 讲role_para的列命改为main_roles 
colnames(role_para) <- main_roles
## 构建数据框，最后一列统计主要地名在全书出现的次数 
role_count = data.frame(role = factor(colnames(role_para), 
                                      levels = c("金陵","地道","悬镜司","苏宅","琅琊阁","天牢","靖王府")), 
                        count = colSums(role_para))

ggplot(role_count, aes(x = role, y = count, fill = role)) + #aes参数说明对x和y进行图形映射，映射方式为fill 
  geom_bar(stat = "identity", width = 0.75) +               #geom_bar指绘制条形图。stat = "identity"表示绘制原始数据，不进行统计变换。 
  xlab("") + #x轴标题为空 ylab("频数") +                    #y轴标题为频数 #theme调整不与数据有关的图的元素的函数 
  theme(axis.text=element_text(size=17),                    #调整文本字体大小 
        axis.title=element_text(size=20,face="bold"),       #调整标题字体
        axis.title.x = element_text(vjust=-2),              #vjust 调整标签竖直位置 
        legend.position="none")                             #图例的位置 设置为无图例


## -------------------
## -------------------
## 任务四
## 取出第一章 
content_num <- grep("第一卷", langya) ## 搜索第一卷出现的段落
## 取出第一个数字作为第一卷的开头 
n1 <- content_num[1]
## 测量content_num的长度，作为第一卷的结尾 
n2 <- content_num[length(content_num)]
## 取出第一卷内容 
content <- langya[n1:n2]

## user是自定义词库，stop_word停用词词库 
chapt1 <- cutter[content]
## 提取每一个词的词性 
chapt1_tag <- lapply(chapt1,names)
##按照词性进行筛选 
chapt1 <- lapply(chapt1,as.matrix) 
## 将meichangsu_tag从list转换为matrix 
chapt1_tag <- lapply(chapt1_tag,as.matrix)
## 转化为data.frame 
chapt1_all <- as.data.frame(do.call(rbind, chapt1), stringsAsFactors = F)
## 转化为data.frame 
chapt1_tag_all <- as.data.frame(do.call(rbind, chapt1_tag), stringsAsFactors = F)
## 找出tag中属于choose_tags中任意一个词性的小标 
num <- apply(choose_tags,1,function(x) {return(which(chapt1_tag_all[,1]==x))})
## 将tnum从list变为vector 
num <- unlist(num)
## 得到筛选后的tag 
tag_all <- chapt1_tag_all[num,1]
## 得到筛选后的text 
text_all <- chapt1_all[num,1]
## 将删选后的tag和text合并到一起 
tmp <- cbind(text_all,tag_all)
## 将tmp数据集变为data.frame形式 
tmp = as.data.frame(tmp)
## 转换为字符串 
tmp$text_all <- as.character(tmp$text_all)
## 转换为字符串
tmp$tag_all <- as.character(tmp$tag_all)
## 词频与词性一一对应 
tmp <- unique(tmp)

## 进行词频统计 
freq <- as.data.frame(table(text_all),stringsAsFactors = F) 
test <- merge(freq, tmp, by ="text_all", all.x = T)
## 按词频个数降序排列
test <- test[order(-test[,2]),]

## 提取出名词的词性标志 
mingci <- test[which(test$tag_all=="n"|test$tag_all=="ns"|test$tag_all=="nsf"|
                       test$tag_all=="nt"|test$tag_all=="nz"|test$tag_all=="nl"|
                       test$tag_all=="ng"),]
## 对选出的词进行第二次筛选 
## 找出每个词的长度 
Len <- nchar(mingci$text_all)
## 选出长度大于1的词
## 找出长度大于1的词的下标 
num <- which(Len>1)
## 得到筛选后的tag和text 
text_1 <- mingci[num,1:2]
## 显示前6行 
head(text_1)



## -------------------
## -------------------
## 任务五
## 读入主角名单，可以自己创建
roles <- readLines("主角名单.txt")
## 人物出场频次
roles1 <- paste0("(", gsub(" ",")|(",roles),")")
## 每个主要人物出现的段落
role_para <- sapply(roles1[1:7], grepl, langya)
main_roles <- c("梅长苏","景琰","霓凰","景睿","飞流","蒙挚","誉王")
colnames(role_para) <- main_roles
role_count <- data.frame(role = factor(colnames(role_para), 
                                       levels = c("梅长苏","景琰","景睿","誉王","飞流","蒙挚","霓凰")), 
                         count = colSums(role_para))
ggplot(role_count, aes(x = role, y = count, fill = role)) + 
  geom_bar(stat = "identity", width = 0.75) + 
  xlab("") + 
  ylab("频数") + 
  theme(axis.text=element_text(size=17), 
        axis.title=element_text(size=20,face="bold"), 
        axis.title.x = element_text(vjust=-2), 
        legend.position="none")
