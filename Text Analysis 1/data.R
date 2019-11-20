setwd("C:\\Users\\小竹子\\Desktop\\新建文件夹")

## ---------------------
## ---------------------
## 任务一
## 读入comments数据
com_data=read.csv("./comments_infor.csv", header = T, fileEncoding = 'utf-8',
                  stringsAsFactors = F)
## 提取评论所在的列
comments=com_data$评论内容
## 加载R包                                     
library(jiebaRD) #用于jieba分词
library(jiebaR)  #用于jieba分词
## 指定停用词的文件名
stoppath="./stopwords.dat"
## 初始化分词器，可以在分词的时候去停用词
cutter = worker(bylines = TRUE,stop_word=stoppath) 
## 进行分词,这步会比较慢                                      
res = cutter[comments]
## 查看前6行的分词效果
head(res)


## ---------------------
## ---------------------
## 任务二
## 将text从list转换为matrix格式
text = lapply(res,as.matrix)
## 将每行文本的分词结果逐一排列起来
text = as.data.frame(do.call(rbind, text), stringsAsFactors = F) 
## 进行词频统计
freq = as.data.frame(table(text),stringsAsFactors = F) 
## 按词频个数降序排列           
freq = freq[order(-freq[,2]),]  
## 挑选前50个高频词                                
top50 = freq[1:50,]
top50


## ---------------------
## ---------------------
## 任务三
## 找出前50个词中有明确含义的属性词，其标号分别为
sele_num=c(6,7,11,14)
## 找出key_words
key_words=top50[sele_num,1]
key_words
# [1] "屏幕" "电池" "客服" "物流"

##计算所有评论总数
N=length(comments)
##计算key_words的个数
K=length(key_words)
##初始化结果矩阵，判断每个热评词是否在每条评论中出现，如果出现记为1，不出现记为0
key_mat=matrix(0,nrow=N,ncol=K)
##对每个词进行循环
for(k in 1:K)
{
  theword=key_words[k]   #找到该热评词
  num=grep(theword,comments)  #判断出现该热评词的评论标号
  key_mat[num,k]=1  #记录那些评论包括该热评词
}
##提取评论数据集com_data中的手机编号信息
phoneid=com_data$手机编号
##统计每部手机的评论总数
NO_comments=table(phoneid)
##计算手机ID的个数
I=length(unique(phoneid))
##初始化结果矩阵，每行为一部手机，每列为这部手机出现该热评词的频率
freq_mat=matrix(0,nrow=I,ncol=K)
##为每个热评词计算频率
for(k in 1:K)
{
  freq_mat[,k]=tapply(key_mat[,k],phoneid,sum)/NO_comments  #统计每部手机中，第k个热评词的频率
}

##找出手机的id
iid=unique(phoneid)
##添加手机ID，便于下面和手机的其他数据进行合并
com_reg=data.frame(iid,freq_mat)
##为com_reg进行命名
colnames(com_reg)=c("手机编号",key_words)
head(com_reg)



## ---------------------
## ---------------------
## 任务四
## 读入手机数据
phone_infor=read.csv("./phone_infor.csv", header=T, fileEncoding = 'utf-8',
                     stringsAsFactors = F)
## 使用merge将phone_infor和com_reg按照"手机编号"整合到一起
reg_data=merge(phone_infor, com_reg, by = "手机编号", all.x = TRUE)
## 计算好评率
good_freq=reg_data$好评数/reg_data$总评论数
## 建立回归模型
model=lm(good_freq~价格+品牌+屏幕尺寸+指纹识别+GPS定位+促销信息
         +屏幕+电池+客服+物流
         ,data=reg_data)
## 查看回归结果
summary(model)



## ---------------------
## ---------------------
## 任务五
## 统计句子，我们按照“，。！？”来表明一个句子
sentence=strsplit(comments,"，|。|！|？")
sentence2=unlist(sentence)
## 判断每个句子中是否包含“电池”
aa=lapply(sentence2,function(x){grep("电池",x)})
## 如果句子不包含电池，aa在相应位置的结果是integer(0)，如果可以通过计算aa每个位置的长度来判断是否出现了integer(0)
bb=lapply(aa,length)
## 将bb由list转换为向量
bb=unlist(bb)
## 提取出bb中大于0的评论编号，就是
com_dianchi=sentence2[bb>0]
head(com_dianchi)

## 对只包含电池的短句进行分词
res=cutter[com_dianchi]
## 将分词后的结果从list转换为matrix格式
res=lapply(res,as.matrix)
## 将每行文本的分词结果逐一排列起来
res = as.data.frame(do.call(rbind, res), stringsAsFactors = F) 
## 计算每个词出现的词频
freq = as.data.frame(table(res),stringsAsFactors = F) 
## 按词频个数降序排列           
freq = freq[order(-freq[,2]),]  
## 挑选前50个高频词                               
top50_dianchi = freq[1:50,]
top50_dianchi



## ---------------------
## ---------------------
## 任务六
## 找到所有包含“电池”的评论以及其对应的得分
ratings=com_data$评论得分
tnum=grep("电池",comments)
comments2=comments[tnum]
ratings2=ratings[tnum]
## 统计在出现电池的评论中，出现某个关注点的评论标号
num1=grep("耐用",comments2)
num2=grep("续航",comments2)
num3=grep("待机",comments2)
num4=grep("充电",comments2)
## 得到这些评论的相应得分
rat_NY=ratings2[num1]
rat_XH=ratings2[num2]
rat_DJ=ratings2[num3]
rat_CD=ratings2[num4]

##计算每个关注点的得分的平均分
rr1=mean(rat_NY)
rr2=mean(rat_XH)
rr3=mean(rat_DJ)
rr4=mean(rat_CD)
##计算所有包含电池的用户评论的平均分
rr=mean(ratings2)
##计算行业标准，即所有评论的平均分
themean=mean(ratings)
##将所有平均分放入一个向量并命名
ruse=c(rr1,rr2,rr3,rr4,rr)
names(ruse)=c("耐用","续航","待机","充电","电池")
####作图进行对比
library(RColorBrewer) #用于画图时调用更多颜色
barplot(ruse,col=c(brewer.pal(4, 'Greens'),"dark red"),ylim=c(0,5),ylab="平均分")
abline(h=themean)
