library(stringr)

# ---------------
# 任务一
# ---------------
setwd('C:\\Users\\小竹子\\Desktop\\统计仿真实验\\2')
dat <- read.csv('data.csv')
dat1 <- dat[!is.na(dat$price),]
s <- as.integer(str_extract_all(dat1$monthly_sales, '\\d+'))
dat1 <- cbind(dat1, s)

# 词云
library(jiebaR)
sg <- segment(paste(dat1$goodsName),worker())
fq <- freq(sg)
fq <- fq[order(fq$freq,decreasing = T),]
fq1 <- fq[1:30,]
fq1 <- fq1[-c(2,4,5,8,11,17,21,25,29),]
library(wordcloud2)
wordcloud2(fq1)



# ---------------
# 任务二
# ---------------
# 提取年龄
loc <- str_locate_all(dat1$productDescription,'适用年龄:?')
loc1 <- str_locate_all(dat1$productDescription, '周岁')
age <- vector('character',nrow(dat1))
for(i in 1:length(loc)){
  if(length(loc[[i]][,1])==0){
    age[i] <- NA
  }else{
    age[i] <- str_sub(dat1$productDescription[i], loc[[i]][,2]+2, loc1[[i]][,1]-1)
    }
}

dat1 <- cbind(dat1,age)
dat1 <- dat1[!is.na(age),]


# “新款”加入数据集
xinkuan <- vector('integer', nrow(dat1))
l_xinkuan <- str_locate_all(dat1$goodsName, '新款')
for(i in 1:length(l_xinkuan)){
  if(length(l_xinkuan[[i]][,1])==0){
    xinkuan[i] <- 0
  }else{
    xinkuan[i] <- 1
  }
}

# “中长款”加入数据集
zckuan <- vector('integer', nrow(dat1))
l_zckuan <- str_locate_all(dat1$goodsName, '中长款')
for(i in 1:length(l_zckuan)){
  if(length(l_zckuan[[i]][,1])==0){
    zckuan[i] <- 0
  }else{
    zckuan[i] <- 1
  }
}

# “修身”加入数据集
xiushen <- vector('integer', nrow(dat1))
l_xiushen <- str_locate_all(dat1$goodsName, '修身')
for(i in 1:length(l_xiushen)){
  if(length(l_xiushen[[i]][,1])==0){
    xiushen[i] <- 0
  }else{
    xiushen[i] <- 1
  }
}

# “显瘦”加入数据集
xianshou <- vector('integer', nrow(dat1))
l_xianshou <- str_locate_all(dat1$goodsName, '显瘦')
for(i in 1:length(l_xianshou)){
  if(length(l_xianshou[[i]][,1])==0){
    xianshou[i] <- 0
  }else{
    xianshou[i] <- 1
  }
}

# “韩版”加入数据集
hanban <- vector('integer', nrow(dat1))
l_hanban <- str_locate_all(dat1$goodsName, '韩版')
for(i in 1:length(l_hanban)){
  if(length(l_hanban[[i]][,1])==0){
    hanban[i] <- 0
  }else{
    hanban[i] <- 1
  }
}

# “印花”加入数据集
yinhua <- vector('integer',nrow(dat1))
l_yinhua <- str_locate_all(dat1$goodsName,'印花')
for(i in 1:length(l_yinhua)){
  if(length(l_yinhua[[i]][,1])==0){
    yinhua[i] <- 0
  }else{
    yinhua[i] <- 1
  }
}


# 将价格和销量取对数
log_price <- log(dat1$price)
log_sale <- log(dat1$s)


# 将0-1变量改为是否
xinkuan_ <- rep('是',length(xinkuan))
xinkuan_[xinkuan == 0] <- '否'

zckuan_ <- rep('是',length(zckuan))
zckuan_[zckuan == 0] <- '否'

xiushen_ <- rep('是',length(xiushen))
xiushen_[xiushen == 0] <- '否'

xianshou_ <- rep('是',length(xianshou))
xianshou_[xianshou == 0] <- '否'

hanban_ <- rep('是',length(hanban))
hanban_[hanban == 0] <- '否'

lgdata3 <- cbind(dat1, xinkuan, zckuan, xiushen, xianshou, hanban,
                 xinkuan_, zckuan_, xiushen_, xianshou_, hanban_,
                 log_price, log_sale)

# -----
# （1）销量整体的直方分布图：
# -----

yxliang <- vector('character', length(s))
yxliang[s<=150] <- '100-150'
yxliang[s>150 & s<=200] <- '150-200'
yxliang[s>200 & s<=250] <- '200-250'
yxliang[s>250 & s<=300] <- '250-300'
yxliang[s>300 & s<=400] <- '300-400'
yxliang[s>400 & s<=500] <- '400-500'
yxliang[s>500 & s<=1000] <- '500-1000'
yxliang[s>1000] <- '>1000'

df <- freq(yxliang)
df$char <- factor(df$char, levels = unique(df$char), ordered = T)
library(ggplot2)
ggplot(data = df, aes(x=char, y=freq)) +
  geom_bar(stat="identity",fill='darkseagreen') +
  xlab('月销量（笔）') + ylab('商品数量（件）')


# -----
# （2）商品价格对销量的箱线图：
# -----
jiage <- vector('character', length(lgdata3$price))
jiage[lgdata3$price<=100] <- '0-100'
jiage[lgdata3$price<=200 & lgdata3$price>100] <- '100-200'
jiage[lgdata3$price<=300 & lgdata3$price>200] <- '200-300'
jiage[lgdata3$price<=400 & lgdata3$price>300] <- '300-400'
jiage[lgdata3$price<=500 & lgdata3$price>400] <- '400-500'
jiage[lgdata3$price>500] <- '>500'

jiage <- factor(jiage, levels = c('0-100','100-200','200-300','300-400',
                                  '400-500','>500'), 
                ordered = T)

df <- data.frame(jiage, log_sale)
ggplot(df, aes(x=jiage,y=log_sale)) +     
  geom_boxplot(fill='orange',varwidth = T) + xlab('商品价格（元）') + ylab('对数销售量')

# -----
# （3）不同年龄层对月销量的影响
# -----

df <- data.frame(age = lgdata3$age, log_sale)
df$age <- factor(df$age, levels = c('17','18-24','25-29','30-34','35-39','40-49'), 
              labels = c('17周岁以下','18-24周岁','25-29周岁','30-34周岁',
                         '35-39周岁','40-49周岁'),ordered = T)
ggplot(df, aes(x=age,y=log_sale)) +     
  geom_boxplot(fill='darkseagreen',varwidth = T) + xlab('适用年龄') + ylab('对数销售量')

# -----
# （4）不同款式对销量的影响
# -----
# 显瘦
df <- data.frame(xianshou=lgdata3$xianshou_, log_sale, age=lgdata3$age)
ggplot(df, aes(x=xianshou,y=log_sale)) +     
  geom_boxplot() + xlab('适用年龄') + ylab('对数销售量') + coord_flip() +
  facet_wrap(~ age)

# 新款
df <- data.frame(xinkuan=lgdata3$xinkuan_, log_sale, age=lgdata3$age)
ggplot(df, aes(x=xinkuan,y=log_sale)) +     
  geom_boxplot() + xlab('适用年龄') + ylab('对数销售量') + coord_flip() +
  facet_wrap(~ age)

# 中长款
df <- data.frame(zckuan=lgdata3$zckuan_, log_sale, age=lgdata3$age)
ggplot(df, aes(x=zckuan,y=log_sale)) +     
  geom_boxplot() + xlab('适用年龄') + ylab('对数销售量') + coord_flip() +
  facet_wrap(~ age)

# 韩版
df <- data.frame(hanban=lgdata3$hanban_, log_sale, age=lgdata3$age)
ggplot(df, aes(x=hanban,y=log_sale)) +     
  geom_boxplot() + xlab('适用年龄') + ylab('对数销售量') + coord_flip() +
  facet_wrap(~ age)


# -----
# （5）地区的影响
# -----
# 地区变量
area <- vector('integer', nrow(lgdata3))
l_area <- str_locate_all(lgdata3$seller, '地区')
for(i in 1: length(l_area)){
  area[i] <- str_sub(lgdata3$seller[i],l_area[[i]][,2]+2,l_area[[i]][,2]+3)
} 

df <- data.frame(area,log_sale)
df <- df[(df$area == '武汉' | df$area == '东莞' | df$area == '杭州'|
           df$area == '嘉兴' | df$area == '北京'|
           df$area == '佛山' | df$area == '广州' | df$area == '深圳' |
           df$area == '天津' | df$area == '上海' | df$area == '宁波'|
           df$area == '济南'),]
df <- df[order(df, decreasing = T),]
df$area <- factor(df$area, levels = c('武汉','东莞','杭州','嘉兴','北京',
                                      '佛山','广州','深圳','天津','上海',
                                      '宁波','济南'), ordered = T)
ggplot(df, aes(x=area,y=log_sale)) +     
  geom_boxplot(fill = 'darkseagreen3',varwidth = T) + xlab('商家所在地') + ylab('对数销售量') + coord_flip()


# -----
# （6）开店时间长短
# -----
loc <- str_locate_all(lgdata3$seller,'年')
shijian <- vector('character',nrow(lgdata3))
for(i in 1:length(loc)){
  if(length(loc[[i]][,1])==0){
    shijian[i] <- '0-2年店'
  }else{
    shijian[i] <- str_sub(lgdata3$seller[i], loc[[i]][,1]-1, loc[[i]][,1]+1)
  }
}


df <- data.frame(shijian, log_sale)
df <- df[(df$shijian == '0-2年店' |
         df$shijian == '3年店' |
         df$shijian == '4年店'|
         df$shijian == '5年店'|
         df$shijian == '6年店'),]

ggplot(df, aes(x=shijian,y=log_sale)) +     
  geom_boxplot(fill='orange') + xlab('店家年数') + ylab('对数销售量') + coord_flip()

# -----
# (7)商品评价影响
# -----
wuliupf <- vector('numeric',nrow(lgdata3))
wuliupf <- lgdata3$wuliu
wuliupf[wuliupf<=4.6] <- '4.6及以下'
df <- data.frame(wuliupf,log_sale)
df <- df[complete.cases(df),]
p1 <- ggplot(df, aes(x=wuliupf,y=log_sale)) +     
  geom_boxplot(fill='darkseagreen', varwidth = T) + 
  xlab('物流评分') + ylab('') + coord_flip() + labs(title = '商品评价对销量的影响')

fuwupf <- vector('numeric',nrow(lgdata3))
fuwupf <- lgdata3$fuwu
fuwupf[fuwupf<=4.6] <- '4.6及以下'
df <- data.frame(fuwupf,log_sale)
df <- df[complete.cases(df),]
p2 <- ggplot(df, aes(x=fuwupf,y=log_sale)) +     
  geom_boxplot(fill='darkseagreen', varwidth = T) + 
  xlab('服务评分') + ylab('') + coord_flip()

miaoshupf <- vector('numeric',nrow(lgdata3))
miaoshupf <- lgdata3$miaoshu
miaoshupf[miaoshupf<=4.6] <- '4.6及以下'
df <- data.frame(miaoshupf,log_sale)
df <- df[complete.cases(df),]
p3 <- ggplot(df, aes(x=miaoshupf,y=log_sale)) +     
  geom_boxplot(fill='darkseagreen', varwidth = T) + 
  xlab('描述评分') + ylab('') + coord_flip()

library(Rmisc)
multiplot(p1, p2, p3) 


# ---------------
# 任务三
# ---------------
a <- vector('integer',length(lgdata3$age))
a[lgdata3$age == '17'] <- 0
a[lgdata3$age == '18-24'] <- 1
a[lgdata3$age == '25-29'] <- 2
a[lgdata3$age == '30-34'] <- 3
a[lgdata3$age == '35-39'] <- 4
a[lgdata3$age == '40-49'] <- 5
lgdata6 <- data.frame(sales = log_sale, price = log_price,
                      是否韩版 = lgdata3$hanban, 是否新款 = lgdata3$xinkuan,
                      是否显瘦 = lgdata3$xianshou, 是否中长款 = lgdata3$zckuan,
                      是否修身 = lgdata3$xiushen, 是否印花 = yinhua,
                      old = a)
lm <- lm(sales ~ . - sales, data = lgdata6)
summary(lm)


lm1 <- lm(sales ~ . + 是否韩版 * old + 是否中长款 * 
            old + 是否印花 * old + 是否修身 * old + 是否显瘦 * 
            old + 是否新款 * old, data = lgdata6)
summary(lm1)            


# 交互作用
ratio_han <- aggregate(hanban, by=list(lgdata3$age), 
                       function(x) mean(lgdata3$s[x==1])/mean(lgdata3$s[x==0]))
ratio_shou <- aggregate(xianshou,by=list(lgdata3$age),
                        function(x) mean(lgdata3$s[x==1])/mean(lgdata3$s[x==0]))
colnames(ratio_han) <- c('年龄','比值') 
colnames(ratio_shou) <- c('年龄','比值')
ratio_han
ratio_shou

