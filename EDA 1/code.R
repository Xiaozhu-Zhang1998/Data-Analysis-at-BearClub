
# -----------
# 任务一
# -----------
setwd('C:\\Users\\小竹子\\Desktop\\统计仿真实验\\1\\1. 模块1--探索性数据分析\\案例难度：____\\2. 北美旅游产品选择攻略')
library(openxlsx)
travel_dat <- read.xlsx('travel.xlsx', sheet = 1, startRow = 1, colNames = T)
travel_dat <- travel_dat[,c('产品名称','旅游方式','供应商', '等级', '景点个数', 
                            '交通', '用餐', '自由活动', '总评价', '出游人数', '出发地',
                            '周日报价', '周一报价', '周二报价', '周三报价', '周四报价',
                            '周五报价','周六报价','旅游线路')]
colnames(travel_dat) <- c('Product', 'TravelMethod', 'Agency', 'Star',
                          'Place', 'Traffic', 'Meal', 'FreeActivitie', 'Evaluate',
                          'Sale', 'Depart', 'SunPrice', 'MonPrice', 'Tuesprice', 'WedPrice',
                          'ThusPrice', 'Friprice', 'Satprice', 'Routine')

# 去除产品名称中带有“健康医疗”以及从国内出发（出发地为上海或北京）的样本。
library(stringr)
r1 <- as.integer(rownames(travel_dat[str_detect(travel_dat$Product,'健康医疗'),]))
travel_dat <- travel_dat[-r1,]
r2 <- as.integer(rownames(travel_dat[str_detect(travel_dat$Depart, '北京|上海'),]))
travel_dat <- travel_dat[-r2,]
head(travel_dat)

#                                             Product
# 1                                                                                           美国洛杉矶+旧金山6日5晚跟团游(3钻)·七大主题项目任选二【限时特惠】含必付
# 2                                                            美国西海岸+洛杉矶+拉斯维加斯+旧金山+黄石国家公园10日9晚跟团游(3钻)·大峡谷+羚羊彩穴+主题项目选一 含必付
# 3 美国黄石国家公园+洛杉矶+拉斯维加斯+盐湖城5日半自助游·大提顿国家公园、布莱斯国家公园·(5天)美西黄石经济游：绝美黄石公园、秀丽大提顿国家公园、布莱斯国家公园奇景不断
# 4                                                                                         美国洛杉矶+旧金山+硅谷5日4晚跟团游·17里湾优胜美地二选一【含主题乐园门票】
# 5                                                                                              美国洛杉矶+拉斯维加斯+旧金山14日13晚半自助游·1号公路+6大国家公园自驾
# 6                                                                                              美国拉斯维加斯+洛杉矶7日6晚跟团游(3钻)·七大主题公园任选三+携程大礼包
# TravelMethod                   Agency         Star                          Place        Traffic         Meal    FreeActivitie
# 1       跟团游         供应商：熊大国旅       5晚3钻   共7个景点，包含7个经典景点：   暂无交通信息 暂无用餐信息      自由活动1次
# 2       跟团游         供应商：熊大国旅       9晚3钻 共14个景点，包含12个经典景点： 行车时长38小时 暂无用餐信息      自由活动1次
# 3     半自助游 供应商：途风（熊大旗下） 暂无酒店信息  共12个景点，包含5个经典景点：   暂无交通信息 暂无用餐信息 暂无自由活动信息
# 4       跟团游 供应商：Namei Group Inc.       4晚3钻  共11个景点，包含9个经典景点：  行车时长6小时 暂无用餐信息      自由活动1次
# 5     半自助游         供应商：熊大国旅 暂无酒店信息 共54个景点，包含15个经典景点： 行车时长33小时     42次自理 暂无自由活动信息
# 6       跟团游         供应商：熊大国旅       6晚3钻   共6个景点，包含6个经典景点：   暂无交通信息 暂无用餐信息      自由活动1次
# Evaluate      Sale Depart SunPrice MonPrice Tuesprice WedPrice ThusPrice Friprice Satprice Routine
# 1 4.3<U+00A0>分  39人出游 洛杉矶 ￥4796起 ￥4796起  ￥4796起 ￥4496起  ￥4496起 ￥4496起 ￥4496起  西海岸
# 2 3.8<U+00A0>分 136人出游 洛杉矶 ￥6885起 ￥6885起  ￥6885起 ￥6585起                    ￥6585起  西海岸
# 3 3.0<U+00A0>分  42人出游 洛杉矶 实时计价                    ￥2970起  ￥2970起          ￥2970起  西海岸
# 4 3.3<U+00A0>分  27人出游 洛杉矶 ￥3058起 ￥3058起  ￥3058起 ￥3058起  ￥3058起 ￥3058起 ￥3058起  西海岸
# 5 5.0<U+00A0>分  13人出游 洛杉矶 ￥8898起 ￥8898起  ￥8898起 ￥8898起  ￥8898起 ￥8898起 ￥8898起  西海岸
# 6 5.0<U+00A0>分  27人出游 洛杉矶 ￥5047起 ￥5047起  ￥5047起 ￥4747起  ￥4747起 ￥4747起 ￥4747起  西海岸

# -----------
# 任务二
# -----------
l_Sun <- str_extract_all(travel_dat$SunPrice, '[0-9]+[0-9]')
l_Mon <- str_extract_all(travel_dat$MonPrice, '[0-9]+[0-9]')
l_Tue <- str_extract_all(travel_dat$Tuesprice, '[0-9]+[0-9]')
l_Wed <- str_extract_all(travel_dat$WedPrice, '[0-9]+[0-9]')
l_Thu <- str_extract_all(travel_dat$ThusPrice, '[0-9]+[0-9]')
l_Fri <- str_extract_all(travel_dat$Friprice, '[0-9]+[0-9]')
l_Sat <- str_extract_all(travel_dat$Satprice, '[0-9]+[0-9]')
l_Sun <- str_extract_all(travel_dat$SunPrice, '[0-9]+[0-9]')


l2c <- function(list){
  v <- vector('integer', length(list))
  for(i in 1:length(list)){
    if(length(list[[i]])==0){
      v[i] <- NA
    }else{
      v[i] <- list[[i]]
    }
  }
  return(as.integer(v))
}

v_Sun <- l2c(l_Sun)
v_Mon <- l2c(l_Mon)
v_Tue <- l2c(l_Tue)
v_Wed <- l2c(l_Wed)
v_Thu <- l2c(l_Thu)
v_Fri <- l2c(l_Fri)
v_Sat <- l2c(l_Sat)

dat1 <- data.frame(v_Sun, v_Mon, v_Tue, v_Wed, v_Thu, v_Fri, v_Sat)
logic <- vector('logical',nrow(dat1))
for(i in 1:nrow(dat1)){
  logic[i] <- sum(is.na(dat1[i,]))
}

dat1 <- dat1[logic != 7,]
index <- as.integer(rownames(dat1))

# 求平均值
price <- apply(dat1, 1, function(x) mean(x,na.rm = T))
travel_dat <- travel_dat[index,]
travel_dat <- cbind(travel_dat, price)

# 画出直方图
library(ggplot2)
ggplot(data = travel_dat, aes(log(1+price)) ) + 
  geom_histogram(bins=35) +
  scale_fill_manual(values = c('yellow')) +
  xlab('产品价格（对数变换）') +
  ylab('频数')

# -----------
# 任务三
# -----------
travel_dat$Place
P <- str_extract_all(travel_dat$Place, '\\d+')

AllPlace <- vector('integer', length(P))
ClassicPlace <- vector('integer', length(P))

for(i in 1:length(P)){
  if(length(P[[i]])==0){
    AllPlace[i] <- NA
    ClassicPlace[i] <- NA
  }else{
    AllPlace[i] <- P[[i]][1]
    ClassicPlace[i] <- P[[i]][2]
  }
}
AllPlace <- as.numeric(AllPlace)
ClassicPlace <- as.numeric(ClassicPlace)

travel_dat <- cbind(travel_dat, AllPlace, ClassicPlace)

library(dplyr)
AllPlacesGroup <- vector('character',length(P))
AllPlacesGroup[AllPlace <= 9] <- '(0,9]'
AllPlacesGroup[AllPlace > 9 & AllPlace <= 16] <- '(9,16]'
AllPlacesGroup[AllPlace > 16 & AllPlace <= 25] <- '(16,25]'
AllPlacesGroup[AllPlace > 25 & AllPlace <= 77] <- '(25,77]'
AllPlacesGroup[AllPlace > 77] <- NA
AllPlacesGroup <- factor(AllPlacesGroup, levels = c('(0,9]','(9,16]','(16,25]','(25,77]',NA),
                         ordered = T)

travel_dat <- cbind(travel_dat, AllPlacesGroup)
summarise(group_by(travel_dat,AllPlacesGroup),mean(price), na.rm = T)


# -----------
# 任务四
# -----------
datef <- function(x){
  a <- sum(is.na(x[1]) + is.na(x[7]))
  b <- sum(is.na(x[2]) + is.na(x[3]) + is.na(x[4]) +
           is.na(x[5]) + is.na(x[6]))
  return(list(a=a,b=b))
}

Date <- vector('character', nrow(dat1))
for(i in 1:nrow(dat1)){
  l = datef(dat1[i,])
  if(l[[1]]==2){
    Date[i] <- '仅工作日'
  }
  if(l[[2]]==5){
    Date[i] <- '仅周末'
  }
  if(l[[1]]!=2 & l[[2]]!=5){
    Date[i] <- '工作日和周末'
  }
}

travel_dat <- cbind(travel_dat, Date)
summarise(group_by(travel_dat,Date),mean(price))

# -----------
# 任务五
# -----------
loc <- str_locate_all(travel_dat$Star,'钻')

Star2 <- vector('character',nrow(travel_dat))
for(i in 1:length(loc)){
  if(length(loc[[i]][,1])==0){
    Star2[i] <- '无信息'
  }else{
    m <- max(as.integer(str_sub(travel_dat$Star[i],loc[[i]][,1]-1,loc[[i]][,2]-1)))
    Star2[i] <- paste(m, '钻', sep='')
  }
}

Star2 <- factor(Star2, levels = c('2钻','无信息','3钻','4钻','5钻'), ordered = T)

travel_dat <- cbind(travel_dat, Star2)
boxplot(log(price)~Star2, data = travel_dat)
ggplot(data = travel_dat, aes(x = Star2,y = log(price))) + geom_boxplot() +
  xlab('') + ylab('产品价格（对数变换）')
