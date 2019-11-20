

# ---------------
# 任务一
# ---------------
setwd('C:\\Users\\小竹子\\Desktop\\统计仿真实验\\案例难度：___\\1. 双十一销售数据分析')
datDesc <- read.csv('double11_addvar.csv')

datDesc[datDesc$dif == max(datDesc$dif), c('name','price_sale','dif')]

hist(log(datDesc$dif),col = 'orange', border = 'white', xlab = '对数销售量', ylab = '频数',
     main = '双十一期间销量分布直方图')

# ---------------
# 任务二
# ---------------
datDesc$category

s <- aggregate(datDesc$dif, by = list(datDesc$category), FUN = mean)
ss <- s$x
names(ss) <- s$Group.1
ss <- sort(ss, decreasing = T)
barplot(height = ss, col = 'orange', border = 'white', ylim = c(0,60),
        xlab = '商品类型', ylab = '双十一期间平均销量')
text(0.75, 60.3, '59.3')
text(1.9, 31.4, '30.4')
text(3.1, 9.8, '8.8')
text(4.3, 6.6, '5.6')
text(5.5 ,3.2, '2.1')

# ---------------
# 任务三
# ---------------
s1 <-c(mean(datDesc$dif[datDesc$双十一红包 == 0]),
       mean(datDesc$dif[datDesc$双十一红包 == 1]),
       mean(datDesc$dif[datDesc$满减券 == 0]),
       mean(datDesc$dif[datDesc$满减券 == 1]),
       mean(datDesc$dif[datDesc$其他优惠 == 0]),
       mean(datDesc$dif[datDesc$其他优惠 == 1]))
leibie <- factor(c(rep('双十一红包',2), rep('满减券',2),rep('其他优惠',2)), 
                 levels = c('双十一红包', '满减券','其他优惠'), ordered = T)
youwu <- factor(rep(c('不包含该优惠','包含该优惠'),3), 
                levels = c('不包含该优惠','包含该优惠'), ordered = T)

barplot(s1 ~ youwu + leibie, beside = T, ylim = c(0,40), col = c('skyblue2','orange'),border=F,
        legend = F, xlab = '', ylab = '双十一期间平均销量')
legend(7,40, legend = c('不包含该优惠','包含该优惠'),bty='n',col = c('skyblue2','orange'),pch=8)
text(1.5,20.9,'19.9')
text(2.5,32.7,'31.7')
text(4.5,20.7,'19.7')
text(5.5,24.1,'23.1')
text(7.5,23.8,'22.8')
text(8.5,5.8,'4.8')


# ---------------
# 任务四
# ---------------
# 提取女装类
nz <- datDesc[datDesc$category=='女装' & datDesc$dif >0,]

# 进行分词
library(jiebaR)

sg <- segment(paste(nz$name),worker())
fq <- freq(sg)
fq <- fq[order(fq$freq,decreasing = T),]
head(fq,10)

nvzhuang_key <- fq$freq[1:10]
names(nvzhuang_key) <- fq$char[1:10]
nvzhuang_key

barplot(nvzhuang_key[-1], ylim = c(0,50), xlab='女装类商品名称热词',
        ylab='词频', col=c(rep('salmon',3), rep('lightpink',3),rep('yellow2',3)),
        border=F)
text(0.75,46,'44')
text(1.95,37,'35')
text(3.2,34,'32')
text(4.3,32,'30')
text(5.5,24,'22')
text(6.7,23,'21')
text(7.9,17,'15')
text(9.1,16,'14')
text(10.3,16,'14')

# ---------------
# 任务五
# ---------------
Y <- vector('integer',nrow(datDesc))
Y[datDesc$dif > 0] <- 1
attach(datDesc)
datReg <- data.frame(Y, category, inventory, price_cut, price_sale, 
                     des_score, quality_score, service_score, 包邮, 双十一,
                     新款, 韩版, 英伦, 欧美, 冬装, 礼盒, 明星, 均码,通勤, 
                     学院, 情侣, 双十一红包, 其他优惠, dis_num1, dis_strength1,
                     easy_satisfy1)
m1 <- glm(Y ~., family = binomial(), data = datReg)
mstep <- step(m1)

m2<- glm(formula = Y ~ category + price_cut + price_sale + service_score + 
      包邮 + 韩版 + 冬装 + 学院 + 情侣 + dis_strength1, family = binomial(), 
      data = datReg)
summary(m2)

# 画ROC曲线
Y_P <- predict(m2,data = datReg)
library(pROC)
roc_m <- roc(Y,Y_P)
plot(roc_m)             
