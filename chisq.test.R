# 独立性检验
# ====================

###Part 1转化和输入数据###
# iconv工具对文件的编码进行转换。Convert encoding of given files from one encoding to another.
#
# iconv -l 查看支持的编码转换
# iconv -f GBK -t utf-8 survey.csv -o survey1.csv
# -f From某个编码     -t To某个编码    -o输出到文件

###Part 2将数据制成列联表###
dat <- read.csv("sample.csv")
#dat
#   SEX RES
#1    F   Y
#2    F   N
#3    M   Y
#..   ..  ..
#生成表的函数table()
#table(dat)等价于dat %>% table; %>%是管道运算符,让左边的数据流到右边

library(tidyverse)
library(dplyr)
#如果dplyr加载太早,早于stats,所以最后stats::filter覆盖了dplyr::filter

dat2 <- dat %>% table

###Part 3独立性检验###
chisq.test(dat2) #dat2 %>% chisq.test
# Pearson's Chi-squared test with Yates' continuity correction
# data:  dat2
# X-squared = 0.15476, df = 1, p-value = 0.694
# 皮尔逊卡方检验，已进行叶氏连续性修正
# 数据名: dat2
# 卡方值 = 0.15476, 自由度 = 1, 概率 = 0.694

# 统计学以卡方值为基准判断列联表中的数值是否存在偏差,此处的偏差指"男女对A候选人的支持率是否不同",
# 由于卡方值很小,所以认为表中不存在偏差值.

# p-value的数值表示出现这么大偏差的概率，这个概率到不到5%为判断基准。如果不足5%，就认为它存在显著偏差
# 存在显著偏差就是指这种偏差绝非偶然

survey <- read.csv("survey1.csv")
# Error in read.table(file = file, header = header, sep = sep, quote = quote,  : 
# 列的数目比列的名字要多

#加入出现报错的情况
survey <- read.csv("survey1.csv",fileEncoding = "utf-8") #等价于survey <- read.csv(file.choose(),fileEncoding = "utf-8")

table1 <- survey %>% select(立场,回答6) %>% table; table1


# 列联表的图
library(ggplot2)
table1 %>% as.data.frame %>% ggplot(aes(x = 立场,y = Freq, fill = 回答6)) + geom_bar(stat="identity") + ylab("人数")

# as.data.frame(table1)
# 立场        回答6    Freq
# 1 店主       促销     11
# 2 顾客       促销     39
# 3 店主 服务态度好      9
# 4 顾客 服务态度好     35
# 5 店主       活动     38
# 6 顾客       活动     11
# 7 店主   商品齐全     42
# 8 顾客   商品齐全     18

survey %>% select(立场,回答6) %>% table %>% chisq.test

# 1.生成立场与回答6的列联表
# 2.零假设"立场与回答6的回答相互独立" --> "即店主与顾客的回答没有偏差"
# 3.独立性检验(卡方检验)
# 4.观察结果中P值发现有显著偏差 --> "抛弃零假设"
# 5.立场与回答6不相互独立 --> 店主和顾客对商业街的期待有所不同

survey %>% select(立场,回答7) %>% table %>% chisq.test
