library(dplyr)
data_txt <-"
AK/Alaska/2/2013/1/1999/1
NV/Nevada/1/2015/1/2004/1
ME/Maine/1/2015/1/1999/1
MI/Michigan/12/2015/1/2008/1
IL/Illinois/1/2017/1/2015/1
AZB/Arizona/11/2019/1/2011/1
MT/Montana/1/2019/1/2004/1
NY/New York/3/2019/1/2016/1
NM/New Mexico/6/10000/0/2007/1
CT/Connecticut/7/10000/0/2014/1
VA/Virginia/7/10000/0/2020/1
ND/North Dakota/0/10000/0/2019/1
OK/Oklahoma/0/10000/0/2018/1
WV/West Virginia/0/10000/0/2015/1
MO/Missouri/0/10000/0/2020/1
NH/New Hampshire/0/10000/0/2016/1
MD/Maryland/0/10000/0/2015/1
AR/Arkansas/0/10000/0/2016/1
FL/Florida/0/10000/0/2016/1
HI/Hawaii/0/10000/0/2000/1
"


# data_txt <-"
# NV/Nevada/1/2015/1/2004/1
# ME/Maine/1/2015/1/1999/1
# MI/Michigan/12/2015/1/2008/1
# IL/Illinois/1/2019/1/2015/1
# AZB/Arizona/11/2019/1/2011/1
# MT/Montana/1/2019/1/2004/1
# NY/New York/3/10000/0/2016/1
# NM/New Mexico/6/10000/0/2007/1
# CT/Connecticut/7/10000/0/2014/1
# VA/Virginia/7/10000/0/2020/1
# ND/North Dakota/0/10000/0/2019/1
# OK/Oklahoma/0/10000/0/2018/1
# WV/West Virginia/0/10000/0/2015/1
# MO/Missouri/0/10000/0/2020/1
# NH/New Hampshire/0/10000/0/2016/1
# MD/Maryland/0/10000/0/2015/1
# AR/Arkansas/0/10000/0/2016/1
# FL/Florida/0/10000/0/2016/1
# HI/Hawaii/0/10000/0/2000/1
# "
geo_df <- read.delim(text = data_txt, sep = "/", header = FALSE, stringsAsFactors = FALSE)       
colnames(geo_df) <- c("sitecode","statename","month","ReferencePoint","Treatment","MMLyear","Flag")
geotable <- geo_df 
# %>% select(sitecode, everything())
# AZB/Arizona/11/2020/1/2011/1
# MT/Montana/1/2021/1/2004/1
# AK/Alaska/2/2015/1/1999/1

# alcohol_balance <- geo_df %>% filter(
#   MMLyear < 2015,
#   sitecode != AK
# ) 
# バランステストはSTUCKED用に2回やってもいいね（MMLを考慮したうえで、）
# 欠損ありVermont 7/1/2018