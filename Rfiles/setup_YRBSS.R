# 依存関係 --------------------------------------------------------------------

setwd("C:/Users/watan/OneDrive/デスクトップ/2024_spring_project")

library(readxl)
library(tidyr)
source("Rfiles/my_func.R")
source("Rfiles/RMLs.R")
library(fixest)
library(gtsummary)
library(ggplot2)
library(modelsummary)
library(broom)
library(tidyverse)
library("SciViews")
library(dplyr)
library(conflicted)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
# データの読み込み ------------------------------------------------------------

# YRBSSFiles <- list.files('./data/source/',full.names = T,pattern = "SADC")
# df <- map_dfr(YRBSSFiles,read.csv)

al <- df


# 必要な変数のみセレクト -------------------------------------------------------------

# ever系の変数も使ってみよう

# crosssection
clean_df <- al %>% dplyr::select(
  sitecode,
  sitename,
  year,
  grade,
  survyear,
  # PSU,
  record,
  age,
  sex,
  race4,
  race7,
  bmi,
  # Drinking and driving
  # q10,
  # considered suicide
  q26,
  # Made a suicide plan past 12 months
  q27,
  # Current cigarette use
  q28,
  q29,
  q32,
  # Current electronic vapor product use
  # q35,
  # Current smokeless tobacco use
  # q37,
  # All tobacco product cessation
  # q39,
  # ever use marijuana
  # q45,
  # Current alcohol use
  q41,
  # Current binge drinking
  # q42,
  # Current marijuana use
  q47,
  # Ever prescription pain medicine use
  # q49,
  # Ever cocaine use
  # q50,
  # Ever inhalant use
  # q51,
  # Ever heroin use
  # q52,
  # Ever methamphetamine use
  # q53,
  # Ever ecstasy use
  # q54,
  # depression 30days
  # q85
)


# 変数に命名 -------------------------------------------------------------------

clean_df <- clean_df %>% dplyr::filter(
  year > 2007
) %>% rename(
  suicide_consideration = 'q26',
  suicide_plan = 'q27',
  suicideattempt = 'q28',
  suicideinjury = 'q29',
  tabaco_use = 'q32',
  alcohol_use = 'q41',
  # evermarijuana = 'q45',
  marijuana_use = 'q47',
  # everpainmed = 'q49',
  # everheroin = 'q52',
)

# file_path <- "./data/source/cleandf.csv"
# write.csv(clean_df,file_path)


# ここから実行 ------------------------------------------------------------------


# clean_df <- read.csv("./data/source/cleandf.csv")
# clean_df <- clean_df %>% select(
#   -X
# )
clean_df <- inner_join(clean_df, geo_df, by = 'sitecode')
# LastTreatedは削除
# 年代に欠損がある州のデータは削除
clean_df <- clean_df %>% filter(
    year > 2009,
    year < 2021) %>% filter(
  # sitename != "Alabama (AL)",
  # sitename != "California (CA)",
  # sitename != "Colorado (CO)",
  # sitename != "Delaware (DE)",
  # sitename != "Georgia (GA)",
  # sitename != "Indiana (IN)",
  # sitename != "Iowa (IA)",
  # sitename != "Kansas (KS)",
  # sitename != "Louisiana (LA)",
  sitename != "Mississippi (MS)",
  # sitename != "Missouri (MO)",
  sitename != "Nevada (NV)",
  # sitename != "New Jersey (NJ)",
  # sitename != "Pennsylvania (PA)",
  # sitename != "South Dakota (SD)",
  # sitename != "Texas (TX)",
  # sitename != "Utah (UT)",
  # sitename != "Vermont (VT)",
  # sitename != "Wisconsin (WI)",
  # sitename != "Wyoming (WY)",
  # sitename != "Nevada (NV)",
  # sitename != "Mississippi (MS)",
  # evermarijuana != 7,
  # marijuana_use != 1,
  marijuana_use < 6,
  alcohol_use < 7,
  tabaco_use < 7,
  suicideattempt < 5,
)
# 2011データのみ欠損
# sitename != "Nevada (NV)",
# sitename != "Mississippi (MS)",

# NA消去
naclean_df <- na.omit(clean_df)
# nastucked_df <- naclean_df %>% mutate(
#   is_female = ifelse(sex == 1,1,0),
#   # race4 = as.factor(race4),
#   # race7 = as.factor(race7),
#   suicide_consideration = ifelse(suicide_consideration == 1,1,0),
#   suicide_plan = ifelse(suicide_plan == 1,1,0),
#   suicide_attempt = ifelse(suicideattempt == 1,0,0),
#   suicide_attempt = ifelse(suicideattempt == 2,1,suicide_attempt),
#   suicide_attempt = ifelse(suicideattempt == 3,3,suicide_attempt),
#   suicide_attempt = ifelse(suicideattempt == 4,5,suicide_attempt),
#   suicide_attempt = ifelse(suicideattempt == 5,6,suicide_attempt),
#   suicide_injury = ifelse(suicideinjury == 2,1,0),
#   tabaco_month = 0,
#   tabaco_month = ifelse(tabaco_use == 2,2,tabaco_month),
#   tabaco_month = ifelse(tabaco_use == 3,4,tabaco_month),
#   tabaco_month = ifelse(tabaco_use == 4,7,tabaco_month),
#   tabaco_month = ifelse(tabaco_use == 5,15,tabaco_month),
#   tabaco_month = ifelse(tabaco_use == 6,25,tabaco_month),
#   tabaco_month = ifelse(tabaco_use == 7,30,tabaco_month),
#   alcohol_month = 0,
#   alcohol_month = ifelse(alcohol_use == 2,2,alcohol_month),
#   alcohol_month = ifelse(alcohol_use == 3,4,alcohol_month),
#   alcohol_month = ifelse(alcohol_use == 4,7,alcohol_month),
#   alcohol_month = ifelse(alcohol_use == 5,15,alcohol_month),
#   alcohol_month = ifelse(alcohol_use == 6,25,alcohol_month),
#   alcohol_month = ifelse(alcohol_use == 7,30,alcohol_month),
#   marijuana_month = ifelse(marijuana_use == 1,0,0),
#   marijuana_month = ifelse(marijuana_use == 2,2,marijuana_month),
#   marijuana_month = ifelse(marijuana_use == 3,6,marijuana_month),
#   marijuana_month = ifelse(marijuana_use == 4,15,marijuana_month),
#   marijuana_month = ifelse(marijuana_use == 5,30,marijuana_month),
#   marijuana_month = ifelse(marijuana_use == 6,40,marijuana_month),
#   marijuana_dummy = ifelse(marijuana_month == 0,0,1),
#   # ever_marijuana = ifelse(evermarijuana == 1,0,1),
#   # ever_marijuana = ifelse(evermarijuana == 2,2,0),
#   # ever_marijuana = ifelse(evermarijuana == 3,6,ever_marijuana),
#   # ever_marijuana = ifelse(evermarijuana == 4,15,ever_marijuana),
#   # ever_marijuana = ifelse(evermarijuana == 5,30,ever_marijuana),
#   # ever_marijuana = ifelse(evermarijuana == 6,60,ever_marijuana),
#   # ever_marijuana = ifelse(evermarijuana == 7,100,ever_marijuana),
#   age = age + 11,
# )

nastucked_df <- naclean_df %>% mutate(
  is_female = ifelse(sex == 1,1,0),
  # race4 = as.factor(race4),
  # race7 = as.factor(race7),
  suicide_consideration = ifelse(suicide_consideration == 1,1,0),
  suicide_plan = ifelse(suicide_plan == 1,1,0),
  suicide_attempt = ifelse(suicideattempt == 1,0,0),
  suicide_attempt = ifelse(suicideattempt == 2,1,suicide_attempt),
  suicide_attempt = ifelse(suicideattempt == 3,2,suicide_attempt),
  suicide_attempt = ifelse(suicideattempt == 4,4,suicide_attempt),
  suicide_attempt = ifelse(suicideattempt == 5,6,suicide_attempt),
  suicide_injury = ifelse(suicideinjury == 2,1,0),
  tabaco_month = 0,
  tabaco_month = ifelse(tabaco_use == 2,1,tabaco_month),
  tabaco_month = ifelse(tabaco_use == 3,4,tabaco_month),
  tabaco_month = ifelse(tabaco_use == 4,7,tabaco_month),
  tabaco_month = ifelse(tabaco_use == 5,14,tabaco_month),
  tabaco_month = ifelse(tabaco_use == 6,24,tabaco_month),
  tabaco_month = ifelse(tabaco_use == 7,30,tabaco_month),
  alcohol_month = 0,
  alcohol_month = ifelse(alcohol_use == 2,1,alcohol_month),
  alcohol_month = ifelse(alcohol_use == 3,4,alcohol_month),
  alcohol_month = ifelse(alcohol_use == 4,7,alcohol_month),
  alcohol_month = ifelse(alcohol_use == 5,14,alcohol_month),
  alcohol_month = ifelse(alcohol_use == 6,24,alcohol_month),
  alcohol_month = ifelse(alcohol_use == 7,30,alcohol_month),
  marijuana_month = ifelse(marijuana_use == 1,0,0),
  marijuana_month = ifelse(marijuana_use == 2,1,marijuana_month),
  marijuana_month = ifelse(marijuana_use == 3,6,marijuana_month),
  marijuana_month = ifelse(marijuana_use == 4,14,marijuana_month),
  marijuana_month = ifelse(marijuana_use == 5,29,marijuana_month),
  marijuana_month = ifelse(marijuana_use == 6,40,marijuana_month),
  marijuana_dummy = ifelse(marijuana_month == 0,0,1),
  # ever_marijuana = ifelse(evermarijuana == 1,0,1),
  # ever_marijuana = ifelse(evermarijuana == 2,2,0),
  # ever_marijuana = ifelse(evermarijuana == 3,6,ever_marijuana),
  # ever_marijuana = ifelse(evermarijuana == 4,15,ever_marijuana),
  # ever_marijuana = ifelse(evermarijuana == 5,30,ever_marijuana),
  # ever_marijuana = ifelse(evermarijuana == 6,60,ever_marijuana),
  # ever_marijuana = ifelse(evermarijuana == 7,100,ever_marijuana),
  age = age + 11,
)


# stuckedDIDのdf作成 ---------------------------------------------------------
# Alaska 2/24/2015 California 11/9/2016 Maine 1/31/2017 Michigan 12/6/2018        Montana 1/1/2021
# Nevada 1/1/2017 Vermont 7/1/2018
df_13<- nastucked_df %>% filter(
  year < 2016
) %>% mutate(
  # 介入群
  Treatment = ifelse(sitename == "Alaska (AK)",1,0),
  # year_i =　時点効果
  year_i = paste(year,"_13"),
  reference_point = 2013,
  # year_L = 変動期間を表す
  year_L = as.numeric(year - reference_point)
)
# クロスセクションなら本来いらないかな
df_13$ID <- paste(1:nrow(df_13),"_13")

df_15<- nastucked_df %>% filter(
  year < 2018,
  sitecode != 'AK',
)%>%  mutate(
  # 介入群
  Treatment = ifelse(sitename == "Michigan (MI)",1,0),
  Treatment = ifelse(sitename == "Nevada (NV)",1,Treatment),
  Treatment = ifelse(sitename == "Maine (ME)",1,Treatment),
  # year_i =　時点効果
  year_i = paste(year,"_15"),
  reference_point = 2015,
  year_L = as.numeric(year - reference_point)
)
df_15$ID <- paste(1:nrow(df_15),"_15")

df_17<- nastucked_df %>% filter(
  year < 2020,
  sitecode != 'AK',
  sitecode != 'MI',
  sitecode != 'NV',
  sitecode != 'ME',
)%>%  mutate(
  # 介入群
  Treatment = ifelse(sitecode == "IL",1,0),
  # year_i =　時点効果
  year_i = paste(year,"_17"),
  reference_point = 2017,
  year_L = as.numeric(year - reference_point)
)
df_17$ID <- paste(1:nrow(df_17),"_17")

df_131517_list <- list(df_13,df_15,df_17)
c_stucked_df <- bind_rows(df_131517_list)

# データ整形 -------------------------------------------------------------------

# 値の挿入
c_stucked_df <- c_stucked_df %>% mutate(
  # "L-8" = ifelse(year_L == "-8",1,0),
  "L-6" = ifelse(year_L == "-6",1,0),
  "L-4" = ifelse(year_L == "-4",1,0),
  "L-2" = ifelse(year_L == "-2",1,0),
  "L0" = ifelse(year_L == "0",1,0),
  "L2" = ifelse(year_L == "2",1,0),
  "L4" = ifelse(year_L == "4",1,0)
) %>% select(
  -year
) %>% rename(
  year = year_i
) %>% filter(
  year_L > -3,
  year_L < 5,
)

s_stucked_df <- c_stucked_df %>% select(
  -grade,
  -survyear,
  -record,
  -suicideattempt,
  -suicideinjury,
  -tabaco_use,
  -alcohol_use,
  -marijuana_use,
  -month,
  -ReferencePoint,
  -MMLyear,
  -Flag,
  -reference_point,
  -year_L,
  -bmi,
  -sex
) %>% select(
  age,
  race4,
  race7,
  Treatment,
  is_female,
  suicide_consideration,
  suicide_plan,
  suicide_attempt,
  suicide_injury,
  marijuana_month,
  alcohol_month,
  tabaco_month,
  `L-2`,
  `L0`,
  `L2`
)

# pl_summary <- tbl_summary(s_stucked_df)

pl_summary <- datasummary(All(s_stucked_df) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max) ),
                       data = s_stucked_df,
                       na.rm = TRUE,
                       fmt = 2,
                       output = "default",

)
# cor(c_stucked_df$marijuana_month,c_stucked_df$is_female)

model_stucked <- fixest::feols(marijuana_month ~ i(year_L,Treatment,ref = 0) | year + sitecode ,data = c_stucked_df,cluster = c("sitecode"))
# model_stucked <- fixest::feols(`marijuana_month` ~ `L-6`:Treatment +`L-4`:Treatment +`L-2`:Treatment +`L2`:Treatment | year + sitecode ,data = c_stucked_df,cluster = c("sitecode"))
# model_stucked <- fixest::feols(`marijuana_month` ~ `L-6`:Treatment +`L-4`:Treatment +`L-2`:Treatment +`L2`:Treatment +
#                                `L-6`:Treatment:is_female +`L-4`:Treatment:is_female +`L-2`:Treatment:is_female +`L2`:Treatment:is_female + Treatment:is_female| year + is_female + sitecode + age + race7,data = c_stucked_df,cluster = c("sitecode"))
pl <- fixest::iplot(model_stucked,zero = TRUE,pt.join = TRUE)
summary(model_stucked)


etable(model_stucked)





















































t_stucked_df <- c_stucked_df %>% mutate(
  Treatment = ifelse(sitename == "Alaska (AK)",1,0),
  Treatment = ifelse(sitename == "Michigan (MI)",1,Treatment),
  Treatment = ifelse(sitename == "Illinois (IL)",1,Treatment),
  Treatment = ifelse(sitename == "Nevada (NV)",1,Treatment),
  # Treatment = ifelse(sitename == "Maine (ME)",1,Treatment),
) %>% filter(
  year_L == -0
  )






# 各t検定の結果を取得
result_marijuana_month <- t.test(marijuana_month ~ Treatment, nastucked_df) %>% tidy()
result_alcohol_month <- t.test(alcohol_month ~ Treatment, nastucked_df) %>% tidy()
result_tabaco_month <- t.test(tabaco_month ~ Treatment, nastucked_df) %>% tidy()
result_suicide_consideration <- t.test(suicide_consideration ~ Treatment, nastucked_df) %>% tidy()
result_suicide_plan <- t.test(suicide_plan ~ Treatment, nastucked_df) %>% tidy()
result_suicide_attempt <- t.test(suicide_attempt ~ Treatment, nastucked_df) %>% tidy()
result_suicide_injury <- t.test(suicide_injury ~ Treatment, nastucked_df) %>% tidy()

# 各結果をリストにまとめる
results_list <- c(
  marijuana_month = result_marijuana_month,
  alcohol_month = result_alcohol_month,
  tabaco_month = result_tabaco_month,
  suicide_consideration = result_suicide_consideration,
  suicide_plan = result_suicide_plan,
  suicide_attempt = result_suicide_attempt,
  suicide_injury = result_suicide_injury
)

all_results_df <- rbind(
  data.frame(Test = "Marijuana Month", result_marijuana_month),
  data.frame(Test = "Alcohol Month", result_alcohol_month),
  data.frame(Test = "Tabaco Month", result_tabaco_month),
  data.frame(Test = "Suicide Consideration", result_suicide_consideration),
  data.frame(Test = "Suicide Plan", result_suicide_plan),
  data.frame(Test = "Suicide Attempt", result_suicide_attempt),
  data.frame(Test = "Suicide Injury", result_suicide_injury)
)

# エクセルファイルに書き込む
write.csv(all_results_df, "./outputs/t_test_results.csv")
library(readr)
t_test_results <- read_csv("outputs/t_test_results.csv")
View(t_test_results)


# names(c_stucked_df)


# table <- as.data.frame(table(c_stucked_df$sitename, c_stucked_df$ReferencePoint))
# View(table)
# write.csv(table, "./outputs/st__table.csv")


# sun & abraham -----------------------------------------------------------

# sunab_df <- nastucked_df %>% 
#   mutate(
#     year_treated = ifelse(sitecode == 'AK'& year_i > 2016,1,Treatment),
  # )

# T.test(変数 ~ Treatment,data)