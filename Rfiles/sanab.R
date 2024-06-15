# 依存関係 --------------------------------------------------------------------

setwd("C:/Users/watan/OneDrive/デスクトップ/2024_spring_project")

library(readxl)
source("Rfiles/my_func.R")
source("Rfiles/setup_YRBSS.R")
source("Rfiles/RMLs.R")
library(fixest)
library(gtsummary)
library(ggplot2)
library(modelsummary)
# library(confilcted)

# データの読み込み ------------------------------------------------------------
source("Rfiles/RMLs.R")
clean_df <- read.csv("./data/source/cleandf.csv")
clean_df <- clean_df %>% select(
  -X
)
clean_df <- inner_join(clean_df, geo_df, by = 'sitecode')
clean_df09 <- clean_df %>% filter(
  year > 2009,
  # sitecode != 'AK',
  # year < 2021
  ) %>% filter(
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
    # marijuana_use < 6,
    # alcohol_use < 7,
    # tabaco_use < 7,
    # suicideattempt < 5,
  )
# NA消去
naclean_df <- na.omit(clean_df09)



# 値の挿入
# naclean_df <- naclean_df %>% mutate(
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

naclean_df <- naclean_df %>% mutate(
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

# 雑だけど、T検定2011 はめちゃくちゃバランスしてる
t_sunab_df <- naclean_df %>% filter(
  year == 2013 |year == 2011
)


# 各t検定の結果を取得
result_age <- t.test(age ~ Treatment, t_sunab_df) %>% tidy()
result_is_female <- t.test(is_female ~ Treatment, t_sunab_df) %>% tidy()
result_marijuana_month <- t.test(marijuana_month ~ Treatment, t_sunab_df) %>% tidy()
result_alcohol_month <- t.test(alcohol_month ~ Treatment, t_sunab_df) %>% tidy()
result_tabaco_month <- t.test(tabaco_month ~ Treatment, t_sunab_df) %>% tidy()
result_suicide_consideration <- t.test(suicide_consideration ~ Treatment, t_sunab_df) %>% tidy()
result_suicide_plan <- t.test(suicide_plan ~ Treatment, t_sunab_df) %>% tidy()
result_suicide_attempt <- t.test(suicide_attempt ~ Treatment, t_sunab_df) %>% tidy()
result_suicide_injury <- t.test(suicide_injury ~ Treatment, t_sunab_df) %>% tidy()

# 各結果をリストにまとめる
results_list <- c(
  age = result_age,
  is_female = result_is_female,
  marijuana_month = result_marijuana_month,
  alcohol_month = result_alcohol_month,
  tabaco_month = result_tabaco_month,
  suicide_consideration = result_suicide_consideration,
  suicide_plan = result_suicide_plan,
  suicide_attempt = result_suicide_attempt,
  suicide_injury = result_suicide_injury
)

all_results_df <- rbind(
  data.frame(Test = "Age", result_age),
  data.frame(Test = "Is Female", result_is_female),
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

# year(時点効果),Treatment(グループ固定効果),year_L,referecnce_point(cohort)をつくる。


naclean_df <- naclean_df %>%mutate(
  # year_L = 変動期間を表す
  year_L = as.numeric(year - ReferencePoint)
)



baseline_df13 <- naclean_df %>% filter(
  year < 2015,
  year > 2009,
  year_L == 0 |year_L == -2|year_L == -7989|year_L == -7987,
)


baseline_df15 <- naclean_df %>% filter(
  year > 2011,
  year < 2017,
  year_L == 0 |year_L == -2|year_L == -7987|year_L == -7985,
)


baseline_df17 <- naclean_df %>% filter(
  year > 2013,
  year < 2019,
  year_L == 0 |year_L == -2|year_L == -7985|year_L == -7983,
)

baseline_df19 <- naclean_df %>% filter(
  year > 2015,
  year < 2021,
  year_L == 0 |year_L == -2|year_L == -7983|year_L == -7981,
)

df_1319_list <- list(baseline_df13,baseline_df15,baseline_df17,baseline_df19)
df_1319 <- bind_rows(df_1319_list)


# 各t検定の結果を取得
result_age <- t.test(age ~ Treatment, df_1319) %>% tidy()
result_is_female <- t.test(is_female ~ Treatment, df_1319) %>% tidy()
result_marijuana_month <- t.test(marijuana_month ~ Treatment, df_1319) %>% tidy()
result_alcohol_month <- t.test(alcohol_month ~ Treatment, df_1319) %>% tidy()
result_tabaco_month <- t.test(tabaco_month ~ Treatment, df_1319) %>% tidy()
result_suicide_consideration <- t.test(suicide_consideration ~ Treatment, df_1319) %>% tidy()
result_suicide_plan <- t.test(suicide_plan ~ Treatment, df_1319) %>% tidy()
result_suicide_attempt <- t.test(suicide_attempt ~ Treatment, df_1319) %>% tidy()
result_suicide_injury <- t.test(suicide_injury ~ Treatment, df_1319) %>% tidy()

# 各結果をリストにまとめる
results_list <- c(
  age = result_age,
  is_female = result_is_female,
  marijuana_month = result_marijuana_month,
  alcohol_month = result_alcohol_month,
  tabaco_month = result_tabaco_month,
  suicide_consideration = result_suicide_consideration,
  suicide_plan = result_suicide_plan,
  suicide_attempt = result_suicide_attempt,
  suicide_injury = result_suicide_injury
)

all_results_df <- rbind(
  data.frame(Test = "Age", result_age),
  data.frame(Test = "Is Female", result_is_female),
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

naclean_df <- naclean_df %>% filter(
  year_L != 6,
  year_L != 4,
  year_L != -8,
  year_L != -6,
  year_L != -4,
)

t_naclean_df <- naclean_df%>% select(
  age,
  race4,
  race7,
  Treatment,
  year_L,
  # ReferencePoint,
  is_female,
  suicide_consideration,
  suicide_plan,
  suicide_attempt,
  suicide_injury,
  marijuana_month,
  alcohol_month,
  tabaco_month,
)



pl_summary <- datasummary(All(t_naclean_df) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max) ),
                          data = t_naclean_df,
                          na.rm = TRUE,
                          fmt = 2,
                          output = "default",

)

t_sunab_df <- t_naclean_df %>% filter(
  year_L != -7979,
  year_L != 2
)

# 各t検定の結果を取得
result_age <- t.test(age ~ Treatment, t_sunab_df) %>% tidy()
result_is_female <- t.test(is_female ~ Treatment, t_sunab_df) %>% tidy()
result_marijuana_month <- t.test(marijuana_month ~ Treatment, t_sunab_df) %>% tidy()
result_alcohol_month <- t.test(alcohol_month ~ Treatment, t_sunab_df) %>% tidy()
result_tabaco_month <- t.test(tabaco_month ~ Treatment, t_sunab_df) %>% tidy()
result_suicide_consideration <- t.test(suicide_consideration ~ Treatment, t_sunab_df) %>% tidy()
result_suicide_plan <- t.test(suicide_plan ~ Treatment, t_sunab_df) %>% tidy()
result_suicide_attempt <- t.test(suicide_attempt ~ Treatment, t_sunab_df) %>% tidy()
result_suicide_injury <- t.test(suicide_injury ~ Treatment, t_sunab_df) %>% tidy()

# 各結果をリストにまとめる
results_list <- c(
  age = result_age,
  is_female = result_is_female,
  marijuana_month = result_marijuana_month,
  alcohol_month = result_alcohol_month,
  tabaco_month = result_tabaco_month,
  suicide_consideration = result_suicide_consideration,
  suicide_plan = result_suicide_plan,
  suicide_attempt = result_suicide_attempt,
  suicide_injury = result_suicide_injury
)

all_results_df <- rbind(
  data.frame(Test = "Age", result_age),
  data.frame(Test = "Is Female", result_is_female),
  data.frame(Test = "Marijuana Month", result_marijuana_month),
  data.frame(Test = "Alcohol Month", result_alcohol_month),
  data.frame(Test = "Tabaco Month", result_tabaco_month),
  data.frame(Test = "Suicide Consideration", result_suicide_consideration),
  data.frame(Test = "Suicide Plan", result_suicide_plan),
  data.frame(Test = "Suicide Attempt", result_suicide_attempt),
  data.frame(Test = "Suicide Injury", result_suicide_injury)
)
write.csv(all_results_df, "./outputs/t_test_results.csv")
library(readr)
t_test_results <- read_csv("outputs/t_test_results.csv")
View(t_test_results)



table <- as.data.frame(table(naclean_df$sitename, naclean_df$ReferencePoint))
View(table)
write.csv(table, "./outputs/sunab__table.csv")


# two-way fixed （event-study）で推定 -------------------------------------------------------


res_twfx = feols(log(marijuana_month) ~ i(year_L,Treatment,ref = 0) | year + sitecode ,data = naclean_df,cluster = c("sitecode"))
etable(res_twfx)
pl <- fixest::iplot(res_twfx,zero = TRUE,pt.join = TRUE,drop = "[[:digit:]]{4}")
table(naclean_df$year_L)

# SANABで推定 ----------------------------------------------------------------

# res_sunab = feols(marijuana_month ~ sunab(ReferencePoint, year) | year+ sitecode, naclean_df,cluster = "sitecode")
res_sunab = feols(marijuana_month ~ sunab(ReferencePoint, year, ref.p = 0) | year+ sitecode, naclean_df,cluster = "sitecode")
# res_sunab = feols(marijuana_month ~ sunab(ReferencePoint, year, ref.p = 0) | year+ sitecode, naclean_df,cluster = "sitecode")
etable(res_sunab)
pl <- fixest::iplot(res_sunab,zero = TRUE,pt.join = TRUE)


# 
# 
# clean_df1921 <- clean_df %>% filter(
#   sitecode != 'AK',
#   sitecode != 'MI'
#   # year < 2021
# ) %>% filter(
#   # sitename != "Alabama (AL)",
#   # sitename != "California (CA)",
#   # sitename != "Colorado (CO)",
#   # sitename != "Delaware (DE)",
#   # sitename != "Georgia (GA)",
#   # sitename != "Indiana (IN)",
#   # sitename != "Iowa (IA)",
#   # sitename != "Kansas (KS)",
#   # sitename != "Louisiana (LA)",
#   # sitename != "Mississippi (MS)",
#   sitename != "Missouri (MO)",
#   # sitename != "Nevada (NV)",
#   # sitename != "New Jersey (NJ)",
#   # sitename != "Pennsylvania (PA)",
#   # sitename != "South Dakota (SD)",
#   # sitename != "Texas (TX)",
#   # sitename != "Utah (UT)",
#   # sitename != "Vermont (VT)",
#   # sitename != "Wisconsin (WI)",
#   # sitename != "Wyoming (WY)",
#   sitename != "Nevada (NV)",
# )
# # NA消去
# naclean_df <- na.omit(clean_df1921)
# 
# # year(時点効果),Treatment(グループ固定効果),year_L,referecnce_point(cohort)をつくる。
# 
# 
# naclean_df <- naclean_df %>%mutate(
#   # year_L = 変動期間を表す
#   year_L = as.numeric(year - ReferencePoint)
# ) %>% filter(
#   year_L != -8,
#   year_L != 4,
# )
# 
# 
# # 値の挿入
# naclean_df <- naclean_df %>% mutate(
#   is_female = ifelse(sex == 1,1,0),
#   suicide_consideration = ifelse(suicide_consideration == 1,1,0),
#   suicide_plan = ifelse(suicide_plan == 1,1,0),
#   suicide_attempt = ifelse(suicideattempt == 1,0,0),
#   suicide_attempt = ifelse(suicideattempt == 2,1,suicide_attempt),
#   suicide_attempt = ifelse(suicideattempt == 3,2,suicide_attempt),
#   suicide_attempt = ifelse(suicideattempt == 4,4,suicide_attempt),
#   suicide_attempt = ifelse(suicideattempt == 5,6,suicide_attempt),
#   suicide_injury = ifelse(suicideinjury == 2,1,0),
#   tabaco_month = 0,
#   tabaco_month = ifelse(tabaco_use == 2,4,tabaco_month),
#   tabaco_month = ifelse(tabaco_use == 3,7,tabaco_month),
#   tabaco_month = ifelse(tabaco_use == 4,14,tabaco_month),
#   tabaco_month = ifelse(tabaco_use == 5,24,tabaco_month),
#   tabaco_month = ifelse(tabaco_use == 6,30,tabaco_month),
#   alcohol_month = 0,
#   alcohol_month = ifelse(alcohol_use == 2,1,alcohol_month),
#   alcohol_month = ifelse(alcohol_use == 3,4,alcohol_month),
#   alcohol_month = ifelse(alcohol_use == 4,14,alcohol_month),
#   alcohol_month = ifelse(alcohol_use == 5,24,alcohol_month),
#   alcohol_month = ifelse(alcohol_use == 6,30,alcohol_month),
#   marijuana_month = ifelse(marijuana_use == 1,0,0),
#   marijuana_month = ifelse(marijuana_use == 2,1,marijuana_month),
#   marijuana_month = ifelse(marijuana_use == 3,6,marijuana_month),
#   marijuana_month = ifelse(marijuana_use == 4,14,marijuana_month),
#   marijuana_month = ifelse(marijuana_use == 5,29,marijuana_month),
#   marijuana_month = ifelse(marijuana_use == 6,40,marijuana_month),
#   age = age + 11,
# )
# 
# pl_summary <- datasummary(All(naclean_df) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max) ),
#                           data = naclean_df,
#                           na.rm = TRUE,
#                           fmt = 3,
#                           output = "default",
#                           
# )
# # 平行トレンド見れればおK
# # baseline_df15 <- naclean_df %>% filter(
# #   year < 2015
# # )
# # Ttest(baseline_df15,"baseline_df15")
# # 
# # 
# # baseline_df17 <- naclean_df %>% filter(
# #   year < 2017
# # )
# # Ttest(baseline_df17,"baseline_df17")
# # 
# # baseline_15 <- naclean_df %>% filter(
# #   year < 2015,
# #   MMLyear < 2015,
# # )
# # 
# # Ttest(baseline_15,"baseline_15")
# # 
# # baseline_17 <- naclean_df %>% filter(
# #   year < 2017,
# #   MMLyear < 2017,
# #   # sitecode != "AK",
# # )
# # Ttest(baseline_17,"baseline_17")
# 
# # endline_df21 <- naclean_df %>% filter(
# #   year == 2021
# # )
# # Ttest(endline_df21,"endline_df21")
# # 
# # 
# # endline_df21_m <- naclean_df %>% filter(
# #   year == 2021,
# #   MMLyear < 2021
# # )
# # Ttest(endline_df21_m,"endline_df21_m")
# # 
# # 
# # endline_df19 <- naclean_df %>% filter(
# #   year == 2019
# # ) %>% mutate(
# #   Treatment = ifelse(ReferencePoint > 2019,0,Treatment)
# # )
# # Ttest(endline_df19,"endline_df19")
# # 
# # 
# # endline_df19_m <- naclean_df %>% filter(
# #   year == 2019,
# #   MMLyear < 2019
# # ) %>% mutate(
# #   Treatment = ifelse(ReferencePoint > 2019,0,Treatment)
# # )
# # Ttest(endline_df19_m,"endline_df19_m")
# 
# # two-way fixed （event-study）で推定 -------------------------------------------------------
# 
# 
# res_twfx = feols(marijuana_month ~ i(year_L,Treatment,ref = 0) | year + sitecode ,data = naclean_df,cluster = c("sitecode"))
# etable(res_twfx)
# pl <- fixest::iplot(res_twfx,zero = TRUE,pt.join = TRUE)
# table(naclean_df$year_L)
# 
# # SANABで推定 ----------------------------------------------------------------
# 
# # res_sunab = feols(marijuana_month ~ sunab(ReferencePoint, year) | year+ sitecode, naclean_df,cluster = "sitecode")
# res_sunab = feols(marijuana_month ~ sunab(ReferencePoint, year, ref.p = 0) | year+ sitecode, naclean_df,cluster = "sitecode")
# # res_sunab = feols(marijuana_month ~ sunab(ReferencePoint, year, ref.p = 0) | year+ sitecode, naclean_df,cluster = "sitecode")
# etable(res_sunab)
# pl <- fixest::iplot(res_sunab,zero = TRUE,pt.join = TRUE)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 
# # 
# # 
# # 
# # 
# # 
# # # SANAB 2021 省く ------------------------------------------------------------
# # 
# # 
# # clean_df21 <- clean_df %>% filter(
# #   year > 2009,
# #   year < 2021
# # ) %>% filter(
# #   # sitename != "Alabama (AL)",
# #   # sitename != "California (CA)",
# #   # sitename != "Colorado (CO)",
# #   # sitename != "Delaware (DE)",
# #   # sitename != "Georgia (GA)",
# #   # sitename != "Indiana (IN)",
# #   # sitename != "Iowa (IA)",
# #   # sitename != "Kansas (KS)",
# #   # sitename != "Louisiana (LA)",
# #   # sitename != "Mississippi (MS)",
# #   sitename != "Missouri (MO)",
# #   # sitename != "Nevada (NV)",
# #   # sitename != "New Jersey (NJ)",
# #   # sitename != "Pennsylvania (PA)",
# #   # sitename != "South Dakota (SD)",
# #   # sitename != "Texas (TX)",
# #   # sitename != "Utah (UT)",
# #   # sitename != "Vermont (VT)",
# #   # sitename != "Wisconsin (WI)",
# #   # sitename != "Wyoming (WY)",
# #   sitename != "Nevada (NV)",
# # )
# # # NA消去
# # naclean_df <- na.omit(clean_df21)
# # 
# # # year(時点効果),Treatment(グループ固定効果),year_L,referecnce_point(cohort)をつくる。
# # 
# # 
# # naclean_df <- naclean_df %>%mutate(
# #   # year_L = 変動期間を表す
# #   year_L = as.numeric(year - ReferencePoint)
# # )
# # 
# # 
# # # 値の挿入
# # naclean_df <- naclean_df %>% mutate(
# #   is_female = ifelse(sex == 1,1,0),
# #   suicide_consideration = ifelse(suicide_consideration == 1,1,0),
# #   suicide_plan = ifelse(suicide_plan == 1,1,0),
# #   suicide_attempt = ifelse(suicideattempt == 1,0,0),
# #   suicide_attempt = ifelse(suicideattempt == 2,1,suicide_attempt),
# #   suicide_attempt = ifelse(suicideattempt == 3,2,suicide_attempt),
# #   suicide_attempt = ifelse(suicideattempt == 4,4,suicide_attempt),
# #   suicide_attempt = ifelse(suicideattempt == 5,6,suicide_attempt),
# #   suicide_injury = ifelse(suicideinjury == 2,1,0),
# #   tabaco_month = 0,
# #   tabaco_month = ifelse(tabaco_use == 2,4,tabaco_month),
# #   tabaco_month = ifelse(tabaco_use == 3,7,tabaco_month),
# #   tabaco_month = ifelse(tabaco_use == 4,14,tabaco_month),
# #   tabaco_month = ifelse(tabaco_use == 5,24,tabaco_month),
# #   tabaco_month = ifelse(tabaco_use == 6,30,tabaco_month),
# #   alcohol_month = 0,
# #   alcohol_month = ifelse(alcohol_use == 2,1,alcohol_month),
# #   alcohol_month = ifelse(alcohol_use == 3,4,alcohol_month),
# #   alcohol_month = ifelse(alcohol_use == 4,14,alcohol_month),
# #   alcohol_month = ifelse(alcohol_use == 5,24,alcohol_month),
# #   alcohol_month = ifelse(alcohol_use == 6,30,alcohol_month),
# #   marijuana_month = ifelse(marijuana_use == 1,0,0),
# #   marijuana_month = ifelse(marijuana_use == 2,1,marijuana_month),
# #   marijuana_month = ifelse(marijuana_use == 3,6,marijuana_month),
# #   marijuana_month = ifelse(marijuana_use == 4,14,marijuana_month),
# #   marijuana_month = ifelse(marijuana_use == 5,29,marijuana_month),
# #   marijuana_month = ifelse(marijuana_use == 6,40,marijuana_month),
# #   age = age + 11,
# # 
# # )
# # 
# # pl_summary <- datasummary(All(naclean_df) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max) ),
# #                           data = naclean_df,
# #                           na.rm = TRUE,
# #                           fmt = 3,
# #                           output = "default",
# # 
# # )
# # # 平行トレンド見れればおK
# # # baseline_df15 <- naclean_df %>% filter(
# # #   year < 2015
# # # )
# # # Ttest(baseline_df15,"baseline_df15")
# # #
# # #
# # # baseline_df17 <- naclean_df %>% filter(
# # #   year < 2017
# # # )
# # # Ttest(baseline_df17,"baseline_df17")
# # #
# # # baseline_15 <- naclean_df %>% filter(
# # #   year < 2015,
# # #   MMLyear < 2015,
# # # )
# # #
# # # Ttest(baseline_15,"baseline_15")
# # #
# # # baseline_17 <- naclean_df %>% filter(
# # #   year < 2017,
# # #   MMLyear < 2017,
# # #   # sitecode != "AK",
# # # )
# # # Ttest(baseline_17,"baseline_17")
# # 
# # # endline_df21 <- naclean_df %>% filter(
# # #   year == 2021
# # # )
# # # Ttest(endline_df21,"endline_df21")
# # #
# # #
# # # endline_df21_m <- naclean_df %>% filter(
# # #   year == 2021,
# # #   MMLyear < 2021
# # # )
# # # Ttest(endline_df21_m,"endline_df21_m")
# # #
# # #
# # # endline_df19 <- naclean_df %>% filter(
# # #   year == 2019
# # # ) %>% mutate(
# # #   Treatment = ifelse(ReferencePoint > 2019,0,Treatment)
# # # )
# # # Ttest(endline_df19,"endline_df19")
# # #
# # #
# # # endline_df19_m <- naclean_df %>% filter(
# # #   year == 2019,
# # #   MMLyear < 2019
# # # ) %>% mutate(
# # #   Treatment = ifelse(ReferencePoint > 2019,0,Treatment)
# # # )
# # # Ttest(endline_df19_m,"endline_df19_m")
# # 
# # 
# # # two-way fixed で推定 -------------------------------------------------------
# # 
# # 
# # res_twfx = feols(`marijuana_month` ~ i(year_L,Treatment,ref = 0) | year_L + sitecode ,data = c_stucked_df,cluster = c("sitecode"))
# # etable(res_twfx)
# # pl <- fixest::iplot(res_twfx,zero = TRUE,pt.join = TRUE,drop = "[[:digit:]]{4}")
# # # 
# # # # SANABで推定 ----------------------------------------------------------------
# # 
# # 
# # # res_sunab = feols(marijuana_month ~ sunab(ReferencePoint, year) | year+ sitecode, naclean_df,cluster = "sitecode")
# # res_sunab = feols(marijuana_month ~ sunab(ReferencePoint, year, ref.p = 0) | year+ sitecode, naclean_df,cluster = "sitecode")
# # etable(res_sunab)
# # pl <- fixest::iplot(res_sunab,zero = TRUE,pt.join = TRUE)
# # # 
# # # # アラスカ(2021欠損)省く ------------------------------------------------------------------
# # # 
# # # 
# # # clean_dfar <- clean_df %>% filter(
# # #   year > 2009,
# # #   sitecode != 'AK'
# # # ) %>% filter(
# # #   # sitename != "Alabama (AL)",
# # #   # sitename != "California (CA)",
# # #   # sitename != "Colorado (CO)",
# # #   # sitename != "Delaware (DE)",
# # #   # sitename != "Georgia (GA)",
# # #   # sitename != "Indiana (IN)",
# # #   # sitename != "Iowa (IA)",
# # #   # sitename != "Kansas (KS)",
# # #   # sitename != "Louisiana (LA)",
# # #   # sitename != "Mississippi (MS)",
# # #   sitename != "Missouri (MO)",
# # #   # sitename != "Nevada (NV)",
# # #   # sitename != "New Jersey (NJ)",
# # #   # sitename != "Pennsylvania (PA)",
# # #   # sitename != "South Dakota (SD)",
# # #   # sitename != "Texas (TX)",
# # #   # sitename != "Utah (UT)",
# # #   # sitename != "Vermont (VT)",
# # #   # sitename != "Wisconsin (WI)",
# # #   # sitename != "Wyoming (WY)",
# # #   sitename != "Nevada (NV)",
# # # )
# # # # NA消去
# # # naclean_df <- na.omit(clean_dfar)
# # # 
# # # # year(時点効果),Treatment(グループ固定効果),year_L,referecnce_point(cohort)をつくる。
# # # 
# # # 
# # # # naclean_df <- naclean_df %>% filter(
# # # #   year > 2011,
# # # #   sitecode != 'AK'
# # # # )
# # # # %>% mutate(
# # # #   # year_L = 変動期間を表す
# # # #   year_L = as.numeric(year - ReferencePoint)
# # # # )
# # # 
# # # 
# # # # 値の挿入
# # # naclean_df <- naclean_df %>% mutate(
# # #   is_female = ifelse(sex == 1,1,0),
# # #   suicide_consideration = ifelse(suicide_consideration == 1,1,0),
# # #   suicide_plan = ifelse(suicide_plan == 1,1,0),
# # #   suicide_attempt = ifelse(suicideattempt == 1,0,0),
# # #   suicide_attempt = ifelse(suicideattempt == 2,1,suicide_attempt),
# # #   suicide_attempt = ifelse(suicideattempt == 3,2,suicide_attempt),
# # #   suicide_attempt = ifelse(suicideattempt == 4,4,suicide_attempt),
# # #   suicide_attempt = ifelse(suicideattempt == 5,6,suicide_attempt),
# # #   suicide_injury = ifelse(suicideinjury == 2,1,0),
# # #   tabaco_month = 0,
# # #   tabaco_month = ifelse(tabaco_use == 2,4,tabaco_month),
# # #   tabaco_month = ifelse(tabaco_use == 3,7,tabaco_month),
# # #   tabaco_month = ifelse(tabaco_use == 4,14,tabaco_month),
# # #   tabaco_month = ifelse(tabaco_use == 5,24,tabaco_month),
# # #   tabaco_month = ifelse(tabaco_use == 6,30,tabaco_month),
# # #   alcohol_month = 0,
# # #   alcohol_month = ifelse(alcohol_use == 2,1,alcohol_month),
# # #   alcohol_month = ifelse(alcohol_use == 3,4,alcohol_month),
# # #   alcohol_month = ifelse(alcohol_use == 4,14,alcohol_month),
# # #   alcohol_month = ifelse(alcohol_use == 5,24,alcohol_month),
# # #   alcohol_month = ifelse(alcohol_use == 6,30,alcohol_month),
# # #   marijuana_month = ifelse(marijuana_use == 1,0,0),
# # #   marijuana_month = ifelse(marijuana_use == 2,1,marijuana_month),
# # #   marijuana_month = ifelse(marijuana_use == 3,6,marijuana_month),
# # #   marijuana_month = ifelse(marijuana_use == 4,14,marijuana_month),
# # #   marijuana_month = ifelse(marijuana_use == 5,29,marijuana_month),
# # #   marijuana_month = ifelse(marijuana_use == 6,40,marijuana_month),
# # #   age = age + 11,
# # #   
# # # )
# # # 
# # # pl_summary <- datasummary(All(naclean_df) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max) ),
# # #                           data = naclean_df,
# # #                           na.rm = TRUE,
# # #                           fmt = 3,
# # #                           output = "default",
# # #                           
# # # )
# # # # 平行トレンド見れればおK
# # # # baseline_df15 <- naclean_df %>% filter(
# # # #   year < 2015
# # # # )
# # # # Ttest(baseline_df15,"baseline_df15")
# # # # 
# # # # 
# # # # baseline_df17 <- naclean_df %>% filter(
# # # #   year < 2017
# # # # )
# # # # Ttest(baseline_df17,"baseline_df17")
# # # # 
# # # # baseline_15 <- naclean_df %>% filter(
# # # #   year < 2015,
# # # #   MMLyear < 2015,
# # # # )
# # # # 
# # # # Ttest(baseline_15,"baseline_15")
# # # # 
# # # # baseline_17 <- naclean_df %>% filter(
# # # #   year < 2017,
# # # #   MMLyear < 2017,
# # # #   # sitecode != "AK",
# # # # )
# # # # Ttest(baseline_17,"baseline_17")
# # # 
# # # # endline_df21 <- naclean_df %>% filter(
# # # #   year == 2021
# # # # )
# # # # Ttest(endline_df21,"endline_df21")
# # # # 
# # # # 
# # # # endline_df21_m <- naclean_df %>% filter(
# # # #   year == 2021,
# # # #   MMLyear < 2021
# # # # )
# # # # Ttest(endline_df21_m,"endline_df21_m")
# # # # 
# # # # 
# # # # endline_df19 <- naclean_df %>% filter(
# # # #   year == 2019
# # # # ) %>% mutate(
# # # #   Treatment = ifelse(ReferencePoint > 2019,0,Treatment)
# # # # )
# # # # Ttest(endline_df19,"endline_df19")
# # # # 
# # # # 
# # # # endline_df19_m <- naclean_df %>% filter(
# # # #   year == 2019,
# # # #   MMLyear < 2019
# # # # ) %>% mutate(
# # # #   Treatment = ifelse(ReferencePoint > 2019,0,Treatment)
# # # # )
# # # 
# # # # Ttest(endline_df19_m,"endline_df19_m")
# # # # res_sunab = feols(marijuana_month ~ sunab(ReferencePoint, year) | year+ sitecode, naclean_df,cluster = "sitecode")
# # # res_sunab = feols(marijuana_month ~ sunab(ReferencePoint, year, ref.p = c(.F + 0:3, 0)) | year+ sitecode, naclean_df,cluster = "sitecode")
# # # etable(res_sunab)
# # # pl <- fixest::iplot(res_sunab,zero = TRUE,pt.join = TRUE)
# # # 
