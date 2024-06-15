# 依存関係 --------------------------------------------------------------------

setwd("C:/Users/watan/OneDrive/デスクトップ/2024_spring_project")

library(readxl)
source("Rfiles/my_func.R")
source("Rfiles/RMLs.R")
library(fixest)
library(gtsummary)
library(ggplot2)
library(modelsummary)
library(did)
library(fastDummies)

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
  # sitename != "Mississippi (MS)",
  sitename != "Missouri (MO)",
  # sitename != "Nevada (NV)",
  # sitename != "New Jersey (NJ)",
  # sitename != "Pennsylvania (PA)",
  # sitename != "South Dakota (SD)",
  # sitename != "Texas (TX)",
  # sitename != "Utah (UT)",
  # sitename != "Vermont (VT)",
  # sitename != "Wisconsin (WI)",
  # sitename != "Wyoming (WY)",
  sitename != "Nevada (NV)",
)
# NA消去
naclean_df <- na.omit(clean_df09)

# year(時点効果),Treatment(グループ固定効果),year_L,referecnce_point(cohort)をつくる。


naclean_df <- naclean_df %>%mutate(
  # year_L = 変動期間を表す
  year_L = as.numeric(year - ReferencePoint)
)


# 値の挿入
naclean_df <- naclean_df %>% mutate(
  is_female = ifelse(sex == 1,1,0),
  suicide_consideration = ifelse(suicide_consideration == 1,1,0),
  suicide_plan = ifelse(suicide_plan == 1,1,0),
  suicide_attempt = ifelse(suicideattempt == 1,0,0),
  suicide_attempt = ifelse(suicideattempt == 2,1,suicide_attempt),
  suicide_attempt = ifelse(suicideattempt == 3,2,suicide_attempt),
  suicide_attempt = ifelse(suicideattempt == 4,4,suicide_attempt),
  suicide_attempt = ifelse(suicideattempt == 5,6,suicide_attempt),
  suicide_injury = ifelse(suicideinjury == 2,1,0),
  tabaco_per_month = 0,
  tabaco_per_month = ifelse(tabaco_use == 2,4,tabaco_per_month),
  tabaco_per_month = ifelse(tabaco_use == 3,7,tabaco_per_month),
  tabaco_per_month = ifelse(tabaco_use == 4,14,tabaco_per_month),
  tabaco_per_month = ifelse(tabaco_use == 5,24,tabaco_per_month),
  tabaco_per_month = ifelse(tabaco_use == 6,30,tabaco_per_month),
  alcohol_month = 0,
  alcohol_month = ifelse(alcohol_use == 2,1,alcohol_month),
  alcohol_month = ifelse(alcohol_use == 3,4,alcohol_month),
  alcohol_month = ifelse(alcohol_use == 4,14,alcohol_month),
  alcohol_month = ifelse(alcohol_use == 5,24,alcohol_month),
  alcohol_month = ifelse(alcohol_use == 6,30,alcohol_month),
  marijuana_month = ifelse(marijuana_use == 1,0,0),
  marijuana_month = ifelse(marijuana_use == 2,1,marijuana_month),
  marijuana_month = ifelse(marijuana_use == 3,6,marijuana_month),
  marijuana_month = ifelse(marijuana_use == 4,14,marijuana_month),
  marijuana_month = ifelse(marijuana_use == 5,29,marijuana_month),
  marijuana_month = ifelse(marijuana_use == 6,40,marijuana_month),
  age = age + 11,
  first_treated = ReferencePoint + 2,
  first_treated = ifelse(first_treated > 10000,0,first_treated)
) %>% rename(
  treat = Treatment
)

# ダミー変数の作成
naclean_df <- dummy_cols(naclean_df, select_columns = "sitecode")


pl_summary <- datasummary(All(naclean_df) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max) ),
                          data = naclean_df,
                          na.rm = TRUE,
                          fmt = 3,
                          output = "default",
                          
)


names(naclean_df)


Cs_res <- att_gt(
                 yname = "marijuana_month",
                 idname = "record",
                 tname = "year",
                 gname = "first_treated",
                 # 共変量
                 # xformla = ~sitecode_AR + sitecode_AZB + sitecode_FL + sitecode_HI +
                 #   sitecode_IL + sitecode_MD + sitecode_MI + sitecode_MT + sitecode_NH +
                 #   sitecode_NM + sitecode_OK + sitecode_VA + sitecode_WV,
                 # xformla = ~sitecode,
                 data = naclean_df,
                 clustervars   = "sitecode",
                 panel = FALSE,
                 )
Cs_res = aggte(Cs_res, type = "dynamic",min_e = -4, max_e = 0)
summary(Cs_res)

ggdid(Cs_res)

summary(factor(naclean_df$sitecode))


