# 依存関係 --------------------------------------------------------------------
library(tidyr)
library(readxl)
source("Rfiles/my_func.R")
source("Rfiles/geotable.R")
library(gtsummary)
library(ggplot2)
library(modelsummary)

# データの読み込み ------------------------------------------------------------
NLSY97_df <- read.csv("./data/source/NLSY97/default.csv")
NLSY97_colum <- read.csv("./data/source/NLSY97/default (1).CHILDYA")

NLSY97_colum <- NLSY97_colum %>% 
  mutate(
    year = Y,
    year = ifelse(Y == "2000","2000",year),
    year = ifelse(Y == "2002","2002",year),
    year = ifelse(Y == "2004","2004",year),
    year = ifelse(Y == "2006","2006",year),
    year = ifelse(Y == "2008","2008",year),
    year = ifelse(Y == "2010","2010",year),
    year = ifelse(Y == "2012","2012",year),
    year = ifelse(Y == "2014","2014",year),
    year = ifelse(Y == "2016","2016",year),
    year = ifelse(Y == "2018","2018",year),
    year = ifelse(Y == "2020","2020",year),
    syn = paste(Name,year,sep = "_"),
    columname = syn
  ) %>% 
  select(
    columname
  )
# 
vec_col <- as.vector(NLSY97_colum$Name)
colnames(NLSY97_df) <- vec_col

vec_col <- as.vector(NLSY97_colum$columname)
colnames(NLSY97_df) <- vec_col


# データ整形 -------------------------------------------------------------------

df_NLSY97_longer <- NLSY97_df %>% 
  pivot_longer(cols = c("Q14-2A_2000","Q14-2A_2002","Q14-2A_2004","Q14-2A_2006","Q14-2A_2008","Q14-2A_2010","Q14-2A_2012","Q14-2A_2014","Q14-2A_2016","Q14-2A_2018","Q14-2A_2020"),names_to = "year",names_prefix = "Q14-2A_",values_to = "Health") %>%
  select(
    -year
  ) %>% 
  pivot_longer(cols = vec_col[c(9,22,35,48,61,76,90,105,120,135,149)],names_to = "year",names_prefix = "yasr-7",values_to = "TABACO-")
  
  
  # データを縦持ちに
  # widerデータ（横持ち）
  # ---------------------------------------------------------------------------------------
# | ID 　　　| data_00    | data_02   | data_04    | data_06   | data_08    | data_10   | ←name
# ---------------------------------------------------------------------------------------
# |1　       | a          | b         | c          | d       　| e          | f         | ←value
# ---------------------------------------------------------------------------------------
# |2　       | A          | B         | C          | D       　| E          | F         | ←value


# ↓ pivot_longer()

#↓names　　　↓value
# ------------------------------------
# | ID       | 大麻消費   | CPUBID   |
# ------------------------------------
# |1         | data_00    | a        |
# ------------------------------------
# |1         | data_02    | b        |
# ------------------------------------
# |1         | data_04    | c        |
# ------------------------------------
# |1         | data_06    | d        |
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------------
# |2         | data_08    | E        |
# ------------------------------------
# |2         | data_10    | F        |



