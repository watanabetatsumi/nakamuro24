# 依存関係 --------------------------------------------------------------------

library(tidyr)
library(dplyr)
library(gtsummary)
library(ggplot2)
library(modelsummary)
#.pngファイルをアウトプットするため
webshot::install_phantomjs()
#高画質に
library(magick)

# 基本統計量 --------------------------------------------------------------------

df_summary <- df_1117 %>%
  select(
    depression,                             
    drinkperday,         
    how_marijuana,         
    marijuana_wo_doc,   
    # how_painreliver,    
    # painreliver_wi_doc, 
    # painreliver_wo_doc, 
    drunk_drive,         
    risky_beh,          
  ) %>%
  rename(
    'うつ病経験率'  = depression,                             
    '飲酒頻度'      = drinkperday,         
    '大麻使用頻度'  = how_marijuana,         
    '法外大麻使用頻度'  = marijuana_wo_doc,   
    '鎮静剤使用頻度' =how_painreliver,    
    '鎮静剤処方率' = painreliver_wi_doc, 
    '法外鎮静剤使用頻度' =painreliver_wo_doc, 
    '危険運転頻度' =drunk_drive,         
    '問題行為度' =risky_beh,          
  )

file_path <- "./outputs/summary_table.png"

if (file.exists(file_path)) {
  file.remove(file_path)
}

summary <- datasummary(All(df_summary) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD)),
                       data = df_summary,
                       na.rm = TRUE,
                       fmt = 3,
                       output = file_path,
)


df_1117 <- df_1117 %>% mutate(
  `2011` = ifelse(df_1117$year == 2011,1,0),
  `2013` = ifelse(df_1117$year == 2013,1,0),
  `2015` = ifelse(df_1117$year == 2015,1,0),
  `2017` = ifelse(df_1117$year == 2017,1,0),
)
res_twfe = feols(y ~ i(time_to_treatment, ref = c(2013)) | year+id, data = df_1117, cluster = "id")
