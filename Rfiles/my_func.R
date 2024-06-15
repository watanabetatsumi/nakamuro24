
# 依存関係 --------------------------------------------------------------------

library(dplyr)
library(purrr)
library(tidyr)


# 自作関数 --------------------------------------------------------------------

IsColname <- function(word,df){
 list <- names(df)
 if(word %in% list){
   index <- grep(word,list)
   return(index)
   }
 else{
   return ("none")
   }
}

df_name <- function(year){
  df_name <- paste0("df",year)
  return (df_name)
}

df_list <- function(years){
  frames <- map(years, df_name)
  return(frames)
}

make_df <- function(years){
  data_frames <- df_list(years)
  datafiles <- list.files("data/csv",pattern=".csv",full.names = T)
  map(datafiles, read_csv) %>% setNames(data_frames) %>% list2env(envir = .GrobalEnv)
}


Ttest <- function(df_T,f_name){
  # 各t検定の結果を取得
  result_marijuana_month <- t.test(marijuana_month ~ Treatment, df_T ) %>% tidy()
  result_alcohol_month <- t.test(alcohol_month ~ Treatment, df_T ) %>% tidy()
  result_tabaco_month <- t.test(tabaco_per_month ~ Treatment, df_T ) %>% tidy()
  result_suicide_consideration <- t.test(suicide_consideration ~ Treatment, df_T ) %>% tidy()
  result_suicide_plan <- t.test(suicide_plan ~ Treatment, df_T ) %>% tidy()
  result_suicide_attempt <- t.test(suicide_attempt ~ Treatment, df_T ) %>% tidy()
  result_suicide_injury <- t.test(suicide_injury ~ Treatment, df_T ) %>% tidy()
  
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
  
  file_path <- "./outputs/"
  file_path <- paste(file_path,as.character(f_name),".csv")
  if (file.exists(file_path)) {
    file.remove(file_path)
  }
  # 
  # エクセルファイルに書き込む
  write.csv(all_results_df, file_path)
  library(readr)
  dfname <- "T_"
  dfname <- paste(dfname,as.character(f_name))
  dfname<- read_csv(file_path)
  View(dfname)
}