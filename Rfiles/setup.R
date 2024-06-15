

# 依存関係 --------------------------------------------------------------------

library(readxl)
source("Rfiles/my_func.R")
source("Rfiles/geotable.R")

# データの読み込み ------------------------------------------------------------

df <- read_excel("./data/source/J334234.xlsx")

# データフレームの読み込み ------------------------------------------------------------

# years <- c("09","11","13 ,"15","17","19")
df_09 <- df %>% mutate(
  id_f               = df$ER30001,
  id_i               = df$ER30002,
  id               = (id_f)*1000 + id_i,
  is_09            = na_if(df$TAS09,0),
  depression         = df$TA090737,                    
  how_drink          = df$TA090797,            
  drinkperday        = df$TA090798,   
  how_marijuana      = df$TA090819,   
  marijuana_wo_doc   = df$TA090822,
  drunk_drive        = df$TA090899,  
  risky_beh          = df$TA090984,
  state              = df$TA090005
) %>% filter(
  is_09 == 1
) %>% select(
  id,
  depression,                             
  how_drink,                
  drinkperday,         
  how_marijuana,         
  marijuana_wo_doc,   
  drunk_drive,         
  risky_beh,          
  state              
) %>% mutate(
  year  = 2009,
) %>% na.omit

df_11 <- df %>% mutate(
  id_f               = df$ER30001,
  id_i               = df$ER30002,
  id               = (id_f)*1000 + id_i,
  is_11            = na_if(df$TAS11,0),
  depression         = df$TA110827,                    
  how_drink          = df$TA110913,            
  drinkperday        = df$TA110914,   
  how_marijuana      = df$TA110935,   
  marijuana_wo_doc   = df$TA110938,
  drunk_drive        = df$TA111030,  
  risky_beh          = df$TA111126,
  state              = df$TA110005
) %>% filter(
  is_11 == 1
) %>% select(
  id,
  depression,                             
  how_drink,                
  drinkperday,         
  how_marijuana,         
  marijuana_wo_doc,   
  drunk_drive,         
  risky_beh,          
  state              
) %>% mutate(
  year  = 2011,
) %>% na.omit

df_13 <- df %>% mutate(
  id_f               = df$ER30001,
  id_i               = df$ER30002,
  id               = (id_f)*1000 + id_i,
  is_13            = na_if(df$TAS13,0),
  depression         = df$TA130850,                    
  how_drink          = df$TA130946,            
  drinkperday        = df$TA130947,   
  how_marijuana      = df$TA130968,   
  marijuana_wo_doc   = df$TA130971,
  drunk_drive        = df$TA131065,  
  risky_beh          = df$TA131218,
  state              = df$TA130005
) %>% filter(
  is_13 == 1
) %>% select(
  id,
  depression,                             
  how_drink,                
  drinkperday,         
  how_marijuana,         
  marijuana_wo_doc,   
  drunk_drive,         
  risky_beh,          
  state              
) %>% mutate(
  year  = 2013,
) %>% na.omit

df_15 <- df %>% mutate(
  id_f               = df$ER30001,
  id_i               = df$ER30002,
  id               = (id_f)*1000 + id_i,
  is_15            = na_if(df$TAS15,0),
  depression         = df$TA150867,                    
  how_drink          = df$TA150968,            
  drinkperday        = df$TA150969,   
  how_marijuana      = df$TA150990,   
  marijuana_wo_doc   = df$TA150993,
  how_painreliver    = df$TA151027,
  painreliver_wi_doc = df$TA151029,
  painreliver_wo_doc = df$TA151030,
  drunk_drive        = df$TA151105,  
  risky_beh          = df$TA151278,
  state              = df$TA150005
) %>% filter(
  is_15 == 1
) %>% select(
  id,
  depression,                             
  how_drink,                
  drinkperday,         
  how_marijuana,         
  marijuana_wo_doc,   
  how_painreliver,    
  painreliver_wi_doc, 
  painreliver_wo_doc, 
  drunk_drive,         
  risky_beh,          
  state              
) %>% mutate(
  year  = 2015,
) %>% na.omit

df_17 <- df %>% mutate(
  id_f               = df$ER30001,
  id_i               = df$ER30002,
  id               = (id_f)*1000 + id_i,
  is_17            = na_if(df$TAS17,0),
  depression         = df$TA170907,                    
  how_drink          = df$TA171825,            
  drinkperday        = df$TA171826,   
  how_marijuana      = df$TA171831,   
  marijuana_wo_doc   = df$TA171834,
  how_painreliver    = df$TA171849,
  painreliver_wi_doc = df$TA171851,
  painreliver_wo_doc = df$TA171852,
  drunk_drive        = df$TA171935,  
  risky_beh          = df$TA171976,
  state              = df$TA170005
) %>% filter(
  is_17 == 1
) %>% select(
  id,
  depression,                             
  how_drink,                
  drinkperday,         
  how_marijuana,         
  marijuana_wo_doc,   
  how_painreliver,    
  painreliver_wi_doc, 
  painreliver_wo_doc, 
  drunk_drive,         
  risky_beh,          
  state              
) %>% mutate(
  year  = 2017,
) %>% na.omit

df_19 <- df %>% mutate(
  id_f               = df$ER30001,
  id_i               = df$ER30002,
  id               = (id_f)*1000 + id_i,
  is_19            = na_if(df$TAS19,0),
  depression         = df$TA191052,                    
  how_drink          = df$TA191987,            
  drinkperday        = df$TA191988,   
  how_marijuana      = df$TA191993,   
  marijuana_wo_doc   = df$TA191996,
  how_painreliver    = df$TA192011,
  painreliver_wi_doc = df$TA192013,
  painreliver_wo_doc = df$TA192014,
  drunk_drive        = df$TA192096,  
  risky_beh          = df$TA192157,
  state              = df$TA192196
) %>% filter(
  is_19 == 1
) %>% select(
  id,
  depression,                             
  # how_drink,                
  drinkperday,         
  how_marijuana,         
  marijuana_wo_doc,   
  how_painreliver,    
  painreliver_wi_doc, 
  painreliver_wo_doc, 
  drunk_drive,         
  risky_beh,          
  state              
) %>% mutate(
  year  = 2019,
) %>% na.omit %>% mutate(
  depression = na_if(depression,8),
  depression = na_if(depression,9),
  depression = ifelse(depression == 1,1,0),
  drinkperday = na_if(drinkperday,98),
  drinkperday = na_if(drinkperday,99),
  how_marijuana = na_if(how_marijuana,8),
  how_marijuana = na_if(how_marijuana,9),
  how_marijuana = ifelse(how_marijuana == 1,1,how_marijuana),
  how_marijuana = ifelse(how_marijuana == 2,4,how_marijuana),
  how_marijuana = ifelse(how_marijuana == 3,7,how_marijuana),
  how_marijuana = ifelse(how_marijuana == 4,15,how_marijuana),
  how_marijuana = ifelse(how_marijuana == 5,30,how_marijuana),
  how_marijuana = ifelse(how_marijuana == 6,40,how_marijuana),
  marijuana_wo_doc = na_if(marijuana_wo_doc,8),
  marijuana_wo_doc = na_if(marijuana_wo_doc,9),
  marijuana_wo_doc = ifelse(marijuana_wo_doc == 1,1,marijuana_wo_doc),
  marijuana_wo_doc = ifelse(marijuana_wo_doc == 2,4,marijuana_wo_doc),
  marijuana_wo_doc = ifelse(marijuana_wo_doc == 3,7,marijuana_wo_doc),
  marijuana_wo_doc = ifelse(marijuana_wo_doc == 4,15,marijuana_wo_doc),
  marijuana_wo_doc = ifelse(marijuana_wo_doc == 5,30,marijuana_wo_doc),
  marijuana_wo_doc = ifelse(marijuana_wo_doc == 6,40,marijuana_wo_doc),
  how_painreliver = na_if(how_painreliver,8),
  how_painreliver = na_if(how_painreliver,9),
  how_painreliver = ifelse(how_painreliver == 1,1,how_painreliver),
  how_painreliver = ifelse(how_painreliver == 2,4,how_painreliver),
  how_painreliver = ifelse(how_painreliver == 3,7,how_painreliver),
  how_painreliver = ifelse(how_painreliver == 4,15,how_painreliver),
  how_painreliver = ifelse(how_painreliver == 5,30,how_painreliver),
  how_painreliver = ifelse(how_painreliver == 6,40,how_painreliver),
  painreliver_wi_doc = na_if(painreliver_wi_doc,8),
  painreliver_wi_doc = na_if(painreliver_wi_doc,9),
  painreliver_wi_doc = na_if(painreliver_wi_doc,0),
  painreliver_wi_doc = ifelse(painreliver_wi_doc == 5,0,painreliver_wi_doc),
  painreliver_wo_doc = na_if(painreliver_wo_doc,8),
  painreliver_wo_doc = na_if(painreliver_wo_doc,9),
  painreliver_wo_doc = ifelse(painreliver_wo_doc == 1,1,painreliver_wo_doc),
  painreliver_wo_doc = ifelse(painreliver_wo_doc == 2,4,painreliver_wo_doc),
  painreliver_wo_doc = ifelse(painreliver_wo_doc == 3,7,painreliver_wo_doc),
  painreliver_wo_doc = ifelse(painreliver_wo_doc == 4,15,painreliver_wo_doc),
  painreliver_wo_doc = ifelse(painreliver_wo_doc == 5,30,painreliver_wo_doc),
  painreliver_wo_doc = ifelse(painreliver_wo_doc == 6,40,painreliver_wo_doc),
  drunk_drive = na_if(drunk_drive,8),
  drunk_drive = na_if(drunk_drive,9),
  drunk_drive = ifelse(drunk_drive == 1,0,drunk_drive),
  drunk_drive = ifelse(drunk_drive == 2,1,drunk_drive),
  drunk_drive = ifelse(drunk_drive == 3,2,drunk_drive),
  drunk_drive = ifelse(drunk_drive == 4,5,drunk_drive),
  drunk_drive = ifelse(drunk_drive == 5,8,drunk_drive),
  drunk_drive = ifelse(drunk_drive == 6,16,drunk_drive),
  drunk_drive = ifelse(drunk_drive == 7,21,drunk_drive),
)

df_1117_list <- list(df_11,df_13,df_15,df_17)
df_1117 <- bind_rows(df_1117_list)
df_1117 <- df_1117 %>% rename(
  'state_id' = state
)
df_1117 <- inner_join(df_1117,geotable,by = 'state_id')
df_1117 <- df_1117 %>% select(
  - Code,
  - StateName
)
df_1117 <- df_1117 %>% 
  group_by(id) %>% 
  mutate(
    count = n(),
    move = ifelse(sd(state_id) == 0,0,1)
  ) %>% ungroup(id) %>% filter(
    count == 4,
    move == 0
  )





df_1519_list <- list(df_15,df_17,df_19)
df_1519 <- bind_rows(df_1519_list)
df_1519 <- df_1519 %>% 
  group_by(id) %>% 
  mutate(
    count = n()
  ) %>% ungroup(id) %>% filter(
    count == 3
  ) %>% rename(
    'state_id' = state
  )

df_1319_list <- list(df_11,df_13,df_15,df_17,df_19)
df_1319 <- bind_rows(df_1319_list)
df_1319 <- df_1319 %>% 
  group_by(id) %>% 
  mutate(
    count = n()
  ) %>% ungroup(id) %>% filter(
    count == 5
  ) %>% rename(
    'state_id' = state
  )

df_1319 <- inner_join(df_1319,geotable,by = 'state_id')
df_1319 <- df_1319 %>% filter(
  state_id != 8,
  # state_id != 41,
  # state_id != 2,
  state_id != 53,
) %>% select(
  -painreliver_wi_doc,
  -painreliver_wo_doc,
  -how_painreliver,
  -how_drink,
) %>% mutate(
  depression = na_if(depression,8),
  depression = na_if(depression,9),
  depression = ifelse(depression == 1,1,0),
  drinkperday = na_if(drinkperday,98),
  drinkperday = na_if(drinkperday,99),
  how_marijuana = na_if(how_marijuana,8),
  how_marijuana = na_if(how_marijuana,9),
  how_marijuana = ifelse(how_marijuana == 1,1,how_marijuana),
  how_marijuana = ifelse(how_marijuana == 2,4,how_marijuana),
  how_marijuana = ifelse(how_marijuana == 3,7,how_marijuana),
  how_marijuana = ifelse(how_marijuana == 4,15,how_marijuana),
  how_marijuana = ifelse(how_marijuana == 5,30,how_marijuana),
  how_marijuana = ifelse(how_marijuana == 6,40,how_marijuana),
  marijuana_wo_doc = na_if(marijuana_wo_doc,8),
  marijuana_wo_doc = na_if(marijuana_wo_doc,9),
  marijuana_wo_doc = ifelse(marijuana_wo_doc == 1,1,marijuana_wo_doc),
  marijuana_wo_doc = ifelse(marijuana_wo_doc == 2,4,marijuana_wo_doc),
  marijuana_wo_doc = ifelse(marijuana_wo_doc == 3,7,marijuana_wo_doc),
  marijuana_wo_doc = ifelse(marijuana_wo_doc == 4,15,marijuana_wo_doc),
  marijuana_wo_doc = ifelse(marijuana_wo_doc == 5,30,marijuana_wo_doc),
  marijuana_wo_doc = ifelse(marijuana_wo_doc == 6,40,marijuana_wo_doc),
  # how_painreliver = na_if(how_painreliver,8),
  # how_painreliver = na_if(how_painreliver,9),
  # how_painreliver = ifelse(how_painreliver == 1,1,how_painreliver),
  # how_painreliver = ifelse(how_painreliver == 2,4,how_painreliver),
  # how_painreliver = ifelse(how_painreliver == 3,7,how_painreliver),
  # how_painreliver = ifelse(how_painreliver == 4,15,how_painreliver),
  # how_painreliver = ifelse(how_painreliver == 5,30,how_painreliver),
  # how_painreliver = ifelse(how_painreliver == 6,40,how_painreliver),
  # painreliver_wi_doc = na_if(painreliver_wi_doc,8),
  # painreliver_wi_doc = na_if(painreliver_wi_doc,9),
  # painreliver_wi_doc = na_if(painreliver_wi_doc,0),
  # painreliver_wi_doc = ifelse(painreliver_wi_doc == 5,0,painreliver_wi_doc),
  # painreliver_wo_doc = na_if(painreliver_wo_doc,8),
  # painreliver_wo_doc = na_if(painreliver_wo_doc,9),
  # painreliver_wo_doc = ifelse(painreliver_wo_doc == 1,1,painreliver_wo_doc),
  # painreliver_wo_doc = ifelse(painreliver_wo_doc == 2,4,painreliver_wo_doc),
  # painreliver_wo_doc = ifelse(painreliver_wo_doc == 3,7,painreliver_wo_doc),
  # painreliver_wo_doc = ifelse(painreliver_wo_doc == 4,15,painreliver_wo_doc),
  # painreliver_wo_doc = ifelse(painreliver_wo_doc == 5,30,painreliver_wo_doc),
  # painreliver_wo_doc = ifelse(painreliver_wo_doc == 6,40,painreliver_wo_doc),
  drunk_drive = na_if(drunk_drive,8),
  drunk_drive = na_if(drunk_drive,9),
  drunk_drive = ifelse(drunk_drive == 1,0,drunk_drive),
  drunk_drive = ifelse(drunk_drive == 2,1,drunk_drive),
  drunk_drive = ifelse(drunk_drive == 3,2,drunk_drive),
  drunk_drive = ifelse(drunk_drive == 4,5,drunk_drive),
  drunk_drive = ifelse(drunk_drive == 5,8,drunk_drive),
  drunk_drive = ifelse(drunk_drive == 6,16,drunk_drive),
  drunk_drive = ifelse(drunk_drive == 7,21,drunk_drive),
) %>% na.omit()

# %>%
#   rename(
#     'うつ病経験率'  = depression,                             
#     '飲酒頻度'      = drinkperday,         
#     '大麻使用頻度'  = how_marijuana,         
#     '法外大麻使用頻度'  = marijuana_wo_doc,   
#     # '鎮静剤使用頻度' =how_painreliver,    
#     # '鎮静剤処方率' = painreliver_wi_doc, 
#     # '法外鎮静剤使用頻度' =painreliver_wo_doc, 
#     '危険運転頻度' =drunk_drive,         
#     '問題行為度' =risky_beh,          
#   )

df_1319 <- df_1319 %>%mutate(
  # year_L = 変動期間を表す
  year_L = as.numeric(year - ReferencePoint),
  first_treated = ReferencePoint + 2,
  first_treated = ifelse(ReferencePoint == 10000,0,first_treated)
) %>% filter(
  ReferencePoint != 2013
)

pl_summary <- datasummary(All(df_1319) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max) ),
                          data = df_1319,
                          na.rm = TRUE,
                          fmt = 3,
                          output = "default",
                          
)

Cs_res <- att_gt(
  yname = "drinkperday",
  idname = "id",
  tname = "year",
  gname = "first_treated",
  data = df_1319,
  clustervars   = "id",
)
Cs_res = aggte(Cs_res, min_e = -4, max_e = 0)
summary(Cs_res)

ggdid(Cs_res)
