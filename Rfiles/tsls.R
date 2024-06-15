source("Rfiles/setup_YRBSS.R")
source("Rfiles/sanab.R")
require(parallel)
require(MASS)


# lm_tsls <- tsls(marijuana_month ~二一段階目,instruments = ~一段階目,data)

# reg.iv <- ivreg(log(wage) ~ educ | fatheduc, data=sampleset) 
# 式 <- ivreg(目的変数 ~ Xはっと| Z ,data=)

### 1st stage: reduced form(誘導形)
# stage1 <- lm(educ~exper+I(exper^2)+motheduc+fatheduc, data=sampleset)

m_stucked_df <- c_stucked_df %>% filter(
  marijuana_month > 0,
  # alcohol_month > 0,
  # suicide_consideration > 0
)

# 今回、大麻の合法化を操作変数とすることで、平均的な処置量を仮定する
naclean_df$X_marijuana <- fitted(res_sunab)

stage1 <- fixest::feols(alcohol_month ~ X_marijuana | sitecode + year+ is_female + race7 + age ,data = naclean_df)
# stage1 <- fixest::feols(alcohol_month ~ X | is_female + race7 + age + bmi,data = c_stucked_df)
# stage1 <- fixest::feols(tabaco_per_month ~ X_marijuana ,data = c_stucked_df)

# naclean_df$M_alcohol <- fitted(stage1)

### 2nd stage
result <-fixest::feols(suicide_consideration ~ alcohol_month + X_marijuana | sitecode + year + is_female + race7 + age ,data= naclean_df)
# result <-fixest::feols(suicide_consideration ~ alcohol_month + X | is_female + race7 + age + bmi,data= c_stucked_df)
# result <-fixest::feols(suicide_plan ~ alcohol_month + X ,data= c_stucked_df)


summary(stage1)
summary(result)

etable(stage1)
etable(result)





t_naclean_df <- naclean_df%>% select(
  age,
  race4,
  race7,
  Treatment,
  year_L,
  ReferencePoint,
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






