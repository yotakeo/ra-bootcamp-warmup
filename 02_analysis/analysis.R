#Setting
setwd("/Users/yo/Desktop/ra-bootcamp-warmup")
getwd()
install.packages("tidyverse")
deinstall.packages("devtools")
install.packages("dplyr")
install.packages("summarytools")
install.packages("modelsummary")

library("tidyverse")
library("devtools")
library("dplyr")
library("summarytools")
library("modelsummary")

#Import
df_master <- read_csv("./01_cleaning/master.csv")

###(a)記述統計
#naを数える
sum(is.na(df_master))
na_count <- sapply(df_master, function(y) sum(is.na(y)))
na_count

#記述統計表作成
df_sum <- df_master %>% 
  select(semester, gradrate4yr, womengradrate4yr, mengradrate4yr, faculty, 
         totcohortsize, instatetuition, costs) %>% 
  descr(stats = c("mean", "sd", "min", "q1", "med",
                                     "q3", "max"), transpose = TRUE)

tb(df_sum)
print(df_sum)

#4年卒業率の平均推移
df_4yr_yr <- df_master %>%  
  group_by(year) %>% 
  summarise(mean_4yr = mean(gradrate4yr))

ggplot(df_4yr_yr) + 
  geom_line(aes(x = year, y = mean_4yr))

#semester導入率の推移
df_sem_yr <- df_master %>%  
  group_by(year) %>% 
  summarise(mean_sem = mean(semester))

ggplot(df_sem_yr) + 
  geom_line(aes(x = year, y = mean_sem))

#散布図の作成
#関数の作成方法がわからないので一旦飛ばす

###(b)回帰分析
regs <- list(
  "r1" = lm(formula = gradrate4yr ~ semester, data = df_master),
  "r2" = lm(formula = womengradrate4yr ~ semester, data = df_master),
  "r3" = lm(formula = mengradrate4yr ~ semester, data = df_master)
)

msummary(regs, stars = c("*" = .1, "**" = .05, "***" = .01), 
         gof_omit='RMSE|AIC|BIC|Log.Lik.')

