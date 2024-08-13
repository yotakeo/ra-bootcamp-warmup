#Setting
setwd("/Users/yo/Desktop/ra-bootcamp-warmup")
getwd()
install.packages("tidyverse")
install.packages("devtools")
install.packages("dplyr")
install.packages("readxl")

library("tidyverse")
library("devtools")
library("dplyr")
library("readxl")
library("modelsummary")


### (a)Semister Dataの整形
#Data
df1 <- read.csv("01_cleaning/01_data/raw/semester_dummy/semester_data_1.csv")
df2 <- read.csv("01_cleaning/01_data/raw/semester_dummy/semester_data_2.csv")

colnames(df1) <- df1[1,]
df1 <- df1[-1,]

glimpse(df1) #いずれもcharacter
glimpse(df2) #x1,3,4,5,6:int, x2:chr

# df1のunitid, semester, quarter, year, Yをdoubleに変換
cols_to_convert1 <- c("unitid", "semester", "quarter", "year", "Y")
df1[cols_to_convert1] <- lapply(df1[cols_to_convert1], as.numeric)

glimpse(df1)
sum(is.na(df1))

df3 <- rename(df2, unitid = x1, instnm = x2, semester = x3, quarter = x4, year = x5, Y = x6)
glimpse(df3)

# データの結合
df_combined <- rbind(df1, df3)
summary(df_combined)

#セミスター制導入年を示すダミー変数の作成
#わからないため飛ばす
df_combined <- df_combined %>% 
  group_by(unitid) %>% 
  mutate(mean_sem=mean(semester)*19)
yryr <- unique(df_combined$mean_sem)
yryr


### (b)Gradrate Data [df_grad_91_10]
file_list <- list.files(path = "./warmup\ training\ package/01_data/raw/outcome", full.names = T)
#for (i in file_list) {
#  grad_bind1 <- read_xlsx(i)
#} 値がNAになってしまいうまくいかない
grad_bind <- purrr::map(file_list, ~read_xlsx(.))
df_grad <- bind_rows(grad_bind)

#データ型を数字に揃える
glimpse(df_grad) #totcohortsize, m_4yrgradsがcharacter

df_grad$totcohortsize <- as.numeric(df_grad$totcohortsize)
df_grad$m_4yrgrads <- as.numeric(df_grad$m_4yrgrads)

#女子学生、全員、男子学生の4年卒業率の計算
df_grad$womengradrate4yr <- df_grad$women_gradrate_4yr * 0.01
df_grad <- df_grad %>% 
  mutate(gradrate4yr = tot4yrgrads / totcohortsize) %>% 
  mutate(mengradrate4yr = m_4yrgrads / m_cohortsize)

#有効数字3桁に変更
df_grad$gradrate4yr <- round(df_grad$gradrate4yr, digits = 3)
df_grad$womengradrate4yr <- round(df_grad$womengradrate4yr, digits = 3)
df_grad$mengradrate4yr <- round(df_grad$mengradrate4yr, digits = 3)

glimpse(df_grad)

#yearが1991-2010のデータのみ抽出
df_grad_91_10 <- df_grad %>% 
  filter(year >= "1991" & year <= "2010")

summary(df_grad_91_10)


###(c) covariates dataの変形 [df_cov_wide_adj]
df_cov <- read_xlsx("./warmup\ training\ package/01_data/raw/covariates/covariates.xlsx")
glimpse(df_cov)

#university_idをunitidに変更
df_cov <- df_cov %>% 
  rename(unitid = university_id)

#aaaa削除
df_cov$unitid <- str_remove_all(df_cov$unitid, "aaaa")
glimpse(df_cov)

#データをwide型に変更
df_cov_wide <- df_cov %>% 
  pivot_wider(names_from = "category",
              values_from = "value")

#yearを揃える
summary(df_conbined) #1991-2010
summary(df_grad_91_10) #1991-2010
glimpse(df_cov_wide)

cols_to_convert2 <- c("unitid", "year", "instatetuition", "costs", "faculty", "white_cohortsize")
df_cov_wide[cols_to_convert2] <- lapply(df_cov_wide[cols_to_convert2], as.numeric)

df_cov_wide_91_10 <- df_cov_wide %>% 
  filter(year >= "1991" & year <= "2010")
summary(df_cov_wide_91_10)

#unitidをoutcome_data(df_grad_91_10)に合わせる 
uid_grad <- unique(df_grad_91_10$unitid)
uid_grad

df_cov_wide_adj <- df_cov_wide_91_10 %>% 
  filter(df_cov_wide_91_10$unitid %in% uid_grad)

uid_grad2 <- unique(df_cov_wide_adj$unitid)
a <- uid_grad2 %in% uid_grad
a

###(d) Master dataの作成
#(a)で作成できなかったデータフレームをインポート
df_semester <- read_csv("./warmup\ training\ package/01_data/intermediate/clean_semester_dummy.csv")

#df_semester, df_grad_91_10, df_cov_wide_adjを横につなげる
df_master <- left_join(df_semester, df_grad_91_10, by=c("unitid","year")) %>% 
  left_join(df_cov_wide_adj, by=c("unitid","year"))

write_csv(df_master, file = "./01_cleaning/master.csv")
