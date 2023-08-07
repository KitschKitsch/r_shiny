### 서울시/구 별 (신생아/소아)일인당 병원 비율 구하기 
#라이브러리
library(tidyverse); library(ggplot2)

# 데이터 불러오기
obgy <- read.csv("./data/obgy_new.csv")
ped <- read.csv("./data/ped_new.csv")
cn <- read.csv("./data/seoul_children_cn.csv")

#--------------------------------------------------
# 구별 산부인과 병원수
obgy_gu <- obgy %>% 
  group_by(SIG_KOR_NM) %>% 
  summarise(obgy_cnt = length(Name))

# 구별 신생아 수
newborn_gu <- cn %>% 
  group_by(SIG_KOR_NM) %>% 
  summarise(newborn = population_newborn)
# 서울 전체 신생아 수
newborn_all <- (sum(newborn_gu$newborn))

# 구별 산부인과 신생아 수
obgy_gu_total <- obgy_gu %>% 
  filter(newborn_gu$SIG_KOR_NM == obgy_gu$SIG_KOR_NM) %>% 
  add_column(newborn = newborn_gu$newborn)

# csv로 저장
write.csv(obgy_gu_total, file = "./data/obgy_gu_total.csv", row.names = F)
obgy_gu_total_2 <- read.csv("./data/obgy_gu_total.csv")
obgy_gu_total <-  add_column(obgy_gu_total, (obgy_gu_total$obgy_cnt/obgy_gu_total$newborn))
names(obgy_gu_total)[4] <- "per_newb"
obgy_gu_total <- subset(obgy_gu_total)[,c(1,4)]



#--------------------------------------------------
# 구별 소아청소년과 병원수
ped_gu <- ped %>% 
  group_by(SIG_KOR_NM) %>% 
  summarise(ped_cnt = length(Name))

# 구별 소아 수
child_gu <- cn %>% 
  group_by(SIG_KOR_NM) %>% 
  summarise(child = population_child)

# 서울 전체 소아 수
child_all <- (sum(child_gu$child))

# 구별 소아과 소아 수
ped_gu_total <- ped_gu %>% 
  filter(child_gu$SIG_KOR_NM == ped_gu$SIG_KOR_NM) %>% 
  add_column(child = child_gu$child)

# csv로 저장
write.csv(ped_gu_total, file = "./data/ped_gu_total.csv", row.names = F)
ped_gu_total_2 <- read.csv("./data/ped_gu_total.csv")

ped_gu_total <-  add_column(ped_gu_total, (ped_gu_total$ped_cnt/ped_gu_total$child))
names(ped_gu_total)[4] <- "per_chil"
ped_gu_total <- subset(ped_gu_total)[,c(1,4)]


#--------------------------------------------------
# 신생아 당 산부인과, 소아 당 소아과 합치기
per_hos <- merge(obgy_gu_total, ped_gu_total, by = "SIG_KOR_NM")
row <- c("서울시전체",
         sum(obgy_gu_total_2$obgy_cnt)/sum(obgy_gu_total_2$newborn),
         sum(ped_gu_total_2$ped_cnt)/sum(ped_gu_total_2$child))

per_hos <- rbind(per_hos, row)
per_hos <- per_hos[c(26,1:25),]

save(per_hos, file = "./data/per_hos.rdata")
