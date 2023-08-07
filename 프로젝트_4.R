# 라이브러리
library(rstudioapi)

# 데이터 불러오기
load("./data/hospital_final.rdata") # 채연+영섭+명호 통합 데이터

#--------------------------------------------------------------------
# 통합 url_list_all 만들기
gsub("\\(.*?)", "",hospital_final$Name[1]) # 데이터 이름에서 괄호+문자 삭제하고 추출하는 함수

url_list_all <- list()
cnt <- 0

for(i in 1:length(hospital_final$Name)){ # 통합 데이터의 병원 이름수 만큼 반복
  cnt <- cnt + 1
  
  # 요청 목록 만들기
  url_list_all[cnt] <- paste0("https://map.naver.com/v5/search/",
                               URLencode(as.character(gsub("\\(.*?)", "",hospital_final$Name[i]))))
  Sys.sleep(0.1) # 0.1초 텀을 두고 들어가는 코드
  msg <- paste0(i,"/",length(hospital_final$Name))
  cat(msg, "\n\n")
}

# 요청 목록 확인
head(url_list_all)
length(url_list_all)
# browseURL(paste0(url_list_all[1]))

save(url_list_all, file = "./data/url_list_all.rdata")
load("./data/url_list_all.rdata")
