# 일반 인증키(Encoding): 
service_key = "Oq8716m%2B%2Be846ymhJLGO0k1vUF0kQbBNjBruiLmlmA2lLBIdGn9UaY0DqqiDDj81kdcn1R4zYBeb%2FLQy3NqWUA%3D%3D"

# Call Back URL(병‧의원 목록정보 조회): http://apis.data.go.kr/B552657/HsptlAsembySearchService/getHsptlMdcncListInfoInqire

# 1)병원이름, 2)인증키
# 자료 수집
# install.packages("rstudioapi")
library(rstudioapi)


# --------------------------------------------------
# 서울시 산부인과 요청 목록 만들기
url_list <- list()
cnt <- 0

{ 
  cnt <- cnt + 1
    # 요청 목록 만들기
    url_list[cnt] <- paste0("http://apis.data.go.kr/B552657/HsptlAsembySearchService/getHsptlMdcncListInfoInqire", # Call Back URL
                            "?", 
                            "Q0=",URLencode("서울특별시"), # 시군구 = '서울특별시'에서
                            "&QD=D011", # 진료코드는 산부인과인 데이터들을 
                            "&pageNo=1", # 1페이지에
                            "&numOfRows=1093", # 조건 만족하는 1093개 표시! <-(총 개수는 요조건만 빼고 하면 맨 아래에 나옴!)
                            "&serviceKey=", service_key) # 인증키
    Sys.sleep(0.1) # 0.1초 텀을 두고 들어가는 코드
}


# 요청 목록 확인
head(url_list)
length(url_list)
# browseURL(paste0(url_list[1])) # <- 요놈으루 요청목록  브라우저로 열어보기


# 크롤링 실행
# install.packages("XML")
# install.packages("data.table")
# install.packages("stringr")
library(XML)
library(data.table)
library(stringr)


# 크롤링 시작 
raw_data <- list()
root_node <- list()
total <- list()

# for문 i
for (i in 1:length(url_list)) { # 1부터 url개수만큼 반복
  raw_data[[i]] <- xmlTreeParse(url_list[i], # url_list 파싱해서
                                useInternalNodes = T, # 모든 노드 가져와서
                                encoding = "UTF-8") #인코딩해서 XML raw data(XML 자체) 저장
  
  root_node[[i]] <- xmlRoot(raw_data[[i]]) # 루트 노드 <response> 이하를 저장
  
  items <- root_node[[i]][[2]][['items']] # <header> 말고 <body>에서, <items>만 저장
  size <- xmlSize(items)    # item의 개수 = 행의 개수
  
  item <- list()
  item_temp <- data.table()
  Sys.sleep(0.1) # 0.1초 텀을 두고 진행
  
  # for문 j
  for (j in 1:size) { # 1부터 item의 개수 = 행의 개수만큼 반복
    item_tp <- xmlSApply(items[[j]], xmlValue) # xmlSApply: 여러 item의 '값'들을 따로따로 저장!
    item_temp <- data.table(ID =　item_tp["hpid"], # item_tp에서 필요한 '값'들만 테이블 형태로!
                            Name = item_tp["dutyName"],
                            DivNam = item_tp["dutyDivNam"],
                            Tel = item_tp["dutyTel1"],
                            Eryn = item_tp["dutyEryn"],
                            Addr = item_tp["dutyAddr"],
                            Lat = item_tp["wgs84Lat"],
                            Lon = item_tp["wgs84Lon"])
    item[[j]] <- item_temp } # item 개수(행 개수) 만큼 반복 저장
  hos_rbind <- rbindlist(item) # item 테이블을 통합저장

  write.csv(hos_rbind, file = "./data/obgy_1.csv", row.names = F)
  # 진행 상황 메시지
  msg <- paste0(i,"/",length(url_list))
  cat(msg, "\n\n")
}
# for문 끝
# csv 파일 생성됨!!!



#--------------------------------------------------
# 서울시 소아과 요청 목록 만들기
url_list <- list()
cnt <- 0

{
  cnt <- cnt + 1
  # 요청 목록 만들기
  url_list[cnt] <- paste0("http://apis.data.go.kr/B552657/HsptlAsembySearchService/getHsptlMdcncListInfoInqire", # Call Back URL
                          "?", 
                          "Q0=",URLencode("서울특별시"),
                          "&QD=D002", # 진료코드=소아청소년과
                          "&pageNo=1",
                          "&numOfRows=3089",
                          "&serviceKey=", service_key) # 인증키
  Sys.sleep(0.1) # 0.1초 텀을 두고 들어가는 코드
}


# 요청 목록 확인
head(url_list)
length(url_list)
# browseURL(paste0(url_list[1]))


# 크롤링 실행
# install.packages("XML")
# install.packages("data.table")
# install.packages("stringr")
library(XML)
library(data.table)
library(stringr)


# 크롤링 시작 
raw_data <- list()
root_node <- list()
total <- list()

# for문 시작
for (i in 1:length(url_list)) {
  raw_data[[i]] <- xmlTreeParse(url_list[i], # url_list 파싱해서
                                useInternalNodes = T, # 모든 노드 가져와서
                                encoding = "UTF-8") #인코딩해서 XML raw data(XML 자체) 저장
  root_node[[i]] <- xmlRoot(raw_data[[i]]) # 루트 노드 <response> 이하를 저장
  items <- root_node[[i]][[2]][['items']] # <header> 말고 <body>에서, <items>만 저장
  size <- xmlSize(items)    # item의 개수 = 행의 개수
  
  item <- list()
  item_temp <- data.table()
  Sys.sleep(0.1) # 0.1초 텀을 두고 진행
  
  for (j in 1:size) {
    item_tp <- xmlSApply(items[[j]], xmlValue) # xmlSApply: 여러 item의 '값'들을 따로따로 저장!
    item_temp <- data.table(ID =　item_tp["hpid"], # item_tp에서 필요한 '값'들만 테이블 형태로!
                            Name = item_tp["dutyName"],
                            DivNam = item_tp["dutyDivNam"],
                            Tel = item_tp["dutyTel1"],
                            Eryn = item_tp["dutyEryn"],
                            Addr = item_tp["dutyAddr"],
                            Lat = item_tp["wgs84Lat"],
                            Lon = item_tp["wgs84Lon"])
    item[[j]] <- item_temp } # item 개수(행 개수) 만큼 반복 저장
  hos_rbind <- rbindlist(item) # item 테이블을 통합저장
  
  write.csv(hos_rbind, file = "./data/ped_1.csv", row.names = F)
  
  # 진행 상황 메시지
  msg <- paste0(i,"/",length(url_list))
  cat(msg, "\n\n")
}
# for문 끝
# csv 파일 생성됨!!!