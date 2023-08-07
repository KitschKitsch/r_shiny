### 병원 데이터 불러와서 전처리 후 sf형(좌표정보)dml rdata 만들기  
library(stringr)
# 데이터 불러오기
# 산부인과
obgy <- read.csv("./data/obgy_1.csv")
obgy_data <- strsplit(obgy$Addr, split=" ")

# 문자열에서 특정 위치의 단어 추출
obgy_info <- word(obgy_data[1:1093],2)

# 모든 특수문자 제거
obgy_info <- gsub("[[:punct:]]", "", obgy_info)
obgy$SIG_KOR_NM <- obgy_info
head(obgy)
str(obgy)


# 소아청소년과
ped <- read.csv("./data/ped_1.csv")
ped_data <- strsplit(ped$Addr, split=" ")

# 문자열에서 특정 위치의 단어 추출
ped_info <- word(ped_data[1:3089],2)

# 모든 특수문자 제거
ped_info <- gsub("[[:punct:]]", "", ped_info)
ped$SIG_KOR_NM <- ped_info
head(ped)
str(ped)


# 결측치 확인
table(is.na(obgy)) # 산부인과 결측치 8개
table(is.na(ped)) # 소아청소년과 결측치 16개

obgy <- na.omit(obgy) # 산부인과 결측치 제거
ped <- na.omit(ped) # 소아청소년과 결측치 제거


# 산부인과, 소아청소년과 구정보 포함한 csv파일
write.csv(obgy,file="./data/obgy_new.csv", row.names = F)
write.csv(ped,file="./data/ped_new.csv", row.names = F)

obgy <- read.csv("./data/obgy_new.csv")
ped <- read.csv("./data/ped_new.csv")


# 지오 데이터프레임 만들기
# install.packages("sp")
library(sp)
# install.packages("sf")
library(sf)

# 산부인과 좌표값 설정
coordinates(obgy) <- ~Lon + Lat # sp형식(맨 앞열에 좌표)

# 좌표계 정의(WGS84)
proj4string(obgy) <- "+proj=longlat +datum=WGS84 +no_defs" # WGS84 좌표계로

# sp형을 데이터 변경이 자유로운 sf형으로 변환(맨 뒷열에 좌표)
obgy <- st_as_sf(obgy)
head(obgy)


# 소아청소년과 좌표값 설정
coordinates(ped) <- ~Lon + Lat # sp형식(맨 앞열에 좌표)

# 좌표계 정의(WGS84)
proj4string(ped) <- "+proj=longlat +datum=WGS84 +no_defs" # WGS84 좌표계로

# sp형을 데이터 변경이 자유로운 sf형으로 변환(맨 뒷열에 좌표)
ped <- st_as_sf(ped)
head(ped)


# 시각화
plot(obgy$geometry, axes = T, pch = 1) # pch 1: 원, 2: 세모, 3: 십자가, 4: X표
plot(ped$geometry, axes = T, pch = 1)


# install.packages("leaflet")
library(leaflet)
leaflet() %>% # 빈 지도 위에
  addTiles() %>% 
  addCircleMarkers(data = obgy[1:1000, ], # obgy 1000개까지 Circle Marker로 
                   label = ~Name, # 병원 이름을
                   radius = 5) # 반지름 5
str(obgy)

# install.packages("leaflet")
library(leaflet)
leaflet() %>% # 빈 지도 위에
  addTiles() %>% 
  addCircleMarkers(data = ped[1:1000, ], 
                   label = ~Name,
                   radius = 5) #
str(ped)


# 지오 데이터프레임 저장하기(sf형)
save(obgy, file = "./data/obgy.rdata")
save(ped, file = "./data/ped.rdata")

load("./data/obgy.rdata")
load("./data/ped.rdata")



# --------------------------------------------------
### 경계선 데이터 전처리하기  
# 라이브러리
library(sp); library(sf); library(rgdal);
library(tidyverse); library(rgdal); library(leaflet);
library(raster); library(leaflet.extras)

# 대한민국 지도 경계선 불러오기 -> options = 'ENCODING=CP949'를 사용하여 한글 깨짐 방지
korea <- st_read("./data/CTPRVN_202005/CTPRVN.shp", options = 'ENCODING=CP949')
map <- st_read("./data/SIG_202005/SIG.shp", options = 'ENCODING=CP949')

# shp 파일을 leaflet에서 사용하는 "WGS84" 좌표계를 기준으로 변환
# sf 객체이기 때문에 st_transform를 사용
korea <- st_transform(x = korea, sp::CRS("+proj=longlat +datum=WGS84"))
map <- st_transform(x = map, sp::CRS("+proj=longlat +datum=WGS84"))

# 전국에서 서울시 행정 구역 데이터만 필요함으로 1 ~ 25행까지만 사용
seoul_map <- map[1:25,]

# 전국에서 서울시 데이터만 필요함으로 1행만 사용
seoul_all <- korea[1,]

# 지도에 서울시 행정구역 경계선 그리기
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(127.001699, 37.564214, zoom = 11) %>%
  addPolygons(data = seoul_map,
              fill = NA,
              weight = 3,
              color = "hotpink") %>%
  addPolygons(data = seoul_all,
              fill = NA,
              weight = 5,
              color = "black") %>% 
  addCircleMarkers(data = obgy, 
                   label = ~Name,
                   radius = 2, col = "red", group = "산부인과") %>%  # 반지름 5
  addCircleMarkers(data = ped,
                   label = ~Name,
                   radius = 2, col = "blue", group = "소아청소년과") %>% 
  addLayersControl(baseGroups = c("산부인과", "소아청소년과"), # 체크박스
                   options = layersControlOptions(collapsed = F)) 
