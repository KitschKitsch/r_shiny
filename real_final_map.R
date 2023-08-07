###최종 shiny에 구현하기
# 1. 라이브러리 준비
library(sp); library(sf); library(leaflet); library(tidyverse);
library(raster); library(rgdal); library(tmap); library(mapedit);
library(leafem); library(ggfortify); library(grid); library(shiny);
library(DT); library(shinyWidgets); library(gridExtra);library(dplyr); 
library(leaflet.extras); library(packcircles); library(shinythemes)

# 2. 데이터 불러오기
load("./data/hospital_final.rdata")
load("./data/hos_info_final.rdata")
load("./data/hos_sum_freq.rdata")
load("./data/seoul_hospital_children.rdata")

hospital_final$Name <- paste0("<a href='",hospital_final$link,"' target='_blank'>",hospital_final$Name,"</a>")
hospital_final[hospital_final$Eryn==1, "Eryn"]="YES"
hospital_final[hospital_final$Eryn==2, "Eryn"]="NO"

# 대한민국 지도 경계선 불러오기 -> options = 'ENCODING=CP949'를 사용하여 한글 깨짐 방지
map <- st_read("./data/SIG_202005/SIG.shp", options = 'ENCODING=CP949')
# shp 파일을 leaflet에서 사용하는 "WGS84" 좌표계를 기준으로 변환
# sf 객체이기 때문에 st_transform를 사용
map <- st_transform(x = map, sp::CRS("+proj=longlat +datum=WGS84"))
# 전국에서 서울시 행정 구역 데이터만 필요함으로 1 ~ 25행까지만 사용
seoul_map <- map[1:25,]

# 
total <- unique(hospital_final$SIG_KOR_NM)
total[26] <- "서울시 전체"

# 구별 병원 수에 따라 색깔 단계를 나타내기
pal1 <-  colorBin(palette = 'Blues', domain = seoul_hospital_children$obgy)
pal2 <-  colorBin(palette = 'Greens', domain = seoul_hospital_children$ped)
# 구별 인구 수에 따라 색깔 단계를 나타내기
pal3 <-  colorBin(palette = 'Oranges', domain = seoul_hospital_children$child)
pal4 <-  colorBin(palette = 'Reds', domain = seoul_hospital_children$newborn)


# ui
ui <- bootstrapPage( theme = shinytheme("united"), # shiny 테마
                 navbarPage("Finding Pediatrics and Obstetrics & Gynecology in Seoul"),
                 tags$style(type = "text/css", "html, body {width:100%;height:100%;}"),
                 
                 leafletOutput(outputId = "map", width = "80%", height = "70%"),
            
                   #상단: 왼쪽 - 지도, 오른쪽 - 선택란
                   # 지도
                          # 지역과 병원 선택해서 지도, 테이블에 나타내기(column이랑 column 중 지도 넣어보고 결정)
                          absolutePanel(right=5, top = 50,
                                 
                                 # 지역 선택
                                 selectInput(inputId = "gu", #ID
                                             label = tags$span(style = "color: mediumaquamarine; font-size: 13pt;",  "Please select a area (District)"),
                                             choices = total,
                                             selected = total[26]),
                                 
                                 # 병원 선택
                                 selectInput(inputId = "ped_obg",
                                             label = tags$span(style = "color: mediumaquamarine; font-size: 13pt;",  "Please select a hospital"),
                                             choices = unique(hospital_final$Hospital),
                                             selected = unique(hospital_final$Hospital)[1]),
                                
                                plotOutput("circle"), 
                          ),

                          # 하단: 첫번째 - 데이터 테이블, 두번째 - 시각화
                          column(width = 12, bottom = 50,
                                 dataTableOutput(outputId = "hospital information"),
                                 div(style = "height:10px;"),
                          tabsetPanel(
                            # 테이블
                            tabPanel("Hospital Information",
                                     DT::dataTableOutput("table")
                            ),
                            # 그래프
                            tabPanel("Visualization",# 탭에 노출되어 있는 이름
                                     # style에서 border로 테두리 설정, margin으로 테두리 밖 간격 설정
                                     column(6, tags$div(style = "border: 2px solid black;margin-bottom:10px;", plotOutput("obgy_ped"))),
                                     column(6, tags$div(style = "border: 2px solid black;margin-bottom:10px;", plotOutput("pop"))),
                                     column(6, tags$div(style = "border: 2px solid black;margin-top:10px;", plotOutput("per_newb"))),
                                     column(6, tags$div(style = "border: 2px solid black;margin-top:10px;", plotOutput("per_chil")))))# output의 결과
                          
                   ))


# server
server <- function(input, output, session){
  # 반응식 1
  # 선택한 병원과 서울시 구에 해당하는 데이터 저장 -> 지도에 사용
  selected_Hospital = reactive({
    selected_Hospital = subset(hospital_final,
                               if(input$gu %in% total[1:25]){
                                 Hospital == input$ped_obg & SIG_KOR_NM == input$gu
                               }else{Hospital == input$ped_obg}
    )
    return(selected_Hospital)
  })
  
  # 지도 출력
  output$map <- renderLeaflet({
    leaflet(selected_Hospital(), width = "100%", height = "100%") %>%
      addTiles() %>%
      setView(127.001699, 37.564214, zoom = 11) %>%
      # (1) 서울시 경계선 지도 데이터
      addPolygons(data = seoul_map,
                  fill = NA,
                  weight = 3,
                  color = "black",
                  group = '서울시 구 위치') %>%
      # (2)산부인과 데이터
      addPolygons(data = seoul_hospital_children,
                  weight = 3,
                  fillOpacity = 0.5,
                  fillColor = ~pal1(obgy),
                  color = 'black',
                  popup = ~paste(sep = "<br/>", SIG_KOR_NM, obgy),
                  highlight = highlightOptions(weight = 3,
                                               color = 'red'),
                  group = '산부인과 수') %>%
      # (3) 소아과 데이터
      addPolygons(data = seoul_hospital_children,
                  weight = 3,
                  fillOpacity = 0.5,
                  fillColor = ~pal2(ped),
                  color = 'black',
                  popup = ~paste(sep = "<br/>", SIG_KOR_NM, ped),
                  highlight = highlightOptions(weight = 3,
                                               color = 'red'),
                  group = '소아과 수') %>%
      # (4) 전체 어린이 데이터
      addPolygons(data = seoul_hospital_children,
                  weight = 3,
                  fillOpacity = 0.5,
                  fillColor = ~pal3(child),
                  color = 'black',
                  popup = ~paste(sep = "<br/>", SIG_KOR_NM, child),
                  highlight = highlightOptions(weight = 3,
                                               color = 'red'),
                  group = '전체 어린이 명수') %>%
      # (5) 신생아 데이터
      addPolygons(data = seoul_hospital_children,
                  weight = 3,
                  fillOpacity = 0.5,
                  fillColor = ~pal4(newborn),
                  color = 'black',
                  popup = ~paste(sep = "<br/>", SIG_KOR_NM, newborn),
                  highlight = highlightOptions(weight = 3,
                                               color = 'red'),
                  group = '신생아 명수') %>%
      addLayersControl(baseGroups = c('서울시 구 위치', '산부인과 수', '소아과 수', '전체 어린이 명수', '신생아 명수'),
                       position = "topleft") %>%
      # 서울시 구별 병원 이름
      addCircleMarkers(popup = ~paste(sep = "<br/>", Name, SIG_KOR_NM),
                       lng = ~X, lat = ~Y,
                       radius = 5, col = ifelse(input$ped_obg == "산부인과", "red", ifelse(input$ped_obg == "소아과", "blue", "yellow")))
  })
  
  # 반응식 2 : TAB1 테이블
  # 지역 선택하는 테이블 만들기
  make_table1 <- selectInput("gu",
                             label ="Please select a area (GU)",
                             choices = as.list(unique(hospital_final$SIG_KOR_NM)))
  # 병원 선택하는 테이블 만들기
  make_table2 <- selectInput(inputId = "ped_obg",
                             label = tags$span(style = "color: mediumaquamarine; font-size: 18pt; font-family:Times New Roman;",  "Please select a hospital"),
                             choices = unique(hospital_final$Hospital))
  # 지역 선택 renderUI
  output$gu<- renderUI({make_table1})
  # 병원 선택 renderUI
  output$ped_obg <- renderUI({make_table2})
  # 지역과 병원 선택에 따라 표 나타내기
  dataset <-reactive({
    dataset <- subset(hospital_final, SIG_KOR_NM== input$gu & Hospital== input$ped_obg, select = c(Name, Addr, DivNam, Tel, Eryn))
    return(dataset)
  })
  
  # 지역과 병원 선택에 따른 결과와 엑셀 다운받기 버튼 생성
  output$table<-renderDataTable(
    datatable(dataset(),
              escape = F,
              extensions = 'Buttons',
              options = list(dom = 'Bfrtrip',
                             scrollY = 300,
                             scrollCollapse = F,
                             paging = T,
                             buttons = c('excel'))))  #테이블에 나오는 데이터 출력하기
  # 반응식 3 : 지역 선택에 따른 시각화
  selected_hos_info_final = reactive({
    selected_hos_info_final = subset(hos_info_final,
                                     SIG_KOR_NM == "서울시전체" | SIG_KOR_NM == input$gu)
    return(selected_hos_info_final)
  })
  
  # circular packing plot
  output$circle <- renderPlot({
    tryCatch({
      ggplot() + 
        geom_polygon(data = circleLayoutVertices(circleProgressiveLayout((hos_sum_freq %>% 
                                                                            filter(SIG_KOR_NM == input$gu &
                                                                                     Hospital == input$ped_obg) %>% 
                                                                            group_by(DivNam) %>% 
                                                                            count())[,2], sizetype = 'area'), npoints = 50),
                     aes(x, y, group = id, fill = as.factor(id)),
                     colour = "grey", # 원의 색깔
                     alpha = 0.4) + # 원의 투명도
        geom_label(data = cbind((hos_sum_freq %>% 
                                   filter(SIG_KOR_NM == input$gu &
                                            Hospital == input$ped_obg) %>% 
                                   group_by(DivNam) %>% 
                                   count()),
                                circleProgressiveLayout((hos_sum_freq %>% 
                                                           filter(SIG_KOR_NM == input$gu &
                                                                    Hospital == input$ped_obg) %>% 
                                                           group_by(DivNam) %>% 
                                                           count())[,2], sizetype = 'area')),
                   aes(x, y, size = 5, label = DivNam), 
                   alpha = 0.7, fill ="white", color = "black") + #원 이름 붙이기
        scale_size_continuous(range = c(1,4)) +
        theme_void() +   # General theme:
        theme(legend.position="none") +
        coord_equal() # 가로 세로 비율 맞추기
    },
    # 해당 에러 메세지 무시하는 코드
    error = function(e) {
    })
  })
  
  # 서울시/구 산부인과, 소아과 수 
  output$obgy_ped <- renderPlot({
    gridplot <- grid.arrange(ggplot(data = selected_hos_info_final(),
                                    aes(x = SIG_KOR_NM <- factor(SIG_KOR_NM, levels = c(input$gu, "서울시전체")),
                                        y = obgy,
                                        fill = SIG_KOR_NM)) +
                               geom_col(position = "dodge", alpha=0.8, fill = c("#E95420", "orange")) +
                               theme(panel.background = element_blank()) +
                               coord_flip() +
                               theme(legend.position = "none") +
                               geom_label(aes(label = obgy), hjust = 1.5,
                                          alpha = 0.7, fill ="white", color = "black",
                                          family='serif', fontface='italic',
                                          position = position_dodge(.9), size = 5) +
                               # x축에 나타나는 숫자 blank로 설정
                               theme(axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank()) +
                               xlab(element_blank()) +
                               ylab("OBGY count"),
                             ggplot(data = selected_hos_info_final(),
                                    aes(x = SIG_KOR_NM <- factor(SIG_KOR_NM, levels = c(input$gu, "서울시전체")),
                                        y = ped,
                                        fill = SIG_KOR_NM)) +
                               geom_col(position = "dodge", alpha=0.8, fill = c("#E95420", "orange")) +
                               theme(panel.background = element_blank()) +
                               coord_flip() +
                               theme(legend.position = "none") +
                               geom_label(aes(label = ped), hjust = 1.5,
                                          alpha = 0.7, fill ="white", color = "black",
                                          family='serif', fontface='italic',
                                          position = position_dodge(.9), size = 5) +
                               # x축에 나타나는 숫자 blank로 설정
                               theme(axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank()) +
                               xlab(element_blank()) +
                               ylab("PED count"))
    print(gridplot)
    
  })
  
  # 서울시/구 신생아, 소아 수
  output$pop <- renderPlot({
    gridplot <- grid.arrange(ggplot(data = selected_hos_info_final(),
                                    aes(x = SIG_KOR_NM <- factor(SIG_KOR_NM, levels = c(input$gu, "서울시전체")),
                                        y = pop_newb,
                                        fill = SIG_KOR_NM)) +
                               geom_col(position = "dodge", alpha=0.8, fill = c("#E95420", "green")) +
                               theme(panel.background = element_blank()) +
                               coord_flip() +
                               theme(legend.position = "none") +
                               geom_label(aes(label = scales::comma(as.numeric(pop_newb))), hjust = 1.5,
                                          alpha = 0.7, fill ="white", color = "black",
                                          family='serif', fontface='italic',
                                          position = position_dodge(.9), size = 5) +
                               # x축에 나타나는 숫자 blank로 설정
                               theme(axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank()) +
                               xlab(element_blank()) +
                               ylab("total newborn"),
                             ggplot(data = selected_hos_info_final(),
                                    aes(x = SIG_KOR_NM <- factor(SIG_KOR_NM, levels = c(input$gu, "서울시전체")),
                                        y = pop_child,
                                        fill = SIG_KOR_NM)) +
                               geom_col(position = "dodge", alpha=0.8, fill = c("#E95420", "green")) +
                               theme(panel.background = element_blank()) +
                               coord_flip() +
                               theme(legend.position = "none") +
                               geom_label(aes(label = scales::comma(as.numeric(pop_child))), hjust = 1.5,
                                          alpha = 0.7, fill ="white", color = "black",
                                          family='serif', fontface='italic',
                                          position = position_dodge(.9), size = 5) + 
                               # x축에 나타나는 숫자 blank로 설정
                               theme(axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank()) +
                               xlab(element_blank()) +
                               ylab("total child"))
    print(gridplot)
  })
  
  # 신생아 당 산부인과 수
  output$per_newb <- renderPlot({
    ggplot(data = selected_hos_info_final(),
           aes(x = SIG_KOR_NM <- factor(SIG_KOR_NM, levels = c("서울시전체", input$gu)),
               y = round(as.numeric(per_newb),4),
               fill = SIG_KOR_NM)) +
      geom_col(position = "dodge", alpha=0.8, fill = c("#E95420", "blue")) +
      geom_label(aes(label = round(as.numeric(per_newb),4)), vjust = 1.5,
                 alpha = 0.7, fill ="white", color = "black",
                 family='serif', fontface='italic',
                 position = position_dodge(.9), size = 5) +
      theme(panel.background = element_blank()) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      xlab(element_blank()) +
      ylab("OBGY per newborn")
  })
  
  # 소아 당 소아과 수
  output$per_chil <- renderPlot({
    ggplot(data = selected_hos_info_final(),
           aes(x = SIG_KOR_NM <- factor(SIG_KOR_NM, levels = c("서울시전체", input$gu)),
               y = round(as.numeric(per_chil),4),
               fill = SIG_KOR_NM)) +
      geom_col(position = "dodge", alpha=0.8, fill = c("#E95420", "black")) +
      geom_label(aes(label = round(as.numeric(per_chil),4)), vjust = 1.5,
                 alpha = 0.7, fill ="white", color = "black",
                 family='serif', fontface='italic', position = position_dodge(.9), size = 5) +
      theme(panel.background = element_blank()) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      xlab(element_blank()) +
      ylab("PED per child")
  })
}


# shinyApp
shinyApp(ui, server)
