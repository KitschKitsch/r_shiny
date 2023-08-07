#0323링크 완
# 1. 라이브러리 준비
library(sp); library(sf); library(leaflet); library(tidyverse);
library(raster); library(rgdal); library(tmap); library(mapedit);
library(leafem); library(ggfortify); library(grid); library(shiny);
library(DT); library(shinyWidgets); library(gridExtra); library(packcircles);
# library(shinythemes); 

# 2. 데이터 불러오기(3개)
load("./data/hospital_final.rdata")
load("./data/hos_info_final.rdata")
load("./data/hos_sum_freq.rdata")

hospital_final$Name <- paste0("<a href='",hospital_final$link,"' target='_blank'>",hospital_final$Name,"</a>")

hospital_final[hospital_final$Eryn==1, "Eryn"]="YES"
hospital_final[hospital_final$Eryn==2, "Eryn"]="NO"


# ui
ui <- bootstrapPage( #theme = shinytheme("united"), # shiny 테마
                 navbarPage("Finding Pediatrics and Obstetrics & Gynecology in Seoul"),
                 tags$style(type = "text/css", "html, body {width:100%;height:100%;}"),
                 
                   #상단: 왼쪽 - 지도, 오른쪽 - 선택란
                   # 지도
                   column(width = 12, right=50, left=50,
                          leafletOutput(outputId = "map", width = "100%", height = "100%"),
                          # 지역과 병원 선택해서 지도, 테이블에 나타내기(absolutePanel이랑 column 중 지도 넣어보고 결정)
                          column(width = 5, right=50, left=50, bottom=50,
                                 
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
                          column(width = 12,
                                 dataTableOutput(outputId = "hospital information"),
                                 div(style = "height:10px;")),
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
                   )))
                          


# server
server <- function(input, output, session){
  # 반응식 1 : TAB1 테이블
  # 지역 선택하는 테이블 만들기
  make_table1 <- selectInput("gu",
                             label ="Please select a area (GU)",
                             choices = as.list(unique(hospital_final$SIG_KOR_NM)))
  
  # 병원 선택하는 테이블 만들기
  make_table2 <- selectInput(inputId = "ped_obg",
                             label = tags$span(style = "color: mediumaquamarine; font-size: 18pt;
                                               font-family:Times New Roman;",  "Please select a hospital"),
                             choices = unique(hospital_final$Hospital))
  
  # 지역 선택 renderUI
  output$gu<- renderUI({make_table1})
  
  # 병원 선택 renderUI
  output$ped_obg <- renderUI({make_table2})
  
  # 지역과 병원 선택에 따라 표 나타내기
  dataset <-reactive({
    dataset <- subset(hospital_final, SIG_KOR_NM== input$gu & Hospital== input$ped_obg,
                      select = c(Name, Addr, DivNam, Tel, Eryn))
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
  
  # 반응식 2 : 지역 선택에 따른 시각화
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
  
#-----------------------------------------------------------------------------------------
  
  # 반응식
  # 1.선택한 병원과 서울시 구에 해당하는 데이터 저장 -> 지도에 사용
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
      # 1. 서울시 경계선 지도 데이터
      addPolygons(data = seoul_map,
                  fill = NA,
                  weight = 3,
                  color = "black",
                  group = '서울시 구 위치') %>%
      # 2. 산부인과 데이터
      addPolygons(data = seoul_hospital_children,
                  weight = 3,
                  fillOpacity = 0.5,
                  fillColor = ~pal1(obgy),
                  color = 'black',
                  popup = ~paste(sep = "<br/>", SIG_KOR_NM, obgy),
                  highlight = highlightOptions(weight = 3,
                                               color = 'red'),
                  group = '산부인과 수') %>%
      # 3. 소아과 데이터
      addPolygons(data = seoul_hospital_children,
                  weight = 3,
                  fillOpacity = 0.5,
                  fillColor = ~pal2(ped),
                  color = 'black',
                  popup = ~paste(sep = "<br/>", SIG_KOR_NM, ped),
                  highlight = highlightOptions(weight = 3,
                                               color = 'red'),
                  group = '소아과 수') %>%
      # 4. 전체 어린이 데이터
      addPolygons(data = seoul_hospital_children,
                  weight = 3,
                  fillOpacity = 0.5,
                  fillColor = ~pal3(child),
                  color = 'black',
                  popup = ~paste(sep = "<br/>", SIG_KOR_NM, child),
                  highlight = highlightOptions(weight = 3,
                                               color = 'red'),
                  group = '전체 어린이 명수') %>%
      # 5. 신생아 데이터
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
                       radius = 5, col = "blue")
  })
}


# shinyApp
shinyApp(ui, server)