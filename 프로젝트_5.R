### shiny에 서울시/구 별 (신생아,소아) 일인당 병원 비율 그래프 표시하기
# 데이터 준비
load("./data/hospital_final.rdata")
load("./data/hos_info_final.rdata")

# 라이브러리
# install.packages("plotly")
library(shiny); library(leaflet); library(leaflet.extras);
library(tidyverse); library(shinyWidgets); library(plotly)  


# ui
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  
  # 지도
  leafletOutput(outputId = "map", width = "100%", height = "100%"),
  
  absolutePanel(top = 5, right = 5,
                selectInput(inputId = "district",
                            label = tags$span(style="display:inline-block; margin:10px; padding:5px; border:dashed 2px #FFCC00; color:#663300; font-weight:bold; font-style:italic; background:white; opacity:0.9;", "Please select the district"), # 별로 안중요한 옵션;
                            choices = unique(hospital_final$SIG_KOR_NM),
                            selected = unique(hospital_final$SIG_KOR_NM)[1]),
                
                selectInput(inputId = "hospital",
                            label = tags$span(style="display:inline-block; margin:10px; padding:5px; border:dashed 2px #FFCC00; color:#663300; font-weight:bold; font-style:italic; background:white; opacity:0.9;", "Please select the hospital"),
                            choices = unique(hospital_final$Hospital),
                            selected = unique(hospital_final$Hospital)[1]),
                
                wellPanel(style = "opacity: 0.95",
                          plotOutput("per_newb", height = 250)),
                
                wellPanel(style = "opacity: 0.95",
                          plotOutput("per_chil", height = 250))))
                            
                            
# server
server <- function(input, output, session){
  selected_hospital_final = reactive({
    selected_hospital_final = subset(hospital_final,
                           SIG_KOR_NM == input$district)
    return(selected_hospital_final)
  })
  
  selected_hos_info_final = reactive({
    selected_hos_info_final = subset(hos_info_final,
                           SIG_KOR_NM == "서울시전체" | SIG_KOR_NM == input$district)
    return(selected_hos_info_final)
  })
  
  # 신생아 당 산부인과 수 
  output$per_newb <- renderPlot({
    ggplot(data = selected_hos_info_final(),
           aes(x = SIG_KOR_NM <- factor(SIG_KOR_NM, levels = c("서울시전체", input$district)),
               y = round(as.numeric(per_newb),4),
               fill = SIG_KOR_NM)) +
      geom_col(position = "dodge", fill = c("red", "blue")) +
      geom_text(aes(label = round(as.numeric(per_newb),4)), vjust = 1.5, color = "white", 
                position = position_dodge(.9), size = 3) +
      xlab("District") +
      ylab("OBGY per newborn")
  })
  
  # 소아 당 소아과 수 
  output$per_chil <- renderPlot({
    ggplot(data = selected_hos_info_final(),
           aes(x = SIG_KOR_NM <- factor(SIG_KOR_NM, levels = c("서울시전체", input$district)),
               y = round(as.numeric(per_chil),4),
               fill = SIG_KOR_NM)) +
      geom_col(position = "dodge", fill = c("red", "blue")) +
      geom_text(aes(label = round(as.numeric(per_chil),4)), vjust = 1.5, color = "white", 
                position = position_dodge(.9), size = 3) +
      xlab("District") +
      ylab("PED per child")
  })
}


# shinyApp
 shinyApp(ui, server)
 