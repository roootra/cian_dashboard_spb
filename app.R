#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

#namespace to load
library(shiny)
library(googleway)
library(xgboost)
library(caret)
library(rjson)
library(plyr)
library(dplyr)
library(XML)
library(xtable)

#environment to load
load('www/workspace.RData')

#main routine

ui <- fluidPage(
  
  # Название
  titlePanel("Оценка средней стоимости квартиры"),
  
  #  Price ~ Rooms + Subway_station + Distance + Full_area + Living_area + Floor + Max_floor + Material + Parking +
  #  Balcony + Loggia + WC + Lift + Appartments + New_building + Last_floor + First_floor + Relative_height
  sidebarLayout(
    sidebarPanel(
      selectInput("Rooms", "Количество комнат", levels(cian$Rooms)),
      selectInput("Subway_station", "Станция метро", unique(cian$Subway_station)),
      numericInput(inputId="Distance", label = "Пешком до метро, мин.", value = 5, min = 0, max = 300),
      numericInput(inputId="Full_area", label = "Общая площадь", value = 30, min = 5, max = 1000),
      numericInput(inputId="Living_area", label = "Жилая площадь", value = 25, min = 5, max = 1000),
      numericInput("Floor", "Этаж", min = 1, max = 200, value = 3),
      numericInput("Max_floor", "Всего этажей", value = 10, min = 1, max = 200),
      selectInput("Material", "Строительный материал", unique(cian$Material)),
      selectInput("Parking", "Тип парковки", unique(cian$Parking)),
      sliderInput("Balcony", "Количество балконов", min = 0, max = 8, value = 1),
      sliderInput("Loggia", "Количество лоджий", min = 0, max = 8, value = 1),
      sliderInput("WC", "Количество санузлов", min = 1, max = 8, value = 1),
      sliderInput("Lift", "Количество лифтов", min = 0, max = 8, value = 2),
      checkboxInput("Appartments", "Аппартаменты", FALSE),
      checkboxInput("New_building", "Новостройка", FALSE)
    ),
    
    mainPanel(
      h1("Средняя стоимость квартиры равна:"),
      textOutput("pred"),
      #br(),
      #tableOutput("table"),
      br(),
      sliderInput("Spread", "Укажите отклонение цены для показа на карте (+/-)", min = 100000, max = 2500000, value = 100000, step = 50000),
      google_mapOutput("map")
    )
  )
)

server <- function(input, output) {
  
  #получение данных ввода пользователя в df
  user_input <- reactive({
    data.frame(Rooms = as.factor(input$Rooms), Subway_station = input$Subway_station, Distance = input$Distance, Full_area = input$Full_area,
                             Living_area = input$Living_area, Floor = input$Floor, Max_floor = input$Max_floor, Material = input$Material,
                             Parking = input$Parking, Balcony = input$Balcony, Loggia = input$Loggia, WC = input$WC, Lift = input$Lift,
                             Appartments = as.numeric(input$Appartments), New_building = as.numeric(input$New_building), 
                             Last_floor = ifelse(input$Floor == input$Max_floor, 1, 0), First_floor = ifelse(input$Floor == 1, 1, 0),
                             Relative_height = input$Floor / input$Max_floor, stringsAsFactors = TRUE)})
  
  #записать предсказание в переменную
  price_pred <- reactive({
    user_input <- user_input()
    price_pred <- predict(xgb_caret, user_input)
  })
  
  #вывести предсказание
  output$pred <- renderText({
    price_pred = price_pred()
  })
  
  #вывести карту
  output$map <- renderGoogle_map({
    ggl_map <- google_map(key = api_key,
               location = c(59.938978, 30.315113),
               zoom = 10)
    #уходит в бесконечный цикл
    #cian_selected <- cian_selected()
    #add_markers(ggl_map,data = cian_selected, lat = "lat", lon = "lon", id = "id", info_window = "map_label", title = "Address")
  })
  
  #что подходит под фильтр?
  cian_selected <- reactive({
    price_pred <- price_pred()
    #отсеиваем квартиры по ценам
    cian_selected <- dplyr::filter(cian, Price >= (price_pred - input$Spread) & Price <= (price_pred + input$Spread))
  })
  
  #обновить карту
   observe({
    cian_selected <- cian_selected()
    google_map_update("map") %>%
    clear_markers() %>%
    add_markers(data = cian_selected, lat = "lat", lon = "lon", id = "id", info_window = "map_label", title = "Address")
  })
   
   #вывести таблицу
   #output$table <- renderTable({
     #cian_selected <- cian_selected()
     #cian_to_show <- select(cian, Rooms, Subway_station, Full_area, Price, Link)
     #xtable(head(cian_to_show, 100), bordered = TRUE)
   #})
}










###Внутренний namespace

#ключ
api_key <- "AIzaSyBbT3SVtEXsXa0sGcwtKm2s-Nq5zjxE7SI"

#Яндекс.Геокодер
geoYandex <-function(location)
{
  stopifnot(is.character(location))
  loc <- location
  location <- gsub(",", "", location)
  location <- gsub(" ", "+", location)
  posturl <- paste(location)
  url_string <- paste("http://geocode-maps.yandex.ru/1.x/?geocode=",
                      posturl, sep = "")
  url_string <- URLencode(url_string)
  xmlText <- paste(readLines(url_string), "\n", collapse="")
  data<-xmlParse(xmlText, asText=TRUE)
  xml_data <- xmlToList(data)
  pos<-xml_data$GeoObjectCollection$featureMember$GeoObject$Point$pos
  lon<-as.numeric(substr(pos,1,9))
  lat<-as.numeric(substr(pos,11,19))
  return (data.frame(lon,lat))
}

# Run the application 
shinyApp(ui = ui, server = server)
