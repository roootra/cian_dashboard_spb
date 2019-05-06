require(dplyr)
require(readr)
require(stringr)
require(tidyr)

###ДАННЫЕ
library(readxl)
cian_orig <- read_excel(file.choose()) #"paid_proj/cian_grab/cian_150.xlsx"
cian <- cian_orig
#метро
cian <- cian[complete.cases(cian$Station),]
cian <- separate(cian, Station, c("Subway_station", "Distance"), " \\(")
cian <- separate(cian, Distance, c("Distance", "Dist_type"), "мин")
cian$Dist_type <- str_trim(gsub("\\)", '', cian$Dist_type))
cian$Distance <- as.numeric(cian$Distance)
cian <- cian[complete.cases(cian$Distance),]
cian$Subway_station <- as.factor(cian$Subway_station)
cian$Distance[cian$Dist_type == "на машине"] <- cian$Distance[cian$Dist_type == "на машине"] * 4
#комнаты
cian$Appartments <- 0
cian$Appartments[grep("Аппартаменты", cian$Rooms, value = FALSE)] <- 1
cian$Rooms <- ifelse(cian$Rooms == "Изолированная" | cian$Rooms == "Аппартаменты" | 
                       cian$Rooms == "Изолированная, Аппартаменты",
                     1, cian$Rooms)
cian$Rooms <- substr(cian$Rooms, 1, 1)
cian$Rooms[is.na(cian$Rooms)] <- 'Студия'
cian$Rooms <- factor(cian$Rooms, levels = c("Студия","1","2","3","4","5","6"), ordered = TRUE)

#Новостройки
cian$New_building <- 0
cian$New_building[grep("новостройке", cian$Sale_type)] <- 1
cian$Sale_type <- NULL
#площадь
cian <- separate(cian, Area, c("Full_area", "Living_area", "Kitchen_area"), "/")
cian$Kitchen_area[is.na(cian$Kitchen_area)] <- 0
cian$Full_area <- as.numeric(cian$Full_area)
cian$Living_area <- as.numeric(cian$Living_area)
cian$Kitchen_area <- as.numeric(cian$Living_area)
cian <- cian[complete.cases(cian$Living_area),]
#этажи и материал
cian <- separate(cian, Building_props, c("Floor", "Max_floor"), "/")
cian <- separate(cian, Max_floor, c("Max_floor", "Material"), ", ")
cian$Material[is.na(cian$Material)] <- 'Другое/Неизвестно'
cian$Floor <- as.numeric(cian$Floor)
cian$Max_floor <- as.numeric(cian$Max_floor)
cian$Last_floor <- ifelse(cian$Floor == cian$Max_floor, 1, 0)
cian$First_floor <- ifelse(cian$Floor == 1, 1, 0)
cian$Material <- as.factor(cian$Material)
cian$Relative_height = cian$Floor / cian$Max_floor #искуственная фича, относительная высота
#парковка
cian$Parking[is.na(cian$Parking)] <- 'отсутствует'
cian$Parking <- factor(cian$Parking)
#Цена
cian <- separate(cian, Price, c("Price", "Price_rest"), " ")
cian$Price_rest <- NULL
cian$Price <- as.numeric(cian$Price)
#ремонт - много NA, видимо, черновые
#площади комнат - избыточно
#балконы
cian <- separate(cian, Balcony, c("Balcony", "Loggia"), ", ")
cian$Loggia[grep("Лоджия", cian$Balcony)] <- cian$Balcony[grep("Лоджия", cian$Balcony)]
cian$Balcony[grep("Лоджия", cian$Balcony)] <- 0
cian <- separate(cian, Balcony, c("Rest", "Balcony"), "\\(")
cian$Rest <- NULL
cian$Balcony <- gsub('\\)', '', cian$Balcony)
cian$Balcony[is.na(cian$Balcony)] <- 0
cian$Balcony <- as.numeric(cian$Balcony)
cian <- separate(cian, Loggia, c("Rest", "Loggia"), "\\(")
cian$Rest <- NULL
cian$Loggia <- gsub('\\)', '', cian$Loggia)
cian$Loggia[is.na(cian$Loggia)] <- 0
cian$Loggia <- as.numeric(cian$Loggia)
#вид из окон - избыточно
#санузлы (не будем вдаваться в подробности, укажем общее количество)
levels(as.factor(cian$WC))
cian$WC[cian$WC == "Совмещенный (1), Раздельный (1)"] <- 2
cian$WC[cian$WC == "Совмещенный (1), Раздельный (2)"] <- 3
cian$WC[cian$WC == "Совмещенный (2), Раздельный (1)"] <- 3
cian <- separate(cian, WC, c("Rest", "WC"), "\\(")
cian$Rest <- NULL
cian$WC <- gsub('\\)', '', cian$WC)
cian$WC[is.na(cian$WC)] <- 1 #куда уж без туалета!
cian$WC <- as.numeric(cian$WC)
#стационарный телефон - избыточно
#высота потолков (много NA)
cian$Height <- as.numeric(cian$Height)
#лифт (не будем вдаваться в подробности, укажем общее количество)
levels(as.factor(cian$Lift))
cian$Lift[cian$Lift == "Пасс (1), Груз (1)"] <- 2
cian$Lift[cian$Lift == "Пасс (1), Груз (2)"] <- 3
cian$Lift[cian$Lift == "Пасс (2), Груз (1)"] <- 3
cian$Lift[cian$Lift == "Пасс (2), Груз (2)"] <- 4
cian$Lift[cian$Lift == "Пасс (3), Груз (1)"] <- 4
cian <- separate(cian, Lift, c("Rest", "Lift"), "\\(")
cian$Rest <- NULL
cian$Lift <- gsub('\\)', '', cian$Lift)
cian$Lift[is.na(cian$Lift)] <- 0
cian$Lift <- as.numeric(cian$Lift)
#мусоропровод - неинтересно

###Геолокация
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

#получаем широту и долготу
coord <- data.frame(lon = double(), lat = double())
for(i in 28570:nrow(cian)){
    coord[i,] <- geoYandex(cian$Address[i])
    print(i)
}
cian <- cbind(cian, coord)
cian$id <- NULL

#убираем вранье (59°57′ с. ш. 30°19′ в.д. )
cian_loc <- cian
cian <- filter(cian_loc, lat < 65 & lat > 55 & lon < 35 & lon > 25)

#для карты
cian$map_label <- paste0(cian$Link, "\n", cian$Block_name, "\n", cian$Address, "\n", cian$Description)

###Модель
library(caret)
library(h2o)
library(xgboost)

cian_ind_test = sample(seq_len(nrow(cian)), size = nrow(cian)*0.2)
cian.test = cian[cian_ind_test,]
cian.train = cian[-cian_ind_test,]

formula = as.formula("Price ~ Rooms + Subway_station + Distance + 
                     Full_area + Living_area + Floor + 
                     Max_floor + Material + Parking +Balcony + Loggia + WC + Lift + 
                     Appartments + New_building + Last_floor + First_floor + Relative_height")
xgb_trcontrol <- trainControl(
  method="cv",
  number = 3,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",
  allowParallel = TRUE)

xgb_grid_tree <- expand.grid(nrounds = 200,
                             max_depth = 7,
                             gamma = 0,
                             eta = 0.1,
                             colsample_bytree = 0.75,
                             min_child_weight = 1, 
                             subsample = 0.25)

xgb_caret <- train(formula, data = cian.train, trControl = xgb_trcontrol, tuneGrid = xgb_grid_tree,
                   method = "xgbTree", verbose = 1)

xgb_caret.prediction <- predict(xgb_caret, cian.test)
sqrt(RMSE(xgb_caret.prediction, cian.test$Price))

#бустинг с линейными регрессиями неэффективен
#xgb_grid_linear <- expand.grid(nrounds = 200,
                               #lambda = c(0, 0.1, 0.3),
                               #alpha = c(0, 0.1, 0.3),
                               #eta = c(0.01, 0.1, 0.3))

#xgb_caret_lm <- train(formula, data = cian.train, trControl = xgb_trcontrol, tuneGrid = xgb_grid_linear,
                      #method = "xgbLinear", verbose = 1)

varImp(xgb_caret)
xgb.save(xgb_caret$finalModel, 'xgb_tree.model')
rm(cian_orig, cgb_caret_lm, xgb_grid_linear, xgb_grid_tree, xgb_trcontrol, cian_ind_test, formula, xgb_caret.prediction, xgb_caret_lm)


library(h2o)
h2o.init(nthreads = 16)
y <- "Price"
x<-c("Rooms","Subway_station","Distance","Full_area","Living_area","Floor","Max_floor","Material","Parking","Balcony","Loggia","WC","Lift","Appartments",
     "New_building","Last_floor","First_floor","Relative_height")
cian.train$Rooms <- factor(cian.train$Rooms, ordered = FALSE)
cian.test$Rooms <- factor(cian.test$Rooms, ordered = FALSE)
h2o_train <- as.h2o(cian.train)
h2o_test <- as.h2o(cian.test)
aml <- h2o.automl(x=x, y=y, training_frame = h2o_train, validation_frame = h2o_test, stopping_metric = "RMSE")


