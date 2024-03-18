install.packages('dplyr')
install.packages('tidyr')

library(dplyr)
library(tidyr)
library(stringr)


rm(list=ls())
setwd("~/Desktop/ML_Team2/")
car_data <- read.csv("car_data.csv", stringsAsFactors = FALSE)
View(car_data)

#가격

car_data$가격[is.na(car_data$가격)] <- 0 # NA 값을 0으로 대체
car_data <- car_data[car_data$가격 != '[판매완료]', ]
car_data$가격 <- gsub('만원', '', car_data$가격)
car_data$가격 <- gsub(',', '', car_data$가격)
car_data$가격 <- as.numeric(car_data$가격)

#연식
car_data$연식 <- str_extract(car_data$연식, "^[^ ]+")


car_data$주행거리 <- as.numeric(gsub('km', '', car_data$주행거리))

car_data <- car_data %>%
  mutate(색상 = as.character(색상),
         변속기 = as.character(변속기),
         연료 = as.character(연료)) %>%
  spread(key = 색상, value = 색상, fill = 0, sep = '_') %>%
  spread(key = 변속기, value = 변속기, fill = 0, sep = '_') %>%
  spread(key = 연료, value = 연료, fill = 0, sep = '_')
View(car_data)
