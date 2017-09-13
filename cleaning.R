setwd("G:/data/Arthur/Training/R/Coursera/Capstone")
#'
library(tidyverse)
library(chron)
#'
clean_data <- function(data){}

table <- suppressMessages(readr::read_tsv("data/signif.txt", progress = F)) %>%
  dplyr::filter(YEAR >=0, is.na(LONGITUDE) ==F, is.na(LATITUDE) == F) %>%
  dplyr::select(I_D, YEAR, MONTH, DAY, LONGITUDE, LATITUDE)

table[is.na(table)] <- 01
table <- table %>%
  unite(col = "Date", from = c(YEAR, MONTH, DAY), sep = "-") 

table$Date <- as_date(table$Date)
class(table$LATITUDE) <- "numeric"
class(table$LONGITUDE) <- "numeric"


table$Date <- dates(table$Date)

table <- table %>%
  unite_(col = "Date", from = c(table$YEAR, table$MONTH, table$DAY), sep = "-", remove = FALSE)
  

