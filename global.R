## app.R ##
options(stringsAsFactors = FALSE)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

col_names <- c("Metropolitan_Statistical_Area_Name",	
               "Core_Based_Statistical_Area", 
               "Year",	
               "Quarter", 
               "Index",
               "Index_Standard_Error")

HPI_AT_metro <- read.csv("https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_metro.csv", 
                         header = FALSE, stringsAsFactors = FALSE) %>%
  rename_(.dots = setNames(colnames(.), col_names)) %>%
  filter(Year > 1994) %>%
  transmute(Metropolitan_Statistical_Area_Name = Metropolitan_Statistical_Area_Name,
            Core_Based_Statistical_Area = as.character(Core_Based_Statistical_Area),
            Housing_Price_Index = as.numeric(ifelse(Index == '-', NA, Index)),
            Year = Year,
            Year_Quarter_num = Year + (Quarter / 4)) %>%
  .[complete.cases(.), ] %>%
  mutate(Metropolitan_Statistical_Area_Name = str_replace(Metropolitan_Statistical_Area_Name, '(MSAD)', '')) %>%
  separate(Metropolitan_Statistical_Area_Name, c('Area', 'State'), ', ') %>%
  mutate(State = str_sub(State, 1, 2))
 




