
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(sf)
library(DT)
library(shinythemes)
library(shinydashboard)
library(scales)
library(flexdashboard)
library(plotly)

msa_shp <- readRDS("msa_shp.rds")

msa_shp@data <- msa_shp@data %>%
  mutate(cashReturn = (medianRentEst * .75) * 12 / hvi) %>%
  mutate(cashFlow = medianRentEst * .75 - paymentMonthly30) %>%
  select(cbsaCode, cbsaName, REMV, cashReturn, cashFlow, medianRentEst, paymentMonthly30, hvi, medianIncome, totalPopulation, Lat, Long) #%>%
  #arrange(desc(cashReturn))

msaData <- readRDS("dataMSA.rds")

msaData <- msaData %>% ungroup() %>% filter(date < as.character(Sys.Date()))

#Create data glossary