#rm(list=ls())
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(readxl)
library(stringr)
library(flexdashboard)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(pagedown)
library(stringr)
library(RODBC)
library(RMySQL)
library(RMariaDB)
library(ggtext)
library(fresh)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(reshape2)
library(plyr)
library(tidyverse)
library(zoo)
library(forecast)
library(GGally)
#source(file = "funcoes.R",encoding = "UTF-8")
read_url_csv <- function(url, sep = ",",enc = "UTF-8"){
  tmpFile <- tempfile()
  download.file(url, destfile = tmpFile)
  url_csv <- read.csv(tmpFile, sep = sep,encoding = enc)
  return(url_csv)
}

read_url_xlsx <- function(url,sheet = "Calendario"){
  tmpFile <- tempfile()
  download.file(url, destfile = tmpFile)
  url_csv <- read_xlsx(path = tmpFile,sheet = sheet)
  return(url_csv)
}


library(readxl)


