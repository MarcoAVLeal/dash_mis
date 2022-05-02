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
library(forecast)
library(GGally)
if( stringr::str_detect(string = getwd(),pattern = "marco")){source(file = "https://raw.githubusercontent.com/MarcoAVLeal/dash_mis/main/librarys.R")
}else{source(file = "https://raw.githubusercontent.com/MarcoAVLeal/dash_mis/main/librarys.R",encoding = "utf-8")}



ui = dashboardPage(
freshTheme = create_theme(
adminlte_color(
light_blue = "#273658",
blue = "#E4781C",
navy = "#273658",
red = "#4CC48F"
),
adminlte_sidebar(
dark_bg = "#273658",
dark_hover_bg = "#81A1C1",
dark_color = "#E4781C"
),
adminlte_global(
content_bg = "#FFF",
box_bg = "#273658", 
info_box_bg = "#D8DEE9"
)
),
#tags$a(href='https://site.crefaz.com.br/',tags$img(src='logo2.png',width = 80 ),"MIS")
options = list(sidebarExpandOnHover = TRUE),
header = dashboardHeader(title = strong("MIS"),userOutput("user"),leftUi = tagList(logo= tags$a(href='https://site.crefaz.com.br/',tags$img(src='https://site.crefaz.com.br/public/site/images/logo.png',width = 80 )))),
sidebar = dashboardSidebar(sidebarMenu(
    id = "tabs",
    menuItem(text = strong("Controle de Msg"), tabName = "page1",icon = icon("users")),
    menuItem(text = strong("Indicadores Bitrix"), tabName = "page2",icon = icon("home")),
    menuItem(text = strong("Acompanhamento de Status"), tabName = "page3",icon = icon("home")),
    menuItem(text = strong("Entrada de Propostas"), tabName = "page4",icon = icon("home")),
    menuItem(text = strong("Produção"), tabName = "page5",icon = icon("home")),
    menuItem(text = strong("Expansão"), tabName = "page6",icon = icon("map")),
    menuItem(text = strong("Inadimplência"), tabName = "page7",icon = icon("home"))
    )),
body = dashboardBody(
    tabItems(
        tabItem("page1",
                uiOutput(outputId = "page1")),
    tabItem("page2",
        uiOutput(outputId = "page2")),
    tabItem("page3",
            uiOutput(outputId = "page3")),
    tabItem("page4",
            uiOutput(outputId = "page4")),
    tabItem("page5",
            uiOutput(outputId = "page5")),
    tabItem("page6",
            uiOutput(outputId = "page6")),
    tabItem("page7",
            uiOutput(outputId = "page7"))
)),
controlbar = dashboardControlbar( uiOutput(outputId = "config_ui")),
title = "MIS"
)



