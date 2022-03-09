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

axis.theme <- function(x.angle = 0,vjust=0,hjust=0.5,pos_leg="top",textsize = 10,lengend_title_size = 10,lengend_text_size = 8,title_size = 16){
  
  
  theme_bw()  +
    theme(
      axis.text.x = element_text(angle = x.angle,face = "bold",size = textsize,hjust=hjust, vjust=vjust),
      axis.text.y = element_text(angle = 0,face = "bold",size = textsize),
      legend.background = element_rect(fill = "transparent", colour = NA,size = 2),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      axis.title.x = element_text(colour = "black",size = textsize,face = "bold"),
      axis.title.y = element_text(colour = "black",size = textsize,face = "bold"),
      legend.title = element_text(colour = "black",size = lengend_title_size),
      legend.position = pos_leg,
      legend.text = element_text(colour = "black",size = lengend_text_size,face = "bold"),
      panel.grid = element_line(linetype="dashed"),
      panel.grid.major = element_line(colour = "gray"),
      title =element_text(size=title_size, face='bold',hjust = 0.5),
      plot.title = element_text(hjust = 0.5),
      axis.title = element_text(color="#000000", face="bold", size=textsize,lineheight = 2))
  
}

if( stringr::str_detect(string = getwd(),pattern = "marco")){
  path_pg1 <-  source(file = "https://raw.githubusercontent.com/MarcoAVLeal/dash_mis/main/pagina1.R",encoding = "UTF-8",local = F)
  path_pg2 <-  source(file = "https://raw.githubusercontent.com/MarcoAVLeal/dash_mis/main/pagina2.R",encoding = "UTF-8",local = F)
  path_pg5 <-  source(file = "https://raw.githubusercontent.com/MarcoAVLeal/dash_mis/main/pagina5.R",encoding = "UTF-8",local = F)
  
  
}else{
  path_pg1 <-  source(file = "https://raw.githubusercontent.com/MarcoAVLeal/dash_mis/main/pagina1.R",encoding = "UTF-8",local = F)
  path_pg2 <-  source(file = "https://raw.githubusercontent.com/MarcoAVLeal/dash_mis/main/pagina2.R",encoding = "UTF-8",local = F)
  path_pg5 <-  source(file = "https://raw.githubusercontent.com/MarcoAVLeal/dash_mis/main/pagina5.R",encoding = "UTF-8",local = F)
  }


espaco_html <- function(n=6){
    
    
    return(HTML( rep("<br>",n)))
    
    
}

# #source(file = "funcoes.R",encoding = "UTF-8")
# read_url_csv <- function(url, sep = ","){
#   tmpFile <- tempfile()
#   download.file(url, destfile = tmpFile)
#   url_csv <- read.csv(tmpFile, sep = sep)
#   return(url_csv)
# }
# 
# read_url_xlsx <- function(url,sheet = "Calendario"){
#   tmpFile <- tempfile()
#   download.file(url, destfile = tmpFile)
#   url_csv <- read_xlsx(path = tmpFile,sheet = sheet)
#   return(url_csv)
# }
# 
# 
# library(readxl)
# url <- "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/EbsSsZPj4ntMvyRUG-0QYa4BTEQUCw8VXlCqTriyMQcYiw?download=1"
# destfile <- "calendario.xlsx"
# curl::curl_download(url, destfile)
# calendario <- read_excel(destfile,sheet = "Calendario")
# calendario         = calendario[c("Dia","Class Não Útil")]
# colnames(calendario) <- c("Data","Dia_util")
# calendario$Class_Dia <- is.na(calendario$Dia_util)
# calendario$Data      <- as.Date(calendario$Data,format = "%d/%m/%Y")
# 
# onedrive_url <- "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/ESjlRAy5mzVJh3LRucNvoTYBa5x7ReX2691dJ-5uwaob4w?download=1"
# df <- read_url_csv(onedrive_url)
# 
# names_df          <- c("ID","Tipo","Pipeline", "Fase do negocio","Negocio Recorrente","Negocio Repetido","Contato Fonte","Modificado por ID","Criado por","Pessoa reponsavel ID",
#                        "Data prevista de fechamento","Data de inicio","Valor do emprestimo","Origem do Cliente","Data exportacao","Contato","Grupo de fase" ,
#                        "Base","Produto Crefaz", "Fase automacao",  "Data negociar","Data analisar", "Data prospectar","Modificado em","Desafio","Desafio retencao"
# )
# colnames(df) <- names_df
# df$`Pessoa reponsavel ID`          <- as.character(df$`Pessoa reponsavel ID` )
# 
# onedrive_url <- "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/ES9Opw7qVkROh6yxmaC8ARQB1_RuzanE2l8fl3U7p6r4KQ?download=1"
# 
# users             <- read_url_csv(onedrive_url,sep = ";")
# users$ID          <- as.character(users$ID)
# 
# library(readxl)
# url <- "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/EaODUUKwdOFJtbyx6MQHshsBjMAYRqYYyXr-el08rDnpxQ?download=1"
# destfile <- "regionais.xlsx"
# curl::curl_download(url, destfile)
# regionais <- read_excel(destfile,sheet = "Planilha1")
# 
# 
# df                <- left_join(x = df,y = users,by=c("Pessoa reponsavel ID"="ID"),keep=TRUE,suffix = c("_LEADS","_users"))
# df                <- left_join(x = df,y = regionais,by=c("Departamento"="Departamento"),keep=TRUE,suffix = c("_LEADS","_reg"))
# 
# 
# colnames(df)[colnames(df) == "Departamento_reg"] <- "Lojas"
# 
# 
# df                <- df %>% filter(str_detect(string = Lojas,pattern = "Loja CFZ"))
# 
# 
# 
# # df                <- df %>% filter(str_detect(REGIONAL,pattern = "GILBERTO FELICIO") | str_detect(REGIONAL,pattern = "MAYSA CARVALHO") | str_detect(REGIONAL,pattern = "SC"))
# 
# 
# df                <- df %>% mutate("Fase do negocio" = ifelse(`Fase do negocio` == "PAGO AO CLIENTE","PAGO",`Fase do negocio`))
# df                <- df %>% mutate("Fase do negocio" = ifelse(`Fase do negocio` == "EM ANÁLISE","EM ANÁLISE",`Fase do negocio`))
# # df                <- df %>% mutate("Origem do Cliente"  = ifelse(`Origem do Cliente` == "","Não identificada",`Origem do Cliente`),
# #                                    "Fonte"              = ifelse(`Contato Fonte` =="",`Lead Fonte`,`Contato Fonte`),
# #                                    "Origem do Cliente1" = ifelse(`Origem do Cliente` == "Fonte" & `Contato Fonte` == "" & `Lead Fonte` == "","Fonte (erro de tabulação)",ifelse(Fonte != "","Fonte",`Origem do Cliente`)),
# #                                    "Data fechado"       = ifelse(Fechado == "Sim",`Data prevista de fechamento`, NA),
# #                                    "Data criado"        = ifelse(`Origem do Cliente1` == "Fonte"| `Origem do Cliente1` =="Não identificada",NA,`Criado em`))
# 
# df                <- df %>% mutate("Origem do Cliente"  = ifelse(`Origem do Cliente` == "" | is.na(`Origem do Cliente`) | `Origem do Cliente` == "NA"  ,"Não identificada",`Origem do Cliente`),
#                                    # "Fonte"              = ifelse(`Contato Fonte` =="",`Lead Fonte`,`Contato Fonte`),
#                                    "Origem do Cliente1" = ifelse(!is.na(`Contato Fonte`) ,"Fonte", `Origem do Cliente`),
#                                    "Data fechado"       = ifelse(`Fase do negocio` == "PAGO" | `Fase do negocio` == "DESAFIO",`Data prevista de fechamento`, NA),
#                                    "Data criado"        = ifelse(`Origem do Cliente1` == "Fonte" | `Origem do Cliente1` =="Não identificada",NA,`Data de inicio`))
# 
# #df$`Data de inicio` %>% range
# 
# #df$`Criado em`         <- as.Date(df$`Criado em`,format = "%d/%m/%Y")
# df$`Data fechado`       <- lubridate::as_date(df$`Data fechado` , format = "%Y-%m-%d %H:%M:%S")
# df$`Data analisar`      <- lubridate::as_date(df$`Data analisar`, format = "%Y-%m-%d %H:%M:%S")
# df$`Data criado`        <- lubridate::as_date(df$`Data criado`, format = "%Y-%m-%d %H:%M:%S")
# df$`Data negociar`      <- lubridate::as_date(df$`Data negociar`, format = "%Y-%m-%d %H:%M:%S")
# df$`Data prospectar`    <- lubridate::as_date(df$`Data prospectar`, format = "%Y-%m-%d %H:%M:%S")
# df$`Data de inicio`     <- lubridate::as_date(df$`Data de inicio`, format = "%Y-%m-%d %H:%M:%S")
# df$`Modificado em`      <- lubridate::as_date(df$`Modificado em` , format = "%Y-%m-%d %H:%M:%S")
# 
# 
# df1 <- df


server <- function(input, output, session) {
  #counter <- reactiveValues(countervalue = 0)
    # input$username <- "mleal"
    # input$password <- "mleal"
    ######################################               ###########################################################
    ###################################### Menu de login ###########################################################
    ######################################               ###########################################################
    USER <- reactiveValues(Logged = FALSE)
    observeEvent(input$login, {
        if (isTRUE(as.character(credentials[[input$username]]$pass)==as.character(input$password))){
            USER$Logged <- TRUE
            
            #updateNavbarPage(session = session,inputId = "nav_bar",selected = 1)
            show_toast(
                title = "Entrando ... ",
                text = "",
                type = "succes",
                width = "auto",
                position = "bottom"
            )
            
        } else {
            #showNotification(fluidRow(icon("exclamation-triangle"),"Usuário ou Senha Invalidos"))
            show_toast(
                title = "Usuário e ou Senha Invalidos.",
                text = "Verifique!!!",
                type = "error",
                width = "auto",
                position = "bottom"
            )
        }
    })
    
    observeEvent(input$logout, {
        if (isTRUE(as.character(credentials[[input$username]]$pass)==as.character(input$password))){
            USER$Logged <- FALSE
            show_toast(
                title = "",
                text = "Saindo ... ",
                timer=500,
                type = "default",
                width = "200px",
                position = "bottom"
            )
            session$reload()
        } 
    })    
    
  
    
      
######################################               ###########################################################
###################################### Tela de Usuário ###########################################################
######################################               ###########################################################
 observe({
     
     if( !isTRUE(USER$Logged)){
         
         

         output$user <- renderUser({
             dashboardUser(
                 name = "Username", 
                 image = "https://www.pinclipart.com/picdir/big/157-1578186_user-profile-default-image-png-clipart.png", 
                 title = "Departamento",
                 subtitle = "Cargo", 
                 footer = p("Sem Usuário Logado", class = "text-center"),
                 tags$div(HTML('<b style = "padding-left:25px;color:#000000;font-size:20px">PAINEL DO USUÁRIO </b>')),
                 fluidRow(style = "text-align:center;",
                     div(textInput("username",strong("Usuário : "),width=100),style="padding-left:0px;"),
                     br(style="display: block;content: \"\"; margin-top: 0px;"),
                     div( passwordInput("password", strong("Senha:"),width = 100),style="padding-left:0px;text-align:center"),
                     fluidRow(
                         div(actionBttn(
                             inputId = "login",
                             label = "Entrar",
                             color = "primary",
                             style = "jelly",
                             block = FALSE,size = "xs"),
                             style = sprintf("left: 0px;text-align:center;padding-left:0px;"))),
                     dashboardUserItem(
                         width = 6,
                         socialButton(
                             href = "https://dropbox.com",
                             icon = icon("dropbox")
                         )
                     ),
                     dashboardUserItem(
                         width = 6,
                         socialButton(
                             href = "https://github.com",
                             icon = icon("github")
                         )
                     )
                 )
             )
         })
         
         
         
         }else{
             output$user <- renderUser({
                 dashboardUser(
                     name = credentials[[input$username]]$"name", 
                     image = credentials[[input$username]]$"img", 
                     title = credentials[[input$username]]$"departamento",
                     subtitle = credentials[[input$username]]$"cargo", 
                     footer = p("Logado", class = "text-center"),
                     tags$div(HTML('<b style = "padding-left:25px;color:#000000;font-size:20px">PAINEL DO USUÁRIO </b>')),
                     fluidRow(style = "text-align:center;",
                              actionBttn(
                                  inputId = "logout",
                                  label = "Sair",
                                  color = "primary",
                                  style = "jelly",
                                  block = FALSE,
                                  icon = icon("sign-out-alt")),
                              dashboardUserItem(
                                  width = 6,
                                  socialButton(
                                      href = "https://dropbox.com",
                                      icon = icon("dropbox")
                                  )
                              ),
                              dashboardUserItem(
                                  width = 6,
                                  socialButton(
                                      href = "https://github.com",
                                      icon = icon("github")
                                  )
                              )
                     )
                 )
             })
             
         
     }
     
     
 })   
    ######################################                      ###########################################################
    ###################################### Renderizando Paginas ###########################################################
    ######################################                      ########################################################### 
    observe({
        
        if( !isTRUE(USER$Logged)){
            
            
       }else{
            output$page2 <- renderUI({
           library(dplyr)
              url <- "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/EbsSsZPj4ntMvyRUG-0QYa4BTEQUCw8VXlCqTriyMQcYiw?download=1"
              destfile <- "calendario.xlsx"
              curl::curl_download(url, destfile)
              calendario <- read_excel(destfile,sheet = "Calendario")
              calendario         = calendario[c("Dia","Class Não Útil","Dia Útil2")]
              colnames(calendario) <- c("Data","Dia_util","Dia_Util2")
              calendario$Class_Dia <- is.na(calendario$Dia_util)
              calendario$Data      <- as.Date(calendario$Data,format = "%d/%m/%Y")
              calendario$Class_Dia <- ifelse(calendario$`Dia_Util2` == "Sábado",TRUE,calendario$Class_Dia)
              
              onedrive_url <- "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/ESjlRAy5mzVJh3LRucNvoTYBa5x7ReX2691dJ-5uwaob4w?download=1"
              df <- read_url_csv(onedrive_url,enc = "latin1")
              
              names_df          <- c("ID","Tipo","Pipeline", "Fase do negocio","Negocio Recorrente","Negocio Repetido","Contato Fonte","Modificado por ID","Criado por","Pessoa reponsavel ID",
                                     "Data prevista de fechamento","Data de inicio","Valor do emprestimo","Origem do Cliente","Data exportacao","Contato","Grupo de fase" ,
                                     "Base","Produto Crefaz", "Fase automacao",  "Data negociar","Data analisar", "Data prospectar","Modificado em","Desafio","Desafio retencao","primeira_resposta_do_cliente",
                                     "resposta_do_cliente","cpf","retrabalho_desafio"
                                     
              )
              #,"ID_users" , "COD_PR",  "Nomes.e.sobrenomes" ,   "Lojas" ,  "Regional" ,   "active"  ,"Origem do Cliente1",   "Data fechado" ,  "Data criado"
              colnames(df) <- names_df
              
              # library(janitor)
              # df  %>%
              #   clean_names()
              
              #
              
              df$`Fase do negocio` <- ifelse(df$`Fase do negocio` == "EM ANÃ\u0081LISE","EM ANÁLISE",df$`Fase do negocio`)
              df$`Pessoa reponsavel ID`          <- as.character(df$`Pessoa reponsavel ID` )
              
              onedrive_url <- "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/EZXxlt-n_LxOnG4KTsPLxaIBVslbvDa-2z7_1_-0Y3B1NQ?download=1"
              
              users             <- read_url_csv(onedrive_url,sep = ";",enc = "latin1")
              users$ID          <- as.character(users$ID)
              
              library(readxl)
              url <- "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/EaODUUKwdOFJtbyx6MQHshsBjMAYRqYYyXr-el08rDnpxQ?download=1"
              destfile <- "regionais.xlsx"
              curl::curl_download(url, destfile)
              regionais <- read_excel(destfile,sheet = "Planilha1")
              
              
              
              
              df                <- left_join(x = df,y = users,by=c("Pessoa reponsavel ID"="ID"),keep=TRUE,suffix = c("_LEADS","_users"))
              #df                <- left_join(x = df,y = regionais,by=c("Departamento"="Departamento"),keep=TRUE,suffix = c("_LEADS","_reg"))
              colnames(df)[colnames(df) == "Departamento"] <- "Lojas"
              
              
              df                <- df %>% filter(str_detect(string = Lojas,pattern = "Loja CFZ"))
              
              
              
              df$Regional       <- str_replace(string = df$Regional,pattern = "Super. ES 2 Lojas CFZ",replacement = "Supervisao ES Lojas CFZ")
              df$Regional       <- str_replace(string = df$Regional,pattern = "Super. RJ Lojas CFZ",replacement = "Supervisao RJ Lojas CFZ")
              df$Regional       <- str_replace(string = df$Regional,pattern = "Super. RS Lojas CFZ",replacement = "WAGNER RIBEIRO")
              df$Regional       <- str_replace(string = df$Regional,pattern = "Super. CE lojas CFZ",replacement = "GILBERTO FELICIO")
              df$Regional       <- str_replace(string = df$Regional,pattern = "Super. Lojas SC",replacement = "SC")
              df$Regional       <- str_replace(string = df$Regional,pattern = "Sup. Estadual Eliana",replacement = "ELIANA PORRINO")
              df$Regional       <- str_replace(string = df$Regional,pattern = "Regional Yrlon",replacement = "YRLON ALVES")
              df$Regional       <- str_replace(string = df$Regional,pattern = "Regional Mirele",replacement = "MIRELE DUARTE")
              df$Regional       <- str_replace(string = df$Regional,pattern = "Regional Helen",replacement = "HELEN CAROLINA")
              df$Regional       <- str_replace(string = df$Regional,pattern = "Regional Dejamile",replacement = "DEJAMILE SOUZA")
              df$Regional       <- str_replace(string = df$Regional,pattern = "Regional Igor",replacement = "IGOR ASSIS")
              
              
              
              df                <- df %>% mutate("Fase do negocio" = ifelse(`Fase do negocio` == "PAGO AO CLIENTE","PAGO",`Fase do negocio`))
              df                <- df %>% mutate("Fase do negocio" = ifelse(`Fase do negocio` == "EM ANÁLISE","EM ANÁLISE",`Fase do negocio`))
              # df                <- df %>% mutate("Origem do Cliente"  = ifelse(`Origem do Cliente` == "","Não identificada",`Origem do Cliente`),
              #                                    "Fonte"              = ifelse(`Contato Fonte` =="",`Lead Fonte`,`Contato Fonte`),
              #                                    "Origem do Cliente1" = ifelse(`Origem do Cliente` == "Fonte" & `Contato Fonte` == "" & `Lead Fonte` == "","Fonte (erro de tabulação)",ifelse(Fonte != "","Fonte",`Origem do Cliente`)),
              #                                    "Data fechado"       = ifelse(Fechado == "Sim",`Data prevista de fechamento`, NA),
              #                                    "Data criado"        = ifelse(`Origem do Cliente1` == "Fonte"| `Origem do Cliente1` =="Não identificada",NA,`Criado em`))
              
              df                <- df %>% mutate("Origem do Cliente"  = ifelse(`Origem do Cliente` == "" | is.na(`Origem do Cliente`) | `Origem do Cliente` == "NA"  ,"Não identificada",`Origem do Cliente`),
                                                 # "Fonte"              = ifelse(`Contato Fonte` =="",`Lead Fonte`,`Contato Fonte`),
                                                 "Origem do Cliente1" = ifelse(!is.na(`Contato Fonte`) ,"Fonte", `Origem do Cliente`),
                                                 "Data fechado"       = ifelse(`Fase do negocio` == "PAGO" | `Fase do negocio` == "DESAFIO",`Data prevista de fechamento`, NA),
                                                 "Data criado"        = ifelse(`Origem do Cliente1` == "Fonte" | `Origem do Cliente1` =="Não identificada",NA,`Data de inicio`))
              
              
              
              #df$`Criado em`         <- as.Date(df$`Criado em`,format = "%d/%m/%Y")
              df$`Data fechado`       <- lubridate::as_date(df$`Data fechado` , format = "%Y-%m-%d %H:%M:%S")
              df$`Data analisar`      <- lubridate::as_date(df$`Data analisar`, format = "%Y-%m-%d %H:%M:%S")
              df$`Data criado`        <- lubridate::as_date(df$`Data criado`, format = "%Y-%m-%d %H:%M:%S")
              df$`Data negociar`      <- lubridate::as_date(df$`Data negociar`, format = "%Y-%m-%d %H:%M:%S")
              df$`Data prospectar`    <- lubridate::as_date(df$`Data prospectar`, format = "%Y-%m-%d %H:%M:%S")
              df$`Data de inicio`     <- lubridate::as_date(df$`Data de inicio`, format = "%Y-%m-%d %H:%M:%S")
              df$`Modificado em`      <- lubridate::as_date(df$`Modificado em` , format = "%Y-%m-%d %H:%M:%S")
              
              
              
              df1 <<- df
              
              
              #   onedrive_url <- "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/ESjlRAy5mzVJh3LRucNvoTYBa5x7ReX2691dJ-5uwaob4w?download=1"
              #   df <<- read_url_csv(onedrive_url,enc = "latin1")
              #   
              #   names_df          <- c("ID","Tipo","Pipeline", "Fase do negocio","Negocio Recorrente","Negocio Repetido","Contato Fonte","Modificado por ID","Criado por","Pessoa reponsavel ID",
              #                          "Data prevista de fechamento","Data de inicio","Valor do emprestimo","Origem do Cliente","Data exportacao","Contato","Grupo de fase" ,
              #                          "Base","Produto Crefaz", "Fase automacao",  "Data negociar","Data analisar", "Data prospectar","Modificado em","Desafio","Desafio retencao","primeira_resposta_do_cliente",
              #                          "resposta_do_cliente","cpf","retrabalho_desafio","ID_users" , "COD_PR",  "Nomes.e.sobrenomes" ,   "Lojas" ,  "Regional" ,   "active"  ,"Origem do Cliente1",   "Data fechado" ,  "Data criado"
              #   )
              #   colnames(df) <- names_df
              #   
              # 
              # df1 <<- df
                path_pg1
                #source(file = "source_server_pagina1.R",encoding = "utf-8")
                
            })
            
            
            output$page1 <- renderUI({
              
              onedrive_url <- "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/EZYuY8kORyJIoTYUo9RwWMABYEkZTA2OXtxrUXnrLef9pQ?download=1"
              
              
              
              x1 <<- read_url_csv(onedrive_url)
              
              x1$dia <<- lubridate::as_date(x1$dia)
              x2     <<- x1
              
              path_pg2
                
            })
            
            
            
            output$page3 <- renderUI({
              
              HTML("<h3 style='color:#273658;text-align:center;font-weight:bold;'>ACOMPANHAMENTO DE STATUS</h1> ")
              
            })
            
            output$page4 <- renderUI({
              
              HTML("<h3 style='color:#273658;text-align:center;font-weight:bold;'>ENTRADA DE PROPOSTAS</h1> ")
              
            })
            
            output$page5 <- renderUI({
              
              onedrive_url <- "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/ET_WfaYB2k1IouEbVGeMcPYBFp_36rF5CLDjQgZi3iTrbw?download=1"
              
              
              
              df_prod <<- read_url_csv(onedrive_url)
              df_prod$DATACADASTRO<<-lubridate::as_date(df_prod$DATACADASTRO)
              df_prod$DATA_PAGAMENTO<<-lubridate::as_date(df_prod$DATA_PAGAMENTO)
              
              df_prod$ANO_CADASTRO<<-lubridate::year(df_prod$DATACADASTRO)
              df_prod$ANO_PAGAMENTO<<-lubridate::year(df_prod$DATA_PAGAMENTO)
              df_prod$MES_CADASTRO<<-lubridate::month(df_prod$DATACADASTRO)
              df_prod$MES_PAGAMENTO<<-lubridate::month(df_prod$DATA_PAGAMENTO)
              
              df_pago <<- df_prod %>% dplyr::filter(STATUS_PRINCIPAL == "PAGO AO CLIENTE")
              prod_total <<- df_pago %>% dplyr::summarise(Producao = sum(VLR_PRODUCAO),
                                                         Qntd     =sum(Qntd_Propostas))
              prod_2021 <<- df_pago %>% dplyr::filter(DATA_PAGAMENTO >= "2021-01-01") %>% dplyr::summarise(Producao = sum(VLR_PRODUCAO),
                                                                                                          Qntd     =sum(Qntd_Propostas))
              
              prod_2022 <<- df_pago %>% dplyr::filter(DATA_PAGAMENTO >= "2022-01-01") %>% dplyr::summarise(Producao = sum(VLR_PRODUCAO),
                                                                                                          Qntd     =sum(Qntd_Propostas))
              
              mes_atual <<- lubridate::month(lubridate::today())
              ano_atual <<- lubridate::year(lubridate::today())
              
              prod_mes_atual <<- df_pago %>% dplyr::filter( (ANO_PAGAMENTO==ano_atual)  & (MES_PAGAMENTO==mes_atual))   %>% dplyr::summarise(Producao = sum(VLR_PRODUCAO),
                                                                                                                                            Qntd     =sum(Qntd_Propostas))
              
              
              
              div(
                HTML("<div style='color:#273658;text-align:center;font-weight:bold;'><h1 style='color:#273658;text-align:center;font-weight:bold;'>PRODUÇÃO</h1> </div>"),
                
                
                  path_pg5)
              
            })
            
            output$page6 <- renderUI({
              
              HTML("<h3 style='color:#273658;text-align:center;font-weight:bold;'>INADIMPLÊNCIA</h1> ")
              
            })
            
            
           }
    
}) 
    
    
    
    observeEvent(input$tabs, {
      
      if(input$tabs == "page1"){
        
        output$config_ui <- renderUI({
          
          div(br(),br(),dateRangeInput(inputId = "data_consulta_msg",label = "Data", language = "pt-BR",start = lubridate::as_date("2022-01-01"), end = lubridate::today()))
          
          
        })
        
      }else if(input$tabs == "page2"){
        output$config_ui <- renderUI({
          
          div(br(),br(),
              HTML("<div style='color:#E4781C;text-align:center;font-weight:bold;'><h4 style='color:#E4781C;text-align:center;font-weight:bold;'>Filtros</h4> </div>"),
            fluidRow(column(width = 12,
                            selectInput(inputId = "visao",label =  "VISAO:",
                                        c("Lojas" = "Nomes.e.sobrenomes",
                                          "Regional" = "Lojas")),
                            textOutput("count")
            ),
            column(width = 12, selectInput(inputId = "canal",label =  "CANAL:",
                                          choices = c(sort(unique(df[,"Lojas"]))),
                                          #choices = NULL,
                                          multiple = TRUE)
            ),
            column(width = 12,
                   selectInput(inputId = "regional",label =  "REGIONAL:",
                               choices = c(sort(unique(df[,"Regional"]))),
                               #choices = NULL,
                               multiple = TRUE)
            ),
            column(width = 12,
                   selectInput(inputId = "versao",label =  "Template:",
                               c("V1","V2")))),
            HTML("<div style='color:#E4781C;text-align:center;font-weight:bold;'><h4 style='color:#E4781C;text-align:center;font-weight:bold;'>Filtros Movimentações</h4> </div>"),
            dateInput(inputId = "data_referencia",label = "Data Referência",value = "2022-03-07"))
          
          
        })
        
      }else if(input$tabs == "page3"){
        output$config_ui <- renderUI({
          
         div("NULL")
          
        })
        
      }else if(input$tabs == "page4"){
        output$config_ui <- renderUI({
          div("NULL")
          
          
        })
        
      }else if(input$tabs == "page5"){
        output$config_ui <- renderUI({
          
          div("NULL")
          
        })
        
      }else if(input$tabs == "page6"){
        output$config_ui <- renderUI({
          
          div("NULL")
          
        })
        
      }
      
    })
    
    

    ######################################                            ###########################################################
    ###################################### Renderiznado Info Whatsapp ###########################################################
    ######################################                            ###########################################################  
    
    #x <- read_url_csv(onedrive_url)
    
   
    l <- list(
      title = list(text='<b> Legenda </b>'),
      orientation = 'h',
      xanchor = "center",
      yanchor = "top",
      x = 0.5,
      y = 1.25,
      font = list(
        family = "sans-serif",
        size = 12,
        color = "#000"),
      bgcolor = "#E2E2E2",
      bordercolor = "#FFFFFF",
      borderwidth = 2)
    
    
    
    
    df_msg_bitrix <- reactive({
      
      
      
     
    onedrive_url <- "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/Ea1IGOUCSa1Mjlev_QvrNLAB4I_qcKHjWy908-RxDbWPcQ?download=1"

    
    contatos <<- read_url_csv(url = "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/EUJqJfojMwtMilqTonJmeoABp57gRp0UWzWlzdmo_xDLCA?download=1",sep = ";",enc = "latin1")
    qtd_contatos_enviados <- contatos %>% dplyr::filter(CELULAR != "(44) 99890-6216") %>% dplyr::summarise(
                                                                                              qtd_filiais   = n(),
                                                                                              qtd_regionais = length(unique(Regional)),
                                                                                              qtd_dir       = 3)
    x1$`Qntd. Esperada` <- qtd_contatos_enviados %>% sum
    
    x1 <- x1 %>% dplyr::filter(dia >= input$data_consulta_msg[1] & dia <= input$data_consulta_msg[2])
    mes_atual = lubridate::month(x1$dia) == max(unique(lubridate::month(x1$dia)))
    hoje = lubridate::today() == x1$dia
    output$msgbox_bitrix1 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("TOTAL", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(sum(x1[,"total"]), style = "font-size:50%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })

    output$msgbox_bitrix2 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("PENDENTE", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(sum(x1[,"pending"]), style = "font-size:50%;color:#E4781C;font-weight:bold") ,
                               width = 4 , color = "navy")
    })

    output$msgbox_bitrix3 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("ENVIADO", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(sum(x1[,"sent"]), style = "font-size:50%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })
    output$msgbox_bitrix4 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("TOTAL", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(sum(x1[mes_atual,"total"]), style = "font-size:50%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })

    output$msgbox_bitrix5 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("PENDENTE", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(sum(x1[mes_atual,"pending"]), style = "font-size:50%;color:#E4781C;font-weight:bold") ,
                               width = 4,icon = tags$i(class = "fas fa-phone", style="font-size: 12px"), color = "navy")
    })

    output$msgbox_bitrix6 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("ENVIADO", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(sum(x1[mes_atual,"sent"]), style = "font-size:50%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })

    output$msgbox_bitrix7 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("TOTAL", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(sum(x1[hoje,"total"]), style = "font-size:50%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })

    output$msgbox_bitrix8 <- renderValueBox({
      shinydashboard::valueBox(subtitle =tags$p("PENDENTE", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(sum(x1[hoje,"pending"]), style = "font-size:50%;color:#E4781C;font-weight:bold"),
                               width = 4, color = "navy")
    })

    output$msgbox_bitrix9 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("ENVIADO", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(sum(x1[hoje,"sent"]), style = "font-size:50%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })
    
    df_melt         <- x1 %>% reshape2::melt(id.vars = "dia",measure.vars = c("total","pending","sent"),value.name = "Envios",variable.name= "Legenda")
    df_melt$Legenda <- as.character(df_melt$Legenda)
    #df_melt         <- df_melt %>% dplyr::group_by(Legenda) %>% summarise("Acumulado" = cumsum(Envios))
    df_melt_acumulado  <- tapply(X = df_melt$Envios,INDEX = df_melt$Legenda,FUN = cumsum) 
    acumulado         <- c(df_melt_acumulado$total,df_melt_acumulado$pending,df_melt_acumulado$sent)
    df_melt$Acumulado <- acumulado
    medias <-  tapply(X = df_melt$Envios,INDEX = df_melt$Legenda,FUN = mean) %>% ldply(data.frame)
    #df_melt$Media <- ifelse(df_melt$Legenda == "Pendente",) 
    
    vline <- function(x = 0, color = "green") {
      list(
        type = "line",
        #y0 = 0,
        #y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash="dot")
      )
    }
    
    
    output$plot_envio_diario <- renderPlotly({
      
      p1 <- ggplot(data = df_melt,aes(x= dia, y = Envios)) +
        geom_point(size = 1.2, alpha = 0.75)+
        geom_line(size = 1.2, alpha = 0.75,aes(color  = Legenda,group = Legenda)) +
        scale_color_manual(values = c("darkgreen", "red","darkblue")) +
         scale_x_continuous(breaks = seq(min(x1$dia),max(x1$dia),by = paste(length(unique(lubridate::month(x1$dia))),"days") ) )+
        # scale_y_continuous(breaks = seq(0,1,0.1))+
        axis.theme(title_size = 10,textsize = 10,pos_leg = "bottom",x.angle = 45,vjust = 1,hjust=1) +
        geom_vline(xintercept = max(df_melt$dia),
                   linetype = "dashed", colour = "red", alpha = 1,size = 0.8) +
        geom_hline(yintercept = sum(qtd_contatos_enviados),
                   linetype = "dashed", colour = "red", alpha = 1,size = 0.8) 
     plot <- ggplotly(p1) %>% layout(hovermode = "x", spikedistance =  -1,margin = c(0,0,0,10),legend = l,
                              xaxis = list(title = "<b>Dias</b>", showspikes = TRUE, titlefont = list(size = 16),
                                           spikemode  = 'across', #toaxis, across, marker
                                           spikesnap = 'cursor',  ticks = "outside",tickangle = -45,
                                           showline=TRUE,tickfont = list(size = 12),fixedrange=TRUE,
                                           showgrid=TRUE),
                              yaxis = list (title = "<b>Qntd. Envios</b>",
                                            spikemode  = 'across', #toaxis, across, marker
                                            spikesnap = 'cursor', zeroline=FALSE,titlefont = list(size = 16),
                                            showline=TRUE,tickfont = list(size = 12),fixedrange=TRUE,
                                            showgrid=TRUE),height = 480) %>% config(displayModeBar = FALSE) 
    plot  
      
    })
    
   
    
    output$plot_envio_acumulado <- renderPlotly({
     
      
      p1 <- ggplot(data = df_melt,aes(x= dia, y = Acumulado)) +
        geom_point(size = 1.2, alpha = 0.75)+
        geom_line(size = 1.2, alpha = 0.75,aes(color  = Legenda,group = Legenda)) +
        scale_color_manual(values = c("darkgreen", "red","darkblue")) +
        geom_vline(xintercept = max(df_melt$dia),
                   linetype = "dashed", colour = "red", alpha = 1,size = 0.8) +
      scale_x_continuous(breaks = seq(min(x1$dia),max(x1$dia),by = paste(length(unique(lubridate::month(x1$dia))),"days") ) )+
        # scale_y_continuous(breaks = seq(0,1,0.1))+
        axis.theme(title_size = 12,textsize = 12,pos_leg = "bottom",x.angle = 45,vjust = 1,hjust=1)
      plot <- ggplotly(p1) %>% layout(hovermode = "y", spikedistance =  -1,margin = c(0,0,0,10),legend = l,
                                      xaxis = list(title = "<b>Dias</b>", showspikes = TRUE, titlefont = list(size = 16),
                                                   spikemode  = 'across', #toaxis, across, marker
                                                   spikesnap = 'cursor',  ticks = "outside",tickangle = -45,
                                                   showline=TRUE,tickfont = list(size = 12),fixedrange=TRUE,
                                                   showgrid=TRUE),
                                      yaxis = list (title = "<b>Qntd. Envios</b>",
                                                    spikemode  = 'across', #toaxis, across, marker
                                                    spikesnap = 'cursor', zeroline=FALSE,titlefont = list(size = 16),
                                                    showline=TRUE,tickfont = list(size = 12),fixedrange=TRUE,
                                                    showgrid=TRUE),height = 480) %>% config(displayModeBar = FALSE)
      plot  
      
    })
    
    
    output$plot_envio_acumulado_mes <- renderPlotly({
      x2 <- x2 %>% dplyr::filter( lubridate::month(dia) == lubridate::month(lubridate::today()))
      
      df_melt         <- x2 %>% reshape2::melt(id.vars = "dia",measure.vars = c("total","pending","sent"),value.name = "Envios",variable.name= "Legenda")
      df_melt$Legenda <- as.character(df_melt$Legenda)
      #df_melt         <- df_melt %>% dplyr::group_by(Legenda) %>% summarise("Acumulado" = cumsum(Envios))
      df_melt_acumulado  <- tapply(X = df_melt$Envios,INDEX = df_melt$Legenda,FUN = cumsum) 
      acumulado         <- c(df_melt_acumulado$total,df_melt_acumulado$pending,df_melt_acumulado$sent)
      df_melt$Acumulado <- acumulado
      
      p1 <- ggplot(data = df_melt,aes(x= dia, y = Acumulado)) +
        geom_point(size = 1.2, alpha = 0.75)+
        geom_line(size = 1.2, alpha = 0.75,aes(color  = Legenda,group = Legenda)) +
        scale_color_manual(values = c("darkgreen", "red","darkblue")) +
        geom_hline(yintercept = 5000,
                   linetype = "dashed", colour = "red", alpha = 1,size = 0.8) +  
        geom_vline(xintercept = max(df_melt$dia),
                   linetype = "dashed", colour = "red", alpha = 1,size = 0.8) +
        scale_x_continuous(breaks = seq(min(x1$dia),max(x1$dia),by = paste(length(unique(lubridate::month(x1$dia))),"days") ) )+
        # scale_y_continuous(breaks = seq(0,1,0.1))+
        axis.theme(title_size = 12,textsize = 12,pos_leg = "bottom",x.angle = 45,vjust = 1,hjust=1)
      plot <- ggplotly(p1) %>% layout(hovermode = "y", spikedistance =  -1,margin = c(0,0,0,10),legend = l,
                                      xaxis = list(title = "<b>Dias</b>", showspikes = TRUE, titlefont = list(size = 16),
                                                   spikemode  = 'across', #toaxis, across, marker
                                                   spikesnap = 'cursor',  ticks = "outside",tickangle = -45,
                                                   showline=TRUE,tickfont = list(size = 12),fixedrange=TRUE,
                                                   showgrid=TRUE),
                                      yaxis = list (title = "<b>Qntd. Envios</b>",
                                                    spikemode  = 'across', #toaxis, across, marker
                                                    spikesnap = 'cursor', zeroline=FALSE,titlefont = list(size = 16),
                                                    showline=TRUE,tickfont = list(size = 12),fixedrange=TRUE,
                                                    showgrid=TRUE),height = 480) %>% config(displayModeBar = FALSE)
      plot  
      
    })
    
    
    x1
    })
    
    ######################################               ###########################################################
    ###################################### Informações Bitrix ###########################################################
    ######################################               ###########################################################
   
    
    
    dataset <- reactive({
      
      data = df1
      
      if (length(input$canal)){
        f1 =  data$Lojas %in% input$canal
        data = data[f1,]
      }
      else {
        data
      }
      
      if (length(input$regional)){
        
        f2 = data$Regional %in% input$regional
        data = data[f2,]
        #updateSelectInput(session = session,inputId = "canal",choices = c(sort(unique(data[,"Lojas"]))),selected = input$canal)
      }
      else {
        data
      }
      #data
      
      
      
      
      # updateSelectInput(session = session,inputId = "regional",choices = c(sort(unique(data[,"REGIONAL"]))),selected = input$regional)
      
      output$total_bitrix_vbox <- renderUI({
        p(nrow(data),br(), " Total",style="color:#273658;text-align:center;font-weight:bold;font-size:30px")
      })
      
      group_count <- as.character(input$visao)
     
      Total <- sum  
      #as.character(input$visao)
      table <- table(data[,c(group_count)],data$`Fase do negocio`)  %>% as.data.frame
      table <- reshape2::dcast(table, Var1 ~ Var2, value.var="Freq") 
      
      #table <- table %>% mutate("Taxa de Pagos" = paste0(round((PAGO/Total)*100,2)," %"))
      colnames(table)[1] <- c("Usuário")
      
      table_fases_group <- matrix(data = 0,nrow = nrow(table),ncol = 7) %>% as.data.frame
      colnames(table_fases_group) <- c("Usuário", "NOVO","PROSPECTANDO","NEGOCIANDO","EM ANÁLISE", "PAGO","DESAFIO")
      for(name in colnames(table)){
        
        table_fases_group[name] <- table[name]
        
        
      }
      
      
      output$taxa_leads_bitrix <- renderUI({
        leads_tratados         <<- data %>% filter(`Fase do negocio` != "NOVO") %>% nrow
        pagos                  <<- data %>% filter(`Fase do negocio` == "PAGO") %>% nrow
        taxa_tratados          <<- paste0(round((pagos/leads_tratados)*100,2)," %")
        p(taxa_tratados,br(), " Taxa de Leads Tratados",style="color:#273658;text-align:center;font-weight:bold;font-size:30px")
        
      })
      
      
      
      
      output$proporcao_leads_fase <- renderTable({
        
        resumo_fases           <- data.frame(Fase = c("NOVO","PROSPECTANDO","NEGOCIANDO","EM ANÁLISE","PAGO","DESAFIO"), Qntd. = 0, "QntdP" = 0)
        
        aux1                   <- table(data$`Fase do negocio`) %>% as.data.frame %>%
          mutate(Var1 = as.character(Var1)) %>%
          mutate(Var1 = if_else(Var1 == "PAGO", "PAGO",Var1))
        
        aux2                   <- prop.table(table(data$`Fase do negocio`)) %>%
          as.data.frame %>%
          mutate(Var1 = as.character(Var1)) %>%
          mutate(Var1 = if_else(Var1 == "PAGO", "PAGO",Var1)) %>%
          mutate(Freq = paste0(round(Freq * 100,2),"%"))
        
        
        for(fase in aux1$Var1){
          
          resumo_fases[resumo_fases$Fase == fase,"Qntd."] <- aux1[aux1$Var1 == fase,"Freq"]
          resumo_fases[resumo_fases$Fase == fase,"QntdP"] <- aux2[aux2$Var1 == fase,"Freq"]
          
        }
        
        colnames(resumo_fases) <- c("Fase","Qntd.","%")
        resumo_fases <- resumo_fases  %>%
          mutate(prop = Qntd. / sum(resumo_fases$Qntd.) *100) %>%
          mutate(ypos = cumsum(prop)- 0.5*prop ) %>%
          dplyr::select("Fase","Qntd.","%")
        
        resumo_fases$Qntd. <- round(resumo_fases$Qntd.,0)
        resumo_fases
      })
      
      
      
      
      table_fases_group <- table_fases_group %>% mutate("Total" =  PROSPECTANDO + NEGOCIANDO + `EM ANÁLISE` + PAGO + DESAFIO)
      table_fases_group <- table_fases_group %>% mutate("Taxa de Pagos" = ifelse(is.nan(PAGO/Total),"0 %",paste0(round((PAGO/Total)*100,2)," %"))) 
      table_fases_group
      
      
      
      
    })
    
    
    
    
    output$table_fase_group <- renderDT(
      dataset(),
      extensions = 'Buttons',server = FALSE,
      options = list(
        lengthChange = FALSE,
        # scrollX=TRUE,
        # lengthMenu = c(5,10,15),
        paging = TRUE,
        searching = TRUE,
        # fixedColumns = TRUE,
        # autoWidth = TRUE,
        # ordering = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel','pdf')
      ) ,
      class = "display"
    )
    
    
    dataset1 <- reactive({
      data = df1
     
      if (length(input$canal)){
        f1 =  data$Lojas %in% input$canal
        data = data[f1,]
      }
      else {
        data
      }
      
      if (length(input$regional)){
        
        f2 = data$Regional %in% input$regional
        data = data[f2,]
        #updateSelectInput(session = session,inputId = "canal",choices = c(sort(unique(data[,"Lojas"]))),selected = input$canal)
      }
      else {
        data
      }
      data
      
      
      Total <- sum
      dados <- data %>% dplyr::select(`Data criado`,`Data prospectar`, `Data fechado`, `Data analisar`, `Data negociar`,ID_LEADS) %>%
        reshape2::melt(id.vars = "ID_LEADS",measure.vars = c("Data criado","Data prospectar","Data fechado", "Data analisar", "Data negociar")) %>%
        mutate("Atributo"      = ifelse(variable == "Data criado","Criou Lead",
                                        ifelse(variable == "Data prospectar","Moveu para prospectando",
                                               ifelse(variable == "Data negociar","Moveu para negociando",
                                                      ifelse(variable == "Data analisar","Moveu para em análise",
                                                             ifelse(variable == "Data fechado","Fechou lead(Desafio ou pago)",variable))))))
      
      
      completedados <- dados[complete.cases(dados),]
      dados               <- left_join(completedados,df1,by = c("ID_LEADS"))
      dados   <- dados %>% dplyr::mutate("Atributo" = ifelse(Atributo == "Fechou lead(Desafio ou pago)",paste0("Moveu para ",`Fase do negocio`),Atributo))
      
      
      dados               <- dados %>% filter(value == "2022-03-08")
      total_movimentacoes <- dados %>%  nrow
      
      
      
      output$total_movimentacoes <- renderUI({
        p(total_movimentacoes,br(), "Tota de Movimentações na Data Referência",style="color:#273658;text-align:center;font-weight:bold;font-size:30px")
      })
      
      
      output$table_res_mov <- renderTable({
        
        table_movimentacoes <- table(dados$Atributo) %>% as.data.frame
        
        if(nrow(table_movimentacoes) > 0){
          
          resumo_movimentacoes           <- data.frame(mov = c("Criou Lead","Moveu para prospectando","Moveu para negociando","Moveu para em análise","Moveu para PAGO","Moveu para DESAFIO"), Qntd. = 0)
          
          
          
          
          for(movimentacao in table_movimentacoes$Var1){
            
            resumo_movimentacoes[resumo_movimentacoes$mov == movimentacao,"Qntd."] <- table_movimentacoes[table_movimentacoes$Var1 == movimentacao,"Freq"]
            
          }
          
          resumo_movimentacoes <- resumo_movimentacoes %>% mutate("%" = paste0(round( (`Qntd.`/total_movimentacoes)*100,2)," %"))
          
          
          
          colnames(resumo_movimentacoes) <- c("Movimentação","Qtnd.","%")
          resumo_movimentacoes
          
        }else{
          
          resumo_movimentacoes           <- data.frame(mov = c("Criou Lead","Moveu para prospectando","Moveu para negociando","Moveu para em análise","Moveu para PAGO","Moveu para DESAFIO"), Qntd. = 0,"%" = 0)
          colnames(resumo_movimentacoes) <- c("Movimentação","Qtnd.","%")
          resumo_movimentacoes
        }
        
        
        
      })
      
      Total <- sum
      
      
      table_movimentacoes <- table(dados[,input$visao],dados$Atributo) %>% as.data.frame
      
      if(nrow(table_movimentacoes) > 0){
        table_movimentacoes <- reshape2::dcast(table_movimentacoes, Var1 ~ Var2, value.var="Freq")
        
        
        
        table_fases_group <- matrix(data = 0,nrow = nrow(table_movimentacoes),ncol = 7) %>% as.data.frame
        colnames(table_fases_group) <- c("Var1", "Criou Lead","Moveu para prospectando", "Moveu para negociando","Moveu para em análise","Moveu para PAGO","Moveu para DESAFIO")
        for(name in colnames(table_movimentacoes)){
          
          table_fases_group[name] <- table_movimentacoes[name]
          
          
        }
        
        
        
        
        colnames(table_fases_group)[1] <- c("Lojas")
        table_fases_group <- table_fases_group  %>% mutate("Total" = `Criou Lead` + `Moveu para prospectando` + `Moveu para negociando` + `Moveu para em análise` + `Moveu para PAGO` + `Moveu para DESAFIO`)
        table_fases_group$Lojas <-  str_replace(string = table_fases_group$Lojas,pattern = "Loja CFZ ",replacement = "")
        table_fases_group 
        
        
      }else{
        
        
        table_fases_group <- matrix(data = 0,nrow = 1,ncol = 7) %>% as.data.frame
        colnames(table_fases_group) <- c("Lojas","Criou Lead","Moveu para prospectando", "Moveu para negociando","Moveu para em análise","Moveu para PAGO","Moveu para DESAFIO")
        table_fases_group
        
      }
      
    })
    
    output$table_movimentacoes <- renderDT(
      dataset1(),
      extensions = 'Buttons',server = FALSE,
      options = list(
        lengthChange = FALSE,
        # scrollX=TRUE,
        # lengthMenu = c(5,10,15),
        paging = TRUE,
        searching = TRUE,
        # fixedColumns = TRUE,
        # autoWidth = TRUE,
        # ordering = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel','pdf')
      ) ,
      class = "display"
    )
    
    
    
    output$tb_msg_bitrix <- renderDT(
      df_msg_bitrix(),
      extensions = 'Buttons',server = FALSE,
      options = list(
        lengthChange = FALSE,
        # scrollX=TRUE,
        # lengthMenu = c(5,10,15),
        paging = TRUE,
        searching = TRUE,
        # fixedColumns = TRUE,
        # autoWidth = TRUE,
        # ordering = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel','pdf')
      ) ,
      class = "display"
    )
    ######################################          ###########################################################
    ###################################### PRODUÇÃO ###########################################################
    ######################################          ###########################################################
    
observeEvent(input$tabs,{
  
  if(input$tabs == "page5"){
    
    
    
  }
  

    
  
  output$box_uiprod1 <- renderUI({
    
    
    
    HTML(paste0('
  <center>
<table>
 <tr >
  <td valign="top">
  
           <div style = "background-color:#273658;">
           <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:14px"> ',paste0("R$ ",format(prod_total[1,1],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'  </h1> 
           <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:14px">  Total  </h3>  
           </div>
           <hr style="width:100%;text-align:center;margin-left:0;height:4px;color:#652B91;border-width: 2px;">
           </td>
           
  <td valign="top">
           <div style = "background-color:#273658;">
           <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:14px">',paste0("R$ ",format(prod_total[1,2],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'</h1> 
           <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:14px">  Qntd. Propostas  </h3>  
           </div>
           <hr style="width:100%;text-align:center;margin-left:0;height:4px;color:#652B91;border-width: 2px;">
           </td>
           
  <td valign="top">
           <div style = "background-color:#273658;">
           <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:14px">   </h1> 
           <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:14px">  Projetado </h3>  
           </div>
           <hr style="width:100%;text-align:center;margin-left:0;height:4px;color:#652B91;border-width: 2px;">
           </td>
           </tr>
    </table>
           </center>'))
    
    
    
  })
   
    output$box_prod1 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("Total", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(paste0("R$ ",format(prod_total[1,1],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")), style = "font-size:40%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })
    
    output$box_prod2 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("Qntd. Propostas", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(format(prod_total[1,2],scientific =FALSE,big.mark ="."), style = "font-size:40%;color:#E4781C;font-weight:bold") ,
                               width = 4 , color = "navy")
    })
    
    output$box_prod3 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p("", style = "font-size:40%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })
    output$box_prod4 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("Total", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(paste0("R$ ",format(prod_2021[1,1],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")), style = "font-size:40%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })
    
    output$box_prod5 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("Qntd. Propostas", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(format(prod_2021[1,2],scientific =FALSE,big.mark ="."), style = "font-size:40%;color:#E4781C;font-weight:bold") ,
                               width = 4,icon = tags$i(class = "fas fa-phone", style="font-size: 12px"), color = "navy")
    })
    
    output$box_prod6 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(0, style = "font-size:40%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })
    
    output$box_prod7 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("Total", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(paste0("R$ ",format(prod_2022[1,1],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")), style = "font-size:40%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })
    
    output$box_prod8 <- renderValueBox({
      shinydashboard::valueBox(subtitle =tags$p("Qntd. Propostas", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(format(prod_2022[1,2],scientific =FALSE,big.mark ="."), style = "font-size:40%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })
    
    output$box_prod9 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(0, style = "font-size:40%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })
    
    output$box_prod10 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("Total", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(paste0("R$ ",format(prod_mes_atual[1,1],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")), style = "font-size:40%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })
    
    output$box_prod11 <- renderValueBox({
      shinydashboard::valueBox(subtitle =tags$p("Qntd. Propostas", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(format(prod_mes_atual[1,2],scientific =FALSE,big.mark ="."), style = "font-size:40%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })
    
    output$box_prod12 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(0, style = "font-size:40%;color:#E4781C;font-weight:bold") ,
                               width = 4, color = "navy")
    })
    
      
      
      output$serie_prod <- renderPlot({
        
       plot <- df_pago  %>%  
          dplyr::group_by(DATA_PAGAMENTO) %>% 
          dplyr::summarise(Producao = sum(VLR_PRODUCAO),
                           Qntd     =sum(Qntd_Propostas)) %>%
          
          ggplot(aes(x = DATA_PAGAMENTO,y=Producao)) +
          geom_line(size = 1.2, alpha = 0.75) +
         geom_point(size = 1.2, alpha = 0.75) +
          axis.theme(title_size = 12,textsize = 12,pos_leg = "bottom",x.angle = 45,vjust = 1,hjust=1)
        plot  
        
      })
      
      
}) 
 
    
   
  
}
