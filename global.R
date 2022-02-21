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
#source(file = "funcoes.R",encoding = "UTF-8")
read_url_csv <- function(url, sep = ","){
  tmpFile <- tempfile()
  download.file(url, destfile = tmpFile)
  url_csv <- read.csv(tmpFile, sep = sep)
  return(url_csv)
}

read_url_xlsx <- function(url,sheet = "Calendario"){
  tmpFile <- tempfile()
  download.file(url, destfile = tmpFile)
  url_csv <- read_xlsx(path = tmpFile,sheet = sheet)
  return(url_csv)
}


library(readxl)
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
df <- read_url_csv(onedrive_url)

names_df          <- c("ID","Tipo","Pipeline", "Fase do negocio","Negocio Recorrente","Negocio Repetido","Contato Fonte","Modificado por ID","Criado por","Pessoa reponsavel ID",
                       "Data prevista de fechamento","Data de inicio","Valor do emprestimo","Origem do Cliente","Data exportacao","Contato","Grupo de fase" ,
                       "Base","Produto Crefaz", "Fase automacao",  "Data negociar","Data analisar", "Data prospectar","Modificado em","Desafio","Desafio retencao"
)
colnames(df) <- names_df
df$`Pessoa reponsavel ID`          <- as.character(df$`Pessoa reponsavel ID` )

onedrive_url <- "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/ES9Opw7qVkROh6yxmaC8ARQB1_RuzanE2l8fl3U7p6r4KQ?download=1"

users             <- read_url_csv(onedrive_url,sep = ";")
users$ID          <- as.character(users$ID)

library(readxl)
url <- "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/EaODUUKwdOFJtbyx6MQHshsBjMAYRqYYyXr-el08rDnpxQ?download=1"
destfile <- "regionais.xlsx"
curl::curl_download(url, destfile)
regionais <- read_excel(destfile,sheet = "Planilha1")


df                <- left_join(x = df,y = users,by=c("Pessoa reponsavel ID"="ID"),keep=TRUE,suffix = c("_LEADS","_users"))
#df                <- left_join(x = df,y = regionais,by=c("Departamento"="Departamento"),keep=TRUE,suffix = c("_LEADS","_reg"))

df$Regional       <- str_replace(string = df$Regional,pattern = "Super. ES 2 Lojas CFZ",replacement = "SIMONE FREITAS")
df$Regional       <- str_replace(string = df$Regional,pattern = "Super. RJ Lojas CFZ",replacement = "MAYSA CARVALHO")
df$Regional       <- str_replace(string = df$Regional,pattern = "Super. RS Lojas CFZ",replacement = "WAGNER RIBEIRO")
df$Regional       <- str_replace(string = df$Regional,pattern = "Super. CE lojas CFZ",replacement = "GILBERTO FELICIO")
df$Regional       <- str_replace(string = df$Regional,pattern = "Super. Lojas SC",replacement = "REGIONAL SC")
df$Regional       <- str_replace(string = df$Regional,pattern = "Sup. Estadual Eliana",replacement = "ELIANA PORRINO")
df$Regional       <- str_replace(string = df$Regional,pattern = "Regional Yrlon",replacement = "YRLON ALVES")
df$Regional       <- str_replace(string = df$Regional,pattern = "Regional Mirele CFZ",replacement = "MIRELE DUARTE")
df$Regional       <- str_replace(string = df$Regional,pattern = "Regional Helen",replacement = "HELEN CAROLINA")
df$Regional       <- str_replace(string = df$Regional,pattern = "Regional Dejamile",replacement = "DEJAMILE SOUZA")
df$Regional       <- str_replace(string = df$Regional,pattern = "Regional Helen",replacement = "HELEN CAROLINA")
df$Regional       <- str_replace(string = df$Regional,pattern = "Regional Igor",replacement = "IGOR ASSIS")

colnames(df)[colnames(df) == "Departamento"] <- "Lojas"



df                <- df %>% filter(str_detect(string = Lojas,pattern = "Loja CFZ"))



# df                <- df %>% filter(str_detect(REGIONAL,pattern = "GILBERTO FELICIO") | str_detect(REGIONAL,pattern = "MAYSA CARVALHO") | str_detect(REGIONAL,pattern = "SC"))


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

#df$`Data de inicio` %>% range

#df$`Criado em`         <- as.Date(df$`Criado em`,format = "%d/%m/%Y")
df$`Data fechado`       <- lubridate::as_date(df$`Data fechado` , format = "%Y-%m-%d %H:%M:%S")
df$`Data analisar`      <- lubridate::as_date(df$`Data analisar`, format = "%Y-%m-%d %H:%M:%S")
df$`Data criado`        <- lubridate::as_date(df$`Data criado`, format = "%Y-%m-%d %H:%M:%S")
df$`Data negociar`      <- lubridate::as_date(df$`Data negociar`, format = "%Y-%m-%d %H:%M:%S")
df$`Data prospectar`    <- lubridate::as_date(df$`Data prospectar`, format = "%Y-%m-%d %H:%M:%S")
df$`Data de inicio`     <- lubridate::as_date(df$`Data de inicio`, format = "%Y-%m-%d %H:%M:%S")
df$`Modificado em`      <- lubridate::as_date(df$`Modificado em` , format = "%Y-%m-%d %H:%M:%S")



df1 <- df

