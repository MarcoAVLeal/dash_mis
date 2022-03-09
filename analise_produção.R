source(file = "funcoes.R",encoding = "UTF-8")
library(tidyverse)
library(knitr)
library(kableExtra)
library(caret)
library(GGally)
library(ggplot2)
library(MLmetrics)
library(klaR)
library(plotROC)
library(RColorBrewer)
library(zoo)
library(forecast)
cores <- c(RColorBrewer::brewer.pal(9,name = "Set1"),RColorBrewer::brewer.pal(8,name = "Dark2"))
cores <- cores[-c(4,1)]
knitr::opts_chunk$set(echo = TRUE,warning= FALSE, message= FALSE,
                      out.width = "100%",fig.align = "center",size ="large",fig.height = 3.5)
color_pal = c(RColorBrewer::brewer.pal(n = 8,name = "Dark2")[c(2,3,5,6,7,8,9)],
              RColorBrewer::brewer.pal(n = 8,name = "Set1"),
              RColorBrewer::brewer.pal(n = 8,name = "Set2"))
df <- read.csv(file = "D:\\OneDrive - Crefaz Financiamentos e Investimentos\\dash_mis_dados\\motor\\motor-agregadado.csv")

df$DATACADASTRO <- lubridate::as_date(df$DATACADASTRO)
df$DATA_PAGAMENTO <- lubridate::as_date(df$DATA_PAGAMENTO)

df$ANO_CADASTRO <- lubridate::year(df$DATACADASTRO)
df$ANO_PAGAMENTO <- lubridate::year(df$DATA_PAGAMENTO)
df$MES_CADASTRO <- lubridate::month(df$DATACADASTRO)
df$MES_PAGAMENTO <- lubridate::month(df$DATA_PAGAMENTO)

df_pago <- df %>% dplyr::filter(STATUS_PRINCIPAL == "PAGO AO CLIENTE")

df_pago  %>% dplyr::summarise(Producao = sum(VLR_PRODUCAO),Qntd     =sum(Qntd_Propostas))

producao_df <- df_pago %>%  dplyr::group_by(DATA_PAGAMENTO) %>% dplyr::summarise(Producao = sum(VLR_PRODUCAO),
                                                                                           Qntd     =sum(Qntd_Propostas)) %>% dplyr::select(DATA_PAGAMENTO,Producao)
  
  producao   <- zoo(producao_df$Producao  ,producao_df$DATA_PAGAMENTO)
  
p1 <- autoplot.zoo(producao) + 
  geom_line(size = 0.35,alpha=1,color="black")+
  #geom_point() +
  #geom_point(size = .3,alpha = 0.25,color="black") +
  labs(x = "Data", y = "Produção") +
  # scale_color_manual(values = color_pal) +
  scale_x_date(date_breaks = "12 months",date_labels = "%Y")+
  axis.theme(x.angle = 45,vjust = 1,hjust = 1,axis.title.size.x = 16,axis.title.size.y = 16,tick.size = 16)
  
producao   <- zoo(log(producao_df$Producao)  ,producao_df$DATA_PAGAMENTO)
p2 <- autoplot.zoo(producao) + 
  geom_line(size = 0.35,alpha=1,color="black")+
  #geom_point() +
  #geom_point(size = .3,alpha = 0.25,color="black") +
  labs(x = "Data", y = "Produção") +
  # scale_color_manual(values = color_pal) +
  scale_x_date(date_breaks = "12 months",date_labels = "%Y")+
  axis.theme(x.angle = 45,vjust = 1,hjust = 1,axis.title.size.x = 16,axis.title.size.y = 16,tick.size = 16)
cowplot::plot_grid(p1, p2,ncol=1,nrow=2,labels = LETTERS[1:2],align = "v")

df_pago %>%  dplyr::group_by(DATA_PAGAMENTO) %>% dplyr::summarise(Producao = sum(VLR_PRODUCAO),
                                                                          Qntd     =sum(Qntd_Propostas)) %>%
  ggplot(aes(x = DATA_PAGAMENTO,y=Producao)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "gam",se = TRUE)


Diesel   <- zoo(df$Diesel  ,df$Data)
Gasolina <- zoo(df$Gasolina,df$Data)
