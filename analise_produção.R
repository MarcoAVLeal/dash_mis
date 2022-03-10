source(file = "funcoes.R",encoding = "UTF-8")
library(tidyverse)
library(knitr)
library(kableExtra)
library(caret)

library(ggplot2)
library(MLmetrics)
library(klaR)
library(plotROC)
library(RColorBrewer)
library(zoo)
library(forecast)
library(GGally)
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


n = nrow(df_pago)


library(zoo)
f_data <- as.Date(as.yearmon(as.Date(max(df_pago$DATA_PAGAMENTO,na.rm = TRUE))) -.6, frac = 1)

reservados   <- df_pago %>% dplyr::filter(DATA_PAGAMENTO > f_data)
df_pago      <- df_pago %>% dplyr::filter(DATA_PAGAMENTO <= f_data)



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

# df_pago %>%  dplyr::group_by(DATA_PAGAMENTO) %>% dplyr::summarise(Producao = sum(VLR_PRODUCAO),
#                                                                           Qntd     =sum(Qntd_Propostas)) %>%
#   ggplot(aes(x = DATA_PAGAMENTO,y=Producao)) +
#   geom_point() +
#   geom_line() +
#   geom_smooth(method = "gam",se = TRUE)

producao   <- zoo(log(producao_df$Producao)  ,producao_df$DATA_PAGAMENTO)
p3 <- autoplot.zoo(diff(producao)) + 
  geom_line(size = 0.25,alpha=1,color="black")+
  #geom_point(size = .3,alpha = 0.25,color="black") +
  labs(x = "Data", y = "Preço") +
  #scale_color_manual(values = color_pal) +
  scale_x_date(date_breaks = "12 months",date_labels = "%Y")+
  axis.theme(x.angle = 45,vjust = 1,hjust = 1,axis.title.size.x = 16,axis.title.size.y = 16,tick.size = 10)

pA1 <- ggAcf(as.zoo(diff(producao)),type = "correlation")+
  labs(x = "Lag", y = "FAC",title=NULL) +
  axis.theme(axis.title.size.x = 16,axis.title.size.y = 16,tick.size = 16)

pB1 <- ggAcf(as.zoo(diff(producao)),type = "partial")+
  labs(x = "Lag", y = "FACP",title=NULL) +
  axis.theme(axis.title.size.x = 16,axis.title.size.y = 16,tick.size = 16)

parcial1 <- cowplot::plot_grid(p3,cowplot::plot_grid(pA1, pB1,ncol=1,nrow=2,labels = LETTERS[2:3],align = "v"),labels = LETTERS[1])
p4 <- autoplot.zoo(diff(diff(producao))) + 
  geom_line(size = 0.25,alpha=1,color="black")+
  #geom_point(size = .3,alpha = 0.25,color="black") +
  labs(x = "Data", y = "Preço") +
  #scale_color_manual(values = color_pal[2]) +
  scale_x_date(date_breaks = "12 months",date_labels = "%Y")+
  axis.theme(x.angle = 45,vjust = 1,hjust = 1,axis.title.size.x = 16,axis.title.size.y = 16,tick.size = 10)

pA2 <- ggAcf(as.zoo(diff(diff(producao))),type = "correlation")+
  labs(x = "Lag", y = "FAC",title = NULL) +
  axis.theme(axis.title.size.x = 16,axis.title.size.y = 16,tick.size = 16)

pB2 <- ggAcf(as.zoo(diff(diff(producao))),type = "partial")+
  labs(x = "Lag", y = "FACP",title = NULL) +
  axis.theme(axis.title.size.x = 16,axis.title.size.y = 16,tick.size = 16)

parcial2 <- cowplot::plot_grid(p4,cowplot::plot_grid(pA2, pB2,ncol=1,nrow=2,labels = LETTERS[4:5],align = "v"),labels = LETTERS[3])

cowplot::plot_grid(parcial1,parcial2, ncol=1,nrow=2,align = "v")




producao   <- zoo(producao_df$Producao  ,producao_df$DATA_PAGAMENTO)

library(lubridate)
p1 <- producao_df %>% mutate(Mes = as.factor(month(DATA_PAGAMENTO))) %>%
  ggplot() +
  labs(x = "Meses", y = "Preço") +
  scale_x_discrete(breaks = 1:12,labels = month.abb) +
  geom_boxplot(aes(x = Mes,y = Producao)) + 
  axis.theme(x.angle = 45,vjust = 1,hjust = 1,axis.title.size.x = 12,axis.title.size.y = 12,tick.size = 10)

p2 <- producao_df %>% mutate(Mes = as.factor(month(DATA_PAGAMENTO))) %>%
  ggplot() +
  labs(x = "Meses", y = "Preço") +
  scale_x_discrete(breaks = 1:12,labels = month.abb) +
  geom_boxplot(aes(x = Mes,y = Producao)) + 
  axis.theme(x.angle = 45,vjust = 1,hjust = 1,axis.title.size.x = 12,axis.title.size.y = 12,tick.size = 10)




cowplot::plot_grid(p1, ncol=1,nrow=1,labels = LETTERS[1],align = "v")




library(aTSA)
library(fpp2) 
Diesel_st   <- ts(data = producao_df$Producao, start=2017,frequency = 1)
# Gasolina_st <-  ts(data = dados$A1, start=1995,frequency = 1)
producao           <- zoo(log(producao_df$Producao)  ,producao_df$DATA_PAGAMENTO)
Producao_Holt      = holt(Diesel_st,level = .95)
Producao_SES       = ses(Diesel_st,level = .95)
length(producao)

producao_df <- producao_df %>% mutate(`Ajuste Holt(Producao)`     = Producao_Holt$fitted,
                    `Ajuste SES(Producao)`      = Producao_SES$fitted)

df_diesel <- producao_df %>% dplyr::select(DATA_PAGAMENTO,Producao,`Ajuste Holt(Producao)`,`Ajuste SES(Producao)`) %>% melt("DATA_PAGAMENTO") %>% dplyr::rename( Producao = value,Legenda = variable)

#df_gasolina <- df %>% select(Data,Gasolina,`Ajuste Holt(Gasolina)`,`Ajuste SES(Gasolina)`) %>% melt("Data") %>% dplyr::rename( Preços = value,Legenda = variable)

p1 <- ggplot(data = df_diesel, aes(x = DATA_PAGAMENTO, y = Producao, linetype = Legenda,color = Legenda)) + 
  geom_line(alpha=1,size = 1.2)+
  #geom_line(data = df,aes(x = Data, y = `Ajuste Holt(Diesel)`),color = "red", lty = "dashed") +
  #geom_point(size = .3,alpha = 0.25,color="black") +
  labs(x = "Data", y = "Preço") +
  scale_color_manual(values = c("black","red","darkgreen")) +
  scale_x_date(date_breaks = "12 months",date_labels = "%Y")+
  axis.theme(x.angle = 45,vjust = 1,hjust = 1,axis.title.size.x = 12,axis.title.size.y = 12,tick.size = 8,lengend_title_size = 10,lengend_text_size = 8,pos_leg = "right")

# p2 <- ggplot(data = df_gasolina, aes(x = Data, y = Preços, linetype = Legenda,color = Legenda)) + 
#   geom_line(alpha=0.85,size = 0.75)+
#   #geom_line(data = df,aes(x = Data, y = `Ajuste Holt(Diesel)`),color = "red", lty = "dashed") +
#   #geom_point(size = .3,alpha = 0.25,color="black") +
#   labs(x = "Data", y = "Preço") +
#  # scale_color_manual(values = color_pal) +
#     scale_x_date(date_breaks = "12 months",date_labels = "%Y")+
#   axis.theme(x.angle = 45,vjust = 1,hjust = 1,axis.title.size.x = 12,axis.title.size.y = 12,tick.size = 8,lengend_title_size = 10,lengend_text_size = 8,pos_leg = "right")

cowplot::plot_grid( p1, ncol=1,nrow=1,labels = LETTERS[1],align = "v")
















lambda <- BoxCox.lambda(producao)
fit    <- BoxCox(producao, lambda)

p3 <- autoplot.zoo(diff(fit)) + 
  geom_line(size = 0.25,alpha=1,color="black")+
  #geom_point(size = .3,alpha = 0.25,color="black") +
  labs(x = "Data", y = " Log do Preço") +
  #scale_color_manual(values = color_pal) +
  scale_x_date(date_breaks = "12 months",date_labels = "%Y")+
  axis.theme(x.angle = 45,vjust = 1,hjust = 1,axis.title.size.x = 12,axis.title.size.y = 12,tick.size = 10,lengend_title_size = 10,lengend_text_size = 8,pos_leg = "none")

pA1 <- ggAcf(as.zoo(diff(fit)),type = "correlation")+
  labs(x = "Lag", y = "FAC",title=NULL) +
  axis.theme(axis.title.size.x = 12,axis.title.size.y = 12,tick.size = 10)

pB1 <- ggAcf(as.zoo(diff(fit)),type = "partial")+
  labs(x = "Lag", y = "FACP",title=NULL) +
  axis.theme(axis.title.size.x = 12,axis.title.size.y = 12,tick.size = 10)

parcial1 <- cowplot::plot_grid(p3,cowplot::plot_grid(pA1, pB1,ncol=2,nrow=1,labels = LETTERS[2:3],align = "v"),labels = LETTERS[1],ncol=1,nrow=2)


parcial1

fit011  = arima(log(producao),order = c(0,1,1),)
fit111  = arima(log(producao),order = c(1,1,1))
fitauto = auto.arima(log(producao))

metrics_names = c("$\\phi_1$","Log-Lik","AIC")
data_res <- data.frame(Estimativas = c(fit011$coef,
                                       fit011$loglik,
                                       fit011$aic),
                       `S.E` = c(round(sqrt(fit011$var.coef),5),"","")
                       ,
                       row.names = metrics_names)


metrics_names = c("$\\phi_1$","Log-Lik","AIC")
data_res1 <- data.frame(Estimativas = c(fitauto$coef,
                                       fitauto$loglik,
                                       fitauto$aic),
                       `S.E` = c(round(sqrt(fitauto$var.coef),5),"","")
                       ,
                       row.names = metrics_names)


metrics_names = c("$\\phi_1$","$\\phi_2$","Log-Lik","AIC")
data_res3 <- data.frame(Estimativas = c(round(fit111$coef[1],4),
                                       round(fit111$coef[2],4),
                                       round(fit111$loglik,4),
                                       round(fit111$aic,4)),
                       `S.E`      = c(round(sqrt(diag(fit111$var.coef)[1]),5),
                                      round(sqrt(diag(fit111$var.coef)[2]),5),"",""),
                       row.names = metrics_names)


plot(fit011)






