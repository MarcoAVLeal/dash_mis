df <- read.csv(file = "D:\\OneDrive - Crefaz Financiamentos e Investimentos\\dash_mis_dados\\motor\\motor-agregadado.csv")

df$DATACADASTRO <- lubridate::as_date(df$DATACADASTRO)
df$DATA_PAGAMENTO <- lubridate::as_date(df$DATA_PAGAMENTO)

df$ANO_CADASTRO <- lubridate::year(df$DATACADASTRO)
df$ANO_PAGAMENTO <- lubridate::year(df$DATA_PAGAMENTO)
df$MES_CADASTRO <- lubridate::month(df$DATACADASTRO)
df$MES_PAGAMENTO <- lubridate::month(df$DATA_PAGAMENTO)

df_pago <- df %>% dplyr::filter(STATUS_PRINCIPAL == "PAGO AO CLIENTE")


df_pago %>%  dplyr::group_by(DATA_PAGAMENTO,PRODUTO) %>% dplyr::summarise(Producao = sum(VLR_PRODUCAO),
                                                                                           Qntd     =sum(Qntd_Propostas)) %>%
  ggplot(aes(x = DATA_PAGAMENTO,y=Producao, color = PRODUTO)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "gam",se = TRUE)+
  geom_smooth(method = "loess",span = 0.5)



df_pago %>%  dplyr::group_by(DATA_PAGAMENTO) %>% dplyr::summarise(Producao = sum(VLR_PRODUCAO),
                                                                          Qntd     =sum(Qntd_Propostas)) %>%
  ggplot(aes(x = DATA_PAGAMENTO,y=Producao)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "gam",se = TRUE)



