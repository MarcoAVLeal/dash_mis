div(
    tabsetPanel(
      id = "hidden_tabs",
     # type = "hidden",
      tabPanel(title = p("Envios Msg Bitrix",style="color:#E4781C;text-align:center;font-weight:bold;font-size:14px"),
                   HTML("<div style='color:#E4781C;text-align:center;font-weight:bold;'><h2 style='color:#E4781C;text-align:center;font-weight:bold;'>ENVIO DE MENSAGENS POR WHATSSAPP</h2> </div>"),
                   fluidRow(column(width = 9,
                                   fluidRow(column(width = 4,
                                                   box(title = tags$p("TOTAL",style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                                                       headerBorder = TRUE,
                                                       width = 12,
                                                       collapsible = TRUE,
                                                       solidHeader = TRUE,
                                                       withSpinner(valueBoxOutput("msgbox_bitrix1"),proxy.height = "10px",size = 0.5),
                                                       valueBoxOutput("msgbox_bitrix2"),
                                                       valueBoxOutput("msgbox_bitrix3"))),
                                            column(width = 4,
                                                   box(title = tags$p("MÊS ATUAL",style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                                                       headerBorder = TRUE,
                                                       width = 12,
                                                       collapsible = TRUE,
                                                       solidHeader = TRUE,
                                                       withSpinner(valueBoxOutput("msgbox_bitrix4"),proxy.height = "10px",size = 0.5),
                                                       valueBoxOutput("msgbox_bitrix5"),
                                                       valueBoxOutput("msgbox_bitrix6"))),
                                            column(width = 4,
                                                   box(title = tags$p("HOJE",style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                                                       headerBorder = TRUE,
                                                       width = 12,
                                                       collapsible = TRUE,
                                                       solidHeader = TRUE,
                                                       withSpinner(valueBoxOutput("msgbox_bitrix7"),proxy.height = "10px",size = 0.5),
                                                       valueBoxOutput("msgbox_bitrix8"),
                                                       valueBoxOutput("msgbox_bitrix9")))
                                            ),
                                   fluidRow(column(width=6, plotlyOutput(outputId = "plot_envio_diario",height = 480)),
                                            column(width=6, plotlyOutput(outputId = "plot_envio_acumulado",height = 480))),
                                   fluidRow(column(width=6, plotlyOutput(outputId = "plot_envio_acumulado_mes",height = 480)),
                                            column(width=6 ))
                                   ),
                            column(width = 3,
                                   dateRangeInput(inputId = "data_consulta_msg",label = "Data", language = "pt-BR",start = lubridate::as_date("2022-01-01"), end = lubridate::today()),
                                   withSpinner(DTOutput(outputId = "tb_msg_bitrix"),hide.ui = FALSE,proxy.height = "10px",size = 0.5)
                                   )
                            )),
     tabPanel(title = p("Indicadores Bitrix",style="color:#E4781C;text-align:center;font-weight:bold;font-size:14px"), 
                   HTML("<div style='color:#273658;text-align:center;font-weight:bold;'><h1 style='color:#273658;text-align:center;font-weight:bold;'>INDICADORES DO FLUXO DE VENDAS DO BITRIX</h1> </div>"),
                   HTML('<hr style="color: purple;">'),
                   
                   fluidRow(column(width = 3,
                                   selectInput(inputId = "visao",label =  "VISAO:",
                                               c("Lojas" = "Nomes.e.sobrenomes",
                                                 "Regional" = "Lojas")),
                                   textOutput("count")
                   ),
                   column(width = 3, selectInput(inputId = "canal",label =  "CANAL:",
                                                 choices = c(sort(unique(df[,"Lojas"]))),
                                                 #choices = NULL,
                                                 multiple = TRUE)
                   ),
                   column(width = 3,
                          selectInput(inputId = "regional",label =  "REGIONAL:",
                                      choices = c(sort(unique(df[,"Regional"]))),
                                      #choices = NULL,
                                      multiple = TRUE)
                   ),
                   column(width = 3,
                          selectInput(inputId = "versao",label =  "Template:",
                                      c("V1","V2")))),
                   fluidRow(column(width = 3,
                                   withSpinner(valueBoxOutput("total_bitrix_vbox"),proxy.height = "10px",size = 0.5),
                                   HTML("<h3 style='color:#273658;text-align:center;font-weight:bold;'>PROPORÇÃO DE LEADS POR FASE</h1> "),
                                   div(style="text-align:center",withSpinner(tableOutput(outputId = "proporcao_leads_fase"),hide.ui = FALSE,proxy.height = "10px",size = 0.5)),
                                   valueBoxOutput("taxa_leads_bitrix")
                                   
                   ),
                   column(width = 9,
                          HTML("<h3 style='color:#273658;text-align:center;font-weight:bold;'>CONTATOS EM CADAS FASE</h1> "),
                          withSpinner(DTOutput(outputId = "table_fase_group"),hide.ui = FALSE,proxy.height = "10px",size = 0.5),
                          
                          style = "height:800px; overflow-y: scroll;overflow-x: scroll;")),
               dateInput(inputId = "data_referencia",label = "Data Referência",value = max(df1$`Modificado em`)),
                   fluidRow(column(width = 3,
                                   withSpinner(uiOutput("total_movimentacoes"),proxy.height = "10px",size = 0.5),
                                   div(style="text-align:center",withSpinner(tableOutput(outputId = "table_res_mov"),hide.ui = FALSE,proxy.height = "10px",size = 0.5))),
                            column(width = 9,
                                   HTML("<h3 style='color:#273658;text-align:center;font-weight:bold;'>MOVIMENTAÇÕES DE LEADS POR LOJA NA DATA REFERÊNCIA</h1> "),
                                   withSpinner(DTOutput(outputId = "table_movimentacoes"),hide.ui = FALSE,proxy.height = "10px",size = 0.5),
                                   style = "height:800px; overflow-y: scroll;overflow-x: scroll;"))
               #DTOutput(outputId = "results")
               ),
      tabPanel(title = "panel3", "Panel 3 content")
    
  ))