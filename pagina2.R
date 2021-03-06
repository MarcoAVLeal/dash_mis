div( tabsetPanel(
  id = "hidden_tabs",
  # type = "hidden",
  tabPanel(title = p("Envios Msg Bitrix",style="color:#E4781C;text-align:center;font-weight:bold;font-size:14px"),
           HTML("<div style='color:#E4781C;text-align:center;font-weight:bold;'><h2 style='color:#E4781C;text-align:center;font-weight:bold;'>ENVIO DE MENSAGENS POR WHATSSAPP</h2> </div>"),
           
           fluidRow(column(width = 12,
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
           )),
           fluidRow(
                    column(width = 12,
                           withSpinner(DTOutput(outputId = "tb_msg_bitrix"),hide.ui = FALSE,proxy.height = "10px",size = 0.5)
                    )))
  
))