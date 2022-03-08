div(
    tabsetPanel(
      id = "hidden_tabs",
     # type = "hidden",
      
     tabPanel(title = p("Indicadores Bitrix",style="color:#E4781C;text-align:center;font-weight:bold;font-size:14px"), 
                   HTML("<div style='color:#273658;text-align:center;font-weight:bold;'><h1 style='color:#273658;text-align:center;font-weight:bold;'>INDICADORES DO FLUXO DE VENDAS DO BITRIX</h1> </div>"),
                   HTML('<hr style="color: purple;">'),
                   
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
               )
    
  ))