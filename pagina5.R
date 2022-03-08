div(
  tabsetPanel(
    id = "hidden_tabs",
    # type = "hidden",
    
    tabPanel(title = p("Indicadores Bitrix",style="color:#E4781C;text-align:center;font-weight:bold;font-size:14px"), 
             HTML("<div style='color:#273658;text-align:center;font-weight:bold;'><h1 style='color:#273658;text-align:center;font-weight:bold;'>PRODUÇÃO</h1> </div>"),
             HTML('<hr style="color: purple;">'),
             
             fluidRow(column(width = 3,
                            
                             
             ),
             column(width = 9,
                    
                    style = "overflow-y: scroll;overflow-x: scroll;")),
             HTML('<hr style="background-color: #E4781C;">'),
             fluidRow(column(width = 3),
                      column(width = 9 ,style = "height:800px; overflow-y: scroll;overflow-x: scroll;"))
             #DTOutput(outputId = "results")
    )
    
  ))