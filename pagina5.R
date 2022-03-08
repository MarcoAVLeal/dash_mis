div(
  tabsetPanel(
    id = "hidden_tabs",
    # type = "hidden",
    
    tabPanel(title = p("Produção Geral Crefaz",style="color:#E4781C;text-align:center;font-weight:bold;font-size:14px"), 
             HTML('<hr style="color: purple;">'),
             HTML("<h3 style='color:#273658;text-align:center;font-weight:bold;'>PRODUÇÃO GERAL CREFAZ</h1> "),
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