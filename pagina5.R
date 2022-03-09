div(
  tabsetPanel(
    id = "hidden_tabs",
    # type = "hidden",
    
    tabPanel(title = p("Produção Geral Crefaz",style="color:#E4781C;text-align:center;font-weight:bold;font-size:14px"), 
             HTML('<hr style="color: purple;">'),
             HTML("<h3 style='color:#273658;text-align:center;font-weight:bold;'>PRODUÇÃO GERAL CREFAZ</h1> "),
             fluidRow(column(width = 3,
                             box(title = tags$p("Histórico Produção",style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                                 headerBorder = TRUE,
                                 width = 12,
                                 collapsible = TRUE,
                                 solidHeader = TRUE,
                                 uiOutput("box_uiprod1")
                                 # valueBoxOutput("box_prod1"),
                                 # valueBoxOutput("box_prod2"),
                                 # valueBoxOutput("box_prod3")
                                 )),
                      column(width = 3,
                             box(title = tags$p("Desde 2021",style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                                 headerBorder = TRUE,
                                 width = 12,
                                 collapsible = TRUE,
                                 solidHeader = TRUE,
                                 valueBoxOutput("box_prod4"),
                                 valueBoxOutput("box_prod5"),
                                 valueBoxOutput("box_prod6"))),
                      column(width = 3,
                             box(title = tags$p("Produção 2022",style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                                 headerBorder = TRUE,
                                 width = 12,
                                 collapsible = TRUE,
                                 solidHeader = TRUE,
                                 valueBoxOutput("box_prod7"),
                                 valueBoxOutput("box_prod8"),
                                 valueBoxOutput("box_prod9"))),
                      column(width = 3,
                             box(title = tags$p("Mês Atual",style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                                 headerBorder = TRUE,
                                 width = 12,
                                 collapsible = TRUE,
                                 solidHeader = TRUE,
                                 valueBoxOutput("box_prod10"),
                                 valueBoxOutput("box_prod11"),
                                 valueBoxOutput("box_prod12")))
             ),
             fluidRow(plotOutput(outputId = "serie_prod",height = 480))
    )
    
  ))