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
                                 uiOutput("box_uiprod1"))),
                      column(width = 3,
                             box(title = tags$p("Desde 2021",style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                                 headerBorder = TRUE,
                                 width = 12,
                                 collapsible = TRUE,
                                 solidHeader = TRUE,
                                 uiOutput("box_uiprod2"))),
                      column(width = 3,
                             box(title = tags$p("Produção 2022",style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                                 headerBorder = TRUE,
                                 width = 12,
                                 collapsible = TRUE,
                                 solidHeader = TRUE,
                                 uiOutput("box_uiprod3"))),
                      column(width = 3,
                             box(title = tags$p("Mês Atual",style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                                 headerBorder = TRUE,
                                 width = 12,
                                 collapsible = TRUE,
                                 solidHeader = TRUE,
                                 uiOutput("box_uiprod4")))
             ),
             fluidRow(box(title = tags$p("Histórico diário de produção",style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                          headerBorder = TRUE,
                          width = 12,
                          collapsible = TRUE,
                          collapsed = FALSE,
                          solidHeader = TRUE,
                          plotOutput(outputId = "serie_prod",height = 560)
             )),
             fluidRow(
               tabsetPanel(
                 id = "tabset_series",
                 # type = "hidden",
                 
                 tabPanel(title = p("Correlograma e Diferencição",style="color:#E4781C;text-align:center;font-weight:bold;font-size:14px"), 
                          box(title = tags$p("Série Produção",style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                              headerBorder = TRUE,
                              width = 12,
                              collapsible = TRUE,
                              collapsed = TRUE,
                              solidHeader = TRUE,
                              plotOutput(outputId = "serie_diff_correlogram",height = 760)
                          ),
                          box(title = tags$p("Série Log Produção",style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                              headerBorder = TRUE,
                              width = 12,
                              collapsible = TRUE,
                              collapsed = TRUE,
                              solidHeader = TRUE,
                              plotOutput(outputId = "serie_log_diff_correlogram",height = 760)
                          )
                 ),
                 tabPanel(title = p("Box-Plot",style="color:#E4781C;text-align:center;font-weight:bold;font-size:14px"), 
                          fluidRow(
                            box(title = tags$p("Box-plot",style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                                headerBorder = TRUE,
                                width = 12,
                                collapsible = TRUE,
                                collapsed = TRUE,
                                solidHeader = TRUE,
                                plotOutput(outputId = "boxplot_season",height = 480)
                            )))
                 
               )
              )
             
    )
    
  ))