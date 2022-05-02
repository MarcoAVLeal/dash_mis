fluidRow(column(width = 3,
                box(title = tags$p("Histórico Produção",style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                    headerBorder = TRUE,
                    width = 12,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    uiOutput("box_uiprod1")),
                uiOutput(outputId = "box_prod_dia")),
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
)


#   output$box_uiprod1 <- renderUI({
#     HTML(paste0('
#     <style>
# table, th, td {
#   border: 2px solid black;
#   padding: 5px;
# }
# table {
#   border-spacing: 15px;
# }
# </style>
#   <center>
# <table>
#  <tr >
#   <td valign="top">
#   
#            <div style = "background-color:#273658;">
#            <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px"> ',paste0("R$ ",format(prod_total[1,1],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'  </h1> 
#            <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">  Total  </h3>  
#            </div>
#            </td>
#            
#   <td valign="top">
#            <div style = "background-color:#273658;">
#            <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">',paste0(format(prod_total[1,2],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'</h1> 
#            <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">  Qntd. <br> Propostas  </h3>  
#            </div>
#            </td>
#            
#   <td valign="top">
#            <div style = "background-color:#273658;">
#            <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px"> ',paste0("R$ ",format(prod_total[1,1]  + projetado,scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'  </h1> 
#            <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">  Projetado </h3>  
#            </div>
#            </td>
#            </tr>
#     </table>
#            </center>'))
#     
#     
#     
#   })
#    
#    
#   output$box_uiprod2 <- renderUI({
#     HTML(paste0('
#     <style>
# table, th, td {
#   border: 2px solid black;
#   padding: 5px;
# }
# table {
#   border-spacing: 15px;
# }
# </style>
#   <center>
# <table>
#  <tr >
#   <td valign="top">
#   
#            <div style = "background-color:#273658;">
#            <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px"> ',paste0("R$ ",format(prod_2021[1,1],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'  </h1> 
#            <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">  Total  </h3>  
#            </div>
#            </td>
#            
#   <td valign="top">
#            <div style = "background-color:#273658;">
#            <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">',paste0(format(prod_2021[1,2],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'</h1> 
#            <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">  Qntd. <br> Propostas  </h3>  
#            </div>
#            </td>
#            
#   <td valign="top">
#            <div style = "background-color:#273658;">
#            <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px"> ',paste0("R$ ",format(prod_2021[1,1] + projetado,scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'  </h1> 
#            <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">  Projetado </h3>  
#            </div>
#            </td>
#            </tr>
#     </table>
#            </center>'))
#     
#     
#     
#   })
#   
#   
#   output$box_uiprod3 <- renderUI({
#     HTML(paste0('
#     <style>
# table, th, td {
#   border: 2px solid black;
#   padding: 5px;
# }
# table {
#   border-spacing: 15px;
# }
# </style>
#   <center>
# <table>
#  <tr >
#   <td valign="top">
#   
#            <div style = "background-color:#273658;">
#            <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px"> ',paste0("R$ ",format(prod_2022[1,1],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'  </h1> 
#            <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">  Total  </h3>  
#            </div>
#            </td>
#            
#   <td valign="top">
#            <div style = "background-color:#273658;">
#            <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">',paste0(format(prod_2022[1,2],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'</h1> 
#            <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">  Qntd. <br> Propostas  </h3>  
#            </div>
#            </td>
#            
#   <td valign="top">
#            <div style = "background-color:#273658;">
#            <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px"> ',paste0("R$ ",format(prod_2022[1,1] + projetado,scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'  </h1> 
#            <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">  Projetado </h3>  
#            </div>
#            </td>
#            </tr>
#     </table>
#            </center>'))
#     
#     
#     
#   })
#    
#   
#   output$box_uiprod4 <- renderUI({
#     HTML(paste0('
#     <style>
# table, th, td {
#   border: 2px solid black;
#   padding: 5px;
# }
# table {
#   border-spacing: 15px;
# }
# </style>
#   <center>
# <table>
#  <tr >
#   <td valign="top">
#   
#            <div style = "background-color:#273658;">
#            <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px"> ',paste0("R$ ",format(prod_mes_atual[1,1],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'  </h1> 
#            <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">  Total  </h3>  
#            </div>
#            </td>
#            
#   <td valign="top">
#            <div style = "background-color:#273658;">
#            <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">',paste0(format(prod_mes_atual[1,2],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'</h1> 
#            <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">  Qntd. <br> Propostas  </h3>  
#            </div>
#            </td>
#            
#   <td valign="top">
#            <div style = "background-color:#273658;">
#            <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px"> ',paste0("R$ ",format(prod_mes_atual[1,1] + projetado,scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'  </h1> 
#            <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">  Projetado </h3>  
#            </div>
#            </td>
#            </tr>
#     </table>
#            </center>'))
#     
#     
#     
#   })
#   
#   output$box_uiprod5 <- renderUI({
#     prod_dia <<- df_prod %>% dplyr::filter( DATA_PAGAMENTO >= input$data_producao[1] & DATA_PAGAMENTO <= input$data_producao[2])   %>% dplyr::summarise(Producao = sum(VLR_PRODUCAO),
#                                                                                                                              Qntd     =sum(Qntd_Propostas))
#     HTML(paste0('
#     <style>
# table, th, td {
#   border: 2px solid black;
#   padding: 5px;
# }
# table {
#   border-spacing: 15px;
# }
# </style>
#   <center>
# <table>
#  <tr >
#   <td valign="top">
#   
#            <div style = "background-color:#273658;">
#            <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px"> ',paste0("R$ ",format(prod_dia[1,1],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'  </h1> 
#            <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">  Total  </h3>  
#            </div>
#            </td>
#            
#   <td valign="top">
#            <div style = "background-color:#273658;">
#            <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">',paste0(format(prod_dia[1,2],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'</h1> 
#            <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">  Qntd. <br> Propostas  </h3>  
#            </div>
#            </td>
#            
#   <td valign="top">
#            <div style = "background-color:#273658;">
#            <h1 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px"> ',paste0("R$ ",format(prod_dia[1,1],scientific =FALSE,big.mark =".",nsmall = 2,decimal.mark = ",")),'  </h1> 
#            <h3 style="color:#E4781C;text-align:center;font-weight:bold;font-size:16px">  Projetado </h3>  
#            </div>
#            </td>
#            </tr>
#     </table>
#            </center>'))
#     
#     
#     
#   })
