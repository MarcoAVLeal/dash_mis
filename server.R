if( stringr::str_detect(string = getwd(),pattern = "marco")){
  
  source(file = "librarys.R")
  path_pg1 <- "pagina1.R"
  path_pg2 <- "pagina2.R"
  
}else{
    source(file = "librarys.R")
  }


espaco_html <<- function(n=6){
    
    
    return(HTML( rep("<br>",n)))
    
    
}

credentials[["mleal"]]$pass == "mleal"


shinyServer(function(input, output, session) {
    # input$username <- "mleal"
    # input$password <- "mleal"
    ######################################               ###########################################################
    ###################################### Menu de login ###########################################################
    ######################################               ###########################################################
    USER <- reactiveValues(Logged = FALSE)
    observeEvent(input$login, {
        if (isTRUE(as.character(credentials[[input$username]]$pass)==as.character(input$password))){
            USER$Logged <- TRUE
            
            #updateNavbarPage(session = session,inputId = "nav_bar",selected = 1)
            show_toast(
                title = "Entrando ... ",
                text = "",
                type = "succes",
                width = "auto",
                position = "bottom"
            )
            
        } else {
            #showNotification(fluidRow(icon("exclamation-triangle"),"Usuário ou Senha Invalidos"))
            show_toast(
                title = "Usuário e ou Senha Invalidos.",
                text = "Verifique!!!",
                type = "error",
                width = "auto",
                position = "bottom"
            )
        }
    })
    
    observeEvent(input$logout, {
        if (isTRUE(as.character(credentials[[input$username]]$pass)==as.character(input$password))){
            USER$Logged <- FALSE
            show_toast(
                title = "",
                text = "Saindo ... ",
                timer=500,
                type = "default",
                width = "200px",
                position = "bottom"
            )
            session$reload()
        } 
    })    
    
  
    
      
######################################               ###########################################################
###################################### Tela de Usuário ###########################################################
######################################               ###########################################################
 observe({
     
     if( !isTRUE(USER$Logged)){
         
         

         output$user <- renderUser({
             dashboardUser(
                 name = "Username", 
                 image = "https://www.pinclipart.com/picdir/big/157-1578186_user-profile-default-image-png-clipart.png", 
                 title = "Departamento",
                 subtitle = "Cargo", 
                 footer = p("Sem Usuário Logado", class = "text-center"),
                 tags$div(HTML('<b style = "padding-left:25px;color:#000000;font-size:20px">PAINEL DO USUÁRIO </b>')),
                 fluidRow(style = "text-align:center;",
                     div(textInput("username",strong("Usuário : "),width=100),style="padding-left:0px;"),
                     br(style="display: block;content: \"\"; margin-top: 0px;"),
                     div( passwordInput("password", strong("Senha:"),width = 100),style="padding-left:0px;text-align:center"),
                     fluidRow(
                         div(actionBttn(
                             inputId = "login",
                             label = "Entrar",
                             color = "primary",
                             style = "jelly",
                             block = FALSE,size = "xs"),
                             style = sprintf("left: 0px;text-align:center;padding-left:0px;"))),
                     dashboardUserItem(
                         width = 6,
                         socialButton(
                             href = "https://dropbox.com",
                             icon = icon("dropbox")
                         )
                     ),
                     dashboardUserItem(
                         width = 6,
                         socialButton(
                             href = "https://github.com",
                             icon = icon("github")
                         )
                     )
                 )
             )
         })
         
         
         
         }else{
             output$user <- renderUser({
                 dashboardUser(
                     name = credentials[[input$username]]$"name", 
                     image = credentials[[input$username]]$"img", 
                     title = credentials[[input$username]]$"departamento",
                     subtitle = credentials[[input$username]]$"cargo", 
                     footer = p("Logado", class = "text-center"),
                     tags$div(HTML('<b style = "padding-left:25px;color:#000000;font-size:20px">PAINEL DO USUÁRIO </b>')),
                     fluidRow(style = "text-align:center;",
                              actionBttn(
                                  inputId = "logout",
                                  label = "Sair",
                                  color = "primary",
                                  style = "jelly",
                                  block = FALSE,
                                  icon = icon("sign-out-alt")),
                              dashboardUserItem(
                                  width = 6,
                                  socialButton(
                                      href = "https://dropbox.com",
                                      icon = icon("dropbox")
                                  )
                              ),
                              dashboardUserItem(
                                  width = 6,
                                  socialButton(
                                      href = "https://github.com",
                                      icon = icon("github")
                                  )
                              )
                     )
                 )
             })
             
         
     }
     
     
 })   
    ######################################               ###########################################################
    ###################################### Renderizando Paginas ###########################################################
    ######################################               ########################################################### 
    observe({
        
        if( !isTRUE(USER$Logged)){
            
            
       }else{
            output$page1 <- renderUI({
                
                source(file = path_pg1,encoding = "utf-8")
                #source(file = "source_server_pagina1.R",encoding = "utf-8")
                
            })
            
            
            output$page2 <- renderUI({
                
                source(file = path_pg2,encoding = "utf-8")
                
            })
            
           }
    
})  

    ######################################               ###########################################################
    ###################################### Renderiznado Info Whatsapp ###########################################################
    ######################################               ###########################################################  
    
    onedrive_url <- "https://crefaz-my.sharepoint.com/:x:/g/personal/gestaodedados4_crefaz_onmicrosoft_com/Ea1IGOUCSa1Mjlev_QvrNLAB4I_qcKHjWy908-RxDbWPcQ?download=1"
    x <- read_url_csv(onedrive_url)
    
    #x <- read.csv(file = 'dados\\2022_02.csv') 
    output$msgbox_bitrix1 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("TOTAL", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(x[1,"total"], style = "font-size:50%;color:#E4781C;font-weight:bold") , 
                               width = 4, color = "navy")
    })
    
    output$msgbox_bitrix2 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("PENDENTE", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(x[1,"pending"], style = "font-size:50%;color:#E4781C;font-weight:bold") , 
                               width = 4 , color = "navy")
    })
    
    output$msgbox_bitrix3 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("ENVIADO", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(x[1,"sent"], style = "font-size:50%;color:#E4781C;font-weight:bold") , 
                               width = 4, color = "navy")
    })
    output$msgbox_bitrix4 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("TOTAL", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(x[2,"total"], style = "font-size:50%;color:#E4781C;font-weight:bold") , 
                               width = 4, color = "navy")
    })
    
    output$msgbox_bitrix5 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("PENDENTE", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(x[2,"pending"], style = "font-size:50%;color:#E4781C;font-weight:bold") , 
                               width = 4,icon = tags$i(class = "fas fa-phone", style="font-size: 12px"), color = "navy")
    })
    
    output$msgbox_bitrix6 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("ENVIADO", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(x[2,"sent"], style = "font-size:50%;color:#E4781C;font-weight:bold") , 
                               width = 4, color = "navy")
    })
    
    output$msgbox_bitrix7 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("TOTAL", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(x[3,"total"], style = "font-size:50%;color:#E4781C;font-weight:bold") , 
                               width = 4, color = "navy")
    })
    
    output$msgbox_bitrix8 <- renderValueBox({
      shinydashboard::valueBox(subtitle =tags$p("PENDENTE", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(x[3,"pending"], style = "font-size:50%;color:#E4781C;font-weight:bold") , 
                               width = 4, color = "navy")
    })
    
    output$msgbox_bitrix9 <- renderValueBox({
      shinydashboard::valueBox(subtitle = tags$p("ENVIADO", style = "font-size:100%;color:#E4781C;font-weight:bold;"),
                               value =tags$p(x[3,"sent"], style = "font-size:50%;color:#E4781C;font-weight:bold") , 
                               width = 4, color = "navy")
    })
    
    
    
    ######################################               ###########################################################
    ###################################### Informações Bitrix ###########################################################
    ######################################               ###########################################################
    
    
    
    dataset <- reactive({
      data = df1
      print(input$canal)
      if (length(input$canal)){
        f1 =  data$Lojas %in% input$canal
        data = data[f1,]
      }
      else {
        data
      }
      
      if (length(input$regional)){
        
        f2 = data$REGIONAL %in% input$regional
        data = data[f2,]
        #updateSelectInput(session = session,inputId = "canal",choices = c(sort(unique(data[,"Lojas"]))),selected = input$canal)
      }
      else {
        data
      }
      data
      
      
      
      
      # updateSelectInput(session = session,inputId = "regional",choices = c(sort(unique(data[,"REGIONAL"]))),selected = input$regional)
      
      output$total_bitrix_vbox <- renderUI({
        p(nrow(data),br(), " Total",style="color:#273658;text-align:center;font-weight:bold;font-size:30px")
      })
      
      group_count <- as.character(input$visao)
      print(input$visao)
      Total <- sum  
      #as.character(input$visao)
      table <- table(data[,c(group_count)],data$`Fase do negocio`)  %>% as.data.frame
      table <- reshape2::dcast(table, Var1 ~ Var2, value.var="Freq") 
      
      #table <- table %>% mutate("Taxa de Pagos" = paste0(round((PAGO/Total)*100,2)," %"))
      colnames(table)[1] <- c("Usuário")
      
      table_fases_group <- matrix(data = 0,nrow = nrow(table),ncol = 7) %>% as.data.frame
      colnames(table_fases_group) <- c("Usuário", "NOVO","PROSPECTANDO","NEGOCIANDO","EM ANÁLISE", "PAGO","DESAFIO")
      for(name in colnames(table)){
        
        table_fases_group[name] <- table[name]
        
        
      }
      
      
      output$taxa_leads_bitrix <- renderUI({
        leads_tratados         <<- data %>% filter(`Fase do negocio` != "NOVO") %>% nrow
        pagos                  <<- data %>% filter(`Fase do negocio` == "PAGO") %>% nrow
        taxa_tratados          <<- paste0(round((pagos/leads_tratados)*100,2)," %")
        p(taxa_tratados,br(), " Taxa de Leads Tratados",style="color:#273658;text-align:center;font-weight:bold;font-size:30px")
        
      })
      
      
      
      
      output$proporcao_leads_fase <- renderTable({
        
        resumo_fases           <- data.frame(Fase = c("NOVO","PROSPECTANDO","NEGOCIANDO","EM ANÁLISE","PAGO","DESAFIO"), Qntd. = 0, "QntdP" = 0)
        
        aux1                   <- table(data$`Fase do negocio`) %>% as.data.frame %>%
          mutate(Var1 = as.character(Var1)) %>%
          mutate(Var1 = if_else(Var1 == "PAGO", "PAGO",Var1))
        
        aux2                   <- prop.table(table(data$`Fase do negocio`)) %>%
          as.data.frame %>%
          mutate(Var1 = as.character(Var1)) %>%
          mutate(Var1 = if_else(Var1 == "PAGO", "PAGO",Var1)) %>%
          mutate(Freq = paste0(round(Freq * 100,2),"%"))
        
        
        for(fase in aux1$Var1){
          
          resumo_fases[resumo_fases$Fase == fase,"Qntd."] <- aux1[aux1$Var1 == fase,"Freq"]
          resumo_fases[resumo_fases$Fase == fase,"QntdP"] <- aux2[aux2$Var1 == fase,"Freq"]
          
        }
        
        colnames(resumo_fases) <- c("Fase","Qntd.","%")
        resumo_fases <- resumo_fases  %>%
          mutate(prop = Qntd. / sum(resumo_fases$Qntd.) *100) %>%
          mutate(ypos = cumsum(prop)- 0.5*prop ) %>%
          dplyr::select("Fase","Qntd.","%")
        
        resumo_fases$Qntd. <- round(resumo_fases$Qntd.,0)
        resumo_fases
      })
      
      
      
      
      table_fases_group <- table_fases_group %>% mutate("Total" =  PROSPECTANDO + NEGOCIANDO + `EM ANÁLISE` + PAGO + DESAFIO)
      table_fases_group <- table_fases_group %>% mutate("Taxa de Pagos" = ifelse(is.nan(PAGO/Total),"0 %",paste0(round((PAGO/Total)*100,2)," %"))) 
      table_fases_group
      
      
      
      
    })
    
    
    
    
    output$table_fase_group <- renderDT(
      dataset(),
      extensions = 'Buttons',server = FALSE,
      options = list(
        lengthChange = FALSE,
        # scrollX=TRUE,
        # lengthMenu = c(5,10,15),
        paging = TRUE,
        searching = TRUE,
        # fixedColumns = TRUE,
        # autoWidth = TRUE,
        # ordering = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel','pdf')
      ) ,
      class = "display"
    )
    
    
    dataset1 <- reactive({
      data = df1
      print(input$canal)
      if (length(input$canal)){
        f1 =  data$Lojas %in% input$canal
        data = data[f1,]
      }
      else {
        data
      }
      
      if (length(input$regional)){
        
        f2 = data$REGIONAL %in% input$regional
        data = data[f2,]
        #updateSelectInput(session = session,inputId = "canal",choices = c(sort(unique(data[,"Lojas"]))),selected = input$canal)
      }
      else {
        data
      }
      data
      
      
      Total <- sum
      dados <- data %>% dplyr::select(`Data criado`,`Data prospectar`, `Data fechado`, `Data analisar`, `Data negociar`,ID_LEADS) %>%
        reshape2::melt(id.vars = "ID_LEADS",measure.vars = c("Data criado","Data prospectar","Data fechado", "Data analisar", "Data negociar")) %>%
        mutate("Atributo"      = ifelse(variable == "Data criado","Criou Lead",
                                        ifelse(variable == "Data prospectar","Moveu para prospectando",
                                               ifelse(variable == "Data negociar","Moveu para negociando",
                                                      ifelse(variable == "Data analisar","Moveu para em análise",
                                                             ifelse(variable == "Data fechado","Fechou lead(Desafio ou pago)",variable))))))
      
      
      completedados <- dados[complete.cases(dados),]
      dados               <- left_join(completedados,df1,by = c("ID_LEADS"))
      dados   <- dados %>% dplyr::mutate("Atributo" = ifelse(Atributo == "Fechou lead(Desafio ou pago)",paste0("Moveu para ",`Fase do negocio`),Atributo))
      
      
      dados               <- dados %>% filter(value == input$data_referencia)
      total_movimentacoes <- dados %>%  nrow
      
      
      
      output$total_movimentacoes <- renderUI({
        p(total_movimentacoes,br(), "Tota de Movimentações na Data Referência",style="color:#273658;text-align:center;font-weight:bold;font-size:30px")
      })
      
      
      output$table_res_mov <- renderTable({
        
        table_movimentacoes <- table(dados$Atributo) %>% as.data.frame
        
        if(nrow(table_movimentacoes) > 0){
          
          resumo_movimentacoes           <- data.frame(mov = c("Criou Lead","Moveu para prospectando","Moveu para negociando","Moveu para em análise","Moveu para PAGO","Moveu para DESAFIO"), Qntd. = 0)
          
          
          
          
          for(movimentacao in table_movimentacoes$Var1){
            #print(fase)
            resumo_movimentacoes[resumo_movimentacoes$mov == movimentacao,"Qntd."] <- table_movimentacoes[table_movimentacoes$Var1 == movimentacao,"Freq"]
            
          }
          
          resumo_movimentacoes <- resumo_movimentacoes %>% mutate("%" = paste0(round( (`Qntd.`/total_movimentacoes)*100,2)," %"))
          
          
          
          colnames(resumo_movimentacoes) <- c("Movimentação","Qtnd.","%")
          resumo_movimentacoes
          
        }else{
          
          resumo_movimentacoes           <- data.frame(mov = c("Criou Lead","Moveu para prospectando","Moveu para negociando","Moveu para em análise","Moveu para PAGO","Moveu para DESAFIO"), Qntd. = 0,"%" = 0)
          colnames(resumo_movimentacoes) <- c("Movimentação","Qtnd.","%")
          resumo_movimentacoes
        }
        
        
        
      })
      
      Total <- sum
      
      
      table_movimentacoes <- table(dados[,input$visao],dados$Atributo) %>% as.data.frame
      
      if(nrow(table_movimentacoes) > 0){
        table_movimentacoes <- reshape2::dcast(table_movimentacoes, Var1 ~ Var2, value.var="Freq")
        
        
        
        table_fases_group <- matrix(data = 0,nrow = nrow(table_movimentacoes),ncol = 7) %>% as.data.frame
        colnames(table_fases_group) <- c("Var1", "Criou Lead","Moveu para prospectando", "Moveu para negociando","Moveu para em análise","Moveu para PAGO","Moveu para DESAFIO")
        for(name in colnames(table_movimentacoes)){
          
          table_fases_group[name] <- table_movimentacoes[name]
          
          
        }
        
        
        
        
        colnames(table_fases_group)[1] <- c("Lojas")
        table_fases_group <- table_fases_group  %>% mutate("Total" = `Criou Lead` + `Moveu para prospectando` + `Moveu para negociando` + `Moveu para em análise` + `Moveu para PAGO` + `Moveu para DESAFIO`)
        table_fases_group$Lojas <-  str_replace(string = table_fases_group$Lojas,pattern = "Loja CFZ ",replacement = "")
        table_fases_group 
        
        
      }else{
        
        
        table_fases_group <- matrix(data = 0,nrow = 1,ncol = 7) %>% as.data.frame
        colnames(table_fases_group) <- c("Lojas","Criou Lead","Moveu para prospectando", "Moveu para negociando","Moveu para em análise","Moveu para PAGO","Moveu para DESAFIO")
        table_fases_group
        
      }
      
    })
    
    output$table_movimentacoes <- renderDT(
      dataset1(),
      extensions = 'Buttons',server = FALSE,
      options = list(
        lengthChange = FALSE,
        # scrollX=TRUE,
        # lengthMenu = c(5,10,15),
        paging = TRUE,
        searching = TRUE,
        # fixedColumns = TRUE,
        # autoWidth = TRUE,
        # ordering = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel','pdf')
      ) ,
      class = "display"
    )

  
})