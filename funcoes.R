library(fitdistrplus)
library(ggplot2)
library(gridExtra)
library(knitr)
library(tidyverse)
library(kableExtra)

axis.theme <- function(x.angle = 0,
                       vjust=0,
                       hjust=0.5,
                       pos_leg="top",
                       tick.size = 12,
                       axis.title.size.x = 12,
                       axis.title.size.y = 12,
                       lengend_title_size = 10,
                       lengend_text_size = 8,
                       title_size = 16){
  
  
  theme_bw()  +
    theme(
      axis.text.x = element_text(angle = x.angle,face = "bold",size = tick.size,hjust=hjust, vjust=vjust),
      axis.text.y = element_text(angle = 0,face = "bold",size = tick.size),
      legend.background = element_rect(fill = "transparent", colour = NA,size = 2),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      axis.title.x = element_text(colour = "black",size = axis.title.size.x,face = "bold"),
      axis.title.y = element_text(colour = "black",size = axis.title.size.y,face = "bold"),
      legend.title = element_text(colour = "black",size = lengend_title_size),
      legend.position = pos_leg,
      legend.text = element_text(colour = "black",size = lengend_text_size,face = "bold"),
      panel.grid = element_line(linetype="dashed"),
      panel.grid.major = element_line(colour = "gray"),
      title =element_text(size=title_size, face='bold',hjust = 0.5),
      plot.title = element_text(hjust = 0.5),
      axis.title = element_text(color="#000000", face="bold", size=textsize,lineheight = 2))
  
}

textsize <- 10
textsize2 <- 18

mytheme <- theme(
                 axis.title = element_text(size = textsize),
                 axis.text = element_text(size = textsize),
                 legend.title = element_text(size = textsize),
                 legend.text = element_text(size = textsize))

axis_theme <- theme_bw()  +
  theme(
    axis.text.x = element_text(angle = 0,face = "bold",size = textsize),
    axis.text.y = element_text(angle = 0,face = "bold",size = textsize),
    legend.background = element_rect(fill = "transparent", colour = NA,size = 2),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.title.x = element_text(colour = "black",size = textsize,face = "bold"),
    axis.title.y = element_text(colour = "black",size = textsize,face = "bold"),
    legend.title = element_text(colour = "black",size = 10),
    legend.position = "right",
    legend.text = element_text(colour = "black",size = 8,face = "bold"),
    panel.grid = element_line(linetype="dashed"),
    panel.grid.major = element_line(colour = "gray"),
    title =element_text(size=textsize, face='bold',hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(color="#000000", face="bold", size=textsize,lineheight = 2))


ci.compute<-function(means,mse,n,a,c.lev=95,txt,diff=FALSE){

statistic.val=qt(p=.5+c.lev/200, df = (n*a)-a )

if(isTRUE(diff)){
confidence.interval=round(means+(c(-1,1)*statistic.val*sqrt((2*mse)/n)),4)
}else{confidence.interval=round(means+(c(-1,1)*statistic.val*sqrt(mse/n)),4)}

return(confidence.interval)



}


plot_design <- function(data){
  
  
  library(ggplot2)
  ggplot(data = data, aes(x=data[,1], y=data[,2]))+
    geom_point(alpha=.5)+labs(x = "Tratamentos", y = "Resposta")+
    ggtitle("Gráfico contendo o comportamento do Experimento") +
    theme(plot.title = element_text(hjust = 0.5))+
    stat_summary(fun.y=mean, geom="line",aes(y = data[,2],group=1),
                 colour="red",lwd=0.8,alpha=.6)+
    stat_summary(fun.y=mean, geom="point",colour="blue",size=3,alpha=.8)
    
  
}


confidence.interval <- function(modelo){
  library(agricolae)
  library(ggplot2)
  aov      <- aov(modelo)
  anova    <- anova(modelo)
  hsd.test <- HSD.test(y = aov, trt = "V1",console =  F,group = T)
  mse      <- anova$`Mean Sq`[2]
  means    <- sort(hsd.test$means[,1],decreasing = T)
  a        <- length(unique(modelo$model[,(attr(modelo$terms,which = "dataClasses")=="factor")]))
  n        <- length(modelo$model[,(attr(modelo$terms,which = "dataClasses")=="factor")])/a
   # unique(modelo$model[,(attr(modelo$terms,which = "dataClasses")=="factor")])
  
  intervalos <- sapply(X = means,ci.compute,mse,n,a)
  intervalos <- as.data.frame(t(rbind(means,intervalos)),rownames=T)
  label   <- rownames(HSD.test(y = aov, trt = "V1",console =  F,group = T)$groups)
  colnames(intervalos) <- c("means","LI","LS")
  row.names(intervalos) <- label
  require(ggplot2)

  x_axis  <- sort(as.numeric(label))
  plot <- ggplot() +
    geom_point(size = 1.8,aes(x = x_axis,y = means )) +
    geom_errorbar(aes(x = x_axis,
                      ymax = intervalos$LS,
                      ymin = intervalos$LI))+
    labs(x = "Tratamentos", y = "Resposta")+
    ggtitle("Gráfico Comparativo dos Intervalos de Confianças dos Tratamentos") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(face="bold", 
                                     size=10, angle=45),
          axis.text.y = element_text(face="bold", 
                                     size=10, angle=45))+ 
          scale_x_discrete(limits=x_axis, labels=label)
  #plot
  
  
  return(list(intervalos,plot))
  
}

qq_gplot <- function(data){

fit    <- fitdist(data[["values"]], distr = "norm")
empir  <- ecdf(data[["values"]])
Fempir <- empir(data[["values"]])
Fteori <- pnorm(data[["values"]], fit$estimate[1],fit$estimate[2])

Qteori <-sapply(Fteori, function(x) qnorm(x, fit$estimate[1],fit$estimate[2]))
Qempir <-sapply(Fempir, function(x) qnorm(x, fit$estimate[1],fit$estimate[2]))

ggplot()+
geom_point(aes(x=Qteori, y=Qempir),size=2)+labs(x = "Distribuição Teórica", y = "Distribuição Empírica")+
xlim(range(data[["values"]])+c(-0,+0))+
ylim(range(data[["values"]])+c(-0,+0))+
ggtitle(paste0("Gráfico de Dispersão QQplot ",data[["label"]]))+
geom_abline(slope = 1,intercept = 0,col="red",size=1.20)+
theme(plot.title = element_text(hjust = 0.5))
}

fit_gplot <- function(data,axis=c(1,2),grau){
  ggplot(data = data, aes(x = as.numeric(data[,axis[1]]),y = data[,axis[2]]))+
    geom_point(alpha=.5)+
    labs(x = toupper(colnames(data)[1]), y = toupper(colnames(data)[2]))+
    ggtitle(paste0("Regressão Polinomial de ordem ", grau))+
    stat_smooth(method = "lm", formula = y ~ poly(x,grau),col="red",se = F)+
    theme(plot.title = element_text(hjust = 0.5))
}



summary_descritive <-function(dados){
  library(tidyverse)
  library(knitr)
  library(kableExtra)
  
  min    <- min(dados)
  q1     <- quantile(dados)[2]
  median <- quantile(dados)[3]
  q3     <- quantile(dados)[4]
  mean   <- mean(dados)
  max    <- max(dados)
  
  data <- data.frame(Min.=min,"1st Qu."=q1,Median=median,Mean=mean,"3rd Qu."=q3, Max=max,row.names = "Value")
  
  data  %>%
    kable(booktabs=T,caption = "Tabela contendo medidas descritivas",digits = 4,col.names = c("Min.","1st Qu.","Median","Mean","3rd Qu","Max."),align = "c")%>%
    kable_styling(full_width = F, latex_options = "hold_position") %>%
    row_spec(0, align = "c",bold=T ) %>%
    column_spec(1, bold = T)
}


boxggplot <- function(data,title=" dos Resíduos",xtitle="Resíduos",ytitle="Valores"){
  library(ggplot2)
  outliers<-0L
  #outliers <- as.numeric(boxplot.stats(data)$out)
  outliers <- data[IsOutlier(data = data)]
  if(length(outliers)>0){
    pos <- which(data %in% outliers)
    out <- data.frame(Pos=pos,outliers=outliers)
  }else{
    out=data
    pos <- NA
    outliers <-NA
    label <- NA
  }
  
 plot <- ggplot(data = as.data.frame(data),aes(x=xtitle, y=data))+
    geom_boxplot(outlier.colour="black",alpha=0.8, outlier.shape=16,outlier.size=2, notch=FALSE)+
   
    labs(x = NULL,y = ytitle) +
    ggtitle(paste0("Box-plot",title))+
    axis_theme
   return(list(plot=plot,outliers=out)) 
  
}

r_standard <- function(modelo){
  
 resid(modelo)/summary(modelo)$sigma
}


r_student <- function(modelo){
  X=model.matrix(modelo)  
  h=hat(x = X,intercept = T)
 resid(modelo)/(summary(modelo)$sigma*(1-h)^.5)
}

r_student_external <- function(modelo){
X=model.matrix(modelo)  
p=length(coef(modelo))
n=length(modelo$residuals)
rstudent(modelo)*((n-p-1)/(n-p-rstudent(modelo)))^.5

}



envelope=function(modelo){
  dados=na.omit(modelo$data)
  nsim=100
  n=modelo$df.null+1
  r1=sort(rstandard(modelo,type='deviance'))
  m1=matrix(0,nrow=n,ncol=nsim)
  a2=simulate(modelo,nsim=nsim)
  
  for (i in 1:nsim){
    dados$y=a2[,i]
    aj=update(modelo,y~.,data=dados)
    m1[,i]=sort(rstandard(aj,type='deviance'))}
  
  li=apply(m1,1,quantile,0.025)
  m=apply(m1,1,quantile,0.5)
  ls=apply(m1,1,quantile,0.975)
  
  quantis=qnorm((1:n-0.5)/n)
  
  
  ggplot()+
    geom_point(aes(x=quantis, y=r1), alpha=.5)+
    labs(x = 'Percentil da N(0,1)', y = 'Resíduos')+
    ggtitle('Gráfico Normal de Probabilidades com Envelope')+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_line(aes(x = quantis,y=li,type="l"),size=0.5)+
    geom_line(aes(x = quantis,y=m,type="l"),size=.5,colour="red")+
    geom_line(aes(x = quantis,y=ls,type="l"),size=.5)
  
  
  
}


residual_qqnorm <- function(data,title=" dos Resíduos"){
  
  ggplot(size=.75)+
    stat_qq(aes(sample=data), alpha=.5)+
    stat_qq_line(aes(sample=data), size=.75,colour="red")+
    labs(x = "Quantís Teóricos", y = "Quantís Empíricos")+
    ggtitle(paste0("QQnorm",title))+
    axis_theme
}



plot_resvfit <- function(x,y,labelx="Eixo X",labely="Eixo Y", title="Gráfico de Dispersão"){
 
  ggplot()+
    geom_point(aes(x=x, y=y),alpha=.5)+labs(x = labelx, y = labely)+
    xlim(range(x)+c(-0,+0))+
    ylim(range(y)+c(-0,+0))+
    geom_abline(slope = 0,intercept = 0,col="red",size=.75, alpha=.5)+
    ggtitle(paste0(title))+
    axis_theme
  
  
}





kable_data <- function(data,cap,foot=" ",align="c"){
  library(kableExtra)
  
  data %>%
    kable(booktabs=T,caption = cap,align = align) %>%
    add_footnote(foot) %>%
    kable_styling(full_width = F, latex_options = "hold_position") %>%
    row_spec(0, align = align,bold=T ) %>%
    column_spec(1, bold = T)
  
}

kable_data1 <- function(data,cap,foot=" ",align="c",colnames = NA){
  library(kableExtra)
  library(knitr)
  data %>%
    kable(booktabs=T,caption = cap,align = align,escape=F,col.names = colnames)%>%
    kable_styling(latex_options = "HOLD_position")
  
  
}

cox.stuart.test = function (x)
{ method = "Teste de Cox-Stuart para tendencia"
leng = length(x)
apross = round(leng) %% 2
if (apross == 1) {
  delete = (length(x)+1)/2
  x = x[ -delete ] 
}
half = length(x)/2
x1 = x[1:half]
x2 = x[(half+1):(length(x))]
difference = x1-x2
signs = sign(difference)
signcorr = signs[signs != 0]
pos = signs[signs>0]
neg = signs[signs<0]
if (length(pos) < length(neg)) {
  prop = pbinom(length(pos), length(signcorr), 0.5)
  names(prop) = "Tendencia de crescimento, valor p"
  rval <- list(method = method, statistic = prop)
  class(rval) = "htest"
  return(rval)
}
else {
  prop = pbinom(length(neg), length(signcorr), 0.5)
  names(prop) = "Tendencia de decrescimento, valor p"
  rval <- list(method = method, statistic = prop)
  class(rval) = "htest"
  return(rval)
}
}



kable_cov <- function(data,cap){
  library(kableExtra)
  
  
  cov(data) %>%
    kable(booktabs=T,caption = cap,digits = 4)%>%
    kable_styling(full_width = F, latex_options = "hold_position") %>%
    row_spec(0, align = "c",bold=T ) %>%
    column_spec(1, bold = T)
  
  
}


kable_cor <- function(data,cap){
  
  
  cor(data) %>%
    kable(booktabs=T,caption = cap,digits = 4)%>%
    kable_styling(full_width = F, latex_options = "hold_position") %>%
    row_spec(0, align = "c",bold=T ) %>%
    column_spec(1, bold = T)
  
  
}


kable_estimates <- function(model,cap){
  library(kableExtra)
  if(any(apply(modelo$model,2,typeof)=="character")){
    
    vif=c(" ")
   
    
    
  } else if (length(modelo$coefficients) > 2){
    vif=round(car::vif(modelo),4)
  
  }
  else{vif=c(" ")}

  
  names <- names(model$coefficients) 
  i=1
  col_label<-c(length(names))
  for(item in names){
    
    paste = paste0("$\\mathbf{",item,"}$")
    col_label[i]=paste
    i=i+1
  }
  #col_label
  
  
  summary <- summary(model)
  anova   <- anova(model)
  indices <- c(summary$r.squared,summary$adj.r.squared,summary$fstatistic[1],
                        pf(q = summary$fstatistic[1],df1 = summary$fstatistic[2],summary$fstatistic[3],lower.tail = F ))
  res <- cbind(round(summary$coefficients,4),VIF=c(" ",vif))
  row.names(res) <- col_label
  res <- rbind(res,c("$\\mathbf{R^2}$","$\\mathbf{R^2_{Adj}}$","$\\mathbf{F_0}$","$\\mathbf{Pr(>|F|)}$"," "),c(round(indices,4), " "))
  n <- length(model$coefficients)
  
  
  
     res %>%
    kable(booktabs=T,caption = cap,digits = 4,escape = F)%>%
    kable_styling(full_width = F, latex_options = "hold_position") %>%
    row_spec(0, align = "l",bold=T ) %>%
       row_spec(n+1, align = "c" ) %>%
       row_spec(n+2, align = "c" ) %>%
    column_spec(1, bold = T)
      

  
}



kable_fit <- function(model){
  summary <- summary(model)
  indices <- data.frame(R2=summary$r.squared,R2Adj=summary$adj.r.squared,Fstatistic=summary$fstatistic[1],pvalor=pf(q = summary$fstatistic[1],df1 = summary$fstatistic[2],summary$fstatistic[3],lower.tail = F ))
  kable(indices,booktabs=T,caption = "Coeficiente de Determinação e Estatística para ANOVA",digits = 4)%>%
    kable_styling(full_width = F, latex_options = "hold_position") %>%
    row_spec(0, align = "c",bold=T ) %>%
    column_spec(1, bold = T)
  
}


print_ajuste <- function(coeficientes){
  
  expressao <- c()
  i=1
  sinal <- NULL
  for(item in coeficientes){
    
    if(item > 0 && i==1){
      
      expressao[i] <- paste0(" + ",item)
      
    }else if (item < 0 && i==1){
      
      expressao[i] <- paste0(" - ",abs(item))
      
    }else if(item > 0 && i>1){
      
      expressao[i] <- paste0(" + ",item,"X_",i-1) 
      
    }else {
      
      expressao[i] <- paste0(" - ", abs(item),"X_",i-1)
      
    }
    i=i+1
  }
  exp <- "$$ \\hat{Y}="
  for ( item in expressao){
    
    exp <- str_c(exp,item)
    
  }
  exp <- str_c(exp," $$")
  exp %>% knitr::raw_latex()
  
  
  
  
}



IsOutlier <- function(data) {
  lowerq = quantile(data, na.rm = TRUE)[2]
  upperq = quantile(data, na.rm = TRUE)[4]
  iqr = upperq - lowerq 
  threshold_upper = (iqr * 1.5) + upperq
  threshold_lower = lowerq - (iqr * 1.5)
  data > threshold_upper | data <  threshold_lower 
}





################################ MODELOS ADITIVOS ##########################################


plot.curves <- function(data  = NULL, x,y,labelx="Eixo X",labely="Eixo Y",
                        title = "Gráfico de Dispersão",
                        type  = NULL,
                        se    = FALSE,
                        span  = 3/4,
                        mult  = NULL,
                        fit   = NULL){
  
  point.size  = 3
  point.alpha = .5
  point.color = "black"
  
  data <- data.frame(x=x,y=y)
  data <- data %>% arrange(x)
  plot <- ggplot(data)+
    geom_point(aes(x=x, y=y),alpha=point.alpha,size=point.size, col = point.color)+
    labs(x = labelx, y = labely)+
    xlim(range(x)+c(-0,+0))+
    ylim(range(y)+c(-0,+0))+
    ggtitle(paste0(title))+
    axis_theme
    
    line.size  = 1
    line.alpha = 1
    line.color = "red"
    
    
    if(is.null(type)){
      
      plot
      
      }else if(type == "res"){
        plot +
          geom_abline(slope = 0,intercept = 0,col=line.color,size=line.size,alpha=line.alpha)+
          ggtitle(paste0(title," \nResíduos vs Valores Ajustados"))
        
        }else if(type == "lm"){
          
          plot +
            geom_smooth(method = lm,se = se, col=line.color,size=line.size, alpha=line.alpha, aes(x = x,y = y))+
            ggtitle(paste0(title," \nCom ajuste Linear"))
          
          }else if(type == "loess"){
            
            plot + 
              stat_smooth(method = "loess",se = se, col=line.color,size=line.size, alpha=line.alpha,span = span, aes(x = x,y = y))+
              ggtitle(paste0(title," \nCom ajuste Loess","\n span = ",round(span,2)))
            
            }else if(type == "mult"){
              library(wesanderson)
              library(RColorBrewer)
              
              colors   <- as.list(brewer.pal(7,"Dark2"))
              n_colors <- c("C1","C2","C3","C4","C5","C6","C7")
              names(colors) <- n_colors
              l <- length(mult)
              n_colors1 <-c()
              for(item in mult){
                n_colors1[i] <- paste(n_colors[i], "\nSpan : ", item$span,"\nType : ",item$type)
                names(colors) <- n_colors1
                plot <- ggplot()+
                  geom_point(aes(x=x, y=y),alpha=.5,size=2)+
                  labs(x = labelx, 
                       y = labely,
                       color = "Legend")+
                  xlim(range(x)+c(-0,+0))+
                  ylim(range(y)+c(-0,+0))+
                  ggtitle(paste0(title," \nCom ajuste Loess"))+
                  axis_them
                size = 1
                alpha = 1
                if(l <= 1){
                  
                  plot <- plot + 
                    stat_smooth(method = mult[[1]]$type,se = se,size=size, alpha=alpha,span = mult[[1]]$span, aes(x = x,y = y,color = n_colors1[1]))
                  }else if(l <= 2){
                    
                    plot <- plot + 
                      stat_smooth(method = mult[[1]]$type,se = se,size=size, alpha=alpha,span = mult[[1]]$span, aes(x = x,y = y,color = n_colors1[1]))+ 
                      stat_smooth(method = mult[[2]]$type,se = se,size=size, alpha=alpha,span = mult[[2]]$span, aes(x = x,y = y,color = n_colors1[2]))
      
                  }else if(l <= 3){
                    plot <- plot + 
                      stat_smooth(method = mult[[1]]$type,se = se,size=size, alpha=alpha,span = mult[[1]]$span, aes(x = x,y = y,color = n_colors1[1]))+ 
                      stat_smooth(method = mult[[2]]$type,se = se,size=size, alpha=alpha,span = mult[[2]]$span, aes(x = x,y = y,color = n_colors1[2]))+ 
                      stat_smooth(method = mult[[3]]$type,se = se,size=size, alpha=alpha,span = mult[[3]]$span, aes(x = x,y = y,color = n_colors1[3]))
                    
                  }else if(l <= 4){
                    plot <- plot + 
                      stat_smooth(method = mult[[1]]$type,se = se,size=size, alpha=alpha,span = mult[[1]]$span, aes(x = x,y = y,color = n_colors1[1]))+ 
                      stat_smooth(method = mult[[2]]$type,se = se,size=size, alpha=alpha,span = mult[[2]]$span, aes(x = x,y = y,color = n_colors1[2]))+ 
                      stat_smooth(method = mult[[3]]$type,se = se,size=size, alpha=alpha,span = mult[[3]]$span, aes(x = x,y = y,color = n_colors1[3]))+ 
                      stat_smooth(method = mult[[4]]$type,se = se,size=size, alpha=alpha,span = mult[[4]]$span, aes(x = x,y = y,color = n_colors1[4]))
                    
                  }else if(l <= 5){
                    plot <- plot + 
                      stat_smooth(method = mult[[1]]$type,se = se,size=size, alpha=alpha,span = mult[[1]]$span, aes(x = x,y = y,color = n_colors1[1]))+ 
                      stat_smooth(method = mult[[2]]$type,se = se,size=size, alpha=alpha,span = mult[[2]]$span, aes(x = x,y = y,color = n_colors1[2]))+ 
                      stat_smooth(method = mult[[3]]$type,se = se,size=size, alpha=alpha,span = mult[[3]]$span, aes(x = x,y = y,color = n_colors1[3]))+ 
                      stat_smooth(method = mult[[4]]$type,se = se,size=size, alpha=alpha,span = mult[[4]]$span, aes(x = x,y = y,color = n_colors1[4]))+ 
                      stat_smooth(method = mult[[5]]$type,se = se,size=size, alpha=alpha,span = mult[[5]]$span, aes(x = x,y = y,color = n_colors1[5]))
                    
                  }else if(l <= 6){
                    plot <- plot + 
                      stat_smooth(method = mult[[1]]$type,se = se,size=size, alpha=alpha,span = mult[[1]]$span, aes(x = x,y = y,color = n_colors1[1]))+ 
                      stat_smooth(method = mult[[2]]$type,se = se,size=size, alpha=alpha,span = mult[[2]]$span, aes(x = x,y = y,color = n_colors1[2]))+ 
                      stat_smooth(method = mult[[3]]$type,se = se,size=size, alpha=alpha,span = mult[[3]]$span, aes(x = x,y = y,color = n_colors1[3]))+ 
                      stat_smooth(method = mult[[4]]$type,se = se,size=size, alpha=alpha,span = mult[[4]]$span, aes(x = x,y = y,color = n_colors1[4]))+ 
                      stat_smooth(method = mult[[5]]$type,se = se,size=size, alpha=alpha,span = mult[[5]]$span, aes(x = x,y = y,color = n_colors1[5]))+ 
                      stat_smooth(method = mult[[6]]$type,se = se,size=size, alpha=alpha,span = mult[[6]]$span, aes(x = x,y = y,color = n_colors1[6]))
                    
                  }else if(l <= 7){
                    plot <- plot + 
                      stat_smooth(method = mult[[1]]$type,se = se,size=size, alpha=alpha,span = mult[[1]]$span, aes(x = x,y = y,color = n_colors1[1]))+ 
                      stat_smooth(method = mult[[2]]$type,se = se,size=size, alpha=alpha,span = mult[[2]]$span, aes(x = x,y = y,color = n_colors1[2]))+ 
                      stat_smooth(method = mult[[3]]$type,se = se,size=size, alpha=alpha,span = mult[[3]]$span, aes(x = x,y = y,color = n_colors1[3]))+ 
                      stat_smooth(method = mult[[4]]$type,se = se,size=size, alpha=alpha,span = mult[[4]]$span, aes(x = x,y = y,color = n_colors1[4]))+ 
                      stat_smooth(method = mult[[5]]$type,se = se,size=size, alpha=alpha,span = mult[[5]]$span, aes(x = x,y = y,color = n_colors1[5]))+ 
                      stat_smooth(method = mult[[6]]$type,se = se,size=size, alpha=alpha,span = mult[[6]]$span, aes(x = x,y = y,color = n_colors1[6]))+ 
                      stat_smooth(method = mult[[7]]$type,se = se,size=size, alpha=alpha,span = mult[[7]]$span, aes(x = x,y = y,color = n_colors1[7]))
                    
                  }
              }
                plot <- plot + scale_color_manual(values = colors)
                
                }else if (type == 'rm'){
                  
                  plot + 
                    geom_line(data = fit$data,aes(x = fit$data$x ,y = fit$data$y), col=line.color,size=line.size,alpha=line.alpha) +
                    ggtitle(paste0(title," \n",fit$name,"\n span = ",round(fit$span,2)))
                }else if (type == 'rl'){
                  
                  plot + 
                    geom_line(aes(x, fit$fitted.values), col=line.color,size=line.size,alpha=line.alpha) +
                    ggtitle(paste0(title," \n",fit$name,"\n span = ",round(fit$span,2)))
                }else if (type == 'g'){
                  
                  plot + 
                    geom_line(aes(x, fit), col=line.color,size=line.size,alpha=line.alpha)
                  # +
                  #   ggtitle(paste0(title," \n",fit$name,"\n span = ",round(fit$span,2)))
                }
  
  
  
}




plot.mult.curves <- function(df,labelx="Eixo X",labely="Eixo Y",
                        title = "Gráfico de Dispersão"
                        ){
  
 
  library(wesanderson)
  library(RColorBrewer)
  point.size  = 3
  point.alpha = .2
  point.color = "darkgrey"
  line.size  = 1.2
  line.alpha = 1
  line.color = "red"
  cores               <- c()
  col.brew            <- brewer.pal(n = 9, name = "Set1")
  colors              <- col.brew
  
  #display.brewer.pal(, "Set1")
  plot <- ggplot(data = df,aes(x=x,y=y))+
    geom_point(alpha=point.alpha,size=point.size, col = point.color)+
    labs(x = labelx, 
         y = labely,
         color = "Legend")+
    xlim(range(x)+c(-0,+0))+
    ylim(range(y)+c(-0,+0))+
    ggtitle(paste0(title))+
    geom_line(aes(x=x,y = value,color=variable),size=line.size,alpha=line.alpha) +
    scale_color_manual(values = colors) 
  
  
  plot
  
  
  
}



## Series temporais
gama <- function(serie,k=0){
  x_barra <- mean(serie)
  n       <- length(serie)
  p_n     <- n - k
  return(sum((serie[1:p_n] - x_barra)*(serie[(k+1):n] - x_barra))*1/length(serie))
  
}

fac <- function(serie,k=0){
  x_barra <- mean(serie)
  n       <- length(serie)
  p_n     <- n - k
  gama <- sapply(0:(length(serie)-1), function(k) gama(serie=serie,k = k) )
  rho  <- gama/gama(serie = serie,k = 0)
  parcialvarx <- rho*c(1,0.8,0.6,0.4,0.2)  * c(1,2,2,2,2)
  df <- data.frame(i = 1:length(serie), k = 0:(length(serie)-1), gama = gama, rho = rho, pacialVx = parcialvarx)
  return(df)
  
}




write_matex <- function(x) {
  begin <- "$$\\begin{bmatrix}"
  end <- "\\end{bmatrix}$$"
  X <-
    apply(x, 1, function(x) {
      paste(
        paste(x, collapse = "&"),
        "\\\\"
      )
    })
  writeLines(c(begin, X, end))
}


write_matex2 <- function(x) {
  begin <- "\\begin{bmatrix}"
  end <- "\\end{bmatrix}"
  X <-
    apply(x, 1, function(x) {
      paste(
        paste(x, collapse = "&"),
        "\\\\"
      )
    })
  paste(c(begin, X, end), collapse = "")
}



# w <- function(u){
#   peso <- c()
#   for(i in 1:length(u)){
#     if( (u[i] >= 0 ) && (u[i] < 0.99999)){
#       peso[i] <-(1 - u[i]**3)**3
#       
#     }else{peso[i] = 0}
#     
#   }
#   return(peso)
# }
