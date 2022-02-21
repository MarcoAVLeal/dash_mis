library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(readxl)
library(stringr)
library(flexdashboard)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(pagedown)
library(stringr)
library(RODBC)
library(RMySQL)
library(RMariaDB)
library(webshot)
library(ggtext)
library(fresh)
library(shinyWidgets)
library(shinycssloaders)
library(DT)


credentials <-  list(
  mleal=list(username="mleal",pass="mleal",name="Marco Aurelio Valles Leal",cargo = "Analista de Dados",departamento = "Inteligência de Negócios",img="https://media-exp1.licdn.com/dms/image/C4D03AQExh92rzasW7g/profile-displayphoto-shrink_800_800/0/1565999978646?e=1650499200&v=beta&t=OSrhN0GvN1-fLl-_cJwzBfRH_tLdLBqrDlAdspw6QxI"),
  dctgranzotto=list(username="dctgranzotto",pass="covid19dctgranzotto",name="Daniele Cristina Tita Granzotto "),
  morabreu=list(username="morabreu",pass="covid19morabreu",name="Marcelo Osnar Rodrigues de Abreu"),
  angulo=list(username="angulo",pass="covid19angulo",name="Ângulo"),
  astorga=list(username="astorga",pass="covid19astorga",name="Astorga"),
  atalaia=list(username="atalaia",pass="covid19atalaia",name="Atalaia"),
  colorado=list(username="colorado",pass="covid19colorado",name="Colorado"),
  doutorcamargo=list(username="doutorcamargo",pass="covid19doutorcamargo",name="Doutor Camargo"),
  florai=list(username="florai",pass="covid19florai",name="Florai"),
  floresta=list(username="floresta",pass="covid19floresta",name="Floresta"),
  florida=list(username="florida",pass="covid19florida",name="Flórida"),
  iguaracu=list(username="iguaracu",pass="covid19iguaracu",name="Iguaraçu"),
  itaguaje=list(username="itaguaje",pass="covid19itaguaje",name="Iguajé"),
  itambe=list(username="itambe",pass="covid19itambe",name="Itambé"),
  ivatuba=list(username="ivatuba",pass="covid19ivatuba",name="Ivatuba"),
  lobato=list(username="lobato",pass="covid19lobato",name="Lobato"),
  mandaguacu=list(username="mandaguacu",pass="covid19mandaguacu",name="Mandaguaçu"),
  mandaguari=list(username="mandaguari",pass="covid19mandaguari",name="Mandaguari"),
  marialva=list(username="marialva",pass="covid19marialva",name="Marialva"),
  maringa=list(username="maringa",pass="covid19maringa",name="Maringá"),
  munhozdemelo=list(username="munhozdemelo",pass="covid19munhozdemelo",name="Munhoz de Melo"),
  nossasenhoradasgracas=list(username="nossasenhoradasgracas",pass="covid19nossasenhoradasgracas",name="Nossa Senhora das Graças"),
  novaesperanca=list(username="novaesperanca",pass="covid19novaesperanca",name="Nova Esperança"),
  ourizona=list(username="ourizona",pass="covid19ourizona",name="Ourizona"),
  paicandu=list(username="paicandu",pass="covid19paicandu",name="Paiçandu"),
  paranacity=list(username="paranacity",pass="covid19paranacity",name="Paranacity"),
  presidentecastelobranco=list(username="presidentecastelobranco",pass="covid19castelobranco",name="Presidente Castelo Branco"),
  santafe=list(username="santafe",pass="covid19santafe",name="Santa Fé"),
  santaines=list(username="santaines",pass="covid19santaines",name="Santa Inês"),
  santoinacio=list(username="santoinacio",pass="covid19santoinacio",name="Santo Inácio"),
  saojorgedoivai=list(username="saojorgedoivai",pass="covid19saojorgeivai",name="São Jorge do Ivaí"),
  sarandi=list(username="sarandi",pass="covid19sarandi",name="Sarandi"),
  uniflor=list(username="uniflor",pass="covid19uniflor",name="Uniflor"),
  regional=list(username="regional",pass="covid1915regional",name="15ª Regional"),
  cresems=list(username="cresems",pass="covid19cresems",name="CRESEMS")
)
