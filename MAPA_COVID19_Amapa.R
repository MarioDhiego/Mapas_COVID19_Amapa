###### MAPAS AMAPÁ - COVID19 ####################################################
###### PACOTES            ####################################################
library(sf)         # Ler aquivos tipo .shap 
library(geobr)      # Base Cartográfica/IBGE
library(magrittr)   # utilizar operador pipe
library(dplyr)      # fazer manipulação no banco
library(colorspace) # usar paleta de cores
library(ggplot2)    # gerar mapa por camadas
library(gifski)     #
library(gganimate)  # gerar animação no gráfico
library(leaflet)    # gerar mapas interativos
library(plotly)
#############################################################################

#############################################################################
# Base de dados SobreO COVID-19 p/ UF foi utilizado 
# do site https://brasil.io/home/
# DATA : 25/03/2021
############################################################################

# Ler base cartográica do IBGE via geobr
BASE_AP <- read_municipality(code_muni = "AP", year = 2020)
plot(BASE_AP)


summary(AP_Casos_Covid$deaths)


ggplot(BASE_AP)+
  geom_sf(fill= "#2D3E50", color= "#FEBF57", 
          size=0.15, show.legend = FALSE)


# Limpar Casos
casosAP <- COVID19_MAR_AP
linhas <- c(17)
casosAP <- casosAP[-linhas,]

# Remover Colunas/(variaveis)
colunas <- c(3,4) 
BASE_AP <- BASE_AP[,-colunas]

colunas <- c(1,2,4,7)
casosAP <- casosAP[,-colunas] 


# Juntar as Bases (geobr+ COVID19_PA)
AP_Casos_Covid <- merge(BASE_AP, casosAP, by.x= "code_muni", by.y="city_ibge_code")

# Gerar o Mapa (gerobr + COVID19_RS)
Mapa_AP <- leaflet(AP_Casos_Covid) %>% 
  addTiles()
Mapa_AP %>% addPolygons()

Mapa_AP %>% addPolygons(
  weight = 1.5,
  opacity = 0.5,
  color = "blue",
  dashArray = 1,
  fillOpacity = 0,
)


# Bins e Cores/ pacote (colorspace)

bins <- c(0,50,100,200,500,900,Inf)
pal <- colorBin("YlOrRd", domain = AP_Casos_Covid$deaths, bins = bins)

Mapa_AP %>% addPolygons(
  fillColor = ~pal(deaths),
  weight = 2.0,
  opacity = 1.0,
  color = "white",
  dashArray = 1,
  fillOpacity = 0.7,
)

# Adicionarinteratividade no Mapa

Mapa_AP %>% addPolygons(
  fillColor = ~pal(deaths),
  weight = 1.5,
  opacity = 1,
  color = "white",
  dashArray = "1",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE  
  ))

# Customizando Informações/gerar html

label1 <- sprintf(
  "<strong>%s</strong></br>%g Confirmados</br>%g Òbitos",
  AP_Casos_Covid$name_muni, AP_Casos_Covid$confirmed, AP_Casos_Covid$deaths) %>% lapply(htmltools::HTML)


# Mapa Customizado

Mapa_AP %>% addPolygons(
  fillColor = ~pal(deaths),
  weight =2,
  opacity =1,
  color ="white",
  dashArray ="1",
  fillOpacity =0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray ="",
    fillOpacity =0.7, 
    stroke = FALSE,
    bringToFront =TRUE),
  label = label1,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction= "auto"
  )) %>% addLegend(pal= pal,
                   values = ~deaths,
                   opacity = 0.7,
                   title = "Casos de Òbitos - COVID19",
                   position= "bottomright")

####################################################################

