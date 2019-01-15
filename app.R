options(encoding = 'UTF-8')
options(scipen=999)

# Sys.setlocale("LC_COLLATE", 'French_France.1252')
# Sys.setlocale("LC_TIME", "French_France.1252")
#Sys.setlocale("LC_TIME", "fr_FR.UTF-8")
#Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
#Sys.setlocale(category = "LC_ALL", locale = "French_France.1252")
# pb read csv https://github.com/rstudio/rsconnect/issues/233

# # export dataset source
library(tidyverse)
library(data.table)
library(readr)
library(scales)
library(hrbrthemes)
library(rsconnect)
library(stringi)
#library(ggiraph)
# gestion des spinners pour attente
library(shinycssloaders)
library(shinythemes)
#library(echarts4r)
library(cowplot)
library(rmarkdown)
library(markdown)
library(knitr)
library(ggiraph)
library(RColorBrewer)
#library(shinyWidgets)
library(widgetframe)
library(leaflet)
library(shinyWidgets)
library(shinyhelper)
# table HTML
library(tableHTML)

# imports des fichiers par mailles EPCI/DEP/REG  ####


indics_migres_TERR <- read.csv2("./data/indics_migres_TERR.csv",sep = ',',dec='.',stringsAsFactors =F, fileEncoding="UTF-8-BOM" ) %>% ungroup()
indics_migres_TERR_CS1 <- read.csv2("./data/indics_migres_TERR_CS1.csv",sep = ',',dec='.',stringsAsFactors =F, fileEncoding="UTF-8-BOM", colClasses=c("CS1"="character") ) %>% ungroup()
indics_migres_TERR_AGEREVS <- read.csv2("./data/indics_migres_TERR_AGEREVS.csv",sep = ',',dec='.',stringsAsFactors =F, fileEncoding="UTF-8-BOM" ) %>% ungroup()

indics_migres_TERR_AGEREVS_RENOUV <- read.csv2("./data/indics_migres_TERR_AGEREVS_RENOUV.csv",sep = ',',dec='.',stringsAsFactors =F, fileEncoding="UTF-8-BOM" ) %>% ungroup()
indics_migres_TERR_CS1_RENOUV <- read.csv2("./data/indics_migres_TERR_CS1_RENOUV.csv",sep = ',',dec='.',stringsAsFactors =F, fileEncoding="UTF-8-BOM" ) %>% ungroup()


# rajout des libellés des CS
library(migR)

indics_migres_TERR_CS1 <-
  ajout_libelles_varventil_insee(TABLE = indics_migres_TERR_CS1 %>% ungroup(),
                                 VAR ="CS1",
                                 MILLESIME_RP = 2015)

# volume des flux
flux_migres_TERR.map <- read.csv2("./data/flux_migres_TERR.map.csv",sep = ',',dec='.',stringsAsFactors =F, fileEncoding="UTF-8-BOM" ) %>% ungroup()
flux_migres_TERR.map.SMnet <- read.csv2("./data/flux_migres_TERR.map.SMnet.csv",sep = ',',dec='.',stringsAsFactors =F, fileEncoding="UTF-8-BOM" ) %>% ungroup()


# vecteur des intitulés de territoires
liste_DEP <- indics_migres_TERR %>% filter(NIV_TERR %in% 'DEP') %>% distinct(TERR,LIB_TERR) %>% mutate(lib_code_TERR = glue::glue('{LIB_TERR} ({TERR})')) %>% pull(lib_code_TERR)
liste_EPCI <- indics_migres_TERR %>% filter(NIV_TERR %in% 'EPCI') %>% distinct(TERR,LIB_TERR) %>% mutate(lib_code_TERR = glue::glue('{LIB_TERR} ({TERR})')) %>% pull(lib_code_TERR)
liste_REG <- indics_migres_TERR %>% filter(NIV_TERR %in% 'REG') %>% distinct(TERR,LIB_TERR) %>% mutate(lib_code_TERR = glue::glue('{LIB_TERR} ({TERR})')) %>% pull(lib_code_TERR)


# import carto

geo_TERR_poly <- st_read("./data/geo_TERR_poly.gpkg") %>% st_transform(4326) %>%
  mutate(lib_code_TERR = glue::glue('{LIB_TERR} ({TERR})'))


## fonction pour string ggiraph


# fonction pour gérer accents
conv_accents <- function(x) {
  x <- gsub(pattern = "è", replacement = "&egrave;", x = x)
  x <- gsub(pattern = "é", replacement = "&eacute;", x = x)
  x <- gsub(pattern = "ê", replacement = "&ecirc;", x = x)
  x <- gsub(pattern = "ë", replacement = "&euml;", x = x)
  x <- gsub(pattern = "î", replacement = "&icirc;", x = x)
  x <- gsub(pattern = "ï", replacement = "&iuml;", x = x)
  x <- gsub(pattern = "û", replacement = "&ucirc;", x = x)
  x <- gsub(pattern = "ü", replacement = "&uuml;", x = x)
  x <- gsub(pattern = "ô", replacement = "&ocirc;", x = x)
  x <- gsub(pattern = "à", replacement = "&agrave;", x = x)
  x <- gsub(pattern = "â", replacement = "&acirc;", x = x)
  x <- gsub(pattern = "ç", replacement = "&ccedil;", x = x)
  x <- gsub(pattern = "è", replacement = "&Egrave;", x = x)
  x <- gsub(pattern = "é", replacement = "&Eacute;", x = x)
  x <- gsub(pattern = "ê", replacement = "&Ecirc;", x = x)
  x <- gsub(pattern = "ë", replacement = "&Euml;", x = x)
  x <- gsub(pattern = "î", replacement = "&Icirc;", x = x)
  x <- gsub(pattern = "ï", replacement = "&Iuml;", x = x)
  x <- gsub(pattern = "û", replacement = "&Ucirc;", x = x)
  x <- gsub(pattern = "ü", replacement = "&Uuml;", x = x)
  x <- gsub(pattern = "ô", replacement = "&Ocirc;", x = x)
  x <- gsub(pattern = "à", replacement = "&Agrave;", x = x)
  x <- gsub(pattern = "â", replacement = "&Acirc;", x = x)
  x <- gsub(pattern = "ç", replacement = "&Ccedil;", x = x)
  x <- gsub(pattern = "'", replacement = "&apos;", x = x)
 
  return(x)
}

# fonctions pour menu carto leaflet  ####

func_map_DEP <- function() {
  
  # affichage de la carte
  
  leaflet(geo_TERR_poly %>% filter(NIV_TERR %in% 'DEP'),
          padding = 0, 
          options = leafletOptions(zoomControl = TRUE,minZoom = 5, maxZoom =12) ) %>%
    setMaxBounds(lng1 = st_bbox(geo_TERR_poly)$xmin %>% as.vector(),
                 lng2 = st_bbox(geo_TERR_poly)$xmax %>% as.vector(),
                 lat1 = st_bbox(geo_TERR_poly)$ymin %>% as.vector(),
                 lat2 = st_bbox(geo_TERR_poly)$ymax %>% as.vector()) %>%
    addPolygons(weight = 1, smoothFactor = 0.2, #color = '#00008B', 
                fillColor = '#5b937c',
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                label = ~LIB_TERR, 
                layerId = ~lib_code_TERR,
                highlightOptions = highlightOptions(color = NA, fillColor = "#1e4837",  weight = 1.5, bringToFront = FALSE, opacity = 1))
  
}

func_map_REG <- function() {
  
  leaflet(geo_TERR_poly %>% filter(NIV_TERR %in% 'REG'),
          padding = 0, 
          options = leafletOptions(zoomControl = TRUE,minZoom = 5, maxZoom =12) ) %>%
    setMaxBounds(lng1 = st_bbox(geo_TERR_poly)$xmin %>% as.vector(),
                 lng2 = st_bbox(geo_TERR_poly)$xmax %>% as.vector(),
                 lat1 = st_bbox(geo_TERR_poly)$ymin %>% as.vector(),
                 lat2 = st_bbox(geo_TERR_poly)$ymax %>% as.vector()) %>%
    addPolygons(weight = 1, smoothFactor = 0.2, #color = '#00008B', 
                fillColor = '#5b937c',
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                label = ~LIB_TERR, 
                layerId = ~lib_code_TERR,
                highlightOptions = highlightOptions(color = NA, fillColor = "#1e4837",  weight = 1.5, bringToFront = FALSE, opacity = 1))
  
}

func_map_EPCI <- function() {
  
  leaflet(geo_TERR_poly %>% filter(NIV_TERR %in% 'EPCI'),
          padding = 0, 
          options = leafletOptions(zoomControl = TRUE,minZoom = 5, maxZoom =12) ) %>%
    setMaxBounds(lng1 = st_bbox(geo_TERR_poly)$xmin %>% as.vector(),
                 lng2 = st_bbox(geo_TERR_poly)$xmax %>% as.vector(),
                 lat1 = st_bbox(geo_TERR_poly)$ymin %>% as.vector(),
                 lat2 = st_bbox(geo_TERR_poly)$ymax %>% as.vector()) %>%
    addPolygons(weight = 1, smoothFactor = 0.2, #color = '#00008B', 
                fillColor = '#5b937c',
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                label = ~LIB_TERR, 
                layerId = ~lib_code_TERR,
                highlightOptions = highlightOptions(color = NA, fillColor = "#1e4837",  weight = 1.5, bringToFront = FALSE, opacity = 1))
  
}


# définition de l'interface UI  ####

ui <- navbarPage(
  position = "static-top",
  theme = "styles_v3.css" ,
  title = div(
    div(
      id = "img-id",
      #img(src = "logo_OT.png", alt="http://www.observatoire-des-territoires.gouv.fr/", height = 45, width = 150)
      a(img(src="logo_OT.png", height = 45, width = 150), href="http://www.observatoire-des-territoires.gouv.fr/",target="_blank")
    )
  ),
  tabPanel("Les chiffres clés des mobilités résidentielles dans chaque territoire",
           sidebarLayout(
             sidebarPanel(id="sidebar",
                          width = 3,
                          div(class="test_type",
                              width = 3.5,
                              radioGroupButtons(inputId = "maille_TERR", 
                                                label = "Choisir une maille d'analyse :", 
                                                direction = "vertical",
                                                choices = list("REGION" = 'REG',"DEPARTEMENT" = 'DEP', "INTERCOMMUNALITÉ" = 'EPCI'), 
                                                checkIcon = list(yes = icon("ok", lib = "glyphicon"))),
                              br(),
                              # condition REG
                              conditionalPanel(
                                condition = "input.maille_TERR == 'REG'",
                                pickerInput(inputId = "territoireetude_REG", 
                                            label = "Choisir un territoire :", 
                                            choices = liste_REG,
                                            selected = "Bretagne (53)",
                                            options = list(style = "btn-primary", `live-search` = TRUE))
                              ),
                              # condition DEP
                              conditionalPanel(
                                condition = "input.maille_TERR == 'DEP'",
                                 pickerInput(inputId = "territoireetude_DEP", 
                                            label = "Choisir un territoire :", 
                                            choices = liste_DEP,
                                            selected = "Paris (75)",
                                            options = list(style = "btn-primary", `live-search` = TRUE))
                              ),
                              # condition EPCI
                              conditionalPanel(
                                condition = "input.maille_TERR == 'EPCI'",
                                 pickerInput(inputId = "territoireetude_EPCI", 
                                            label = "Choisir un territoire :", 
                                            choices = liste_EPCI,
                                            selected = "Orléans Métropole (244500468)",
                                            options = list(style = "btn-primary", `live-search` = TRUE))
                              ),

                              br(),
                              leafletOutput("map"),
                              br(),
                              br(),
                              a(img(src="logo_OT.png", height = 45, width = 150, align = "left"),
                                href="http://www.observatoire-des-territoires.gouv.fr/",target="_blank"),
                              a(img(src="CGET-logotype.png", height = 50, width = 55, align = "right"), 
                                href="http://www.cget.gouv.fr/",target="_blank"),
                              br(),
                              br()
                              
                          )
                          # largeur min / max du container
                          , 
                          tags$head(tags$style(type="text/css", ".container-fluid {  /*max-width: 2000px;*/ min-width: 1200px; "))
             ),

             mainPanel(
               tabsetPanel(
                 tabPanel("Arrivées et départs",
                          column(5,htmlOutput("pres")),
                          column(7,ggiraphOutput(outputId="nuagepoint_terr", width="650px",height="500px") %>% withSpinner(type = 1, color="#1B7F3F")) ),
                 tabPanel("Evolution démographique",
                          fluidRow(htmlOutput("intro_onglet_evoldemo"))),
                 tabPanel("Profil de la population",
                          fluidRow(htmlOutput("intro_onglet_profil")),
                          radioButtons("type_pop", "Type de population :",
                                       choices = list("Présente" = 'PRES',
                                                      "Sortante" = 'SORT',
                                                      "Entrante" = 'ENTR'),
                                       inline = TRUE,
                                       selected = 'PRES') ,
                          column(6,ggiraphOutput(outputId="diagcirc_profil_pop_CS1", width="450px",height="600px") %>% withSpinner(type = 1, color="#1B7F3F")),
                          column(6,ggiraphOutput(outputId="diagcirc_profil_pop_AGEREVS", width="450px",height="600px") %>% withSpinner(type = 1, color="#1B7F3F"))
                 ),
                 tabPanel("Impact sociodémographique",
                          fluidRow(htmlOutput("intro_onglet_impact")),
                          column(6,ggiraphOutput(outputId="diag_evolmig_profilpop_CS1", width="450px",height="600px") %>% withSpinner(type = 1, color="#1B7F3F")),
                          column(6,ggiraphOutput(outputId="diag_evolmig_profilpop_AGEREVS", width="450px",height="600px") %>% withSpinner(type = 1, color="#1B7F3F"))#,
                 ),
                 tabPanel("Flux résidentiels", 
                          fluidRow(htmlOutput("intro_onglet_fluxres")),
                          radioButtons("type_flux", "Type de flux :",
                                       choices = list("Flux entrants" = 'ENTR',
                                                      "Flux sortants" = 'SORT',
                                                      "Solde migratoire net" = 'SMN'),
                                       inline = TRUE,
                                       selected = 'ENTR') ,
                          plotOutput("flux", width = 875, height = 700) %>% withSpinner(type = 1, color="#1B7F3F")) #,
               )

             )
           )
  ), 
  
  tabPanel('Sources et méthodologie',
           fluidRow(
              column(10,includeMarkdown("onglet_sources_methodo.rmd"))
           )
  ),
  tabPanel('Rapport',
           fluidRow(
             column(10,includeMarkdown("onglet_ot_rapport.rmd"))
           )
  ),
  # eviter message d'erreur avant output
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  )
)


# Definition de la partie serveur  ####

server <- function(input, output, session) {
  
  # carto leaflet  ####
  
  selection_terr <- reactive({
    
    if(input$maille_TERR %in% 'DEP') {
      geo_TERR_poly %>% 
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP) )
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      geo_TERR_poly %>% 
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI) )
    } else if ( input$maille_TERR %in% 'REG') {
      geo_TERR_poly %>% 
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_REG) )
      
    }
    
  })
  
  
  map_click <- eventReactive(input$map_shape_click, {
    
    x <- input$map_shape_click
    
    y <- x$id
    
    return(y)
    
  })
  
  observe({
    
    if(input$maille_TERR %in% 'DEP') {
      updateSelectInput(session, 'territoireetude_DEP', selected = map_click())
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      updateSelectInput(session, 'territoireetude_EPCI', selected = map_click())
      
    } else if ( input$maille_TERR %in% 'REG') {
      updateSelectInput(session, 'territoireetude_REG', selected = map_click())
    }
  })
  
  observe({
    
    tx <- leafletProxy('map', session) %>%
      addPolygons(data = selection_terr(), fill = FALSE, color = '#0a1812', weight = 1.5,
                  opacity = 1, layerId = 'lib_code_TERR') 
  })
  
  # rendu carto 
  
  output$map <- renderLeaflet({
    
    
    if(input$maille_TERR %in% 'REG') {
      
      tx <- func_map_REG() %>%
        addPolygons(data = selection_terr(), fill = FALSE, color = '#FFFF00', weight = 1,
                    opacity = 1, layerId = 'lib_code_TERR') 
      tx
      
    } else if (input$maille_TERR %in% 'DEP') {
      
      tx <- func_map_DEP() %>%
        addPolygons(data = selection_terr(), fill = FALSE, color = '#FFFF00', weight = 1,
                    opacity = 1, layerId = 'lib_code_TERR') 
      tx
      
    } else if (input$maille_TERR %in% 'EPCI') {
      
      tx <- func_map_EPCI() %>%
        addPolygons(data = selection_terr(), fill = FALSE, color = '#FFFF00', weight = 1,
                    opacity = 1, layerId = 'lib_code_TERR') 
      tx
    }
    
  })
  
  
  
  # filtre sur dataframe TOT
  filteredData_TOT <- reactive({
    if(input$maille_TERR %in% 'DEP') {
      indics_migres_TERR %>% filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP) )
    } else if ( input$maille_TERR %in% 'EPCI') {
      indics_migres_TERR %>% filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI) )
    } else if ( input$maille_TERR %in% 'REG') {
      indics_migres_TERR %>% filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_REG) )
      
    }
  })
  
  
  # chapeau d'introduction sur l'onglet "arrivées et départs"
  output$intro_onglet_evoldemo <- renderUI({
    if(filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'DEP') {
      {HTML(paste0("<font size=2 color=#4f5154 family=Roboto>" ,
                   "Quels sont les moteurs de l'évolution démographique (solde naturel, solde migratoire) en évolution depuis les années 1960 et en comparaison avec les autres départements ?" ,"</b>","</font>",
                   "<br>", "<br>"
      ) )}
    } else if ( filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'EPCI') {
      {HTML(paste0("<font size=2 color=#4f5154 family=Roboto>" ,
                   "Quels sont les moteurs de l'évolution démographique (solde naturel, solde migratoire) en évolution depuis les années 1960 et en comparaison avec les autres intercommunalités ?" ,"</b>","</font>",
                   "<br>", "<br>"
      ) )}
    } else if ( filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'REG') {
      {HTML(paste0("<font size=2 color=#4f5154 family=Roboto>" ,
                   "Quels sont les moteurs de l'évolution démographique (solde naturel, solde migratoire) en évolution depuis les années 1960 et en comparaison avec les autres régions ?" ,"</b>","</font>",
                   "<br>", "<br>"
      ) )}
    }
  })
  
  # chapeau d'introduction sur l'onglet 'profil socio-démographique'
  output$intro_onglet_profil <- renderUI({
    if(filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'DEP') {
      {HTML(paste0("<font size=2 color=#4f5154 family=Roboto>" ,
                   "Quelle est la répartition de la population par classe d'âge et par groupe socioprofessionnel, distinguée selon que l'on s'intéresse à l'ensemble des habitants du département, aux nouveaux arrivants ou à ceux qui en sont partis ?" ,"</b>","</font>",
                   "<br>", "<br>"
      ) )}
    } else if ( filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'EPCI') {
      {HTML(paste0("<font size=2 color=#4f5154 family=Roboto>" ,
                   "Quelle est la répartition de la population par classe d'âge et par groupe socioprofessionnel, distinguée selon que l'on s'intéresse à l'ensemble des habitants de l'intercommunalité, aux nouveaux arrivants ou à ceux qui en sont partis ?" ,"</b>","</font>",
                   "<br>", "<br>"
      ) )}
    } else if ( filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'REG') {
      {HTML(paste0("<font size=2 color=#4f5154 family=Roboto>" ,
                   "Quelle est la répartition de la population par classe d'âge et par groupe socioprofessionnel, distinguée selon que l'on s'intéresse à l'ensemble des habitants de la région, aux nouveaux arrivants ou à ceux qui en sont partis ?" ,"</b>","</font>",
                   "<br>", "<br>"
      ) )}
    }
  })
  
  # chapeau d'introduction sur l'onglet 'impact socio-démographique'
  output$intro_onglet_impact <- renderUI({
    if(filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'DEP') {
    {HTML(paste0("<font size=2 color=#4f5154 family=Roboto>" ,
                 "Comment les mobilités résidentielles modifient la composition (par groupe socioprofessionnel et par classe d'âge) du département ?" ,"</b>","</font>",
                 "<br>", "<br>"
    ) )}
    } else if ( filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'EPCI') {
      {HTML(paste0("<font size=2 color=#4f5154 family=Roboto>" ,
                   "Comment les mobilités résidentielles modifient la composition (par groupe socioprofessionnel et par classe d'âge) de l'intercommunalité ?" ,"</b>","</font>",                   "<br>", "<br>"
      ) )}
    } else if ( filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'REG') {
      {HTML(paste0("<font size=2 color=#4f5154 family=Roboto>" ,
                   "Comment les mobilités résidentielles modifient la composition (par groupe socioprofessionnel et par classe d'âge) de la région ?" ,"</b>","</font>",
                   "<br>", "<br>"
      ) )}
    }
  })


  # chapeau d'introduction sur l'onglet 'flux résidentiels'
  output$intro_onglet_fluxres <- renderUI({
    if(filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'DEP') {
      {HTML(paste0("<font size=2 color=#4f5154 family=Roboto>" ,
                   conv_accents("D'où viennent les individus qui ont emménagé dans le département ? Où se sont installés ceux qui sont partis ?") ,"</b>","</font>",
                   "<br>", "<br>"
      ) )}
    } else if ( filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'EPCI') {
      {HTML(paste0("<font size=2 color=#4f5154 family=Roboto>" ,
                   conv_accents("D'où viennent les individus qui ont emménagé dans l'intercommunalité ? Où se sont installés ceux qui sont partis ?") ,"</b>","</font>",
                   "<br>", "<br>"
      ) )}
    } else if ( filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'REG') {
      {HTML(paste0("<font size=2 color=#4f5154 family=Roboto>" ,
                   conv_accents("D'où viennent les individus qui ont emménagé dans la région ? Où se sont installés ceux qui sont partis ?") ,"</b>","</font>",
                   "<br>", "<br>"
      ) )}
    }
  })


  
  # rédaction du petit laïus sur l'état des migrations résidentielles dans le territoire
  output$pres <- renderUI({
    {HTML(paste0("<br>",
                 "<font size=3 color=#4f5154 family=Roboto>",
                 "D'après les données du dernier recensement de l'Insee :", "<br>","<br>",
                 "<b>",format(round(filteredData_TOT() %>% select(nb_ind_ENTR) %>% pull(),-1), nsmall=0, big.mark=' '),"</b>"," individus sont arrivés dans ce territoire entre 2014 et 2015","<br>",
                 "soit ",filteredData_TOT() %>% select(PE) %>% pull()  %>% percent(.,accuracy = 0.1)," de la population<br>",
                 "<br>",
                 "<b>",format(round(filteredData_TOT() %>% select(nb_ind_SORT) %>% pull(),-1), nsmall=0, big.mark=" "),"</b>"," individusen sont partis","<br>",
                 "soit ",filteredData_TOT() %>% select(PS) %>% pull()  %>% percent(.,accuracy = 0.1)," de la population<br>",
                 "<br>",
                 "Le solde migratoire","<br>"," entre ce territoire et le reste de la France est donc :","<br>",
                 "<b>",paste0(symnum(round(filteredData_TOT() %>% select(SM) %>% pull(),-1), c(-Inf, 0, Inf), c("", "+")), round(filteredData_TOT() %>% select(SM) %>% pull(),-1),"")," individus","</b>", "<br>",
                 "soit ", paste0(symnum(filteredData_TOT()$TM,
                                        c(-Inf, 0, Inf),
                                        c("", "+")),
                                 percent(filteredData_TOT()$TM, accuracy = 0.01),""), " de la population<br>"
                 
    ) )}
  })
  

  
  
  ## nuage de points pour contexte ####
  
  filteredData_nuagepoint_mig <- reactive({
    if(input$maille_TERR %in% 'DEP') {
      indics_migres_TERR %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        mutate(filtre_TERR = case_when(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP) ~ "TERR", TRUE ~ "RESTE"))
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      indics_migres_TERR %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        mutate(filtre_TERR = case_when(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI) ~ "TERR", TRUE ~ "RESTE"))
      
    } else if ( input$maille_TERR %in% 'REG') {
      indics_migres_TERR %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        mutate(filtre_TERR = case_when(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_REG) ~ "TERR", TRUE ~ "RESTE"))
    }
  })
  
  
  output$nuagepoint_terr <- renderggiraph({
    
  p1 =
    ggplot() +
    geom_abline(intercept = 0, slope = 1, color = "grey40",linetype = "dashed") +
    geom_point_interactive(data= filteredData_nuagepoint_mig() %>%
                             mutate(LIB_TERR = stri_trans_general(conv_accents(LIB_TERR),"Latin-ASCII")) %>%
                             mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                 "<b>", LIB_TERR ,"</b>", "<br>",
                                                 "<font size=2.5 color=white family=Roboto>" , PE  %>% percent(.,accuracy = 0.1), " d&apos;entrants","</font>", "<br>",
                                                 "<font size=2.5 color=white family=Roboto>" , PS  %>% percent(.,accuracy = 0.1), " de sortants","</font>", "<br>"
                                                 #"<img src = ",paste0('"' ,logo_png,'"'), " height=\"30\"width=\"30\">", "<br>",
                             )) ,
                           aes(x = PS , y =PE ,
                               size =nb_ind_PRES,
                               tooltip = tip,
                               data_id = TERR,
                               fill=TM),
                           stat = "identity", color = "grey60", shape = 21, stroke = 0.6
    ) +
    geom_point(data= filteredData_nuagepoint_mig() %>%
                 filter(filtre_TERR %in% "TERR"),
               aes(x = PS , y =PE ,
                   size =nb_ind_PRES),
               fill=NA,
               stat = "identity", color = "black", shape = 21, stroke = 1.8
    ) +
    scale_y_continuous(name = "Part d'entrants", limits=c(0,filteredData_nuagepoint_mig() %>% select(PS) %>% max() %>% rbind(filteredData_nuagepoint_mig() %>% select(PE) %>% max()) %>% max()), labels = percent_format(accuracy = 0.1)) +
    scale_x_continuous(name = "Part de sortants", limits=c(0,filteredData_nuagepoint_mig() %>% select(PS) %>% max() %>% rbind(filteredData_nuagepoint_mig() %>% select(PE) %>% max()) %>% max()), labels = percent_format(accuracy = 0.1)) +
    scale_size_continuous(name = "Population", breaks=c(10000,100000,1000000), labels=function(x) format(x, big.mark = " ", scientific = FALSE) ) +
    scale_fill_gradient2(name = "Solde migratoire\nrapporté à la\npopulation",low = "#0a3470", mid = "white", high = "#91141a", midpoint = 0, labels = percent_format(accuracy = 0.1)) +
    guides(fill = guide_legend(reverse=T)) +
    theme(strip.text.y = element_text(angle = 360),
          text = element_text(family = "Roboto", color = "black"),
          panel.background = element_rect(fill = NA, color = NA),
          plot.background = element_rect(fill = NA, color = NA),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.line =  element_line( color = "grey"),
          panel.grid.major = element_line(color = "#2b2b2b99",size = 0.1),
          panel.grid.minor = element_line(color = "#2b2b2b99", size = 0.05),
          legend.direction = "horizontal",
          legend.title=element_text(size=8), 
          legend.text=element_text(size=7),
          legend.position = "bottom")
  
  
  tooltip_css <- "background-color:#272B30;padding:2px;font-size: 120%;color: white;opacity:0.2"
  x <- girafe(ggobj = p1, width = 1, height_svg = 6 )
  girafe_options(x, 
                 opts_tooltip(use_fill = FALSE, offx = 10, offy = -10, use_cursor_pos = TRUE, css = tooltip_css),
                 opts_hover(css = "stroke:red;r:20pt;"),
                 opts_selection(type = "none"),
                 opts_zoom(max = 1),
                 opts_toolbar(position = "bottomleft", saveaspng = FALSE) )
  
  })
  
  # filtre sur dataframe par CS
  
  #stats de profil par TERR  ####
  
  
  filteredData_profilpop_CS1 <- reactive({
    if(input$maille_TERR %in% 'DEP') {
      indics_migres_TERR_CS1 %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP) ) %>%
        select(TERR, LIB_TERR, CS1, nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES) %>%
        mutate_at(.vars = vars(nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES), .funs = funs( ./(sum(.)) ) ) %>%
        ajout_libelles_varventil_insee(TABLE = .,
                                       VAR ="CS1",
                                       MILLESIME_RP = 2015) %>%
        mutate(CS1_LIB= factor(CS1_LIB, levels=rev(unique(CS1_LIB[order(CS1)])), ordered=TRUE) )
      
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      indics_migres_TERR_CS1 %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI) ) %>%
        select(TERR, LIB_TERR, CS1, nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES) %>%
        mutate_at(.vars = vars(nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES), .funs = funs( ./(sum(.)) ) ) %>%
        ajout_libelles_varventil_insee(TABLE = .,
                                       VAR ="CS1",
                                       MILLESIME_RP = 2015) %>%
        mutate(CS1_LIB= factor(CS1_LIB, levels=rev(unique(CS1_LIB[order(CS1)])), ordered=TRUE) ) 
      
    } else if ( input$maille_TERR %in% 'REG') {
      indics_migres_TERR_CS1 %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_REG) ) %>%
        select(TERR, LIB_TERR, CS1, nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES) %>%
        mutate_at(.vars = vars(nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES), .funs = funs( ./(sum(.)) ) ) %>%
        ajout_libelles_varventil_insee(TABLE = .,
                                       VAR ="CS1",
                                       MILLESIME_RP = 2015) %>%
        mutate(CS1_LIB= factor(CS1_LIB, levels=rev(unique(CS1_LIB[order(CS1)])), ordered=TRUE) ) 
    }
  })
  
  # dataframe profil moyen CS
  filteredData_profilpop_CS1_MOY <- reactive({
    
    indics_migres_TERR_CS1 %>%
      as.data.frame() %>%
      filter(NIV_TERR %in% input$maille_TERR) %>%
      select(NIV_TERR,TERR, CS1, nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES) %>%
      group_by(TERR) %>%
      mutate_at(.vars = vars(nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES), .funs = funs( ./(sum(.)) ) ) %>%
      ungroup() %>%
      group_by(NIV_TERR, CS1) %>%
      summarise_if(is.numeric, funs(mean)) %>%
      ajout_libelles_varventil_insee(TABLE = .,
                                     VAR ="CS1",
                                     MILLESIME_RP = 2015)
    
  })
  
  
  # visu graphique profil par CS
  output$diagcirc_profil_pop_CS1 <- renderggiraph({
    
    if(input$type_pop %in% 'PRES') {
      
      VAR_type_pop = "nb_ind_PRES"
      LIB_type_pop = "pr&eacute;sente"
      
    } else if (input$type_pop %in% 'ENTR') {
      
      VAR_type_pop = "nb_ind_ENTR"
      LIB_type_pop = "entrante"    
      
    } else if (input$type_pop %in% 'SORT') {
      
      VAR_type_pop = "nb_ind_SORT"
      LIB_type_pop = "sortante"      
    }
    
    # graphique circulaire profil moyen
    
    gg_diagcirc_TERR <- ggplot(data = filteredData_profilpop_CS1() %>%
                                 arrange(CS1) %>%
                                 rename(value = !!sym(VAR_type_pop)) %>%
                                 add_row(  "CS1_LIB" ="9",    "value" = 0.4) %>%
                                 mutate(value_pct1 = value / sum(value)) %>%
                                 mutate(pos = cumsum(value_pct1)- value_pct1/2) %>%
                                 mutate(CS1_LIB = conv_accents(CS1_LIB)) %>% 
                                 mutate(CS1_LIB = factor(CS1_LIB, levels = c("Agriculteurs exploitants","Artisans, commer&ccedil;ants et chefs d&apos;entreprise","Cadres et professions intellectuelles sup&eacute;rieures","Professions Interm&eacute;diaires",
                                                                             "Employ&eacute;s","Ouvriers","Retrait&eacute;s","Autres personnes sans activit&eacute; professionnelle","9"))) %>% 
                                 mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                     "<b>","<font size=2.5 color=white family=Roboto>" , CS1_LIB,"</b>","</font>", "<br>",
                                                     "<font size=2.2 color=white>", percent(value, accuracy = 2), " de la population ",LIB_type_pop, "<br>"
                                                     #"<img src = ",paste0('"' ,logo_png,'"'), " height=\"30\"width=\"30\">", "<br>",
                                 ))) +
      geom_bar_interactive(aes(x=2, y=value_pct1, fill=CS1_LIB,
                               tooltip = tip,
                               data_id = CS1_LIB),
                           stat="identity")+
      xlim(0.5, 2.5) +
      theme_nothing() +
      theme(text = element_text(family = "Roboto")) +
      geom_text(aes(x = 2.1, y = 0.04, label = "Profil du\nterritoire"), size = 5.5,  color = "black", angle = 30) +
      geom_label(aes(x = 0.5, y = 0.5, label = "par groupe\nsocioprofessionnel"), fontface = "bold", fill = "white") +
      coord_polar(theta = "y", direction = -1,start = 0.6) +
      scale_fill_manual(guide = FALSE, values = c("#00B050","#984807","#558ED5","#7030A0","#E46C0A","#FF0000","#969697","#404040",NA)) 
    
    
    # graphique circulaire profil moyen
    gg_diagcirc_MOY <- ggplot(data = filteredData_profilpop_CS1_MOY() %>%
                                as.data.frame() %>%
                                arrange(CS1) %>%
                                rename(value = !!sym(VAR_type_pop)) %>%
                                add_row(  "CS1_LIB" ="9",    "value" = 0.4) %>%
                                mutate(value_pct1 = value / sum(value)) %>%
                                mutate(pos = cumsum(value_pct1)- value_pct1/2) %>%
                                mutate(CS1_LIB = conv_accents(CS1_LIB)) %>% 
                                mutate(CS1_LIB = factor(CS1_LIB, levels = c("Agriculteurs exploitants","Artisans, commer&ccedil;ants et chefs d&apos;entreprise","Cadres et professions intellectuelles sup&eacute;rieures","Professions Interm&eacute;diaires",
                                                                            "Employ&eacute;s","Ouvriers","Retrait&eacute;s","Autres personnes sans activit&eacute; professionnelle","9"))) %>% 
                                mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                    "<b>","<font size=2.5 color=white family=Roboto>" , CS1_LIB,"</b>","</font>", "<br>",
                                                    "<font size=2.2 color=white>", percent(value, accuracy = 2), " de la population ",LIB_type_pop, "<br>"
                                                    #"<img src = ",paste0('"' ,logo_png,'"'), " height=\"30\"width=\"30\">", "<br>",
                                ))) +
      geom_bar_interactive(aes(x=2, y=value_pct1, fill=CS1_LIB,
                               tooltip = tip,
                               data_id = CS1_LIB),
                           stat="identity")+
      scale_x_continuous(limits = c(-3.5, 2.5)) +
      theme_nothing() +
      theme(text = element_text(family = "Roboto")) +
      geom_text(aes(x = 2.1, y = 0.04, label = "Moyenne \nFrance"), size = 3.3,  color = "black", angle = 30) +
      
      geom_label(aes(x = -3, y = 0.65, label = "par groupe\nsocioprofessionnel"), size = 3.5, fill = "black") +
      coord_polar(theta = "y", direction = -1,start = 0.6) +
      scale_fill_manual(guide = FALSE, values = c("#00B050","#984807","#558ED5","#7030A0","#E46C0A","#FF0000","#969697","#404040",NA)) 
    
    
    # graphique complet ggiraph
    tooltip_css <- "background-color:white;padding:2px;font-size: 80%;color: white;opacity:0.2"
    x <- girafe(code = print(ggdraw() +
                               draw_plot(gg_diagcirc_MOY + theme(legend.justification = "bottom"), x = 0.21, y = 0.28, width = 0.58, height = 0.44) +
                               draw_plot(gg_diagcirc_TERR + theme(legend.justification = "top"), 0, 0, 1, 1) ), width = 1, height_svg = 6 )
    girafe_options(x, 
                   opts_tooltip(css = tooltip_css),
                   opts_tooltip(offx = -40, offy = -30, use_cursor_pos = TRUE),
                   opts_tooltip(use_fill = TRUE),
                   opts_hover(css = "stroke:red;r:10pt;"),
                   opts_selection(type = "none"),
                   opts_zoom(max = 1),
                   opts_toolbar(position = "bottomleft", saveaspng = FALSE) )
    
  })  
  
  ### dataframe sur profil de la pop par TRAGE
  
  filteredData_profilpop_AGEREVS <- reactive({
    if(input$maille_TERR %in% 'DEP') {
      indics_migres_TERR_AGEREVS %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP) ) %>%
        select(TERR, LIB_TERR, AGEREVS, nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES) %>%
        mutate_at(.vars = vars(nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES), .funs = funs( ./(sum(.)) ) ) %>%
        mutate(AGEREVS = factor(AGEREVS, levels = c("< 20 ans","20-29 ans","30-39 ans","40-49 ans","50-64 ans","> 65 ans")))
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      indics_migres_TERR_AGEREVS %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI) ) %>%
        select(TERR, LIB_TERR, AGEREVS, nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES) %>%
        mutate_at(.vars = vars(nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES), .funs = funs( ./(sum(.)) ) ) %>%
        mutate(AGEREVS = factor(AGEREVS, levels = c("< 20 ans","20-29 ans","30-39 ans","40-49 ans","50-64 ans","> 65 ans")))
      
    } else if ( input$maille_TERR %in% 'REG') {
      indics_migres_TERR_AGEREVS %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_REG) ) %>%
        select(TERR, LIB_TERR, AGEREVS, nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES) %>%
        mutate_at(.vars = vars(nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES), .funs = funs( ./(sum(.)) ) ) %>%
        mutate(AGEREVS = factor(AGEREVS, levels = c("< 20 ans","20-29 ans","30-39 ans","40-49 ans","50-64 ans","> 65 ans")))
    }
  })
  

  # dataframe profil moyen CS
  filteredData_profilpop_AGEREVS_MOY <- reactive({
    
    indics_migres_TERR_AGEREVS %>%
      as.data.frame() %>%
      filter(NIV_TERR %in% input$maille_TERR) %>%
      select(NIV_TERR,TERR, AGEREVS, nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES) %>%
      group_by(TERR) %>%
      mutate_at(.vars = vars(nb_ind_ENTR, nb_ind_SORT, nb_ind_PRES), .funs = funs( ./(sum(.)) ) ) %>%
      ungroup() %>%
      group_by(NIV_TERR, AGEREVS) %>%
      summarise_if(is.numeric, funs(mean)) 
    
  })
  
  
  # visu graphique profil par CS
  output$diagcirc_profil_pop_AGEREVS <- renderggiraph({
    
    if(input$type_pop %in% 'PRES') {
      
      VAR_type_pop = "nb_ind_PRES"
      LIB_type_pop = "pr&eacute;sente"
      
    } else if (input$type_pop %in% 'ENTR') {
      
      VAR_type_pop = "nb_ind_ENTR"
      LIB_type_pop = "entrante"    
      
    } else if (input$type_pop %in% 'SORT') {
      
      VAR_type_pop = "nb_ind_SORT"
      LIB_type_pop = "sortante"      
    }
    
    # graphique circulaire profil moyen
    
    gg_diagcirc_TERR = ggplot(data = filteredData_profilpop_AGEREVS() %>%
                                arrange(AGEREVS) %>%
                                rename(value = !!sym(VAR_type_pop)) %>%
                                add_row(  "AGEREVS" ="7",    "value" = 0.4) %>%
                                mutate(value_pct1 = value / sum(value)) %>%
                                mutate(pos = cumsum(value_pct1)- value_pct1/2) %>%
                                mutate(AGEREVS = factor(AGEREVS, levels = c("< 20 ans","20-29 ans","30-39 ans","40-49 ans","50-64 ans","> 65 ans","0"))) %>% 
                                mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                    "<b>","<font size=2.5 color=white family=Roboto>" , AGEREVS,"</b>","</font>", "<br>",
                                                    "<font size=2.2 color=white>", percent(value, accuracy = 2), " de la population ",LIB_type_pop, "<br>"
                                                    #"<img src = ",paste0('"' ,logo_png,'"'), " height=\"30\"width=\"30\">", "<br>",
                                ))) +
      geom_bar_interactive(aes(x=2, y=value_pct1, fill=AGEREVS,
                               tooltip = tip,
                               data_id = AGEREVS),
                           stat="identity")+
      xlim(0.5, 2.5) +
      theme_nothing() +
      theme(text = element_text(family = "Roboto")) +
      geom_text(aes(x = 2.1, y = 0.04, label = "Profil du\nterritoire"), size = 5.5,  color = "black", angle = 30) +
       geom_label(aes(x = 0.5, y = 0.5, label = "par tranche\nd'âge"), fontface = "bold", fill = "white") +
      coord_polar(theta = "y", direction = -1,start = 0.6) +
      scale_fill_manual(guide = FALSE, values = c("#8c510a", "#d8b365", "#dfc27d", "#80cdc1", "#5ab4ac", "#01665e",NA)) 
    
    
    # graphique circulaire profil moyen
    gg_diagcirc_MOY = ggplot(data = filteredData_profilpop_AGEREVS_MOY() %>%
                               as.data.frame() %>%
                               arrange(AGEREVS) %>%
                               rename(value = !!sym(VAR_type_pop)) %>%
                               add_row(  "AGEREVS" ="7",    "value" = 0.4) %>%
                               mutate(value_pct1 = value / sum(value)) %>%
                               mutate(pos = cumsum(value_pct1)- value_pct1/2) %>%
                               mutate(AGEREVS = factor(AGEREVS, levels = c("< 20 ans","20-29 ans","30-39 ans","40-49 ans","50-64 ans","> 65 ans","0"))) %>% 
                               mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                   "<b>","<font size=2.5 color=white family=Roboto>" , AGEREVS,"</b>","</font>", "<br>",
                                                   "<font size=2.2 color=white>", percent(value, accuracy = 2), " de la population ",LIB_type_pop, "<br>"
                                                   #"<img src = ",paste0('"' ,logo_png,'"'), " height=\"30\"width=\"30\">", "<br>",
                               ))) +
      geom_bar_interactive(aes(x=2, y=value_pct1, fill=AGEREVS,
                               tooltip = tip,
                               data_id = AGEREVS),
                           stat="identity")+
      scale_x_continuous(limits = c(-3.5, 2.5)) +
      theme_nothing() +
      theme(text = element_text(family = "Roboto")) +
      geom_text(aes(x = 2.1, y = 0.04, label = "Moyenne\nFrance"), size = 3.3,  color = "black", angle = 30) +
      
       geom_label(aes(x = -3, y = 0.65, label = "par tranche\nd'âge"), size = 3.5, fill = "white") +
      coord_polar(theta = "y", direction = -1,start = 0.6) +
      scale_fill_manual(guide = FALSE, values = c("#8c510a", "#d8b365", "#dfc27d", "#80cdc1", "#5ab4ac", "#01665e",NA))  
    
    
    # graphique complet ggiraph
    tooltip_css <- "background-color:white;padding:2px;font-size: 80%;color: white;opacity:0.2"
    x <- girafe(code = print(ggdraw() +
                               draw_plot(gg_diagcirc_MOY + theme(legend.justification = "bottom"), x = 0.21, y = 0.28, width = 0.58, height = 0.44) +
                               draw_plot(gg_diagcirc_TERR + theme(legend.justification = "top"), 0, 0, 1, 1) ),
                width = 1, height_svg = 6 
    )
    girafe_options(x, 
                   opts_tooltip(css = tooltip_css),
                   opts_tooltip(offx = -40, offy = -30, use_cursor_pos = TRUE),
                   opts_tooltip(use_fill = TRUE),
                   opts_hover(css = "stroke:red;r:10pt;"),
                   opts_selection(type = "none"),
                   opts_zoom(max = 1),
                   opts_toolbar(position = "bottomleft", saveaspng = FALSE) )
    
    
    
  })  
  
  
  
  #### renouvellement / evolution de la pop due aux migrations  ####
  
  # filtre sur dataframe par CS
  #stats de profil par TERR
  
  
  filteredData_evolmigpop_CS1 <- reactive({
    if(input$maille_TERR %in% 'DEP') {
      indics_migres_TERR_CS1_RENOUV %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP) ) %>%
        ajout_libelles_varventil_insee(TABLE = . ,
                                       VAR ="CS1",
                                       MILLESIME_RP = 2015) %>%
        spread(type_indice, valeur) %>%
        filter(!CS1 %in% 'GLOBAL')
      
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      indics_migres_TERR_CS1_RENOUV %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI) ) %>%
        ajout_libelles_varventil_insee(TABLE = . ,
                                       VAR ="CS1",
                                       MILLESIME_RP = 2015) %>%
        spread(type_indice, valeur) %>%
        filter(!CS1 %in% 'GLOBAL')
      
    } else if ( input$maille_TERR %in% 'REG') {
      indics_migres_TERR_CS1_RENOUV %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_REG) ) %>%
        ajout_libelles_varventil_insee(TABLE = . ,
                                       VAR ="CS1",
                                       MILLESIME_RP = 2015) %>%
        spread(type_indice, valeur) %>%
        filter(!CS1 %in% 'GLOBAL')
    }
  })
  
  # dataframe profil moyen CS
  
  filteredData_evolmigpop_CS1_MOY <- reactive({
    if(input$maille_TERR %in% 'DEP') {
      indics_migres_TERR_CS1_RENOUV %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        spread(type_indice, valeur) %>%
        filter(!CS1 %in% 'GLOBAL') %>%
        group_by(CS1) %>%
        summarise_at(.vars = vars( pct_ind_PRES = pct_ind_PRES),
                     .funs = funs(max, min,moy= mean, 
                                  q1 = quantile(.,probs = 0.2), 
                                  q2 = quantile(.,probs = 0.4), 
                                  q3 = quantile(.,probs = 0.6), 
                                  q4 = quantile(.,probs = 0.8) )) %>%
        mutate(evol_pct_AUTO_PRES_q1 = -0.0015, 
               evol_pct_AUTO_PRES_q2 = -0.0005, 
               evol_pct_AUTO_PRES_q3 = 0.0005, 
               evol_pct_AUTO_PRES_q4 = 0.0015)
      
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      indics_migres_TERR_CS1_RENOUV %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        spread(type_indice, valeur) %>%
        filter(!CS1 %in% 'GLOBAL') %>%
        group_by(CS1) %>%
        summarise_at(.vars = vars( pct_ind_PRES = pct_ind_PRES),
                     .funs = funs(max, min,moy= mean, 
                                  q1 = quantile(.,probs = 0.2), 
                                  q2 = quantile(.,probs = 0.4), 
                                  q3 = quantile(.,probs = 0.6), 
                                  q4 = quantile(.,probs = 0.8) )) %>%
        mutate(evol_pct_AUTO_PRES_q1 = -0,005, 
               evol_pct_AUTO_PRES_q2 = -0,001, 
               evol_pct_AUTO_PRES_q3 = 0,001, 
               evol_pct_AUTO_PRES_q4 = 0,005)
      
    } else if ( input$maille_TERR %in% 'REG') {
      indics_migres_TERR_CS1_RENOUV %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        spread(type_indice, valeur) %>%
        filter(!CS1 %in% 'GLOBAL') %>%
        group_by(CS1) %>%
        summarise_at(.vars = vars( pct_ind_PRES = pct_ind_PRES),
                     .funs = funs(max, min,moy= mean, 
                                  q1 = quantile(.,probs = 0.2), 
                                  q2 = quantile(.,probs = 0.4), 
                                  q3 = quantile(.,probs = 0.6), 
                                  q4 = quantile(.,probs = 0.8) )) %>%
        mutate(evol_pct_AUTO_PRES_q1 = -0.0005, 
               evol_pct_AUTO_PRES_q2 = -0.0002, 
               evol_pct_AUTO_PRES_q3 = 0.0002, 
               evol_pct_AUTO_PRES_q4 = 0.0005)
    }
  })
  
  
  # visu graphique profil par CS
  output$diag_evolmig_profilpop_CS1 <- renderggiraph({
    
    
    indics_renouv_CS1_terr_vs_moy <-
      filteredData_evolmigpop_CS1() %>%
      left_join(filteredData_evolmigpop_CS1_MOY() , by = "CS1") %>%
      mutate(Q_pct_ind_PRES = case_when(pct_ind_PRES <= pct_ind_PRES_q1 ~ "Q1",
                                        between(pct_ind_PRES, pct_ind_PRES_q1,pct_ind_PRES_q2) ~ "Q2",
                                        between(pct_ind_PRES, pct_ind_PRES_q2,pct_ind_PRES_q3) ~ "Q3",
                                        between(pct_ind_PRES, pct_ind_PRES_q3,pct_ind_PRES_q4) ~ "Q4",
                                        pct_ind_PRES >= pct_ind_PRES_q4 ~ "Q5"    ),
             Q_evol_pct_AUTO_PRES = case_when(evol_pct_AUTO_PRES <= evol_pct_AUTO_PRES_q1 ~ "Q1",
                                              between(evol_pct_AUTO_PRES, evol_pct_AUTO_PRES_q1,evol_pct_AUTO_PRES_q2) ~ "Q2",
                                              between(evol_pct_AUTO_PRES, evol_pct_AUTO_PRES_q2,evol_pct_AUTO_PRES_q3) ~ "Q3",
                                              between(evol_pct_AUTO_PRES, evol_pct_AUTO_PRES_q3,evol_pct_AUTO_PRES_q4) ~ "Q4",
                                              evol_pct_AUTO_PRES >= evol_pct_AUTO_PRES_q4 ~ "Q5"    )) %>%
      #  libellés quantiles en clair
      mutate(lib_Q_pct_ind_PRES = case_when(Q_pct_ind_PRES %in% 'Q1' ~ "très faible par rapport",
                                            Q_pct_ind_PRES %in% 'Q2' ~ "relativement faible par rapport",
                                            Q_pct_ind_PRES %in% 'Q3' ~ "identique",
                                            Q_pct_ind_PRES %in% 'Q4' ~ "relativement élevée par rapport",
                                            Q_pct_ind_PRES %in% 'Q5' ~ "très élevée par rapport"),
             lib_Q_evol_pct_AUTO_PRES = case_when(Q_evol_pct_AUTO_PRES %in% 'Q1' ~ "diminue fortement",
                                                  Q_evol_pct_AUTO_PRES %in% 'Q2' ~ "diminue légérèment",
                                                  Q_evol_pct_AUTO_PRES %in% 'Q3' ~ "reste stable",
                                                  Q_evol_pct_AUTO_PRES %in% 'Q4' ~ "augmente légérèment",
                                                  Q_evol_pct_AUTO_PRES %in% 'Q5' ~ "augmente fortement") )
    
    
    p1 =
      ggplot(data=indics_renouv_CS1_terr_vs_moy  %>%
               mutate(CS1_LIB = conv_accents(CS1_LIB)) %>%
               mutate(CS1_LIB= factor(CS1_LIB, levels=rev(unique(CS1_LIB[order(CS1)])), ordered=TRUE) ) %>%
               mutate(lib_Q_pct_ind_PRES = conv_accents(lib_Q_pct_ind_PRES)) %>%
               mutate(lib_Q_evol_pct_AUTO_PRES = conv_accents(lib_Q_evol_pct_AUTO_PRES)) %>%
               #mutate(tip = CS1_LIB
               mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                   "<font size=2.5 color=white family=Roboto>" , "La part du groupe ", CS1_LIB," est ","<b>",lib_Q_pct_ind_PRES,"</b>"," (",percent(pct_ind_PRES, accuracy = 1),")", " aux autres territoires de France (",percent(pct_ind_PRES_moy, accuracy = 1), ").</b>","</font>", "<br>",
                                   "<font size=2.5 color=white family=Roboto>" , "Par le jeu des migrations internes, cette part ","<b>",lib_Q_evol_pct_AUTO_PRES, "</b>"," (",
                                   paste0(symnum(evol_pct_AUTO_PRES,
                                                 c(-Inf, 0, Inf),
                                                 c("", "+")),
                                          percent(evol_pct_AUTO_PRES, accuracy = 0.01),""),
                                   ")","</font>", "<br>"
               ))) +
      geom_vline(xintercept = 0, color = "grey40",linetype = "dashed") +
      geom_segment_interactive(aes(x=0, y=pct_ind_PRES,xend = evol_pct_AUTO_PRES, yend = pct_ind_PRES , color = CS1_LIB,
                                   tooltip = tip,
                                   data_id = CS1_LIB),
                               arrow = arrow(angle = 15, ends = "last", type = "closed", length = unit(0.5, "cm")),
                               size = 1.5, alpha = 1) +
      geom_point_interactive(aes(x=0, y=pct_ind_PRES, fill = CS1_LIB,
                                 tooltip = tip,
                                 data_id = CS1_LIB),
                             stat = "identity", shape = 21, size = 3, alpha = 1, stroke = 1.5) +
      scale_y_continuous( name = 'Part de la population présente faisant partie du groupe socio-professionnel', labels = percent_format(accuracy = 1)) +
      scale_x_continuous( name = 'Evolution de la part de la catégorie par les migrations',
                          limits=c(-(indics_renouv_CS1_terr_vs_moy %>% select(evol_pct_AUTO_PRES) %>% min() %>% abs() %>% rbind(indics_renouv_CS1_terr_vs_moy %>% select(evol_pct_AUTO_PRES) %>% max()) %>% max()),
                                   indics_renouv_CS1_terr_vs_moy %>% select(evol_pct_AUTO_PRES) %>% min() %>% abs() %>% rbind(indics_renouv_CS1_terr_vs_moy %>% select(evol_pct_AUTO_PRES) %>% max()) %>% max()),
                          labels = percent_format(accuracy = 0.01)) +
      scale_fill_manual(guide = FALSE, values = rev(c("#00B050","#984807","#558ED5","#7030A0","#E46C0A","#FF0000","#969697","#404040"))) +
      scale_color_manual(guide = FALSE, values = rev(c("#00B050","#984807","#558ED5","#7030A0","#E46C0A","#FF0000","#969697","#404040"))) +
      labs(
        title = "Par groupe socio-professionnel"#,
      ) +
      theme(strip.text.y = element_text(angle = 360),
            text = element_text(family = "Roboto", color = "black"),
            panel.background = element_rect(fill = NA, color = NA),
            plot.background = element_rect(fill = NA, color = NA),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            axis.line =  element_line( color = "grey"),
            panel.grid.major = element_line(color = "#2b2b2b99",size = 0.1),
            panel.grid.minor = element_line(color = "#2b2b2b99", size = 0.05),
            legend.position = "bottom")
    
    
    tooltip_css <- "background-color:#272B30;padding:2px;font-size: 80%;color: white;opacity:0.2"
    x <- girafe(ggobj = p1, width = 1, height_svg = 6 )
    girafe_options(x, 
                   opts_tooltip(use_fill = FALSE, offx = 40, offy = 480, use_cursor_pos = FALSE, css = tooltip_css),
                   opts_hover(css = "stroke:red;r:10pt;"),
                   opts_selection(type = "none"),
                   opts_zoom(max = 1),
                   opts_toolbar(position = "bottomleft", saveaspng = FALSE) )
    
    
    
  })  
  
  
  
  # filtre sur dataframe par AGEREVS
  #stats de profil par TERR
  
  
  filteredData_evolmigpop_AGEREVS <- reactive({
    if(input$maille_TERR %in% 'DEP') {
      indics_migres_TERR_AGEREVS_RENOUV %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP) ) %>%
        spread(type_indice, valeur) %>%
        filter(!AGEREVS %in% 'GLOBAL')
      
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      indics_migres_TERR_AGEREVS_RENOUV %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI) ) %>%
        spread(type_indice, valeur) %>%
        filter(!AGEREVS %in% 'GLOBAL')
      
    } else if ( input$maille_TERR %in% 'REG') {
      indics_migres_TERR_AGEREVS_RENOUV %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_REG) ) %>%
        spread(type_indice, valeur) %>%
        filter(!AGEREVS %in% 'GLOBAL')
    }
  })
  
  # dataframe profil moyen AGEREVS
  
  filteredData_evolmigpop_AGEREVS_MOY <- reactive({
    if(input$maille_TERR %in% 'DEP') {
      indics_migres_TERR_AGEREVS_RENOUV %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        spread(type_indice, valeur) %>%
        filter(!AGEREVS %in% 'GLOBAL') %>%
        group_by(AGEREVS) %>%
        summarise_at(.vars = vars( pct_ind_PRES = pct_ind_PRES),
                     .funs = funs(max, min,moy= mean, 
                                  q1 = quantile(.,probs = 0.2), 
                                  q2 = quantile(.,probs = 0.4), 
                                  q3 = quantile(.,probs = 0.6), 
                                  q4 = quantile(.,probs = 0.8) )) %>%
        mutate(evol_pct_AUTO_PRES_q1 = -0.0015, 
               evol_pct_AUTO_PRES_q2 = -0.0005, 
               evol_pct_AUTO_PRES_q3 = 0.0005, 
               evol_pct_AUTO_PRES_q4 = 0.0015)
      
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      indics_migres_TERR_AGEREVS_RENOUV %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        spread(type_indice, valeur) %>%
        filter(!AGEREVS %in% 'GLOBAL') %>%
        group_by(AGEREVS) %>%
        summarise_at(.vars = vars( pct_ind_PRES = pct_ind_PRES),
                     .funs = funs(max, min,moy= mean, 
                                  q1 = quantile(.,probs = 0.2), 
                                  q2 = quantile(.,probs = 0.4), 
                                  q3 = quantile(.,probs = 0.6), 
                                  q4 = quantile(.,probs = 0.8) )) %>%
        mutate(evol_pct_AUTO_PRES_q1 = -0,005, 
               evol_pct_AUTO_PRES_q2 = -0,001, 
               evol_pct_AUTO_PRES_q3 = 0,001, 
               evol_pct_AUTO_PRES_q4 = 0,005)
      
    } else if ( input$maille_TERR %in% 'REG') {
      indics_migres_TERR_AGEREVS_RENOUV %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        spread(type_indice, valeur) %>%
        filter(!AGEREVS %in% 'GLOBAL') %>%
        group_by(AGEREVS) %>%
        summarise_at(.vars = vars( pct_ind_PRES = pct_ind_PRES),
                     .funs = funs(max, min,moy= mean, 
                                  q1 = quantile(.,probs = 0.2), 
                                  q2 = quantile(.,probs = 0.4), 
                                  q3 = quantile(.,probs = 0.6), 
                                  q4 = quantile(.,probs = 0.8) )) %>%
        mutate(evol_pct_AUTO_PRES_q1 = -0.0005, 
               evol_pct_AUTO_PRES_q2 = -0.0002, 
               evol_pct_AUTO_PRES_q3 = 0.0002, 
               evol_pct_AUTO_PRES_q4 = 0.0005)
    }
  })
  
  
  # visu graphique profil par AGEREVS
  output$diag_evolmig_profilpop_AGEREVS <- renderggiraph({
    
    
    indics_renouv_AGEREVS_terr_vs_moy <-
      filteredData_evolmigpop_AGEREVS() %>%
      left_join(filteredData_evolmigpop_AGEREVS_MOY() , by = "AGEREVS") %>%
      mutate(Q_pct_ind_PRES = case_when(pct_ind_PRES <= pct_ind_PRES_q1 ~ "Q1",
                                        between(pct_ind_PRES, pct_ind_PRES_q1,pct_ind_PRES_q2) ~ "Q2",
                                        between(pct_ind_PRES, pct_ind_PRES_q2,pct_ind_PRES_q3) ~ "Q3",
                                        between(pct_ind_PRES, pct_ind_PRES_q3,pct_ind_PRES_q4) ~ "Q4",
                                        pct_ind_PRES >= pct_ind_PRES_q4 ~ "Q5"    ),
             Q_evol_pct_AUTO_PRES = case_when(evol_pct_AUTO_PRES <= evol_pct_AUTO_PRES_q1 ~ "Q1",
                                              between(evol_pct_AUTO_PRES, evol_pct_AUTO_PRES_q1,evol_pct_AUTO_PRES_q2) ~ "Q2",
                                              between(evol_pct_AUTO_PRES, evol_pct_AUTO_PRES_q2,evol_pct_AUTO_PRES_q3) ~ "Q3",
                                              between(evol_pct_AUTO_PRES, evol_pct_AUTO_PRES_q3,evol_pct_AUTO_PRES_q4) ~ "Q4",
                                              evol_pct_AUTO_PRES >= evol_pct_AUTO_PRES_q4 ~ "Q5"    )) %>%
      #  libellés quantiles en clair
      mutate(lib_Q_pct_ind_PRES = case_when(Q_pct_ind_PRES %in% 'Q1' ~ "très faible par rapport",
                                            Q_pct_ind_PRES %in% 'Q2' ~ "relativement faible par rapport",
                                            Q_pct_ind_PRES %in% 'Q3' ~ "identique",
                                            Q_pct_ind_PRES %in% 'Q4' ~ "relativement élevée par rapport",
                                            Q_pct_ind_PRES %in% 'Q5' ~ "très élevée par rapport"),
             lib_Q_evol_pct_AUTO_PRES = case_when(Q_evol_pct_AUTO_PRES %in% 'Q1' ~ "diminue fortement",
                                                  Q_evol_pct_AUTO_PRES %in% 'Q2' ~ "diminue légérèment",
                                                  Q_evol_pct_AUTO_PRES %in% 'Q3' ~ "reste stable",
                                                  Q_evol_pct_AUTO_PRES %in% 'Q4' ~ "augmente légérèment",
                                                  Q_evol_pct_AUTO_PRES %in% 'Q5' ~ "augmente fortement") )
    
    
    p1 =
      ggplot(data=indics_renouv_AGEREVS_terr_vs_moy  %>%
               mutate(AGEREVS = factor(AGEREVS, levels = c("< 20 ans","20-29 ans","30-39 ans","40-49 ans","50-64 ans","> 65 ans"))) %>%
               mutate(lib_Q_pct_ind_PRES = conv_accents(lib_Q_pct_ind_PRES)) %>%
               mutate(lib_Q_evol_pct_AUTO_PRES = conv_accents(lib_Q_evol_pct_AUTO_PRES)) %>%
               mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                   "<font size=2.5 color=white family=Roboto>" , "La part du groupe ", AGEREVS," est ","<b>",lib_Q_pct_ind_PRES,"</b>"," (",percent(pct_ind_PRES, accuracy = 1),")", " aux autres territoires de France (",percent(pct_ind_PRES_moy, accuracy = 1), ").</b>","</font>", "<br>",
                                   "<font size=2.5 color=white family=Roboto>" , "Par le jeu des migrations internes, cette part ","<b>",lib_Q_evol_pct_AUTO_PRES, "</b>"," (",
                                   # format pourcentage avec signe
                                   paste0(symnum(evol_pct_AUTO_PRES,
                                                 c(-Inf, 0, Inf),
                                                 c("", "+")),
                                          percent(evol_pct_AUTO_PRES, accuracy = 0.01),""),
                                   ")","</font>", "<br>"
                                   #"<img src = ",paste0('"' ,logo_png,'"'), " height=\"30\"width=\"30\">", "<br>",
               ))) +
      geom_vline(xintercept = 0, color = "grey40",linetype = "dashed") +
      geom_segment_interactive(aes(x=0, y=pct_ind_PRES,xend = evol_pct_AUTO_PRES, yend = pct_ind_PRES , color = AGEREVS,
                                   tooltip = tip,
                                   data_id = AGEREVS),
                               arrow = arrow(angle = 15, ends = "last", type = "closed", length = unit(0.5, "cm")),
                               size = 1.5, alpha = 1) +
      geom_point_interactive(aes(x=0, y=pct_ind_PRES, fill = AGEREVS,
                                 tooltip = tip,
                                 data_id = AGEREVS),
                             stat = "identity", shape = 21, size = 3, alpha = 1, stroke = 1.5) +
      scale_y_continuous( name = "Part de la population présente faisant partie de la tranche d'âge", labels = percent_format(accuracy = 1)) +
      scale_x_continuous( name = 'Evolution de la part de la catégorie par les migrations',
                          limits=c(-(indics_renouv_CS1_terr_vs_moy %>% select(evol_pct_AUTO_PRES) %>% min() %>% abs() %>% rbind(indics_renouv_CS1_terr_vs_moy %>% select(evol_pct_AUTO_PRES) %>% max()) %>% max()),
                                   indics_renouv_CS1_terr_vs_moy %>% select(evol_pct_AUTO_PRES) %>% min() %>% abs() %>% rbind(indics_renouv_CS1_terr_vs_moy %>% select(evol_pct_AUTO_PRES) %>% max()) %>% max()),
                          labels = percent_format(accuracy = 0.01)) +
       scale_fill_manual(guide = FALSE, values = rev(c("#8c510a", "#d8b365", "#dfc27d", "#80cdc1", "#5ab4ac", "#01665e"))) +
      scale_color_manual(guide = FALSE, values = rev(c("#8c510a", "#d8b365", "#dfc27d", "#80cdc1", "#5ab4ac", "#01665e"))) +
      labs(
        title = "Par tranche d'âge"#,
      ) +
      theme(strip.text.y = element_text(angle = 360),
            text = element_text(family = "Roboto", color = "black"),
            panel.background = element_rect(fill = NA, color = NA),
            plot.background = element_rect(fill = NA, color = NA),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            axis.line =  element_line( color = "grey"),
            panel.grid.major = element_line(color = "#2b2b2b99",size = 0.1),
            panel.grid.minor = element_line(color = "#2b2b2b99", size = 0.05),
            legend.position = "right")
    
    
    tooltip_css <- "background-color:#272B30;padding:2px;font-size: 80%;color: white;opacity:0.2"
    x <- girafe(ggobj = p1, width = 1, height_svg = 6 )
    girafe_options(x, 
                   opts_tooltip(use_fill = FALSE, offx = 40, offy = 480, use_cursor_pos = FALSE, css = tooltip_css),
                   opts_hover(css = "stroke:red;r:10pt;"),
                   opts_selection(type = "none"),
                   opts_zoom(max = 1),
                   opts_toolbar(position = "bottomleft", saveaspng = FALSE) )
    
  })
  
  ##############################
  # flux
  # filtre sur dataframe des flux
  
  # dataframe pour flux entrants
  filteredData_flux_ENTR <- reactive({
    if(input$maille_TERR %in% 'REG') {
      flux_migres_TERR.map %>% 
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_ACTU %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_REG) ) %>%
        # cappé à 10 flux minimum ?
        filter(nb_ind >= 10) %>%
        # définition du ratio
        mutate(ratio = 15000)
      
      
    } else if ( input$maille_TERR %in% 'DEP') {
      
      flux_migres_TERR.map %>% 
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_ACTU %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP) ) %>%
        # cappé à 10 flux minimum ?
        filter(nb_ind >= 10) %>%
        # définition du ratio
        mutate(ratio = 5000)
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      
      flux_migres_TERR.map %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_ACTU %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI) ) %>%
        # cappé à 10 flux minimum ?
        filter(nb_ind >= 10)%>%
        # définition du ratio
        mutate(ratio = 2000)
      
    }
  })
  
  
  
  # dataframe pour flux sortants
  
  filteredData_flux_SORT <- reactive({
    if(input$maille_TERR %in% 'REG') {
      flux_migres_TERR.map %>% 
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_ANTE %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_REG) ) %>%
        # cappé à 10 flux minimum ?
        filter(nb_ind >= 10) %>%
        # définition du ratio
        mutate(ratio = 15000)
      
    } else if ( input$maille_TERR %in% 'DEP') {
      
      flux_migres_TERR.map %>% 
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_ANTE %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP) ) %>%
        # cappé à 10 flux minimum ?
        filter(nb_ind >= 10) %>%
        # définition du ratio
        mutate(ratio = 5000)
      
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      
      flux_migres_TERR.map %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_ANTE %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI) ) %>%
        # cappé à 10 flux minimum ?
        filter(nb_ind >= 10)%>%
        # définition du ratio
        mutate(ratio = 2000)
      
    }
  })
  
  # dataframe pour flux SM net
  
  filteredData_flux_SMNET <- reactive({
    if(input$maille_TERR %in% 'REG') {
      
      flux_migres_TERR.map.SMnet %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_j %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_REG)) %>%
        # cappé à 10 flux minimum ?
        #filter(nb_ind >= 10) %>%
        # définition du ratio
        mutate(ratio = 15000) %>%
        mutate(type_flow = "entr") %>%
        rbind.data.frame(flux_migres_TERR.map.SMnet %>%
                           filter(NIV_TERR %in% input$maille_TERR) %>%
                           filter(TERR_i %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_REG)) %>%
                           # cappé à 10 flux minimum ?
                           #filter(nb_ind >= 10) %>%
                           # définition du ratio
                           mutate(ratio = 15000) %>%
                           mutate(type_flow = "sort"))
      
      
    } else if ( input$maille_TERR %in% 'DEP') {
      
      
      flux_migres_TERR.map.SMnet %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_j %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP)) %>%
        # cappé à 10 flux minimum ?
        #filter(nb_ind >= 10) %>%
        # définition du ratio
        mutate(ratio = 8000) %>%
        mutate(type_flow = "entr") %>%
        rbind.data.frame(flux_migres_TERR.map.SMnet %>%
                           filter(NIV_TERR %in% input$maille_TERR) %>%
                           filter(TERR_i %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP)) %>%
                           # cappé à 10 flux minimum ?
                           #filter(nb_ind >= 10) %>%
                           # définition du ratio
                           mutate(ratio = 8000) %>%
                           mutate(type_flow = "sort"))
      
      
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      
      flux_migres_TERR.map.SMnet %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_j %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI)) %>%
        # cappé à 10 flux minimum ?
        #filter(nb_ind >= 10) %>%
        # définition du ratio
        mutate(ratio = 1000) %>%
        mutate(type_flow = "entr") %>%
        rbind.data.frame(flux_migres_TERR.map.SMnet %>%
                           filter(NIV_TERR %in% input$maille_TERR) %>%
                           filter(TERR_i %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI)) %>%
                           # cappé à 10 flux minimum ?
                           #filter(nb_ind >= 10) %>%
                           # définition du ratio
                           mutate(ratio = 5000) %>%
                           mutate(type_flow = "sort"))
      
      
    }
  })
  
  
  #########################
  # cartographie des flux 
  
  output$flux <- renderPlot({
    
    if(input$type_flux %in% 'ENTR') {
      
      ratio <- filteredData_flux_ENTR() %>% distinct(ratio) %>% pull()
      niveau_TERR <- filteredData_flux_ENTR() %>% distinct(NIV_TERR) %>% pull()
      
      # affichage du geom_spoke
      p.flux.spoke <- ggplot() +
        geom_spoke(data= filteredData_flux_ENTR() ,
                   aes(x = x_ctr_ANTE ,
                       y = y_ctr_ANTE ,
                       radius = log(nb_ind) * ratio,
                       angle =  angle
                   ))
      
      # récupération des geometries
      g.flux.spoke <- ggplot_build(p.flux.spoke)
      
      # et recalcul pour geom_segment
      # flux entrants
      p.str.entr <- g.flux.spoke$data[[1]] %>%
        select(x, xend, y, yend) %>%
        rowwise() %>%
        mutate(x_moy = round(mean(x, xend),0),
               y_moy = round(mean(y, yend),0)) %>%
        left_join(filteredData_flux_ENTR() %>%
                    mutate(x_ctr_ANTE = round(x_ctr_ANTE,0),
                           y_ctr_ANTE = round(y_ctr_ANTE,0)) %>%
                    select(TERR_ANTE, TERR_ACTU, nb_ind, pct_ind_ACTU_MIG, x_ctr_ANTE, y_ctr_ANTE),
                  by = c("x_moy" ="x_ctr_ANTE","y_moy" ="y_ctr_ANTE")) %>%
        mutate(xdiff = xend - x, ydiff = yend - y) %>%
        mutate(xstart = x - xdiff, ystart = y - ydiff) 
      
      # puis cartographie finale
      
      ggplot() +
        # contours des départements si niv_TERR = DEP ou EPCI
      {if(niveau_TERR %in% c('DEP','EPCI'))geom_sf(data = geo_TERR_poly %>%  filter(NIV_TERR %in% "DEP"),
                                                   color = "grey65", fill = NA ,size = 0.05)} +
        # contours des régions
        geom_sf(data = geo_TERR_poly %>%  filter(NIV_TERR %in% 'REG'),
                color = "grey25", fill = NA ,size = 0.25) +

        # spokes
        geom_segment(data=p.str.entr ,
                     aes(x = xstart ,
                         y = ystart ,
                         xend = x,
                         yend = y,
                         color = pct_ind_ACTU_MIG,
                         size = nb_ind * 10 ),
                     arrow = arrow(ends = "last", length = unit(p.str.entr$nb_ind %>% log() / 20,"cm"), type = "open")
        ) +
        scale_size(range = c(0.05,3.5), name = "Nombre d'individus\nayant emménagé dans\nle territoire") +
        scale_color_gradient(low = "#b5d2b2", high = "#000a00", name = "Répartition des\nindividusayant emmenagé\ndans le territoire") +
        guides(fill = guide_legend(reverse=T)) +
        theme_ipsum() +
        coord_sf(crs = 2154, datum = NA) +
        scale_x_continuous(name = "") +
        scale_y_continuous(name = "") +
        theme(text = element_text(family = "Roboto"),
              panel.background = element_rect(fill = NA, color = NA),
              plot.background = element_rect(fill = NA, color = NA),
              legend.position = "right",
              axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text=element_blank() )
      
      
    } else if (input$type_flux %in% 'SORT') {
      
      ratio <- filteredData_flux_SORT() %>% distinct(ratio) %>% pull()
      niveau_TERR <- filteredData_flux_SORT() %>% distinct(NIV_TERR) %>% pull()
      
      # affichage du geom_spoke
      p.flux.spoke <- ggplot() +
        geom_spoke(data= filteredData_flux_SORT() ,
                   aes(x = x_ctr_ACTU ,
                       y = y_ctr_ACTU ,
                       radius = log(nb_ind) * ratio,
                       angle =  angle
                   ))
      
      
      # récupération des geometries
      g.flux.spoke <- ggplot_build(p.flux.spoke)
      
      # et recalcul pour geom_segment
      # flux entrants
      p.str.sort <- g.flux.spoke$data[[1]] %>%
        select(x, xend, y, yend) %>%
        rowwise() %>%
        mutate(x_moy = round(mean(x, xend),0),
               y_moy = round(mean(y, yend),0)) %>%
        left_join(filteredData_flux_SORT() %>%
                    mutate(x_ctr_ACTU = round(x_ctr_ACTU,0),
                           y_ctr_ACTU = round(y_ctr_ACTU,0)) %>%
                    select(TERR_ANTE, TERR_ACTU, nb_ind, pct_ind_ANTE_MIG, x_ctr_ACTU, y_ctr_ACTU),
                  by = c("x_moy" ="x_ctr_ACTU","y_moy" ="y_ctr_ACTU")) %>%
        mutate(xdiff = xend - x, ydiff = yend - y) %>%
        mutate(xstart = x - xdiff, ystart = y - ydiff)
      
      # puis cartographie finale
      
      ggplot() +
        # contours des départements si niv_TERR = DEP ou EPCI
      {if(niveau_TERR %in% c('DEP','EPCI'))geom_sf(data = geo_TERR_poly %>%  filter(NIV_TERR %in% "DEP"),
                                                   color = "grey65", fill = NA ,size = 0.05)} +
        # contours des régions
        geom_sf(data = geo_TERR_poly %>%  filter(NIV_TERR %in% 'REG'),
                color = "grey25", fill = NA ,size = 0.25) +
        # spokes
        geom_segment(data=p.str.sort ,
                     aes(x = xstart ,
                         y = ystart ,
                         xend = x,
                         yend = y,
                         color = pct_ind_ANTE_MIG,
                         size = nb_ind * 10 ),
                     arrow = arrow(ends = "last", length = unit(p.str.sort$nb_ind %>% log() / 20,"cm"), type = "open")
        ) +
        scale_size(range = c(0.05,3.5), name = "Nombre d'individus\nayant quitté \nle territoire") +
        scale_color_gradient(low = "#fecccc", high = "#190000", name = "Répartition des\nindividus ayant quitté\nle territoire") +
        guides(fill = guide_legend(reverse=T)) +
        theme_ipsum() +
        coord_sf(crs = 2154, datum = NA) +
        scale_x_continuous(name = "") +
        scale_y_continuous(name = "") +
        theme(text = element_text(family = "Roboto"),
              panel.background = element_rect(fill = NA, color = NA),
              plot.background = element_rect(fill = NA, color = NA),
              legend.position = "right",
              axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text=element_blank() )
      
    } else if ( input$type_flux %in% 'SMN') {
      
      
      ## cartographie des flux nets SM
      ratio <- filteredData_flux_SORT() %>% distinct(ratio) %>% pull()
      niveau_TERR <- filteredData_flux_SORT() %>% distinct(NIV_TERR) %>% pull()
      
      # 1- creation des lignes pour flux sortants (SM < 0)
      
      # affichage du geom_spoke
      p.flux.spoke <- ggplot() +
        geom_spoke(data= filteredData_flux_SMNET() %>% filter(type_flow %in% "sort") ,
                   aes(x = x_ctr_j ,
                       y = y_ctr_j ,
                       radius = log(flux_tot) * ratio,
                       angle =  angle
                   ))
      
      # récupération des geometries
      g.flux.spoke <- ggplot_build(p.flux.spoke)
      
      # et recalcul pour geom_segment
      # flux entrants
      p.str.SM.sort <- g.flux.spoke$data[[1]] %>%
        select(x, xend, y, yend) %>%
        rowwise() %>%
        mutate(x_moy = round(mean(x, xend),0),
               y_moy = round(mean(y, yend),0)) %>%
        left_join(filteredData_flux_SMNET() %>% filter(type_flow %in% "sort") %>%
                    mutate(x_ctr_j = round(x_ctr_j,0),
                           y_ctr_j = round(y_ctr_j,0)) %>%
                    select(TERR_i, TERR_j,LIB_TERR_i, LIB_TERR_j,  flux_net_sens, ratio_flux_net_sens,flux_tot, x_ctr_j, y_ctr_j),
                  by = c("x_moy" ="x_ctr_j","y_moy" ="y_ctr_j")) %>%
        mutate(xdiff = xend - x, ydiff = yend - y) %>%
        mutate(xstart = x - xdiff, ystart = y - ydiff) %>%
        filter(!is.na(TERR_i))
      
      
      # 2- creation des lignes pour flux entrants (SM > 0)
      
      # affichage du geom_spoke
      p.flux.spoke <- ggplot() +
        geom_spoke(data= filteredData_flux_SMNET() %>% filter(type_flow %in% "entr") ,
                   aes(x = x_ctr_i ,
                       y = y_ctr_i ,
                       radius = log(flux_tot) * ratio,
                       angle =  angle
                   ))
      
      # récupération des geometries
      g.flux.spoke <- ggplot_build(p.flux.spoke)
      
      # et recalcul pour geom_segment
      # flux entrants
      p.str.SM.entr <- g.flux.spoke$data[[1]] %>%
        select(x, xend, y, yend) %>%
        rowwise() %>%
        mutate(x_moy = round(mean(x, xend),0),
               y_moy = round(mean(y, yend),0)) %>%
        left_join(filteredData_flux_SMNET() %>% filter(type_flow %in% "entr") %>%
                    mutate(x_ctr_i = round(x_ctr_i,0),
                           y_ctr_i = round(y_ctr_i,0)) %>%
                    select(TERR_i, TERR_j,LIB_TERR_i, LIB_TERR_j,  flux_net_sens,ratio_flux_net_sens,flux_tot, x_ctr_i, y_ctr_i),
                  by = c("x_moy" ="x_ctr_i","y_moy" ="y_ctr_i")) %>%
        mutate(xdiff = xend - x, ydiff = yend - y) %>%
        mutate(xstart = x - xdiff, ystart = y - ydiff) %>%
        filter(!is.na(TERR_i))
      
      
      p.str.SM.sort.SMnet <- p.str.SM.sort %>% mutate(flux_net_sens = - flux_net_sens) %>% rbind.data.frame(p.str.SM.entr)
      
      max_flux_net_sens <- filteredData_flux_SMNET() %>% select(flux_net_sens) %>% max()
      
      
      # puis cartographie finale
      
      ggplot() +
        # contours des départements si niv_TERR = DEP ou EPCI
      {if(niveau_TERR %in% c('DEP','EPCI'))geom_sf(data = geo_TERR_poly %>%  filter(NIV_TERR %in% "DEP"),
                                                   color = "grey65", fill = NA ,size = 0.05)} +
        # contours des régions
        geom_sf(data = geo_TERR_poly %>%  filter(NIV_TERR %in% 'REG'),
                color = "grey25", fill = NA ,size = 0.25) +
        # SM net
        geom_segment(data=p.str.SM.sort.SMnet ,
                     aes(x = xstart ,
                         y = ystart ,
                         xend = x,
                         yend = y,
                         color = flux_net_sens,
                         size = flux_tot  ),
                     arrow = arrow(ends = "last", length = unit(p.str.SM.sort.SMnet$flux_tot %>% log() / 20,"cm"), type = "open")
        ) +
        scale_size(range = c(0.05,3.5), name = "Nombre d'individus\nmobiles entre les\ndeux territoires") +
        scale_color_gradient2( name = "Répartition des\nindividus ayant emménagé\ndans le territoire",
                               low = "#0000FF", mid = "#FFFFFF", high ="#FF0000", 
                               space = "rgb", midpoint = 0,
                               limits = c(-max_flux_net_sens,max_flux_net_sens)) +
        guides(fill = guide_legend(reverse=T)) +
        theme_ipsum() +
        coord_sf(crs = 2154, datum = NA) +
        scale_x_continuous(name = "") +
        scale_y_continuous(name = "") +
        theme(text = element_text(family = "Roboto"),
              panel.background = element_rect(fill = NA, color = NA),
              plot.background = element_rect(fill = NA, color = NA),
              legend.position = "right",
              axis.line=element_blank(),
              axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.text=element_blank() )
      
    }
  })
  
  
}


# exécuter l'application
shinyApp(ui = ui, server = server)