options(encoding = 'UTF-8')
options(scipen=999)

# chargement librairies
library(tidyverse)
library(data.table)
library(readr)
library(scales)
library(hrbrthemes)
library(rsconnect)
library(stringi)
library(shinycssloaders)
library(shinythemes)
library(cowplot)
library(rmarkdown)
library(markdown)
library(knitr)
library(ggiraph)
library(cartogram)
library(RColorBrewer)
library(leaflet)
library(shinyWidgets)
library(shinyhelper)
library(shinyalert)

# imports des fichiers par mailles EPCI/DEP/REG  ####


#######################
# format rds

indics_migres_TERR <- readRDS("data/indics_migres_TERR.rds") %>% ungroup() %>% mutate(LIB_TERR = iconv(LIB_TERR, "latin1", "UTF-8"))
indics_migres_TERR_CS1 <- read_rds("data/indics_migres_TERR_CS1.rds") %>% ungroup() %>% mutate(LIB_TERR = iconv(LIB_TERR, "latin1", "UTF-8"))
indics_migres_TERR_AGEREVS <- read_rds("data/indics_migres_TERR_AGEREVS.rds" ) %>% ungroup() %>% mutate(LIB_TERR = iconv(LIB_TERR, "latin1", "UTF-8"))

# indicateurs de renouvellement
indics_migres_TERR_AGEREVS_RENOUV <- read_rds("data/indics_migres_TERR_AGEREVS_RENOUV.rds" ) %>% ungroup() 
indics_migres_TERR_CS1_RENOUV <- read_rds("data/indics_migres_TERR_CS1_RENOUV.rds" ) %>% ungroup() %>% mutate(LIB_TERR = iconv(LIB_TERR, "latin1", "UTF-8"))

# volume des flux
flux_migres_TERR.map <- read_rds("data/flux_migres_TERR.map.rds" ) %>% ungroup() 
flux_migres_TERR.map.SMnet <- read_rds("data/flux_migres_TERR.map.SMnet.rds" ) %>% ungroup() 

# indics histodemo
indics_TERR_histodemo <- read_rds("data/indics_TERR_histodemo.rds" ) %>% ungroup() %>%
  mutate_if(is.numeric, funs(ifelse(is.nan(.), 0, .))) 



# rajout des libellés des CS
library(migR)

indics_migres_TERR_CS1 <-
  ajout_libelles_varventil_insee(TABLE = indics_migres_TERR_CS1 %>% ungroup(),
                                 VAR ="CS1",
                                 MILLESIME_RP = 2015)


# vecteur des intitulés de territoires

liste_DEP <- indics_migres_TERR %>% filter(NIV_TERR %in% 'DEP') %>% distinct(TERR,LIB_TERR) %>% mutate(lib_code_TERR = paste0(LIB_TERR, " (" ,TERR,")")) %>% pull(lib_code_TERR)
liste_EPCI <- indics_migres_TERR %>% filter(NIV_TERR %in% 'EPCI') %>% distinct(TERR,LIB_TERR) %>% mutate(lib_code_TERR = paste0(LIB_TERR, " (" ,TERR,")")) %>% pull(lib_code_TERR)
liste_REG <- indics_migres_TERR %>% filter(NIV_TERR %in% 'REG') %>% distinct(TERR,LIB_TERR) %>% mutate(lib_code_TERR = paste0(LIB_TERR, " (" ,TERR,")")) %>% pull(lib_code_TERR)


# import carto
geo_TERR_poly <- st_read("data/geo_TERR_poly.gpkg") %>% st_transform(4326) %>%
  mutate(lib_code_TERR = paste0(LIB_TERR, " (" ,TERR,")"))


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
  
  windowTitle = "Outil mobilités résidentielles OT",
  position = "static-top",
  theme = "styles_v3.css" ,
  title =               
    div(
      img(
        src = "logo_OT.png",
        height = 31,
        width = 105,
        style = "float:left;left: 5px;top: 1px;"
      ),    ""
    ),
  tabPanel(toupper("Les chiffres clés des mobilités résidentielles dans chaque territoire"),
           sidebarLayout(
             sidebarPanel(id="sidebar",
                          width = 3,
                          div(class="test_type",
                              width = 3.5,
                              radioGroupButtons(inputId = "maille_TERR", 
                                                label = "Choisir une maille d'analyse :", 
                                                direction = "vertical",
                                                choices = list("RÉGION" = 'REG',"DÉPARTEMENT" = 'DEP', "INTERCOMMUNALITÉ" = 'EPCI'), 
                                                selected = "DEP",
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
                                            selected = "Haute-Marne (52)",
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
                              a(img(src="logo-OT-CMJN_carre.png", height = 40, width = 47, align = "left"),
                                href="http://www.observatoire-des-territoires.gouv.fr/",target="_blank"),
                              a(img(src="MCTRCT-horizontal+CGET-logotype.png", height = 42, width = 180, align = "right"), 
                                href="http://www.cget.gouv.fr/",target="_blank"),
                              br(),
                              br()
                              
                          )
                          , 
                          tags$head(tags$style(type="text/css", ".container-fluid {  /*max-width: 2000px;*/ min-width: 1200px; "))
             ),
 
             mainPanel(
               tabsetPanel(
                 tabPanel("Evolution démographique" ,
                          fluidRow(htmlOutput("intro_onglet_evoldemo")) ,

                          column(5,htmlOutput("pres_evoldemo")),
                          
                          column(7,  radioButtons("type_temporalite", "",
                                                  choices = list("Comparaison avec les autres territoires" = '2010_2015',
                                                                 "En historique depuis 1968" = '1968_2015'),
                                                  inline = FALSE,
                                                  width = "400px",
                                                  selected = '2010_2015'),
                                 ggiraphOutput(outputId="nuagepoint_evoldemo", width="600px",height="600px") %>% withSpinner(type = 1, color="#1B7F3F"))
                 ) ,
                 tabPanel("Arrivées et départs",
                          fluidRow(htmlOutput("intro_onglet_arriveesdeparts"))  %>% 
                            helper(type = "inline",
                                   title = "Aide",
                                   icon = "question-circle",
                                   colour = "#5b937c",
                                   content = c("Les mobilités résidentielles quantifiées ici sont qualifiées de 'nettes internes'.",
                                               paste0("Elles ne prennent en compte donc en compte que les ", "<b>" ,"échanges entre le territoire considéré et le reste de la France","</b>" ,", et cela entre deux années consécutives.") ),
                                   size = "m"), 
                          column(5,htmlOutput("pres")),
                          column(7,ggiraphOutput(outputId="nuagepoint_terr", width="650px",height="500px") %>% withSpinner(type = 1, color="#1B7F3F")) ),
                 
                 tabPanel("Profil de la population",
                          fluidRow(htmlOutput("intro_onglet_profil")) %>% 
                            helper(type = "inline",
                                   title = "Aide",
                                   icon = "question-circle",
                                   colour = "#5b937c",
                                   content = c("Les mobilités résidentielles quantifiées ici sont qualifiées de 'nettes internes'.",
                                               paste0("Elles ne prennent en compte donc en compte que les ", "<b>" ,"échanges entre le territoire considéré et le reste de la France","</b>" ,", et cela entre deux années consécutives.") ),
                                   size = "m"), 
                          radioButtons("type_pop", "Type de population :",
                                       choices = list("Présente" = 'PRES',
                                                      "Sortante" = 'SORT',
                                                      "Entrante" = 'ENTR'),
                                       inline = TRUE,
                                       selected = 'PRES') ,
                          column(6,ggiraphOutput(outputId="diagcirc_profil_pop_CS1", width="450px",height="600px") %>% withSpinner(type = 1, color="#1B7F3F")),
                          column(6,ggiraphOutput(outputId="diagcirc_profil_pop_AGEREVS", width="450px",height="500px") %>% withSpinner(type = 1, color="#1B7F3F"))
                 ),
                 tabPanel("Impact sociodémographique",
                          fluidRow(htmlOutput("intro_onglet_impact"))  %>% 
                            helper(type = "inline",
                                   title = "Aide",
                                   icon = "question-circle",
                                   colour = "#5b937c",
                                   content = c("Les mobilités résidentielles quantifiées ici sont qualifiées de 'nettes internes'.",
                                               paste0("Elles ne prennent en compte donc en compte que les ", "<b>" ,"échanges entre le territoire considéré et le reste de la France","</b>" ,", et cela entre deux années consécutives.") ), 
                                   size = "m"), 
                          column(6,ggiraphOutput(outputId="diag_evolmig_profilpop_CS1", width="450px",height="600px") %>% withSpinner(type = 1, color="#1B7F3F")),
                          column(6,ggiraphOutput(outputId="diag_evolmig_profilpop_AGEREVS", width="450px",height="600px") %>% withSpinner(type = 1, color="#1B7F3F"))#,
                 ),
                 tabPanel("Flux résidentiels", 
                          fluidRow(htmlOutput("intro_onglet_fluxres"))   %>% 
                            helper(type = "inline",
                                   title = "Aide",
                                   icon = "question-circle",
                                   colour = "#5b937c",
                                   content = c("Les mobilités résidentielles quantifiées ici sont qualifiées de 'nettes internes'.",
                                               paste0("Elles ne prennent en compte donc en compte que les ", "<b>" ,"échanges entre le territoire considéré et le reste de la France","</b>" ,", et cela entre deux années consécutives.","<br>","<br>",
                                                      "L'icône loupe est affichée en haut à droite de la carte afin de zoomer grâce à un double-clic sur la zone souhaitée.") ),
                                   size = "m"), 
                          radioButtons("type_flux", "Type de flux :",
                                       choices = list("Flux entrants" = 'ENTR',
                                                      "Flux sortants" = 'SORT',
                                                      #"Solde migratoire net v1" = 'SMN',
                                                      "Solde migratoire net" = 'SMN2'),
                                       inline = TRUE,
                                       selected = 'ENTR') ,
                          column(9,ggiraphOutput(outputId="flux", width = 875, height = 600) %>% withSpinner(type = 1, color="#1B7F3F")) )#,
               )

             )
           )
  ), 
  tabPanel('Sources et méthodologie',
           fluidRow(
             # pour publication shinyapps, format md dans dossier 
             column(10,includeMarkdown("onglet_sources_methodo.md"))
           )
  ),
  navbarMenu('A propos',
             tabPanel(a("L'Observatoire", href = 'http://www.observatoire-des-territoires.gouv.fr/observatoire-des-territoires/fr/node', target = '_blank')),
             tabPanel(a("Le rapport", href = 'http://www.observatoire-des-territoires.gouv.fr/observatoire-des-territoires/fr/rapports', target = '_blank'))
  ),
  # eviter message d'erreur avant output
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),useShinyalert()
)


# Definition de la partie serveur  ####

server <- function(input, output, session) {
  
  # shinyhelper
  observe_helpers(withMathJax = TRUE)
  
  # alerte à l'init
  shinyalert(
    text =  paste0(
      "<font size=3.5 color=black family=Roboto>",
      "Cet outil de diagnostic territorial est mis à disposition de tous par l'Observatoire des territoires dans le cadre de la 
      publication de son rapport sur les mobilités résidentielles.<br>", "</font>",
      "<br>",
      "<font size=2.5 color=black family=Roboto>",
      "L'affichage n'est pas optimisé sur les écrans de petite taille et pour Internet Explorer.
      Le temps de chargement sera d'autant plus long que l'échelle d'analyse est grande.
      Vous pouvez sélectionner le territoire de votre choix sur le","<b> menu placé à gauche de votre écran." ,"</font>")
    ,
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = "",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#5b937c",
    timer = 0,
    imageUrl = "http://www.observatoire-des-territoires.gouv.fr/observatoire-des-territoires/themes/datar/img/logo.png",
    imageWidth = 200,
    imageHeight = 60,
    animation = TRUE
    )
  
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
  
 
  # update de l'id TERR uniquement si l'id selectionné via map_click correspond au NIVGEO
  # sinon pas de changement et on conserve celui en mémoire
  
  observe({
    
    if(input$maille_TERR %in% 'DEP') {
      
      if(map_click() %in% liste_DEP) { updateSelectInput(session, 'territoireetude_DEP',choices = liste_DEP, selected = map_click())
      } else {}
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      
      if(map_click() %in% liste_EPCI) { updateSelectInput(session, 'territoireetude_EPCI',choices = liste_EPCI, selected = map_click())
      } else {}
      
    } else if ( input$maille_TERR %in% 'REG') {
      if(map_click() %in% liste_REG) { updateSelectInput(session, 'territoireetude_REG',choices = liste_REG, selected = map_click())
      } else {}
      
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
      {HTML(paste0("<font size=2.5 color=#838587 family=Roboto>" ,
                   "Quels sont les moteurs de l'évolution démographique (solde naturel, solde migratoire) en évolution depuis les années 1960 et en comparaison avec les autres départements ?" ,"</b>","</font>",
                   "<br>", "<br>"
      ) )}
    } else if ( filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'EPCI') {
      {HTML(paste0("<font size=2.5 color=#838587 family=Roboto>" ,
                   "Quels sont les moteurs de l'évolution démographique (solde naturel, solde migratoire) en évolution depuis les années 1960 et en comparaison avec les autres intercommunalités ?" ,"</b>","</font>",
                   "<br>", "<br>"
      ) )}
    } else if ( filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'REG') {
      {HTML(paste0("<font size=2.5 color=#838587 family=Roboto>" ,
                   "Quels sont les moteurs de l'évolution démographique (solde naturel, solde migratoire) en évolution depuis les années 1960 et en comparaison avec les autres régions ?" ,"</b>","</font>",
                   "<br>", "<br>"
      ) )}
    }
  })
  
  
  # chapeau d'introduction sur l'onglet "arrivées et départs"
  output$intro_onglet_arriveesdeparts <- renderUI({
    
    if(input$maille_TERR %in% 'REG') { libelle_NIVGEO <- "la région "} 
    else if ( input$maille_TERR %in% 'DEP') { libelle_NIVGEO <- "le département "}
    else if ( input$maille_TERR %in% 'EPCI') { libelle_NIVGEO <- "l'intercommunalité "}
    
    if(filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'DEP') {
      {HTML(paste0("<font size=2.5 color=#838587 family=Roboto>" ,
                   "Entre 2014 et 2015, quel est le bilan des échanges migratoires entre ", stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),stri_trans_general(conv_accents(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% distinct(LIB_TERR) %>% pull()),"Latin-ASCII")  , " et les autres départements ?" ,"</font>",
                   "<br>", "<br>"
      ) )}
    } else if ( filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'EPCI') {
      {HTML(paste0("<font size=2.5 color=#838587 family=Roboto>" ,
                   "Entre 2014 et 2015, quel est le bilan des échanges migratoires entre ", stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),stri_trans_general(conv_accents(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% distinct(LIB_TERR) %>% pull()),"Latin-ASCII") , " et les autres intercommunalités ?" ,"</font>",
                   "<br>", "<br>"
      ) )}
    } else if ( filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'REG') {
      {HTML(paste0("<font size=2.5 color=#838587 family=Roboto>" ,
                   "Entre 2014 et 2015, quel est le bilan des échanges migratoires entre ", stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),stri_trans_general(conv_accents(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% distinct(LIB_TERR) %>% pull()),"Latin-ASCII")  , " et les autres régions ?" ,"</font>",
                   "<br>", "<br>"
      ) )}
    }
  })
  
  
  
  
  # chapeau d'introduction sur l'onglet 'profil sociodémographique'
  output$intro_onglet_profil <- renderUI({
    
    if(input$maille_TERR %in% 'REG') { libelle_NIVGEO <- "de la région "} 
    else if ( input$maille_TERR %in% 'DEP') { libelle_NIVGEO <- "du département "}
    else if ( input$maille_TERR %in% 'EPCI') { libelle_NIVGEO <- "de l'intercommunalité "}
    
    {HTML(paste0("<font size=2.5 color=#838587 family=Roboto>" ,
                 "Quelle est la répartition de la population par tranche d'âge et par groupe socioprofessionnel, distinguée selon que l'on s'intéresse à l'ensemble des habitants ",
                 stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),stri_trans_general(conv_accents(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% distinct(LIB_TERR) %>% pull()),"Latin-ASCII"),
                 ", aux nouveaux arrivants ou à ceux qui en sont partis ?" ,"</font>",
                 "<br>", "<br>"
    ) )}
    
  })
  
  # chapeau d'introduction sur l'onglet 'impact sociodémographique'
  output$intro_onglet_impact <- renderUI({
    if(filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'DEP') {
      {HTML(paste0("<font size=2 color=#4f5154 family=Roboto>" ,
                   "Comment les mobilités résidentielles modifient-elles la composition (par groupe socioprofessionnel et par classe d'âge) du département ?" ,"</b>","</font>",
                   "<br>", "<br>"
      ) )}
    } else if ( filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'EPCI') {
      {HTML(paste0("<font size=2 color=#4f5154 family=Roboto>" ,
                   "Comment les mobilités résidentielles modifient-elles la composition (par groupe socioprofessionnel et par classe d'âge) de l'intercommunalité ?" ,"</b>","</font>",                   "<br>", "<br>"
      ) )}
    } else if ( filteredData_TOT() %>% distinct(NIV_TERR) %>% pull() %in% 'REG') {
      {HTML(paste0("<font size=2 color=#4f5154 family=Roboto>" ,
                   "Comment les mobilités résidentielles modifient-elles la composition (par groupe socioprofessionnel et par classe d'âge) de la région ?" ,"</b>","</font>",
                   "<br>", "<br>"
      ) )}
    }
  })
  
  
  ####### evol demo ####
  
  ## nuage de points pour contexte ####
  
  filteredData_nuagepoint_evoldemo <- reactive({
    if(input$maille_TERR %in% 'DEP') {
      indics_TERR_histodemo %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        mutate(filtre_TERR = case_when(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP) ~ "TERR", TRUE ~ "RESTE"))
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      indics_TERR_histodemo %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        mutate(filtre_TERR = case_when(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI) ~ "TERR", TRUE ~ "RESTE"))
      
    } else if ( input$maille_TERR %in% 'REG') {
      indics_TERR_histodemo %>%
        as.data.frame() %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        mutate(filtre_TERR = case_when(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_REG) ~ "TERR", TRUE ~ "RESTE"))
    }
  })
  
  
  
  # rédaction du petit laïus sur l'évolution démographique dans le territoire
  output$pres_evoldemo <- renderUI({
    
    if(input$type_temporalite %in% '2010_2015') {
      
      if(input$maille_TERR %in% 'REG') { libelle_NIVGEO <- "de la région "} 
      else if ( input$maille_TERR %in% 'DEP') { libelle_NIVGEO <- "du département "}
      else if ( input$maille_TERR %in% 'EPCI') { libelle_NIVGEO <- "de l'intercommunalité "}
      
      {HTML(paste0("<br>",
                   "<font size=3 color=#4f5154 family=Roboto>",
                   "Entre 2010 et 2015,", "<br>","<br>",
                    "la population ", stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),"<b>",stri_trans_general(conv_accents(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% distinct(LIB_TERR) %>% pull()),"Latin-ASCII"),"</b>" ,"<br>",
                   "<b>",paste0(symnum(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(EVOL_DEMO_TOT) %>% pull(),
                                       c(-Inf, 0, Inf),
                                       c("a diminué de ", "a augmenté de ")),
                                format(round(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(EVOL_DEMO_TOT) %>% pull() %>% abs(),-1), nsmall=0, big.mark=' '),""), " personnes", "</b>", "<br>",
                   "soit ",paste0(symnum(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_TOT) %>% pull(),
                                         c(-Inf,-0.01,-0.0025, 0.0025, 0.01, Inf),
                                         c(" une forte baisse ", " une baisse", " une relative stagnation", "une hausse", " une forte hausse")), ""),
                   
                   " de ", paste0(symnum(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_TOT) %>% pull(),
                                         c(-Inf, 0, Inf),
                                         c("", "+")),
                                  percent(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_TOT) %>% pull(), accuracy = 0.01),""), ".<br>", "<br>",
                   "Cette évolution de la population résulte :", "<br>",
                   "- d'un solde naturel ",  paste0(symnum(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SN) %>% pull(),
                                                           c(-Inf,-0.01,-0.0025, 0.0025, 0.01, Inf),
                                                           c(" très négatif ", " négatif", " quasiment nul ", " positif", " fortement positif")), ""), 
                   " (",  paste0(symnum(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SN) %>% pull(),
                                        c(-Inf, 0, Inf),
                                        c("", "+")),
                                 percent(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SN) %>% pull(), accuracy = 0.01),""),
                   ") <br>",
                   
                   "- d'un solde migratoire * ",  paste0(symnum(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SMA) %>% pull(),
                                                                c(-Inf,-0.01,-0.0025, 0.0025, 0.01, Inf),
                                                                c(" très négatif ", " négatif", " quasiment nul ", " positif", " fortement positif")), ""), 
                   " (",  paste0(symnum(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SMA) %>% pull(),
                                        c(-Inf, 0, Inf),
                                        c("", "+")),
                                 percent(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SMA) %>% pull(), accuracy = 0.01),""),
                   ")","</font>", "<br>", "<br>", "<br>", "<br>","<br>",
                   "<font size=1.5 color=#4f5154 family=Roboto fontface = italic>
                   * Il s'agit ici du solde migratoire apparent, c'est à dire de la différence entre l'évolution démographique totale du territoire et celle
                   due au solde naturel.
                   Le solde migratoire apparent intègre donc les échanges avec l'étranger, et n'est pas comparable avec le solde migratoire net interne
                   (onglets suivants) qui exclue ces échanges.</font>"
                   
      ) )}
    }
    
    else if ( input$type_temporalite %in% '1968_2015') {
      
      if(input$maille_TERR %in% 'REG') { libelle_NIVGEO <- "de la région "} 
      else if ( input$maille_TERR %in% 'DEP') { libelle_NIVGEO <- "du département "}
      else if ( input$maille_TERR %in% 'EPCI') { libelle_NIVGEO <- "de l'intercommunalité "}
      
      {HTML(paste0("<br>",
                   "<font size=3 color=#4f5154 family=Roboto>",
                   "Entre 2010 et 2015,", "<br>","<br>",
                   "la population ", stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),"<b>",stri_trans_general(conv_accents(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% distinct(LIB_TERR) %>% pull()),"Latin-ASCII"),"</b>" ,"<br>",
                   "<b>",paste0(symnum(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(EVOL_DEMO_TOT) %>% pull(),
                                       c(-Inf, 0, Inf),
                                       c("a diminué de ", "a augmenté de ")),
                                format(round(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(EVOL_DEMO_TOT) %>% pull() %>% abs(),-1), nsmall=0, big.mark=' '),""), " personnes", "</b>", "<br>",
                   "soit ",paste0(symnum(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_TOT) %>% pull(),
                                         c(-Inf,-0.01,-0.0025, 0.0025, 0.01, Inf),
                                         c(" une forte baisse ", " une baisse", " une relative stagnation", "une hausse", " une forte hausse")), ""),
                   
                   " de ", paste0(symnum(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_TOT) %>% pull(),
                                         c(-Inf, 0, Inf),
                                         c("", "+")),
                                  percent(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_TOT) %>% pull(), accuracy = 0.01),""), " par an en moyenne.<br>", "<br>",
                   "Cette évolution de la population résulte :", "<br>",
                   "- d'un solde naturel ",  paste0(symnum(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SN) %>% pull(),
                                                           c(-Inf,-0.01,-0.0025, 0.0025, 0.01, Inf),
                                                           c(" très négatif ", " négatif", " quasiment nul ", " positif", " fortement positif")), ""), 
                   " (",  paste0(symnum(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SN) %>% pull(),
                                        c(-Inf, 0, Inf),
                                        c("", "+")),
                                 percent(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SN) %>% pull(), accuracy = 0.01),""),
                   " par an) <br>",

                   "- d'un solde migratoire* ",  paste0(symnum(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SMA) %>% pull(),
                                                               c(-Inf,-0.01,-0.0025, 0.0025, 0.01, Inf),
                                                               c(" très négatif ", " négatif", " quasiment nul ", " positif", " fortement positif")), ""), 
                   " (",  paste0(symnum(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SMA) %>% pull(),
                                        c(-Inf, 0, Inf),
                                        c("", "+")),
                                 percent(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SMA) %>% pull(), accuracy = 0.01),""),
                   " par an)","</font>"
                   
      ) )}
    }
    
    
    } 

) 
  
  
  output$nuagepoint_evoldemo <- renderggiraph({
    
    if(input$type_temporalite %in% '2010_2015') {
      
      borne_axe_max <-
        filteredData_nuagepoint_evoldemo() %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SMA) %>% max() %>%
        rbind(filteredData_nuagepoint_evoldemo() %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SMA) %>% min() %>% abs()) %>%
        rbind(filteredData_nuagepoint_evoldemo() %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SN) %>% max())  %>%
        rbind(filteredData_nuagepoint_evoldemo() %>% filter(periode %in% '2010_2015') %>% select(TX_EVOL_DEMO_AN_SN) %>% min() %>% abs()) %>% max()
      
      p1 =
        ggplot() +
        annotate("text",family="Roboto",  color = "grey60",fontface = "italic", size = 2.8, x = -borne_axe_max*0.75, y = borne_axe_max*0.75, label = "Déficit migratoire\nExcédent naturel") +
        annotate("text",family="Roboto",  color = "grey60",fontface = "italic", size = 2.8, x = -borne_axe_max*0.75, y = -borne_axe_max*0.75, label = "Déficit migratoire\nDéficit naturel") +
        annotate("text",family="Roboto",  color = "grey60",fontface = "italic", size = 2.8, x = borne_axe_max*0.75, y = borne_axe_max*0.75, label = "Excédent migratoire\nExcédent naturel") +
        annotate("text",family="Roboto",  color = "grey60",fontface = "italic", size = 2.8, x = borne_axe_max*0.75, y = -borne_axe_max*0.75, label = "Excédent migratoire\nDéficit naturel") +
        
        geom_vline(xintercept = 0,  color = "grey40",linetype = "dashed") +
        geom_hline(yintercept = 0,  color = "grey40",linetype = "dashed") +
        geom_point_interactive(data= filteredData_nuagepoint_evoldemo() %>%
                                 filter(periode %in% '2010_2015') %>%
                                 mutate(LIB_TERR = stri_trans_general(conv_accents(LIB_TERR),"Latin-ASCII")) %>%
                                 mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                     "<font size=2.5 color=white family=Roboto>" ,"<b>", LIB_TERR ,"</b>","</font>", "<br>",
                                                     "<font size=2 color=white family=Roboto>" , "Taux d&apos;&eacute;volution de la population", "<br>",
                                                     "<font size=2 color=white family=Roboto>" , "totale : ",paste0(symnum(TX_EVOL_DEMO_AN_TOT,
                                                                                                                           c(-Inf, 0, Inf),
                                                                                                                           c("", "+")),
                                                                                                                    percent(TX_EVOL_DEMO_AN_TOT, accuracy = 0.01),""),"</font>", "<br>",
                                                     "<font size=2 color=white family=Roboto>" , "due au solde naturel : ",paste0(symnum(TX_EVOL_DEMO_AN_SN,
                                                                                                                                         c(-Inf, 0, Inf),
                                                                                                                                         c("", "+")),
                                                                                                                                  percent(TX_EVOL_DEMO_AN_SN, accuracy = 0.01),""),"</font>", "<br>",
                                                     "<font size=2 color=white family=Roboto>" , "due au solde migratoire apparent : ",paste0(symnum(TX_EVOL_DEMO_AN_SMA,
                                                                                                                                                     c(-Inf, 0, Inf),
                                                                                                                                                     c("", "+")),
                                                                                                                                              percent(TX_EVOL_DEMO_AN_SMA, accuracy = 0.01),""),"</font>", "<br>"
                                 )) ,
                               aes(x = TX_EVOL_DEMO_AN_SMA , y =TX_EVOL_DEMO_AN_SN ,
                                   size =POPULATION_fin, 
                                   tooltip = tip,
                                   data_id = TERR,
                                   fill=TX_EVOL_DEMO_AN_TOT),
                               stat = "identity", color = "grey30", shape = 21, stroke = 0.2
        ) +
        geom_point(data= filteredData_nuagepoint_evoldemo() %>%
                     filter(periode %in% '2010_2015') %>%
                     filter(filtre_TERR %in% "TERR"),
                   aes(x = TX_EVOL_DEMO_AN_SMA , y =TX_EVOL_DEMO_AN_SN ,
                       size =POPULATION_fin),
                   fill=NA,
                   stat = "identity", color = "black", shape = 21, stroke = 3
        ) +
        scale_x_continuous(name = "Taux d'évolution de la population due au solde migratoire apparent", 
                           limits=c(-borne_axe_max,borne_axe_max),
                           labels=function(x)paste0(symnum(x,c(-Inf, 0, Inf),c("", "+")),percent(x, accuracy = 0.1),"")) +
        scale_y_continuous(name = "Taux d'évolution de la population due au solde naturel",
                           limits=c(-borne_axe_max,borne_axe_max),
                           labels=function(x)paste0(symnum(x,c(-Inf, 0, Inf),c("", "+")),percent(x, accuracy = 0.1),"")) +
        scale_size_continuous(name = "Population", breaks=c(100000,1000000,3000000),labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
        scale_fill_gradient2(name = "Taux d'évolution\nde la population\nentre 2010 et 2015",
                             labels=function(x)paste0(symnum(x,c(-Inf, 0, Inf),c("", "+")),percent(x, accuracy = 1),""),
                             low = "#0a3470", mid = "white", high = "#91141a", midpoint = 0) +
        guides( size = guide_legend(order = 2)) +
        labs(
          title = "Evolution démographique entre 2010 et 2015"#,
        ) +
        theme(strip.text.y = element_text(angle = 360),
              text = element_text(family = "Roboto", color = "black"),
              panel.background = element_rect(fill = NA, color = NA),
              plot.background = element_rect(fill = NA, color = NA),
              axis.title = element_text(size = 10),
              axis.text = element_text(size = 9.5),
              axis.line =  element_line( color = "grey"),
              panel.grid.major = element_line(color = "#2b2b2b99",size = 0.1),
              panel.grid.minor = element_line(color = "#2b2b2b99", size = 0.05),
              legend.box = "horizontal",
              legend.direction = "horizontal",
              legend.title=element_text(size=8), 
              legend.text=element_text(size=7),
              legend.position = "bottom")
      
      tooltip_css <- "background-color:#272B30;padding:2px;font-size: 80%;color: white;opacity:0.2"
      x <- girafe(ggobj = p1, width = 1, height_svg = 6 )
      girafe_options(x, 
                     opts_tooltip(use_fill = FALSE, offx = 10, offy = -10, use_cursor_pos = TRUE, css = tooltip_css),
                     opts_hover(css = "stroke:red;r:10pt;"),
                     opts_selection(type = "none"),
                     opts_zoom(max = 1),
                     opts_toolbar(position = "bottomleft", saveaspng = FALSE) )
      
    }
    
    else if ( input$type_temporalite %in% '1968_2015') {
      
      
      noms_type_evol_POP <- c(
        "TX_EVOL_DEMO_AN_TOT" = "... TOTALE",
        "TX_EVOL_DEMO_AN_SMA" = "... due au solde migratoire apparent",
        "TX_EVOL_DEMO_AN_SN" = "... due au solde naturel"
      )
      
      if(input$maille_TERR %in% 'REG') { libelle_NIVGEO <- "de la région "} 
      else if ( input$maille_TERR %in% 'DEP') { libelle_NIVGEO <- "du département "}
      else if ( input$maille_TERR %in% 'EPCI') { libelle_NIVGEO <- "de l'intercommunalité "}
      
      
      # version ggiraph
      
      p1 <-
        ggplot() + 
        geom_bar_interactive(data =       filteredData_nuagepoint_evoldemo() %>%
                               filter(filtre_TERR %in% "TERR") %>%
                               select(TERR, periode, 
                                      EVOL_DEMO_SN, EVOL_DEMO_SMA ,EVOL_DEMO_TOT ,
                                      TX_EVOL_DEMO_AN_SN, TX_EVOL_DEMO_AN_SMA, TX_EVOL_DEMO_AN_TOT) %>%
                               gather(indic, val, -periode,  -TERR) %>%
                               mutate(periode = str_replace_all(periode, "_","-")) %>%
                               mutate(id_periode_indic = paste0(periode,"_",indic)) %>%
                               filter(indic %in% c('TX_EVOL_DEMO_AN_SN', 'TX_EVOL_DEMO_AN_SMA', 'TX_EVOL_DEMO_AN_TOT')) %>%
                               mutate(signe = case_when(val >0 ~ "+", TRUE ~ "-")) %>%
                               mutate(tip = case_when(indic %in% 'TX_EVOL_DEMO_AN_SN' ~ paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                                               "<font size=2 color=white family=Roboto>" , "Taux d&apos;&eacute;volution de la population", "<br>",
                                                                                               "<font size=2 color=white family=Roboto>" , "due au solde naturel ","<br>",
                                                                                               "<font size=2.5 color=white family=Roboto>", "entre ",substr(periode,1,4)," et ",substr(periode,6,9) ," : ", "<br>",
                                                                                               paste0(symnum(   val,c(-Inf, 0, Inf), c("", "+")), percent( val, accuracy = 0.01),""),"</font>", "<br>"
                               ),
                               indic %in% 'TX_EVOL_DEMO_AN_SMA' ~ paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                         "<font size=2 color=white family=Roboto>" , "Taux d&apos;&eacute;volution de la population", "<br>",
                                                                         "<font size=2 color=white family=Roboto>" , "due au solde migratoire apparent ","<br>",
                                                                         "<font size=2.5 color=white family=Roboto>", "entre ",substr(periode,1,4)," et ",substr(periode,6,9) ," : ", "<br>",
                                                                         paste0(symnum(   val, c(-Inf, 0, Inf),c("", "+")), percent( val, accuracy = 0.01),""),"</font>", "<br>"
                               ),
                               indic %in% 'TX_EVOL_DEMO_AN_TOT' ~ paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                         "<font size=2 color=white family=Roboto>" , "Taux d&apos;&eacute;volution de la population", "<br>",
                                                                         "<font size=2 color=white family=Roboto>" , "totale ","<br>",
                                                                         "<font size=2.5 color=white family=Roboto>", "entre ",substr(periode,1,4)," et ",substr(periode,6,9) ," : ", "<br>",
                                                                         paste0(symnum(  val, c(-Inf, 0, Inf), c("", "+")),percent(  val, accuracy = 0.01),""),"</font>", "<br>")) )
                             
                             
                             ,
                             aes(fill=indic, y=val,
                                 x=periode, 
                                 data_id = periode,
                                 tooltip = tip ),
                             color = "grey", 
                             size = 0.7,
                             position="dodge", stat="identity") +
        scale_alpha_discrete(range=c(0.5,1)) +
        scale_fill_manual(guide = FALSE, values = c("#f67a36","#5B937C", '#666666')) +
        scale_y_continuous(name = "Taux d'évolution annuelle de la population", 
                           limits = c(-(filteredData_nuagepoint_evoldemo() %>%
                                          filter(filtre_TERR %in% "TERR") %>%
                                          select(TERR, periode, 
                                                 EVOL_DEMO_SN, EVOL_DEMO_SMA ,EVOL_DEMO_TOT ,
                                                 TX_EVOL_DEMO_AN_SN, TX_EVOL_DEMO_AN_SMA, TX_EVOL_DEMO_AN_TOT) %>%
                                          gather(indic, val, -periode,  -TERR) %>% filter(grepl('TX', indic)) %>% select(val)  %>% max() %>%
                                          rbind(filteredData_nuagepoint_evoldemo() %>%
                                                  filter(filtre_TERR %in% "TERR") %>%
                                                  select(TERR, periode, 
                                                         EVOL_DEMO_SN, EVOL_DEMO_SMA ,EVOL_DEMO_TOT ,
                                                         TX_EVOL_DEMO_AN_SN, TX_EVOL_DEMO_AN_SMA, TX_EVOL_DEMO_AN_TOT) %>%
                                                  gather(indic, val, -periode,  -TERR) %>% filter(grepl('TX', indic)) %>% select(val) %>% min() %>% abs()) %>% max()),
                                      filteredData_nuagepoint_evoldemo() %>%
                                        filter(filtre_TERR %in% "TERR") %>%
                                        select(TERR, periode, 
                                               EVOL_DEMO_SN, EVOL_DEMO_SMA ,EVOL_DEMO_TOT ,
                                               TX_EVOL_DEMO_AN_SN, TX_EVOL_DEMO_AN_SMA, TX_EVOL_DEMO_AN_TOT) %>%
                                        gather(indic, val, -periode,  -TERR) %>% filter(grepl('TX', indic)) %>% select(val)  %>% max() %>%
                                        rbind(filteredData_nuagepoint_evoldemo() %>%
                                                filter(filtre_TERR %in% "TERR") %>%
                                                select(TERR, periode, 
                                                       EVOL_DEMO_SN, EVOL_DEMO_SMA ,EVOL_DEMO_TOT ,
                                                       TX_EVOL_DEMO_AN_SN, TX_EVOL_DEMO_AN_SMA, TX_EVOL_DEMO_AN_TOT) %>%
                                                gather(indic, val, -periode,  -TERR) %>% filter(grepl('TX', indic)) %>% select(val) %>% min() %>% abs()) %>% max()),
                           labels=function(x)paste0(symnum(x,c(-Inf, 0, Inf),c("", "+")),percent(x, accuracy = 0.1),"")) +
        scale_x_discrete( name = '') +

        geom_hline(yintercept = 0, size = 1,color = "black") +
        facet_wrap("indic", nrow = 3, labeller = as_labeller(noms_type_evol_POP)) +
        labs(
          title = "Evolution démographique",
          subtitle = paste0(libelle_NIVGEO,"\n",filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% distinct(LIB_TERR) %>% pull(), " ..." )#,
        ) +
        theme(strip.text.y = element_text(angle = 360),
              text = element_text(family = "Roboto", color = "black"),
              panel.background = element_rect(fill = NA, color = NA),
              plot.background = element_rect(fill = NA, color = NA),
              axis.title = element_text(size = 10),
              axis.text = element_text(size = 9.5),
              axis.line =  element_line( color = "grey"),
              panel.grid.major = element_line(color = "#2b2b2b99",size = 0.1),
              panel.grid.minor = element_line(color = "#2b2b2b99", size = 0.05),
              plot.title = element_text(hjust = 0,size = 13),
              plot.subtitle = element_text(hjust = 0,size = 11),
              legend.box = "horizontal",
              legend.direction = "horizontal",
              legend.title=element_text(size=8), 
              legend.text=element_text(size=7),
              legend.position = "top")
      
      
      tooltip_css <- "background-color:#272B30;padding:2px;font-size: 80%;color: white;opacity:0.2"
      x <- girafe(ggobj = p1, width = 1, height_svg = 6 )
      girafe_options(x, 
                     opts_tooltip(use_fill = TRUE, offx = 10, offy = -10, use_cursor_pos = TRUE, css = tooltip_css),
                     opts_hover(css = "fill:#323232;stroke:#000000;r:10pt;"),
                     opts_selection(type = "none"),
                     opts_zoom(max = 1),
                     opts_toolbar(position = "bottomleft", saveaspng = FALSE) )
      
      
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
  
  
  
  # rédaction du petit laïus sur l'état des mobilités résidentielles dans le territoire
  output$pres <- renderUI({
    
    if(input$maille_TERR %in% 'REG') { libelle_NIVGEO <- "la région "} 
    else if ( input$maille_TERR %in% 'DEP') { libelle_NIVGEO <- "le département "}
    else if ( input$maille_TERR %in% 'EPCI') { libelle_NIVGEO <- "l'intercommunalité "}
    
    if(input$maille_TERR %in% 'REG') { libelle_NIVGEO_s <- "cette région "} 
    else if ( input$maille_TERR %in% 'DEP') { libelle_NIVGEO_s <- "ce département "}
    else if ( input$maille_TERR %in% 'EPCI') { libelle_NIVGEO_s <- "cette intercommunalité "}
    
    
    {HTML(paste0("<br>",
                 "<font size=3 color=#4f5154 family=Roboto>",
                 "Entre 2014 et 2015,", "<br>","<br>",
                 "<b>",format(round(filteredData_TOT() %>% select(nb_ind_ENTR) %>% pull(),-1), nsmall=0, big.mark=' '),"</b>"," individus sont arrivés","<br>",  "dans ",
                 stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),"<b>",stri_trans_general(conv_accents(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% distinct(LIB_TERR) %>% pull()),"Latin-ASCII"),"</b>","</font>" ,"<br>",
                 "<font size=2 color=#4f5154 family=Roboto> soit ",filteredData_TOT() %>% select(PE) %>% pull()  %>% percent(.,accuracy = 0.1)," de la population présente en 2015. </font> <br>",
                 "<br>",
                 "<b>","<font size=3 color=#4f5154 family=Roboto>",format(round(filteredData_TOT() %>% select(nb_ind_SORT) %>% pull(),-1), nsmall=0, big.mark=" "),"</b>"," individus en sont partis </font>","<br>",
                 "<font size=2 color=#4f5154 family=Roboto>", "soit ",filteredData_TOT() %>% select(PS) %>% pull()  %>% percent(.,accuracy = 0.1)," de la population présente en 2014. </font> <br>",
                 "<br>",
                 "<font size=3 color=#4f5154 family=Roboto>","Le solde migratoire","<br>"," entre ",
                 stri_trans_general(conv_accents(libelle_NIVGEO_s),"Latin-ASCII"),
                 " et le reste de la France est donc :","<br>",
                 "<b>",paste0(symnum(round(filteredData_TOT() %>% select(SM) %>% pull(),-1), c(-Inf, 0, Inf), c("", "+")), round(filteredData_TOT() %>% select(SM) %>% pull(),-1),"")," individus","</b>","</font>"#, "<br>",
                 # "<font size=2 color=#4f5154 family=Roboto>","soit ", paste0(symnum(filteredData_TOT()$TM,
                 #                                                                    c(-Inf, 0, Inf),
                 #                                                                    c("", "+")),
                 #                                                             percent(filteredData_TOT()$TM, accuracy = 0.01),""), " de la population. </font>"
                 
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
    
    borne_axe_max <- filteredData_nuagepoint_mig() %>% select(PS) %>% max() %>% rbind(filteredData_nuagepoint_mig() %>% select(PE) %>% max()) %>% max()
    
    
    p1 =
      ggplot() +
      annotate("text",family="Roboto", color = "grey60",fontface = "italic", size = 2.8, x = borne_axe_max*0.1, y = borne_axe_max*0.9, label = "Solde migratoire\npositif") +
      annotate("text",family="Roboto", color = "grey60",fontface = "italic", size = 2.8, x = borne_axe_max*0.1, y = borne_axe_max*0.15, label = "Faible\nrotation") +
      annotate("text",family="Roboto", color = "grey60",fontface = "italic", size = 2.8, x = borne_axe_max*0.9, y = borne_axe_max*0.95, label = "Forte\nrotation") +
      annotate("text",family="Roboto", color = "grey60",fontface = "italic", size = 2.8, x = borne_axe_max*0.9, y = borne_axe_max*0.1, label = "Solde migratoire\nnégatif") +
      
      geom_abline(intercept = 0, slope = 1, color = "grey40",linetype = "dashed") +
      geom_point_interactive(data= filteredData_nuagepoint_mig() %>%
                               mutate(LIB_TERR = stri_trans_general(conv_accents(LIB_TERR),"Latin-ASCII")) %>%
                              mutate(tip = paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                   "<b>", LIB_TERR ,"</b>", "<br>",
                                                   "<font size=2.5 color=white family=Roboto>" , PE  %>% percent(.,accuracy = 0.1), " d&apos;entrants","</font>", "<br>",
                                                   "<font size=2.5 color=white family=Roboto>" , PS  %>% percent(.,accuracy = 0.1), " de sortants","</font>", "<br>"
                               )) ,
                             aes(x = PS , y =PE ,
                                 size =nb_ind_PRES,
                                 tooltip = tip,
                                 data_id = TERR,
                                 fill=TM),
                             stat = "identity", color = "grey80", shape = 22, stroke = 0.5
      ) +
      geom_point(data= filteredData_nuagepoint_mig() %>%
                   filter(filtre_TERR %in% "TERR"),
                 aes(x = PS , y =PE ,
                     size =nb_ind_PRES),
                 fill=NA,
                 stat = "identity", color = "black", shape = 22, stroke = 1.2
      ) +
      scale_y_continuous(name = "Part d'entrants", limits=c(0,borne_axe_max), labels = percent_format(accuracy = 0.1)) +
      scale_x_continuous(name = "Part de sortants", limits=c(0,borne_axe_max), labels = percent_format(accuracy = 0.1)) +
      scale_size_continuous(name = "Population", breaks=c(10000,100000,1000000), trans = "sqrt",
                            labels=function(x) format(x, big.mark = " ", scientific = FALSE) ) +
      scale_fill_gradient2(name = "Solde migratoire\nrapporté à la\npopulation",
                           breaks = c(filteredData_nuagepoint_mig() %>% select(TM) %>% min() ,
                                      0,
                                      filteredData_nuagepoint_mig() %>% select(TM) %>% max()),
                           labels=function(x)paste0(symnum(x,c(-Inf, 0, Inf),c("", "+")),percent(x, accuracy = 0.1),""),
                           low = "#0a3470", mid = "white", high = "#91141a", midpoint = 0) +
      guides( size = guide_legend(order = 2)) +
      labs(
        title = "Composantes du solde migratoire (arrivées/départs)"#,
      ) +
      theme(strip.text.y = element_text(angle = 360),
            text = element_text(family = "Roboto", color = "black"),
            panel.background = element_rect(fill = NA, color = NA),
            plot.background = element_rect(fill = NA, color = NA),
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 9.5),
            axis.line =  element_line( color = "grey"),
            panel.grid.major = element_line(color = "#2b2b2b99",size = 0.1),
            panel.grid.minor = element_line(color = "#2b2b2b99", size = 0.05),
            plot.title = element_text(size = 11),
            legend.box = "horizontal",
            legend.direction = "horizontal",
            legend.title=element_text(size=8), 
            legend.text=element_text(size=7),
            legend.position = "bottom")
    
    
    tooltip_css <- "background-color:#272B30;padding:2px;font-size: 120%;color: white;opacity:0.2"
    x <- girafe(ggobj = p1, width = 1, height_svg = 6 )
    girafe_options(x, 
                   opts_tooltip(use_fill = FALSE, offx = 10, offy = -10, use_cursor_pos = TRUE, css = tooltip_css),
                   opts_hover(css = "stroke:red;r:5pt;"),
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
      select(NIV_TERR,TERR, CS1, nb_ind_ENTR, nb_ind_SORT) %>%
      group_by(TERR) %>%
      mutate_at(.vars = vars(nb_ind_ENTR, nb_ind_SORT), .funs = funs( ./(sum(.)) ) ) %>%
      ungroup() %>%
      group_by(NIV_TERR, CS1) %>%
      summarise_if(is.numeric, funs(mean)) %>%
      ajout_libelles_varventil_insee(TABLE = .,
                                     VAR ="CS1",
                                     MILLESIME_RP = 2015) %>%
      # avec moyennes pondérées pour pop présente
      left_join(
        indics_migres_TERR_CS1 %>%
          as.data.frame() %>%
          filter(NIV_TERR %in% input$maille_TERR) %>%
          select(NIV_TERR,TERR, CS1, nb_ind_PRES) %>%
          ungroup() %>%
          group_by(CS1) %>%
          summarise(nb_ind_PRES = sum(nb_ind_PRES) ) %>%
          mutate(nb_ind_PRES = nb_ind_PRES / sum(nb_ind_PRES)),
        by = "CS1")
    
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
    
    if(input$maille_TERR %in% 'REG') { libelle_NIVGEO <- "la région "} 
    else if ( input$maille_TERR %in% 'DEP') { libelle_NIVGEO <- "le département "}
    else if ( input$maille_TERR %in% 'EPCI') { libelle_NIVGEO <- "l'intercommunalité "}
    
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
                                mutate(tip = case_when(input$type_pop %in% 'PRES' ~ paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                                          "<font size=2.5 color=white family=Roboto>",
                                                                                          "<b>", percent(value, accuracy = 2),"</b>"," des individus pr&eacute;sents ",  
                                                                                          "dans ",  stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),"<b>",stri_trans_general(conv_accents(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% distinct(LIB_TERR) %>% pull()),"Latin-ASCII"),"</b>" ,"<br>",
                                                                                          "en 2015  ","sont des ","<b>",tolower(CS1_LIB),"</b>", "<br>"),
                                                      input$type_pop %in% 'ENTR' ~ paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                                          "<font size=2.5 color=white family=Roboto>",
                                                                                          "<b>", percent(value, accuracy = 2),"</b>"," des individus arriv&eacute;s ",  
                                                                                          "dans ",stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),"<b>",stri_trans_general(conv_accents(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% distinct(LIB_TERR) %>% pull()),"Latin-ASCII"),"</b>" ,"<br>",
                                                                                          "sont des ","<b>",tolower(CS1_LIB),"</b>", "<br>"),
                                                      input$type_pop %in% 'SORT' ~ paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                                          "<font size=2.5 color=white family=Roboto>",
                                                                                          "<b>", percent(value, accuracy = 2),"</b>"," des individus ayant quitt&eacute; ", 
                                                                                          stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),"<b>",stri_trans_general(conv_accents(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% distinct(LIB_TERR) %>% pull()),"Latin-ASCII"),"</b>" ,"<br>",
                                                                                          "entre 2014 et 2015 ","sont des ","<b>",tolower(CS1_LIB),"</b>", "<br>")
                               ))
    ) +
      geom_bar_interactive(aes(x=2, y=value_pct1, fill=CS1_LIB,
                               tooltip = tip,
                               data_id = CS1_LIB),
                           stat="identity")+
      xlim(0.5, 2.5) +
      geom_text(aes(x = 2.1, y = 0.04, label = "Profil du\nterritoire"), size = 5.5,  color = "black", angle = 30) +
      coord_polar(theta = "y", direction = -1,start = 0.6) +
      scale_fill_manual(#guide = FALSE,
        name = "",
        labels = c("Agriculteurs exploitants","Artisans, commerçants et chefs d'entreprise","Cadres et professions intellectuelles supérieures","Professions Intermédiaires",
                   "Employés","Ouvriers","Retraités","Autres personnes sans activité professionnelle","(dont élèves et étudiants)"),
        values = c("#00B050","#984807","#558ED5","#7030A0","#E46C0A","#FF0000","#969697","#404040",NA)) +
      labs(
        title = "Par groupe socioprofessionnel",
        subtitle = ""#,
      ) +
      theme(line = element_blank(), rect = element_blank(),
            text = element_text(family = "Roboto", face = "plain",
                                colour = "black", size = 8, lineheight = 0.9,
                                hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                                debug = FALSE), axis.line = element_blank(),
            axis.line.x = NULL, axis.line.y = NULL, axis.text = element_blank(),
            axis.text.x = element_blank(), axis.text.x.top = element_blank(),
            axis.text.y = element_blank(), axis.text.y.right = element_blank(),
            axis.ticks = element_blank(), axis.ticks.length = unit(0,"pt"),
            axis.title.x = element_blank(), axis.title.x.top = element_blank(),
            axis.title.y = element_blank(), axis.title.y.right = element_blank(),
            legend.background = element_blank(), legend.spacing = unit(0.4, "cm"),
            legend.spacing.x = NULL, legend.spacing.y = NULL,
            legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
            legend.key = element_blank(), legend.key.size = unit(0.6,"lines"),
            legend.box = "horizontal",
            legend.direction = "vertical",
            legend.text = element_text(size = rel(1.5)), legend.text.align = NULL,
            legend.title = element_text(hjust = 0), legend.title.align = NULL,
            legend.position = "bottom", 
            legend.justification = "left", 
            legend.box.margin = margin(0, 0, 0, 0, "cm"), legend.box.background = element_blank(),
            legend.box.spacing = unit(0.4, "cm"), panel.background = element_blank(),
            panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), panel.spacing = unit(0,"pt"),
            panel.spacing.x = NULL, panel.spacing.y = NULL,
            panel.ontop = FALSE, strip.background = element_blank(),
            strip.text = element_blank(), strip.text.x = element_blank(),
            strip.text.y = element_blank(), strip.placement = "inside",
            strip.placement.x = NULL, strip.placement.y = NULL,
            strip.switch.pad.grid = unit(0, "cm"), strip.switch.pad.wrap = unit(0, "cm"),
            plot.background = element_blank(), 
            plot.title =  element_text(family = "Roboto", face = "bold", color = "black", size = 16),
            plot.subtitle = element_text(family = "Roboto", face = "bold", color = "black", size = 40),
            plot.caption = element_blank(),
            plot.margin = margin(0, 0, 0, 0), complete = TRUE)
    
    
    if(input$maille_TERR %in% 'REG') { libelle_NIVGEO <- "une région française "} 
    else if ( input$maille_TERR %in% 'DEP') { libelle_NIVGEO <- "un département français "}
    else if ( input$maille_TERR %in% 'EPCI') { libelle_NIVGEO <- "une intercommunalité française "}
    
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

                                mutate(tip = case_when(input$type_pop %in% 'PRES' ~ paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                                           "<font size=2.5 color=white family=Roboto>",
                                                                                           "En moyenne ","<b>", percent(value, accuracy = 2),"</b>"," des individus pr&eacute;sents ",  
                                                                                           "dans ",  stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),"<br>",
                                                                                           "en 2015 ","sont des ","<b>",tolower(CS1_LIB),"</b>", "<br>"),
                                                       input$type_pop %in% 'ENTR' ~ paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                                           "<font size=2.5 color=white family=Roboto>",
                                                                                           "En moyenne ","<b>", percent(value, accuracy = 2),"</b>"," des individus arriv&eacute;s ",  
                                                                                           "dans ",  stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),"<br>",
                                                                                           "entre 2014 et 2015 ","sont des ","<b>",tolower(CS1_LIB),"</b>", "<br>"),
                                                       input$type_pop %in% 'SORT' ~ paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                                           "<font size=2.5 color=white family=Roboto>",
                                                                                           "En moyenne ","<b>", percent(value, accuracy = 2),"</b>"," des individus ayant quitt&eacute; ",  
                                                                                           stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),"<br>",
                                                                                           "entre 2014 et 2015 ","sont des ","<b>",tolower(CS1_LIB),"</b>", "<br>")
                                ))
    ) +      geom_bar_interactive(aes(x=2, y=value_pct1, fill=CS1_LIB,
                                      tooltip = tip,
                                      data_id = CS1_LIB),
                                  stat="identity")+
      scale_x_continuous(limits = c(-3.5, 2.5)) +
      theme_nothing() +
      geom_text(aes(x = 2.1, y = 0.04, label = "Moyenne \nFrance"), size = 3.3,  color = "black", angle = 30) +
      
      coord_polar(theta = "y", direction = -1,start = 0.6) +
      scale_fill_manual(guide = TRUE, values = c("#00B050","#984807","#558ED5","#7030A0","#E46C0A","#FF0000","#969697","#404040",NA)) 
    
    
    
    
    # graphique complet ggiraph
    tooltip_css <- "background-color:white;padding:2px;font-size: 80%;color: white;opacity:0.2"
    x <- girafe(code = print(ggdraw() +
                               draw_plot(gg_diagcirc_MOY + theme(legend.justification = "bottom"), x = 0.305, y = 0.445, width = 0.4, height = 0.3) +
                               draw_plot(gg_diagcirc_TERR + theme(legend.justification = "top"), 0, 0, 1, 1) ),
                width = 1,
                width_svg = 6, height_svg = 8  )
    
    
    girafe_options(x, 
                   opts_tooltip(use_fill = TRUE, offx = 30, offy = 25, use_cursor_pos = FALSE, css = tooltip_css),
                   opts_hover(css = "stroke:red;r:5pt;"),
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
      select(NIV_TERR,TERR, AGEREVS, nb_ind_ENTR, nb_ind_SORT) %>%
      group_by(TERR) %>%
      mutate_at(.vars = vars(nb_ind_ENTR, nb_ind_SORT), .funs = funs( ./(sum(.)) ) ) %>%
      ungroup() %>%
      group_by(NIV_TERR, AGEREVS) %>%
      summarise_if(is.numeric, funs(mean)) %>%
      # avec moyennes pondérées pour pop présente
      left_join(
        indics_migres_TERR_AGEREVS %>%
          as.data.frame() %>%
          filter(NIV_TERR %in% input$maille_TERR) %>%
          select(NIV_TERR,TERR, AGEREVS, nb_ind_PRES) %>%
          ungroup() %>%
          group_by(AGEREVS) %>%
          summarise(nb_ind_PRES = sum(nb_ind_PRES) ) %>%
          mutate(nb_ind_PRES = nb_ind_PRES / sum(nb_ind_PRES)),
        by = "AGEREVS")
    
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
    
    if(input$maille_TERR %in% 'REG') { libelle_NIVGEO <- "la région "} 
    else if ( input$maille_TERR %in% 'DEP') { libelle_NIVGEO <- "le département "}
    else if ( input$maille_TERR %in% 'EPCI') { libelle_NIVGEO <- "l'intercommunalité "}
    
    # graphique circulaire profil moyen
    
    gg_diagcirc_TERR = ggplot(data = filteredData_profilpop_AGEREVS() %>%
                                arrange(AGEREVS) %>%
                                rename(value = !!sym(VAR_type_pop)) %>%
                                add_row(  "AGEREVS" ="7",    "value" = 0.4) %>%
                                mutate(value_pct1 = value / sum(value)) %>%
                                mutate(pos = cumsum(value_pct1)- value_pct1/2) %>%
                                mutate(AGEREVS = factor(AGEREVS, levels = c("< 20 ans","20-29 ans","30-39 ans","40-49 ans","50-64 ans","> 65 ans","0"))) %>% 
                                mutate(AGEREVS_txt = recode(AGEREVS, "< 20 ans" = "moins de 20 ans",
                                                            "20-29 ans" = "entre 20 et 29 ans",
                                                            "30-39 ans" = "entre 30 et 39 ans",
                                                            "40-49 ans" = "entre 40 et 49 ans",
                                                            "50-64 ans" = "entre 50 et 64 ans",
                                                            "> 65 ans" = "plus de 65 ans"))  %>%
                                mutate(tip = case_when(input$type_pop %in% 'PRES' ~ paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                                           "<font size=2.5 color=white family=Roboto>",
                                                                                           "<b>", percent(value, accuracy = 2),"</b>"," des individus pr&eacute;sents dans ", 
                                                                                           stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),"<b>",stri_trans_general(conv_accents(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% distinct(LIB_TERR) %>% pull()),"Latin-ASCII"),"</b>" ,"<br>",
                                                                                           "en 2015 ","ont ","<b>",AGEREVS_txt,"</b>", "<br>"),
                                                       input$type_pop %in% 'ENTR' ~ paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                                           "<font size=2.5 color=white family=Roboto>",
                                                                                           "<b>", percent(value, accuracy = 2),"</b>"," des individus arriv&eacute;s dans ", 
                                                                                           stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),"<b>",stri_trans_general(conv_accents(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% distinct(LIB_TERR) %>% pull()),"Latin-ASCII"),"</b>" ,"<br>",
                                                                                           "entre 2014 et 2015 ","ont ","<b>",AGEREVS_txt,"</b>", "<br>"),
                                                       input$type_pop %in% 'SORT' ~ paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                                           "<font size=2.5 color=white family=Roboto>",
                                                                                           "<b>", percent(value, accuracy = 2),"</b>"," des individus ayant quitt&eacute; ",
                                                                                           stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),"<b>",stri_trans_general(conv_accents(filteredData_nuagepoint_evoldemo() %>% filter(filtre_TERR %in% 'TERR') %>% distinct(LIB_TERR) %>% pull()),"Latin-ASCII"),"</b>" ,"<br>",
                                                                                           "entre 2014 et 2015 ","ont ","<b>",AGEREVS_txt,"</b>", "<br>")
                                ))
    ) +
      
      geom_bar_interactive(aes(x=2, y=value_pct1, fill=AGEREVS,
                               tooltip = tip,
                               data_id = AGEREVS),
                           stat="identity")+
      xlim(0.5, 2.5) +
      labs(
        title = "Par groupe socioprofessionnel"#,
      ) +
      geom_text(aes(x = 2.1, y = 0.04, label = "Profil du\nterritoire"), size = 5.5,  color = "black", angle = 30) +
      coord_polar(theta = "y", direction = -1,start = 0.6) +
      scale_fill_manual(#guide = FALSE,
        name = "",
        labels = c("< 20 ans","20-29 ans","30-39 ans","40-49 ans","50-64 ans","> 65 ans","","",""),
        values = c("#8c510a", "#d8b365", "#dfc27d", "#80cdc1", "#5ab4ac", "#01665e",NA,NA,NA)) +
      labs(
        title = "Par tranche d'âge",
        subtitle = ""#,
      ) +
      theme(line = element_blank(), rect = element_blank(),
            text = element_text(family = "Roboto", face = "plain",
                                colour = "black", size = 8, lineheight = 0.9,
                                hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                                debug = FALSE), axis.line = element_blank(),
            axis.line.x = NULL, axis.line.y = NULL, axis.text = element_blank(),
            axis.text.x = element_blank(), axis.text.x.top = element_blank(),
            axis.text.y = element_blank(), axis.text.y.right = element_blank(),
            axis.ticks = element_blank(), axis.ticks.length = unit(0,"pt"),
            axis.title.x = element_blank(), axis.title.x.top = element_blank(),
            axis.title.y = element_blank(), axis.title.y.right = element_blank(),
            legend.background = element_blank(), legend.spacing = unit(0.4, "cm"),
            legend.spacing.x = NULL, legend.spacing.y = NULL,
            legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
            legend.key = element_blank(), legend.key.size = unit(0.6,"lines"),
            legend.box = "horizontal",
            legend.direction = "vertical",
            legend.text = element_text(size = rel(1.5)), legend.text.align = NULL,
            legend.title = element_text(hjust = 0), legend.title.align = NULL,
            legend.position = "bottom", 
            legend.justification = "left", 
            legend.box.margin = margin(0, 0, 0, 0, "cm"), legend.box.background = element_blank(),
            legend.box.spacing = unit(0.4, "cm"), panel.background = element_blank(),
            panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), panel.spacing = unit(0,"pt"),
            panel.spacing.x = NULL, panel.spacing.y = NULL,
            panel.ontop = FALSE, strip.background = element_blank(),
            strip.text = element_blank(), strip.text.x = element_blank(),
            strip.text.y = element_blank(), strip.placement = "inside",
            strip.placement.x = NULL, strip.placement.y = NULL,
            strip.switch.pad.grid = unit(0, "cm"), strip.switch.pad.wrap = unit(0, "cm"),
            plot.background = element_blank(), 
            plot.title =  element_text(family = "Roboto", face = "bold", color = "black", size = 16),
            plot.subtitle = element_text(family = "Roboto", face = "bold", color = "black", size = 40),
            plot.caption = element_blank(),
            plot.margin = margin(0, 0, 0, 0), complete = TRUE)
    
    
    if(input$maille_TERR %in% 'REG') { libelle_NIVGEO <- "une région française "} 
    else if ( input$maille_TERR %in% 'DEP') { libelle_NIVGEO <- "un département français "}
    else if ( input$maille_TERR %in% 'EPCI') { libelle_NIVGEO <- "une intercommunalité française "}
    
    
    # graphique circulaire profil moyen
    gg_diagcirc_MOY = ggplot(data = filteredData_profilpop_AGEREVS_MOY() %>%
                               as.data.frame() %>%
                               arrange(AGEREVS) %>%
                               rename(value = !!sym(VAR_type_pop)) %>%
                               add_row(  "AGEREVS" ="7",    "value" = 0.4) %>%
                               mutate(value_pct1 = value / sum(value)) %>%
                               mutate(pos = cumsum(value_pct1)- value_pct1/2) %>%
                               mutate(AGEREVS = factor(AGEREVS, levels = c("< 20 ans","20-29 ans","30-39 ans","40-49 ans","50-64 ans","> 65 ans","0"))) %>% 
                               mutate(AGEREVS_txt = recode(AGEREVS, "< 20 ans" = "moins de 20 ans",
                                                           "20-29 ans" = "entre 20 et 29 ans",
                                                           "30-39 ans" = "entre 30 et 39 ans",
                                                           "40-49 ans" = "entre 40 et 49 ans",
                                                           "50-64 ans" = "entre 50 et 64 ans",
                                                           "> 65 ans" = "plus de 65 ans")) %>%
                               mutate(tip = case_when(input$type_pop %in% 'PRES' ~ paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                                          "<font size=2.5 color=white family=Roboto>",
                                                                                          "En moyenne ","<b>", percent(value, accuracy = 2),"</b>"," des individus pr&eacute;sents dans ", 
                                                                                          stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),"<br>",
                                                                                          "en 2015 ","ont ","<b>",AGEREVS_txt,"</b>", "<br>"),
                                                      input$type_pop %in% 'ENTR' ~ paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                                          "<font size=2.5 color=white family=Roboto>",
                                                                                          "En moyenne ","<b>", percent(value, accuracy = 2),"</b>"," des individus arriv&eacute;s dans ", 
                                                                                          stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),"<br>",
                                                                                          "entre 2014 et 2015 ","ont ","<b>",AGEREVS_txt,"</b>", "<br>"),
                                                      input$type_pop %in% 'SORT' ~ paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                                          "<font size=2.5 color=white family=Roboto>",
                                                                                          "En moyenne ","<b>", percent(value, accuracy = 2),"</b>"," des individus ayant quitt&eacute; ", 
                                                                                          stri_trans_general(conv_accents(libelle_NIVGEO),"Latin-ASCII"),"<br>",
                                                                                          "entre 2014 et 2015 ","ont ","<b>",AGEREVS_txt,"</b>", "<br>")
                               ))
    ) +
      geom_bar_interactive(aes(x=2, y=value_pct1, fill=AGEREVS,
                               tooltip = tip,
                               data_id = AGEREVS),
                           stat="identity")+
      scale_x_continuous(limits = c(-3.5, 2.5)) +
      theme_nothing() +
      theme(text = element_text(family = "Roboto")) +
      geom_text(aes(x = 2.1, y = 0.04, label = "Moyenne\nFrance"), size = 3.3,  color = "black", angle = 30) +
      coord_polar(theta = "y", direction = -1,start = 0.6) +
      scale_fill_manual(guide = FALSE, values = c("#8c510a", "#d8b365", "#dfc27d", "#80cdc1", "#5ab4ac", "#01665e",NA,NA,NA)) 
    
    
    # graphique complet ggiraph
    tooltip_css <- "background-color:white;padding:2px;font-size: 80%;color: white;opacity:0.2"
    
    x <- girafe(code = print(ggdraw() +
                               draw_plot(gg_diagcirc_MOY + theme(legend.justification = "bottom"), x = 0.30, y = 0.4, width = 0.41, height = 0.34) +
                               draw_plot(gg_diagcirc_TERR + theme(legend.justification = "top"), 0, 0, 1, 1) ),
                width = 1,
                width_svg = 6, height_svg = 8  )
    
    
    girafe_options(x, 
                   opts_tooltip(use_fill = TRUE, offx = 30, offy = 25, use_cursor_pos = FALSE, css = tooltip_css),
                   opts_hover(css = "stroke:red;r:5pt;"),
                   opts_selection(type = "none"),
                   opts_zoom(max = 1),
                   opts_toolbar(position = "bottomleft", saveaspng = FALSE) )
    
    
    
  })  
  
  
  
  #### renouvellement / evolution de la pop due aux mobilités  ####
  
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
        summarise_at(.vars = vars( pct_ind_PRES = pct_ind_PRES, evol_pct_AUTO_PRES = evol_pct_AUTO_PRES),
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
        summarise_at(.vars = vars( pct_ind_PRES = pct_ind_PRES, evol_pct_AUTO_PRES = evol_pct_AUTO_PRES),
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
        summarise_at(.vars = vars( pct_ind_PRES = pct_ind_PRES, evol_pct_AUTO_PRES = evol_pct_AUTO_PRES),
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
      mutate(lib_Q_pct_ind_PRES = case_when(Q_pct_ind_PRES %in% 'Q1' ~ "très faible",
                                            Q_pct_ind_PRES %in% 'Q2' ~ "relativement faible",
                                            Q_pct_ind_PRES %in% 'Q3' ~ "similaire",
                                            Q_pct_ind_PRES %in% 'Q4' ~ "relativement élevée",
                                            Q_pct_ind_PRES %in% 'Q5' ~ "très élevée")) %>%
      #  libellés quantiles en clair simplifiés
      mutate(lib_Q_evol_pct_AUTO_PRES = case_when( evol_pct_AUTO_PRES < -0.0002 ~ "diminue",
                                                   evol_pct_AUTO_PRES > 0.0002 ~ "augmente",
                                                   TRUE ~ "reste stable"))
    
    
    # bornes limites des axes
    # gestion du cas avec valeur intercommunalité atypique :
    # si valeur max du territoire > à 25% de la valeur max globale, on borne à +/-0.1 pts
    borne_axe_max <-
      indics_renouv_CS1_terr_vs_moy %>% select(evol_pct_AUTO_PRES_min) %>% min() %>% abs() %>% rbind(indics_renouv_CS1_terr_vs_moy %>% select(evol_pct_AUTO_PRES_max) %>% max()) %>% max()
    
    borne_axe_max <-
      if( indics_renouv_CS1_terr_vs_moy %>% select(evol_pct_AUTO_PRES) %>% min() %>% abs() %>% rbind(indics_renouv_CS1_terr_vs_moy %>% select(evol_pct_AUTO_PRES) %>% max()) %>% max() >  0.25 * borne_axe_max) {
        borne_axe_max
        
      } else { borne_axe_max * 0.35  }
    
    
    
    niveau_TERR <- indics_renouv_CS1_terr_vs_moy %>% distinct(NIV_TERR) %>% pull()
    
    
    
    p1 =
      ggplot(data=indics_renouv_CS1_terr_vs_moy  %>%
               mutate(CS1_LIB = conv_accents(CS1_LIB)) %>%
               mutate(CS1_LIB= factor(CS1_LIB, levels=rev(unique(CS1_LIB[order(CS1)])), ordered=TRUE) ) %>%
               mutate(lib_Q_pct_ind_PRES = conv_accents(lib_Q_pct_ind_PRES)) %>%
               mutate(lib_Q_evol_pct_AUTO_PRES = conv_accents(lib_Q_evol_pct_AUTO_PRES)) %>%
               mutate(LIB_TERR = stri_trans_general(conv_accents(LIB_TERR),"Latin-ASCII")) %>%
             mutate(tip = case_when( niveau_TERR %in% 'REG' ~
                                       paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                              "<font size=2.5 color=white family=Roboto>" , "Les ","<b>",tolower(CS1_LIB),"</b>"," repr&eacute;sentent ",percent(pct_ind_PRES, accuracy = 1)," de la population de la r&eacute;gion ",LIB_TERR, "<br>",
                                              "soit une part ","<b>",  lib_Q_pct_ind_PRES,"</b>"," par rapport aux autres r&eacute;gions de France (moyenne : ",percent(pct_ind_PRES_moy, accuracy = 1), ").","</font>", "<br>",
                                              "<font size=2.5 color=white family=Roboto>" , "Par le jeu des mobilit&eacute;s r&eacute;sidentielles, cette part ","<b>",lib_Q_evol_pct_AUTO_PRES, "</b>", " (",
                                              # format pourcentage avec signe
                                              paste0(symnum(evol_pct_AUTO_PRES,
                                                            c(-Inf, 0, Inf),
                                                            c("", "+")),
                                                     #percent(evol_pct_AUTO_PRES, accuracy = 0.01),""),
                                                     round(evol_pct_AUTO_PRES*100, 2)," points de %)")," entre 2014 et 2015.", "<br>"),
                                     #" entre 2014 et 2015 par rapport aux autres r&eacute;gions.</font>", "<br>"  ),
                                     niveau_TERR %in% 'DEP' ~
                                       paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                              "<font size=2.5 color=white family=Roboto>" , "Les ","<b>",tolower(CS1_LIB),"</b>"," repr&eacute;sentent ",percent(pct_ind_PRES, accuracy = 1)," de la population du d&eacute;partement ",LIB_TERR, "<br>",
                                              "soit une part ","<b>",  lib_Q_pct_ind_PRES,"</b>"," par rapport aux autres d&eacute;partements de France (moyenne : ",percent(pct_ind_PRES_moy, accuracy = 1), ").","</font>", "<br>",
                                              "<font size=2.5 color=white family=Roboto>" , "Par le jeu des mobilit&eacute;s r&eacute;sidentielles, cette part ","<b>",lib_Q_evol_pct_AUTO_PRES, "</b>", " (",
                                              # format pourcentage avec signe
                                              paste0(symnum(evol_pct_AUTO_PRES,
                                                            c(-Inf, 0, Inf),
                                                            c("", "+")),
                                                     #percent(evol_pct_AUTO_PRES, accuracy = 0.01),""),
                                                     round(evol_pct_AUTO_PRES*100, 2)," points de %)")," entre 2014 et 2015.","<br>"),
                                     #" entre 2014 et 2015 par rapport aux autres d&eacute;partements.</font>", "<br>"  ),
                                     niveau_TERR %in% 'EPCI' ~
                                       paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                              "<font size=2.5 color=white family=Roboto>" , "Les ","<b>",tolower(CS1_LIB),"</b>"," repr&eacute;sentent ",percent(pct_ind_PRES, accuracy = 1)," de la population de l&apos;intercommunalit&eacute; ",LIB_TERR, "<br>",
                                              "soit une part ","<b>",  lib_Q_pct_ind_PRES,"</b>"," par rapport aux autres intercommunalit&eacute;s de France (moyenne : ",percent(pct_ind_PRES_moy, accuracy = 1), ").","</font>", "<br>",
                                              "<font size=2.5 color=white family=Roboto>" , "Par le jeu des mobilit&eacute;s r&eacute;sidentielles, cette part ","<b>",lib_Q_evol_pct_AUTO_PRES, "</b>", " (",
                                              # format pourcentage avec signe
                                              paste0(symnum(evol_pct_AUTO_PRES,
                                                            c(-Inf, 0, Inf),
                                                            c("", "+")),
                                                     #percent(evol_pct_AUTO_PRES, accuracy = 0.01),""),
                                                     round(evol_pct_AUTO_PRES*100, 2)," points de %)")," entre 2014 et 2015.","<br>")#,
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
                             stat = "identity", shape = 21, size = 2, alpha = 1, stroke = 0.5, color = "grey30") +
      scale_y_continuous( name = 'Part du groupe parmi la population présente', labels = percent_format(accuracy = 1)) +
      scale_x_continuous( name = 'Évolution de la part du groupe par les mobilités résidentielles',
                          limits=c(-borne_axe_max,borne_axe_max),
                          #labels=function(x)paste0(symnum(x,c(-Inf, 0, Inf),c("", "+")),percent(x, accuracy = 0.1),"")) +
                          labels=function(x)paste0(symnum(x,c(-Inf, 0, Inf),c("", "+")), round(x*100, 1)," pts")) +
      scale_fill_manual(#guide = FALSE,
        name = "",
        labels = rev(c("Agriculteurs exploitants","Artisans, commerçants et chefs d'entreprise","Cadres et professions intellectuelles supérieures","Professions intermédiaires",
                       "Employés","Ouvriers","Retraités","Autres personnes sans activité professionnelle\n(dont élèves et étudiants)")),
        values = rev(c("#00B050","#984807","#558ED5","#7030A0","#E46C0A","#FF0000","#969697","#404040"))) +  
      scale_color_manual(guide = FALSE, values = rev(c("#00B050","#984807","#558ED5","#7030A0","#E46C0A","#FF0000","#969697","#404040"))) +
       guides(fill = guide_legend(reverse=T)) +
      labs(
        title = "Par groupe socioprofessionnel",
        subtitle = ""#,
      ) +
      theme(strip.text.y = element_text(angle = 360),
            text = element_text(family = "Roboto", color = "black"),
            plot.title =  element_text(family = "Roboto", face = "bold", color = "black", size = 16),
            plot.subtitle = element_text(family = "Roboto", face = "bold", color = "black", size = 120),
            panel.background = element_rect(fill = NA, color = NA),
            plot.background = element_rect(fill = NA, color = NA),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            axis.line =  element_line( color = "grey"),
            panel.grid.major = element_line(color = "#2b2b2b99",size = 0.1),
            panel.grid.minor = element_line(color = "#2b2b2b99", size = 0.05),
            legend.spacing = unit(0.4, "cm"),
            legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
            legend.key = element_blank(), legend.key.size = unit(0.6,"lines"),
            legend.box = "horizontal",
            legend.direction = "vertical",
            legend.text = element_text(size = rel(0.9)), 
            legend.title = element_text(hjust = 0), 
            legend.position = "bottom", 
            legend.justification = "left", 
            legend.box.margin = margin(0, 0, 0, 0, "cm"),
            legend.box.spacing = unit(0.4, "cm"))
    
    
    tooltip_css <- "background-color:#272B30;padding:2px;font-size: 80%;color: white;opacity:0.2"
    x <- girafe(ggobj = p1, width = 1,
                width_svg = 6, height_svg = 9 )
    girafe_options(x, 
                   opts_tooltip(use_fill = TRUE, offx = 0, offy = 35, use_cursor_pos = FALSE, css = tooltip_css),
                   opts_hover(css = "stroke:red;r:5pt;"),
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
        summarise_at(.vars = vars( pct_ind_PRES = pct_ind_PRES, evol_pct_AUTO_PRES = evol_pct_AUTO_PRES),
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
        summarise_at(.vars = vars( pct_ind_PRES = pct_ind_PRES, evol_pct_AUTO_PRES = evol_pct_AUTO_PRES),
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
        summarise_at(.vars = vars( pct_ind_PRES = pct_ind_PRES, evol_pct_AUTO_PRES = evol_pct_AUTO_PRES),
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
      mutate(lib_Q_pct_ind_PRES = case_when(Q_pct_ind_PRES %in% 'Q1' ~ "très faible",
                                            Q_pct_ind_PRES %in% 'Q2' ~ "relativement faible",
                                            Q_pct_ind_PRES %in% 'Q3' ~ "similaire",
                                            Q_pct_ind_PRES %in% 'Q4' ~ "relativement élevée",
                                            Q_pct_ind_PRES %in% 'Q5' ~ "très élevée")) %>%
      #  libellés quantiles en clair simplifiés
      mutate(lib_Q_evol_pct_AUTO_PRES = case_when( evol_pct_AUTO_PRES < -0.0002 ~ "diminue",
                                                   evol_pct_AUTO_PRES > 0.0002 ~ "augmente",
                                                   TRUE ~ "reste stable"))
    
    
    
    # bornes limites des axes
    # gestion du cas avec valeur intercommunalité atypique :
    # si valeur max du territoire > à 25% de la valeur max globale, on borne à +/-0.1 pts
    borne_axe_max <-
      indics_renouv_AGEREVS_terr_vs_moy %>% select(evol_pct_AUTO_PRES_min) %>% min() %>% abs() %>% rbind(indics_renouv_AGEREVS_terr_vs_moy %>% select(evol_pct_AUTO_PRES_max) %>% max()) %>% max()
    
    borne_axe_max <-
      if( indics_renouv_AGEREVS_terr_vs_moy %>% select(evol_pct_AUTO_PRES) %>% min() %>% abs() %>% rbind(indics_renouv_AGEREVS_terr_vs_moy %>% select(evol_pct_AUTO_PRES) %>% max()) %>% max() >  0.25 * borne_axe_max) {
        borne_axe_max
        
      } else { borne_axe_max * 0.35  }
    
    
    niveau_TERR <- indics_renouv_AGEREVS_terr_vs_moy %>% distinct(NIV_TERR) %>% pull()
    
    
    p1 =
      ggplot(data=indics_renouv_AGEREVS_terr_vs_moy  %>%
               mutate(AGEREVS = factor(AGEREVS, levels = c("< 20 ans","20-29 ans","30-39 ans","40-49 ans","50-64 ans","> 65 ans"))) %>%
               mutate(AGEREVS_txt = recode(AGEREVS, "< 20 ans" = "moins de 20 ans",
                                           "20-29 ans" = "20 à 29 ans",
                                           "30-39 ans" = "30 à 39 ans",
                                           "40-49 ans" = "40 à 49 ans",
                                           "50-64 ans" = "50 à 64 ans",
                                           "> 65 ans" = "plus de 65 ans"))  %>%
               mutate(AGEREVS_txt = stri_trans_general(conv_accents(AGEREVS_txt),"Latin-ASCII")) %>%
               mutate(lib_Q_pct_ind_PRES = conv_accents(lib_Q_pct_ind_PRES)) %>%
               mutate(lib_Q_evol_pct_AUTO_PRES = conv_accents(lib_Q_evol_pct_AUTO_PRES)) %>%
               mutate(LIB_TERR = stri_trans_general(conv_accents(LIB_TERR),"Latin-ASCII")) %>%
               mutate(tip = case_when( niveau_TERR %in% 'REG' ~
                                         paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                "<font size=2.5 color=white family=Roboto>" , "Les ","<b>",AGEREVS_txt,"</b>"," repr&eacute;sentent ",percent(pct_ind_PRES, accuracy = 1)," de la population de la r&eacute;gion ",LIB_TERR, "<br>",
                                                "soit une part ","<b>",  lib_Q_pct_ind_PRES,"</b>"," par rapport aux autres r&eacute;gions de France (moyenne : ",percent(pct_ind_PRES_moy, accuracy = 1), ").","</font>", "<br>",
                                                "<font size=2.5 color=white family=Roboto>" , "Par le jeu des mobilit&eacute;s r&eacute;sidentielles, cette part ","<b>",lib_Q_evol_pct_AUTO_PRES, "</b>", " (",
                                                # format pourcentage avec signe
                                                paste0(symnum(evol_pct_AUTO_PRES,
                                                              c(-Inf, 0, Inf),
                                                              c("", "+")),
                                                       round(evol_pct_AUTO_PRES*100, 2)," points de %)")," entre 2014 et 2015.", "<br>"),
                                       niveau_TERR %in% 'DEP' ~
                                         paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                "<font size=2.5 color=white family=Roboto>" , "Les ","<b>",AGEREVS_txt,"</b>"," repr&eacute;sentent ",percent(pct_ind_PRES, accuracy = 1)," de la population du d&eacute;partement ",LIB_TERR, "<br>",
                                                "soit une part ","<b>",  lib_Q_pct_ind_PRES,"</b>"," par rapport aux autres d&eacute;partements de France (moyenne : ",percent(pct_ind_PRES_moy, accuracy = 1), ").","</font>", "<br>",
                                                "<font size=2.5 color=white family=Roboto>" , "Par le jeu des mobilit&eacute;s r&eacute;sidentielles, cette part ","<b>",lib_Q_evol_pct_AUTO_PRES, "</b>", " (",
                                                # format pourcentage avec signe
                                                paste0(symnum(evol_pct_AUTO_PRES,
                                                              c(-Inf, 0, Inf),
                                                              c("", "+")),
                                                       round(evol_pct_AUTO_PRES*100, 2)," points de %)")," entre 2014 et 2015.", "<br>"),
                                       niveau_TERR %in% 'EPCI' ~
                                         paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                "<font size=2.5 color=white family=Roboto>" , "Les ","<b>",AGEREVS_txt,"</b>"," repr&eacute;sentent ",percent(pct_ind_PRES, accuracy = 1)," de la population de l&apos;intercommunalit&eacute; ",LIB_TERR, "<br>",
                                                "soit une part ","<b>",  lib_Q_pct_ind_PRES,"</b>"," par rapport aux autres intercommunalit&eacute;s de France (moyenne : ",percent(pct_ind_PRES_moy, accuracy = 1), ").","</font>", "<br>",
                                                "<font size=2.5 color=white family=Roboto>" , "Par le jeu des mobilit&eacute;s r&eacute;sidentielles, cette part ","<b>",lib_Q_evol_pct_AUTO_PRES, "</b>", " (",
                                                # format pourcentage avec signe
                                                paste0(symnum(evol_pct_AUTO_PRES,
                                                              c(-Inf, 0, Inf),
                                                              c("", "+")),
                                                       round(evol_pct_AUTO_PRES*100, 2)," points de %)")," entre 2014 et 2015.", "<br>")
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
                             stat = "identity", shape = 21, size = 2, alpha = 1, stroke = 0.5, color = "grey30") +
      scale_y_continuous( name = "Part de la tranche d'âge parmi la population présente", labels = percent_format(accuracy = 1)) +
      scale_x_continuous( name = "Évolution de la part de la tranche d'âge par les mobilités résidentielles",
                          limits=c(-borne_axe_max,borne_axe_max),
                          labels=function(x)paste0(symnum(x,c(-Inf, 0, Inf),c("", "+")), round(x*100, 1)," pts")) +
      scale_fill_manual(#guide = FALSE,
        name = "",
        values = rev(c("#8c510a", "#d8b365", "#dfc27d", "#80cdc1", "#5ab4ac", "#01665e"))) +
      scale_color_manual(guide = FALSE, values = rev(c("#8c510a", "#d8b365", "#dfc27d", "#80cdc1", "#5ab4ac", "#01665e"))) +
      labs(
        title = "Par tranche d'âge",
        subtitle = ""#,
      ) +
      theme(strip.text.y = element_text(angle = 360),
            text = element_text(family = "Roboto", color = "black"),
            plot.title =  element_text(family = "Roboto", face = "bold", color = "black", size = 16),
            plot.subtitle = element_text(family = "Roboto", face = "bold", color = "black", size = 120),
            panel.background = element_rect(fill = NA, color = NA),
            plot.background = element_rect(fill = NA, color = NA),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            axis.line =  element_line( color = "grey"),
            panel.grid.major = element_line(color = "#2b2b2b99",size = 0.1),
            panel.grid.minor = element_line(color = "#2b2b2b99", size = 0.05),
            legend.spacing = unit(0.4, "cm"),
            legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
            legend.key = element_blank(), legend.key.size = unit(0.6,"lines"),
            legend.box = "horizontal",
            legend.direction = "vertical",
            legend.text = element_text(size = rel(0.9)), 
            legend.title = element_text(hjust = 0), 
            legend.position = "bottom", 
            legend.justification = "center", 
            legend.box.margin = margin(0, 0, 0, 0, "cm"),
            legend.box.spacing = unit(0.4, "cm"))
    
    
    tooltip_css <- "background-color:#272B30;padding:2px;font-size: 80%;color: white;opacity:0.2"
    x <- girafe(ggobj = p1, width = 1,
                width_svg = 6, height_svg = 9 )
    girafe_options(x, 
                   opts_tooltip(use_fill = TRUE, offx = 0, offy = 35, use_cursor_pos = FALSE, css = tooltip_css),
                   opts_hover(css = "stroke:red;r:5pt;"),
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
        mutate(ratio_ligne = 15000) %>%
        mutate(ratio_fleche = 20) 
      
      
    } else if ( input$maille_TERR %in% 'DEP') {
      
      flux_migres_TERR.map %>% 
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_ACTU %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP) ) %>%
        # cappé à 10 flux minimum ?
        filter(nb_ind >= 10) %>%
        # définition du ratio
        mutate(ratio_ligne = 8000) %>%
        mutate(ratio_fleche = 30) 
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      
      flux_migres_TERR.map %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_ACTU %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI) ) %>%
        # cappé à 10 flux minimum ?
        filter(nb_ind >= 10)%>%
        # définition du ratio
        mutate(ratio_ligne = 2500) %>%
        mutate(ratio_fleche = 60) 
      
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
        mutate(ratio_ligne = 15000) %>%
        mutate(ratio_fleche = 20) 
      
    } else if ( input$maille_TERR %in% 'DEP') {
      
      flux_migres_TERR.map %>% 
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_ANTE %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP) ) %>%
        # cappé à 10 flux minimum ?
        filter(nb_ind >= 10) %>%
        # définition du ratio
        mutate(ratio_ligne = 8000) %>%
        mutate(ratio_fleche = 30) 
      
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      
      flux_migres_TERR.map %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_ANTE %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI) ) %>%
        # cappé à 10 flux minimum ?
        filter(nb_ind >= 10)%>%
        # définition du ratio
        mutate(ratio_ligne = 2500) %>%
        mutate(ratio_fleche = 60) 
      
    }
  })
  
  # dataframe pour flux SM net
  
  filteredData_flux_SMNET <- reactive({
    if(input$maille_TERR %in% 'REG') {
      
      flux_migres_TERR.map.SMnet %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_j %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_REG)) %>%

        # définition du ratio
        mutate(ratio_ligne = 15000) %>%
        mutate(ratio_fleche = 20)  %>%
        mutate(type_flow = "entr") %>%
        rbind.data.frame(flux_migres_TERR.map.SMnet %>%
                           filter(NIV_TERR %in% input$maille_TERR) %>%
                           filter(TERR_i %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_REG)) %>%
                           # définition du ratio
                           mutate(ratio_ligne = 15000) %>%
                           mutate(ratio_fleche = 20) %>%
                           mutate(type_flow = "sort"))
      
      
    } else if ( input$maille_TERR %in% 'DEP') {
      
      
      flux_migres_TERR.map.SMnet %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_j %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP)) %>%
        # définition du ratio
        mutate(ratio_ligne = 8000) %>%
        mutate(ratio_fleche = 30)  %>%
        mutate(type_flow = "entr") %>%
        rbind.data.frame(flux_migres_TERR.map.SMnet %>%
                           filter(NIV_TERR %in% input$maille_TERR) %>%
                           filter(TERR_i %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP)) %>%
                          # définition du ratio
                           mutate(ratio_ligne = 8000) %>%
                           mutate(ratio_fleche = 30) %>%
                           mutate(type_flow = "sort"))
      
      
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      
      flux_migres_TERR.map.SMnet %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR_j %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI)) %>%
        # définition du ratio
        mutate(ratio_ligne = 2500) %>%
        mutate(ratio_fleche = 60) %>%
        mutate(type_flow = "entr") %>%
        rbind.data.frame(flux_migres_TERR.map.SMnet %>%
                           filter(NIV_TERR %in% input$maille_TERR) %>%
                           filter(TERR_i %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI)) %>%
                           # définition du ratio
                           mutate(ratio_ligne = 2500) %>%
                           mutate(ratio_fleche = 60) %>%
                           mutate(type_flow = "sort"))
      
      
    }
  })
  
  ### territoire cible geo
  
  filteredData_geo_TERR <- reactive({
    if(input$maille_TERR %in% 'REG') {
      
      geo_TERR_poly %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_REG)) 
      
    } else if ( input$maille_TERR %in% 'DEP') {
      geo_TERR_poly %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_DEP)) 
      
    } else if ( input$maille_TERR %in% 'EPCI') {
      
      geo_TERR_poly %>%
        filter(NIV_TERR %in% input$maille_TERR) %>%
        filter(TERR %in% gsub(".*\\((.*)\\).*", "\\1", input$territoireetude_EPCI)) 
    }
  })
  
  #########################
  # cartographie des flux 
  
  output$flux <- renderggiraph({
    
    if(input$type_flux %in% 'ENTR') {
      
      ratio_ligne <- filteredData_flux_ENTR() %>% distinct(ratio_ligne) %>% pull()
      ratio_fleche <- filteredData_flux_ENTR() %>% distinct(ratio_fleche) %>% pull()
      niveau_TERR <- filteredData_flux_ENTR() %>% distinct(NIV_TERR) %>% pull()
      
      # cartogrammes de dorling pour exploser les lignes si superposition
      filteredData_flux_ENTR.sf <- st_as_sf(x = filteredData_flux_ENTR(), 
                                            coords = c("x_ctr_ANTE", "y_ctr_ANTE"),
                                            crs = 2154) %>%
        mutate(nb_ind.log = log(nb_ind))
      
      if(niveau_TERR %in% 'EPCI') {
        filteredData_flux_ENTR.sf.do <- cartogram_dorling(filteredData_flux_ENTR.sf, "nb_ind.log", k=0.01, itermax = 100) %>%
          mutate(x_ctr_ANTE.do = map_dbl(geometry,  ~st_centroid(.x)[[1]]),
                 y_ctr_ANTE.do = map_dbl(geometry,  ~st_centroid(.x)[[2]]))
        
      } else  {
        filteredData_flux_ENTR.sf.do <- cartogram_dorling(filteredData_flux_ENTR.sf, "nb_ind.log", k=0.1, itermax = 100) %>%
          mutate(x_ctr_ANTE.do = map_dbl(geometry,  ~st_centroid(.x)[[1]]),
                 y_ctr_ANTE.do = map_dbl(geometry,  ~st_centroid(.x)[[2]]))
      }
      
      # affichage du geom_spoke
      p.flux.spoke <- ggplot() +
        geom_spoke(data= filteredData_flux_ENTR.sf.do ,
                   aes(x = x_ctr_ANTE.do ,
                       y = y_ctr_ANTE.do ,
                       radius = log(nb_ind) * ratio_ligne,
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
        left_join(filteredData_flux_ENTR.sf.do %>%
                    mutate(x_ctr_ANTE.do = round(x_ctr_ANTE.do,0),
                           y_ctr_ANTE.do = round(y_ctr_ANTE.do,0)) %>%
                    select(TERR_ANTE, TERR_ACTU, LIB_TERR_ANTE, LIB_TERR_ACTU, nb_ind, pct_ind_ACTU_MIG, x_ctr_ANTE.do, y_ctr_ANTE.do),
                  by = c("x_moy" ="x_ctr_ANTE.do","y_moy" ="y_ctr_ANTE.do")) %>%
        mutate(xdiff = xend - x, ydiff = yend - y) %>%
        mutate(xstart = x - xdiff, ystart = y - ydiff) 
      
      # libellés légende
      if(input$maille_TERR %in% 'REG') { libelle_NIVGEO <- "la région "} 
      else if ( input$maille_TERR %in% 'DEP') { libelle_NIVGEO <- "le département "}
      else if ( input$maille_TERR %in% 'EPCI') { libelle_NIVGEO <- "l'intercommunalité "}
      
      # version ggiraph
      p_map_entr <-
        ggplot() +
        # territoire cible
        geom_sf(data = filteredData_geo_TERR(),
                color = "grey65", fill ="grey80", size = 0.3) +
        # contours des départements si niv_TERR = DEP ou EPCI
                {if(niveau_TERR %in% c('DEP','EPCI'))geom_sf(data = geo_TERR_poly %>%  filter(NIV_TERR %in% "DEP"),
                                                             color = "grey55", fill = NA ,size = 0.05)} +
        # contours des régions
        geom_sf(data = geo_TERR_poly %>%  filter(NIV_TERR %in% 'REG'),
                color = "grey50", fill = NA ,size = 0.15) +
        # spokes
        geom_segment_interactive(data=p.str.entr %>% 
                                   mutate(LIB_TERR_ACTU = stri_trans_general(conv_accents(LIB_TERR_ACTU),"Latin-ASCII"),
                                          LIB_TERR_ANTE = stri_trans_general(conv_accents(LIB_TERR_ANTE),"Latin-ASCII")) %>%
                                   
                                   mutate(tip = case_when( niveau_TERR %in% 'REG' ~
                                                             paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                    "<font size=2.5 color=white family=Roboto>", #"Entre 2014 et 2015,", "<br>",
                                                                    format(round(nb_ind,-1), nsmall=0, big.mark=" ")," personnes sont arriv&eacute;es ",#"<br>",
                                                                    "dans la r&eacute;gion ","<b>",LIB_TERR_ACTU,"</b>","<br>"," en provenance de la r&eacute;gion ", "<b>", LIB_TERR_ANTE, "</b>", "<br>",
                                                                    "entre 2014 et 2015, soit ", percent(pct_ind_ACTU_MIG, accuracy = 1), " des nouveaux arrivants." ),
                                                           niveau_TERR %in% 'DEP' ~
                                                             paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                    "<font size=2.5 color=white family=Roboto>", #"Entre 2014 et 2015,","<br>",
                                                                    format(round(nb_ind,-1), nsmall=0, big.mark=" ")," personnes sont arriv&eacute;es ",#"<br>",
                                                                    "dans le d&eacute;partement " ,"<b>",LIB_TERR_ACTU,"</b>","<br>"," en provenance du d&eacute;partement ",  "<b>", LIB_TERR_ANTE, "</b>",  "<br>",
                                                                    "entre 2014 et 2015, soit ", percent(pct_ind_ACTU_MIG, accuracy = 1), " des nouveaux arrivants." ),
                                                           niveau_TERR %in% 'EPCI' ~
                                                             paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                    "<font size=2.5 color=white family=Roboto>", #"Entre 2014 et 2015,","<br>",
                                                                    format(round(nb_ind,-1), nsmall=0, big.mark=" ")," personnes sont arriv&eacute;es ",#"<br>",
                                                                    "dans l&apos;intercommunalit&eacute; " ,"<b>",LIB_TERR_ACTU,"</b>","<br>"," en provenance de l&apos;intercommunalit&eacute; ",  "<b>", LIB_TERR_ANTE, "</b>",  "<br>",
                                                                    "entre 2014 et 2015, soit ", percent(pct_ind_ACTU_MIG, accuracy = 1), " des nouveaux arrivants." )
                                   )),
                                 aes(x = xstart ,
                                     y = ystart ,
                                     xend = x,
                                     yend = y,
                                     tooltip = tip,
                                     data_id = TERR_ANTE,
                                     color = pct_ind_ACTU_MIG,
                                     size = nb_ind  ),
                                 arrow = arrow(ends = "last", length = unit(p.str.entr$nb_ind %>% log() /ratio_fleche,"cm"), type = "open")
        ) +
        # taille maximale de la ligne différente selon NIVGEO
        {if(niveau_TERR %in% c('EPCI'))scale_size(range = c(0.05,2),
                                                  guide = FALSE,
                                                  name = "Nombre d'individus\nayant emménagé dans\nle territoire")
          else if(niveau_TERR %in% c('DEP'))scale_size(range = c(0.05,2.5),
                                                       guide = FALSE,
                                                       name = "Nombre d'individus\nayant emménagé dans\nle territoire")
          else scale_size(range = c(0.05,3.5),
                          guide = FALSE,
                          name = "Nombre d'individus\nayant emménagé dans\nle territoire")}  +
        # légende
        scale_color_gradient(low = "#b5d2b2", high = "#000a00",
                             labels = percent_format(accuracy = 1),
                             #guide = FALSE,
                             name = paste0("Répartition des individus ayant emmenagé dans\n",libelle_NIVGEO, filteredData_geo_TERR()$LIB_TERR) ) +
        guides(fill = guide_legend(reverse=T, order = 1)) +
        theme_ipsum() +
        coord_sf(crs = 2154, datum = NA) +
        scale_x_continuous(name = "") +
        scale_y_continuous(name = "") +
        theme(strip.text.y = element_text(angle = 360),
              text = element_text(family = "Roboto", color = "black"),
              panel.background = element_rect(fill = NA, color = NA),
              plot.background = element_rect(fill = NA, color = NA),
              axis.title = element_text(size = 9),
              axis.text = element_text(size = 8),
              axis.line =  element_line( color = "grey"),
              panel.grid.major = element_line(color = "#2b2b2b99",size = 0.1),
              panel.grid.minor = element_line(color = "#2b2b2b99", size = 0.05),
              legend.direction = "horizontal",
              legend.title=element_text(size=6), 
              legend.text=element_text(size=5),
              legend.position = "top")
      
      tooltip_css <- "background-color:#272B30;padding:2px;font-size: 120%;color: white;opacity:0.2"
      x <- girafe(ggobj = p_map_entr, width = 1, height_svg = 6 )
      girafe_options(x, 
                     opts_tooltip(use_fill = FALSE, offx = 10, offy = -10, use_cursor_pos = TRUE, css = tooltip_css),
                     opts_hover(css = "stroke:red;r:2pt;"),
                     opts_selection(type = "none"),
                     opts_zoom(max = 2),
                     opts_toolbar(position = "topright", saveaspng = FALSE) )
      
      
    } else if (input$type_flux %in% 'SORT') {
      
      ratio_ligne <- filteredData_flux_SORT() %>% distinct(ratio_ligne) %>% pull()
      ratio_fleche <- filteredData_flux_SORT() %>% distinct(ratio_fleche) %>% pull()
      niveau_TERR <- filteredData_flux_SORT() %>% distinct(NIV_TERR) %>% pull()
      
      # catrogrammes de dorling pour exploser les lignes si superposition
      filteredData_flux_SORT.sf <- st_as_sf(x = filteredData_flux_SORT(), 
                                            coords = c("x_ctr_ACTU", "y_ctr_ACTU"),
                                            crs = 2154) %>%
        mutate(nb_ind.log = log(nb_ind))
      
      
      if(niveau_TERR %in% 'EPCI') {
        filteredData_flux_SORT.sf.do <- cartogram_dorling(filteredData_flux_SORT.sf, "nb_ind.log", k=0.01, itermax = 100) %>%
          mutate(x_ctr_ACTU.do = map_dbl(geometry,  ~st_centroid(.x)[[1]]),
                 y_ctr_ACTU.do = map_dbl(geometry,  ~st_centroid(.x)[[2]]))
        
      } else  {
        filteredData_flux_SORT.sf.do <- cartogram_dorling(filteredData_flux_SORT.sf, "nb_ind.log", k=0.1, itermax = 100) %>%
          mutate(x_ctr_ACTU.do = map_dbl(geometry,  ~st_centroid(.x)[[1]]),
                 y_ctr_ACTU.do = map_dbl(geometry,  ~st_centroid(.x)[[2]]))
      }
      
      
      
      # affichage du geom_spoke
      p.flux.spoke <- ggplot() +
        geom_spoke(data= filteredData_flux_SORT.sf.do ,
                   aes(x = x_ctr_ACTU.do ,
                       y = y_ctr_ACTU.do ,
                       radius = log(nb_ind) * ratio_ligne,
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
        left_join(filteredData_flux_SORT.sf.do %>%
                    mutate(x_ctr_ACTU.do = round(x_ctr_ACTU.do,0),
                           y_ctr_ACTU.do = round(y_ctr_ACTU.do,0)) %>%
                    select(TERR_ANTE, TERR_ACTU, LIB_TERR_ANTE, LIB_TERR_ACTU, nb_ind, pct_ind_ANTE_MIG, x_ctr_ACTU.do, y_ctr_ACTU.do),
                  by = c("x_moy" ="x_ctr_ACTU.do","y_moy" ="y_ctr_ACTU.do")) %>%
        mutate(xdiff = xend - x, ydiff = yend - y) %>%
        mutate(xstart = x - xdiff, ystart = y - ydiff)
      
      # libellés légende
      if(input$maille_TERR %in% 'REG') { libelle_NIVGEO <- "la région "} 
      else if ( input$maille_TERR %in% 'DEP') { libelle_NIVGEO <- "le département "}
      else if ( input$maille_TERR %in% 'EPCI') { libelle_NIVGEO <- "l'intercommunalité "}
      
      # puis cartographie finale
      p_map_sort <-
        ggplot() +
        # territoire cible
        geom_sf(data = filteredData_geo_TERR(),
                color = "grey65", fill ="grey80", size = 0.3) +
        # contours des départements si niv_TERR = DEP ou EPCI
                {if(niveau_TERR %in% c('DEP','EPCI'))geom_sf(data = geo_TERR_poly %>%  filter(NIV_TERR %in% "DEP"),
                                                             color = "grey55", fill = NA ,size = 0.05)} +
        # contours des régions
        geom_sf(data = geo_TERR_poly %>%  filter(NIV_TERR %in% 'REG'),
                color = "grey50", fill = NA ,size = 0.15) +
        # spokes
        geom_segment_interactive(data=p.str.sort %>% 
                                   mutate(LIB_TERR_ACTU = stri_trans_general(conv_accents(LIB_TERR_ACTU),"Latin-ASCII"),
                                          LIB_TERR_ANTE = stri_trans_general(conv_accents(LIB_TERR_ANTE),"Latin-ASCII")) %>%
                                   
                                   mutate(tip = case_when( niveau_TERR %in% 'REG' ~
                                                             paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                    "<font size=2.5 color=white family=Roboto>", #"Entre 2014 et 2015,","<br>",
                                                                    format(round(nb_ind,-1), nsmall=0, big.mark=" ")," personnes ont quitt&eacute; ",#"<br>",
                                                                    "la r&eacute;gion " ,"<b>", LIB_TERR_ANTE,"</b>", "<br>"," vers la r&eacute;gion ",  "<b>", LIB_TERR_ACTU,"</b>",  "<br>",
                                                                    "entre 2014 et 2015, soit ", percent(pct_ind_ANTE_MIG, accuracy = 1), " des partants."),
                                                           niveau_TERR %in% 'DEP' ~
                                                             paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                    "<font size=2.5 color=white family=Roboto>", #"Entre 2014 et 2015,","<br>",
                                                                    format(round(nb_ind,-1), nsmall=0, big.mark=" ")," personnes ont quitt&eacute; ",#"<br>",
                                                                    "le d&eacute;partement " ,"<b>", LIB_TERR_ANTE,"</b>","<br>"," vers le d&eacute;partement ",  "<b>", LIB_TERR_ACTU,"</b>",  "<br>",
                                                                    "entre 2014 et 2015, soit ", percent(pct_ind_ANTE_MIG, accuracy = 1), " des partants."),
                                                           niveau_TERR %in% 'EPCI' ~
                                                             paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                    "<font size=2.5 color=white family=Roboto>", #"Entre 2014 et 2015,","<br>",
                                                                    format(round(nb_ind,-1), nsmall=0, big.mark=" ")," personnes ont quitt&eacute; ",#"<br>",
                                                                    "l&apos;intercommunalit&eacute; " ,"<b>", LIB_TERR_ANTE,"</b>","<br>"," vers l&apos;intercommunalit&eacute; ",  "<b>", LIB_TERR_ACTU,"</b>",  "<br>" ,
                                                                    "entre 2014 et 2015, soit ", percent(pct_ind_ANTE_MIG, accuracy = 1), " des partants.")
                                   )),
                                 aes(x = xstart ,
                                     y = ystart ,
                                     xend = x,
                                     yend = y,
                                     tooltip = tip,
                                     data_id = TERR_ACTU,
                                     color = pct_ind_ANTE_MIG,
                                     size = nb_ind * 10 ),
                                 arrow = arrow(ends = "last", length = unit(p.str.sort$nb_ind %>% log() /ratio_fleche,"cm"), type = "open")
        ) +
        # taille maximale de la ligne différente selon NIVGEO
        {if(niveau_TERR %in% c('EPCI'))scale_size(range = c(0.05,1.5),
                                                  guide = FALSE,
                                                  name = "Nombre d'individus\nayant emménagé dans\nle territoire")
          else if(niveau_TERR %in% c('DEP'))scale_size(range = c(0.05,2),
                                                       guide = FALSE,
                                                       name = "Nombre d'individus\nayant emménagé dans\nle territoire")
          else scale_size(range = c(0.05,3.5),
                          guide = FALSE,
                          name = "Nombre d'individus\nayant emménagé dans\nle territoire")}  +
        
        scale_color_gradient(low = "#fecccc", high = "#190000", 
                             name = paste0("Répartition des individus ayant quitté\n",libelle_NIVGEO, filteredData_geo_TERR()$LIB_TERR) ,
                             labels = percent_format(accuracy = 1)) +

        guides(fill = guide_legend(reverse=T, order = 1)) +
        theme_ipsum() +
        coord_sf(crs = 2154, datum = NA) +
        scale_x_continuous(name = "") +
        scale_y_continuous(name = "") +
        theme(strip.text.y = element_text(angle = 360),
              text = element_text(family = "Roboto", color = "black"),
              panel.background = element_rect(fill = NA, color = NA),
              plot.background = element_rect(fill = NA, color = NA),
              axis.title = element_text(size = 9),
              axis.text = element_text(size = 8),
              axis.line =  element_line( color = "grey"),
              panel.grid.major = element_line(color = "#2b2b2b99",size = 0.1),
              panel.grid.minor = element_line(color = "#2b2b2b99", size = 0.05),
              legend.direction = "horizontal",
              legend.title=element_text(size=6), 
              legend.text=element_text(size=5),
              legend.position = "top")
      
      tooltip_css <- "background-color:#272B30;padding:2px;font-size: 120%;color: white;opacity:0.2"
      x <- girafe(ggobj = p_map_sort, width = 1, height_svg = 6 )
      girafe_options(x, 
                     opts_tooltip(use_fill = FALSE, offx = 10, offy = -10, use_cursor_pos = TRUE, css = tooltip_css),
                     opts_hover(css = "stroke:red;r:2pt;"),
                     opts_selection(type = "none"),
                     opts_zoom(max = 2),
                     opts_toolbar(position = "topright", saveaspng = FALSE) )
      
    } else if ( input$type_flux %in% 'SMN') {
      
      # version carrés
      
      ## cartographie des flux nets SM
      niveau_TERR <- filteredData_flux_SMNET() %>% distinct(NIV_TERR) %>% pull()
      max_flux_net_sens <- filteredData_flux_SMNET() %>% select(flux_net_sens) %>% max()
      
      # data avec SMnet et polygone
      filteredData_flux_SMNET.pt = filteredData_flux_SMNET() %>%
        select(TERR_i,TERR_j,type_flow, flux_tot, flux_net_sens) %>%
        mutate(TERR_lien =case_when(type_flow %in% 'entr' ~ TERR_i, TRUE ~TERR_j)) %>%
        mutate(flux_sens =case_when(type_flow %in% 'entr' ~ as.numeric(flux_net_sens), TRUE ~ -as.numeric(flux_net_sens))) %>%
        left_join(geo_TERR_poly %>%
                    st_transform(2154) %>%
                    filter(NIV_TERR %in% niveau_TERR) %>%
                    st_centroid(of_largest_polygon = T) %>%
                    mutate(x_ctr = map_dbl(geom, ~st_centroid(.x)[[1]]),
                           y_ctr = map_dbl(geom, ~st_centroid(.x)[[2]])),
                  by =c('TERR_lien'= 'TERR')) %>%
        as.data.frame() %>% select(-geom)
      
      
      # cartogrammes de dorling pour exploser les lignes si superposition
      filteredData_flux_SMNET.sf <- st_as_sf(x = filteredData_flux_SMNET.pt,
                                             coords = c("x_ctr", "y_ctr"),
                                             crs = 2154)
      
      filteredData_flux_SMNET.sf.do <- cartogram_dorling(filteredData_flux_SMNET.sf, "flux_net_sens", k=0.1, itermax = 100) %>%
        mutate(x_ctr.do = map_dbl(geometry,  ~st_centroid(.x)[[1]]),
               y_ctr.do = map_dbl(geometry,  ~st_centroid(.x)[[2]])) %>%
        as.data.frame() %>% select(-geometry)
      
      # libellés légende
      if(input$maille_TERR %in% 'REG') { libelle_NIVGEO <- "de la région "} 
      else if ( input$maille_TERR %in% 'DEP') { libelle_NIVGEO <- "du département "}
      else if ( input$maille_TERR %in% 'EPCI') { libelle_NIVGEO <- "de l'intercommunalité "}
      
      
      # puis cartographie finale
      
      p_map_smnet <-
        ggplot() +
        # territoire cible
        geom_sf(data = filteredData_geo_TERR(),
                color = "grey65", fill ="grey80") +
        # contours des départements si niv_TERR = DEP ou EPCI
                {if(niveau_TERR %in% c('DEP','EPCI'))geom_sf(data = geo_TERR_poly %>%  filter(NIV_TERR %in% "DEP"),
                                                             color = "grey55", fill = NA ,size = 0.05)} +
        # contours des régions
        geom_sf(data = geo_TERR_poly %>%  filter(NIV_TERR %in% 'REG'),
                color = "grey50", fill = NA ,size = 0.15) +
        
        # carto bulles      
        geom_point_interactive(data = filteredData_flux_SMNET.sf.do  %>%
                                 mutate(LIB_TERR = stri_trans_general(conv_accents(LIB_TERR),"Latin-ASCII")) %>%
                                 mutate(tip = case_when(type_flow %in% 'entr' & niveau_TERR %in% 'REG' ~
                                                          paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                 "<font size=2.5 color=white family=Roboto>",'La r&eacute;gion ',"<b>", LIB_TERR,"</b>" ,"<br>"," a perdu ",
                                                                 format(round(flux_net_sens,-1), nsmall=0, big.mark=" "), " habitants vis-&agrave;-vis ","<br>","de la r&eacute;gion ","<b>",stri_trans_general(conv_accents(filteredData_geo_TERR()$LIB_TERR),"Latin-ASCII") ,"</b>","</font>", "<br>" ),
                                                        type_flow %in% 'entr' & niveau_TERR %in% 'DEP' ~
                                                          paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                 "<font size=2.5 color=white family=Roboto>",'Le d&eacute;partement ',"<b>", LIB_TERR,"</b>" ,"<br>"," a perdu ",
                                                                 format(round(flux_net_sens,-1), nsmall=0, big.mark=" "), " habitants vis-&agrave;-vis ","<br>","du d&eacute;partement ","<b>",stri_trans_general(conv_accents(filteredData_geo_TERR()$LIB_TERR),"Latin-ASCII") ,"</b>" ,"</font>", "<br>" ),
                                                        type_flow %in% 'entr' & niveau_TERR %in% 'EPCI' ~
                                                          paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                 "<font size=2.5 color=white family=Roboto>","L&apos;intercommunalit&eacute; ","<b>",LIB_TERR,"</b>"  ,"<br>"," a perdu ",
                                                                 format(round(flux_net_sens,-1), nsmall=0, big.mark=" "), " habitants vis-&agrave;-vis ","<br>","de l&apos;intercommunalit&eacute; ","<b>",stri_trans_general(conv_accents(filteredData_geo_TERR()$LIB_TERR),"Latin-ASCII") ,"</b>" ,"</font>", "<br>" ),
                                                        type_flow %in% 'sort' & niveau_TERR %in% 'REG' ~
                                                          paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                 "<font size=2.5 color=white family=Roboto>",'La r&eacute;gion ',"<b>",LIB_TERR,"</b>" ,"<br>"," a gagn&eacute; ",
                                                                 format(round(flux_net_sens,-1), nsmall=0, big.mark=" "), " habitants vis-&agrave;-vis ","<br>","de la r&eacute;gion ","<b>",stri_trans_general(conv_accents(filteredData_geo_TERR()$LIB_TERR),"Latin-ASCII") ,"</b>" ,"</font>", "<br>" ),
                                                        type_flow %in% 'sort' & niveau_TERR %in% 'DEP' ~
                                                          paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                 "<font size=2.5 color=white family=Roboto>",'Le d&eacute;partement ',"<b>",LIB_TERR,"</b>"  ,"<br>"," a gagn&eacute; ",
                                                                 format(round(flux_net_sens,-1), nsmall=0, big.mark=" "), " habitants vis-&agrave;-vis ","<br>","du d&eacute;partement ","<b>",stri_trans_general(conv_accents(filteredData_geo_TERR()$LIB_TERR),"Latin-ASCII") ,"</b>" ,"</font>", "<br>" ),
                                                        type_flow %in% 'sort' & niveau_TERR %in% 'EPCI' ~
                                                          paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                 "<font size=2.5 color=white family=Roboto>","L&apos;intercommunalit&eacute; ","<b>",LIB_TERR ,"</b>" ,"<br>"," a gagn&eacute; ",
                                                                 format(round(flux_net_sens,-1), nsmall=0, big.mark=" "), " habitants vis-&agrave;-vis ","<br>","de l&apos;intercommunalit&eacute; ","<b>",stri_trans_general(conv_accents(filteredData_geo_TERR()$LIB_TERR),"Latin-ASCII") ,"</b>","</font>", "<br>" ),
                                                        TRUE ~ "NA"
                                 )),
                               aes(x = x_ctr.do, y = y_ctr.do,
                                   alpha = flux_net_sens,
                                   tooltip = tip,
                                   data_id = TERR_lien,
                                   fill = type_flow,
                                   size =flux_net_sens),
                               color = "grey65", shape = 22, stroke = 0.4)  +
        scale_size(range = c(0.5,15), limits =c(0,15000),#max_flux_net_sens),
                   guide= F,
                   name = "Intensité du solde migratoire") +
        scale_alpha_continuous(range = c(0.5,1), guide = F)+
      scale_fill_manual(values = c( '#1877be','#d6241b'),
                        name = "",
                        labels = c(paste0("Solde migratoire déficitaire ", libelle_NIVGEO,"\n",
                                          " vis à vis ",libelle_NIVGEO, filteredData_geo_TERR()$LIB_TERR),
                                   paste0("Solde migratoire excédentaire ", libelle_NIVGEO,"\n",
                                          " vis à vis ",libelle_NIVGEO, filteredData_geo_TERR()$LIB_TERR))) +
        theme_ipsum() +
        coord_sf(crs = 2154, datum = NA) +
        scale_x_continuous(name = "") +
        scale_y_continuous(name = "") +
       theme(strip.text.y = element_text(angle = 360),
              text = element_text(family = "Roboto", color = "black"),
              panel.background = element_rect(fill = NA, color = NA),
              plot.background = element_rect(fill = NA, color = NA),
              axis.title = element_text(size = 9),
              axis.text = element_text(size = 8),
              axis.line =  element_line( color = "grey"),
              panel.grid.major = element_line(color = "#2b2b2b99",size = 0.1),
              panel.grid.minor = element_line(color = "#2b2b2b99", size = 0.05),
              legend.box = "horizontal",
              legend.direction = "horizontal",
              legend.title=element_text(size=6), 
              legend.text=element_text(size=6),
              legend.position = "top")
      
      
      
      tooltip_css <- "background-color:#272B30;padding:2px;font-size: 120%;color: white;opacity:0.2"
      x <- girafe(ggobj = p_map_smnet, width = 1, height_svg = 6 )
      girafe_options(x, 
                     opts_tooltip(use_fill = FALSE, offx = 10, offy = -10, use_cursor_pos = TRUE, css = tooltip_css),
                     opts_hover(css = "stroke:black;r:2pt;"),
                     opts_selection(type = "none"),
                     opts_zoom(max = 2),
                     opts_toolbar(position = "topright", saveaspng = FALSE) )
      
    } else if ( input$type_flux %in% 'SMN2') {
      
      
      # version fleches
      
      ## cartographie des flux nets SM
      ratio_ligne <- filteredData_flux_SMNET() %>% distinct(ratio_ligne) %>% pull()
      ratio_fleche <- filteredData_flux_SMNET() %>% distinct(ratio_fleche) %>% pull()
      niveau_TERR <- filteredData_flux_SMNET() %>% distinct(NIV_TERR) %>% pull()
      
      
      # catrogrammes de dorling pour exploser les lignes si superposition
      filteredData_flux_SMN.sf <- st_as_sf(x = filteredData_flux_SMNET() %>%
                                             filter(type_flow %in% "sort") %>%
                                             mutate(x_ctr_SM = x_ctr_j, y_ctr_SM = y_ctr_j ) %>%
                                             rbind.data.frame(filteredData_flux_SMNET() %>%
                                                                filter(type_flow %in% "entr") %>%
                                                                mutate(x_ctr_SM = x_ctr_i, y_ctr_SM = y_ctr_i )), 
                                           coords = c("x_ctr_SM", "y_ctr_SM"),
                                           crs = 2154) %>%
        mutate(flux_tot.log = log(flux_tot))
      
      
      if(niveau_TERR %in% 'EPCI') {
        filteredData_flux_SMN.sf.do <- cartogram_dorling(filteredData_flux_SMN.sf, "flux_tot.log", k=0.01, itermax = 100) %>%
          mutate(x_ctr_SM.do = map_dbl(geometry,  ~st_centroid(.x)[[1]]),
                 y_ctr_SM.do = map_dbl(geometry,  ~st_centroid(.x)[[2]]))
        
      } else  {
        filteredData_flux_SMN.sf.do <- cartogram_dorling(filteredData_flux_SMN.sf, "flux_tot.log", k=0.1, itermax = 100) %>%
          mutate(x_ctr_SM.do = map_dbl(geometry,  ~st_centroid(.x)[[1]]),
                 y_ctr_SM.do = map_dbl(geometry,  ~st_centroid(.x)[[2]]))
      }
      
      # 1- creation des lignes pour flux sortants (SM < 0)
      
      # affichage du geom_spoke
      p.flux.spoke <- ggplot() +
        geom_spoke(data= filteredData_flux_SMN.sf.do %>% filter(type_flow %in% "sort") ,
                   aes(x = x_ctr_SM.do ,
                       y = y_ctr_SM.do ,
                       radius = log(flux_tot) * ratio_ligne,
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
        left_join(filteredData_flux_SMN.sf.do %>% filter(type_flow %in% "sort") %>%
                    mutate(x_ctr_j = round(x_ctr_SM.do,0),
                           y_ctr_j = round(y_ctr_SM.do,0)) %>%
                    select(TERR_i, TERR_j,LIB_TERR_i, LIB_TERR_j,  flux_net_sens, ratio_flux_net_sens,flux_tot, x_ctr_j, y_ctr_j),
                  by = c("x_moy" ="x_ctr_j","y_moy" ="y_ctr_j")) %>%
        mutate(xdiff = xend - x, ydiff = yend - y) %>%
        mutate(xstart = x - xdiff, ystart = y - ydiff) %>%
        filter(!is.na(TERR_i))%>%
        select(-geometry)
      
      
      # 2- creation des lignes pour flux entrants (SM > 0)
      
      # affichage du geom_spoke
      p.flux.spoke <- ggplot() +
        geom_spoke(data= filteredData_flux_SMN.sf.do %>% filter(type_flow %in% "entr") ,
                   aes(x = x_ctr_SM.do ,
                       y = y_ctr_SM.do ,
                       radius = log(flux_tot) * ratio_ligne,
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
        left_join(filteredData_flux_SMN.sf.do %>% filter(type_flow %in% "entr") %>%
                    mutate(x_ctr_i = round(x_ctr_SM.do,0),
                           y_ctr_i = round(y_ctr_SM.do,0)) %>%
                    select(TERR_i, TERR_j,LIB_TERR_i, LIB_TERR_j,  flux_net_sens,ratio_flux_net_sens,flux_tot, x_ctr_i, y_ctr_i),
                  by = c("x_moy" ="x_ctr_i","y_moy" ="y_ctr_i")) %>%
        mutate(xdiff = xend - x, ydiff = yend - y) %>%
        mutate(xstart = x - xdiff, ystart = y - ydiff) %>%
        filter(!is.na(TERR_i)) %>%
        select(-geometry)
      
      
      p.str.SM.sort.SMnet <- p.str.SM.sort %>% mutate(flux_net_sens = - flux_net_sens) %>% rbind.data.frame(p.str.SM.entr)
      
      max_flux_net_sens <- filteredData_flux_SMNET() %>% select(flux_net_sens) %>% max()
      
      # libellés légende
      if(input$maille_TERR %in% 'REG') { libelle_NIVGEO <- "de la région "
      libelle_NIVGEO_plur <- "régions."} 
      else if ( input$maille_TERR %in% 'DEP') { libelle_NIVGEO <- "du département "
      libelle_NIVGEO_plur <- "départements."}
      else if ( input$maille_TERR %in% 'EPCI') { libelle_NIVGEO <- "de l'intercommunalité "
      libelle_NIVGEO_plur <- "intercommunalités."}
      
      # puis cartographie finale
      p_map_smnet <-
        ggplot() +
        # territoire cible
        geom_sf(data = filteredData_geo_TERR(),
                color = "grey65", fill ="grey80", size = 0.3) +
        # contours des départements si niv_TERR = DEP ou EPCI
                {if(niveau_TERR %in% c('DEP','EPCI'))geom_sf(data = geo_TERR_poly %>%  filter(NIV_TERR %in% "DEP"),
                                                             color = "grey55", fill = NA ,size = 0.05)} +
        # contours des régions
        geom_sf(data = geo_TERR_poly %>%  filter(NIV_TERR %in% 'REG'),
                color = "grey50", fill = NA ,size = 0.15) +
        # SM net
        geom_segment_interactive(data=p.str.SM.sort.SMnet %>% 
                                   mutate(LIB_TERR_i = stri_trans_general(conv_accents(LIB_TERR_i),"Latin-ASCII"),
                                          LIB_TERR_j = stri_trans_general(conv_accents(LIB_TERR_j),"Latin-ASCII")) %>%
                                   mutate(id = paste0(TERR_i,"_",TERR_j)) %>%
                                   mutate(tip = case_when(flux_net_sens >= 0 & niveau_TERR %in% 'REG' ~
                                                            paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                   "<font size=2.5 color=white family=Roboto>",'La r&eacute;gion ',"<b>",LIB_TERR_j ,"</b>","<br>"," a gagn&eacute; ",
                                                                   format(round(flux_net_sens,-1), nsmall=0, big.mark=" "), " habitants vis-&agrave;-vis ","<br>","de la r&eacute;gion ","<b>",LIB_TERR_i ,"</b>" , "<br>",
                                                                   "entre 2014 et 2015.","</font>"),
                                                          flux_net_sens >= 0 & niveau_TERR %in% 'DEP' ~
                                                            paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                   "<font size=2.5 color=white family=Roboto>",'Le d&eacute;partement ',"<b>",LIB_TERR_j ,"</b>","<br>"," a gagn&eacute; ",
                                                                   format(round(flux_net_sens,-1), nsmall=0, big.mark=" "), " habitants vis-&agrave;-vis ","<br>","du d&eacute;partement ","<b>",LIB_TERR_i ,"</b>" , "<br>",
                                                                   "entre 2014 et 2015.","</font>" ),
                                                          flux_net_sens >= 0 & niveau_TERR %in% 'EPCI' ~
                                                            paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                   "<font size=2.5 color=white family=Roboto>","L&apos;intercommunalit&eacute; ","<b>",LIB_TERR_j ,"</b>","<br>"," a gagn&eacute; ",
                                                                   format(round(flux_net_sens,-1), nsmall=0, big.mark=" "), " habitants vis-&agrave;-vis ","<br>","de l&apos;intercommunalit&eacute; ","<b>",LIB_TERR_i ,"</b>" , "<br>",
                                                                   "entre 2014 et 2015.","</font>" ),
                                                          flux_net_sens < 0 & niveau_TERR %in% 'REG' ~
                                                            paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                   "<font size=2.5 color=white family=Roboto>",'La r&eacute;gion ',"<b>", LIB_TERR_i ,"</b>","<br>"," a perdu ",
                                                                   format(round(abs(flux_net_sens),-1), nsmall=0, big.mark=" "), " habitants vis-&agrave;-vis ","<br>","de la r&eacute;gion ","<b>",LIB_TERR_j ,"</b>" , "<br>",
                                                                   "entre 2014 et 2015.","</font>" ),
                                                          flux_net_sens < 0 & niveau_TERR %in% 'DEP' ~
                                                            paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                   "<font size=2.5 color=white family=Roboto>",'Le d&eacute;partement ',"<b>",LIB_TERR_i ,"</b>" ,"<br>"," a perdu ",
                                                                   format(round(abs(flux_net_sens),-1), nsmall=0, big.mark=" "), " habitants vis-&agrave;-vis ","<br>","du d&eacute;partement ","<b>",LIB_TERR_j ,"</b>" , "<br>",
                                                                   "entre 2014 et 2015.","</font>" ),
                                                          flux_net_sens < 0 & niveau_TERR %in% 'EPCI' ~
                                                            paste0("<style> div.leaflet-popup-content {width:auto!important;}</style>",
                                                                   "<font size=2.5 color=white family=Roboto>","L&apos;intercommunalit&eacute; ","<b>", LIB_TERR_i,"</b>" ,"<br>"," a perdu ",
                                                                   format(round(abs(flux_net_sens),-1), nsmall=0, big.mark=" "), " habitants vis-&agrave;-vis ","<br>","de l&apos;intercommunalit&eacute; ","<b>",LIB_TERR_j ,"</b>", "<br>",
                                                                   "entre 2014 et 2015." ,"</font>"),
                                                          TRUE ~ "NA"
                                   )),
                                 aes(x = xstart ,
                                     y = ystart ,
                                     xend = x,
                                     yend = y,
                                     color = flux_net_sens,
                                     tooltip = tip,
                                     data_id = id,
                                     size = abs(flux_net_sens)  ),
                                 arrow = arrow(ends = "last", length = unit(p.str.SM.sort.SMnet$flux_net_sens %>% abs() %>% log() /ratio_fleche,"cm"), type = "open")
        ) +
        # taille maximale de la ligne différente selon NIVGEO
        {if(niveau_TERR %in% c('EPCI'))scale_size(range = c(0.05,2),
                                                  guide = FALSE,
                                                  name = "Nombre d'individus\nayant emménagé dans\nle territoire")
          else if(niveau_TERR %in% c('DEP'))scale_size(range = c(0.05,2.5),
                                                       guide = FALSE,
                                                       name = "Nombre d'individus\nayant emménagé dans\nle territoire")
          else scale_size(range = c(0.05,3.5),
                          guide = FALSE,
                          name = "Nombre d'individus\nayant emménagé dans\nle territoire")}  +
        scale_color_gradient2( 
          name = paste0("Solde migratoire ", libelle_NIVGEO, filteredData_geo_TERR()$LIB_TERR," \n",
                        "vis à vis des autres ",libelle_NIVGEO_plur),
          low ="#FF0000", mid = "#f0f0f0", high = "#0000FF", 
          midpoint = 0,
          limits = c(-max_flux_net_sens,max_flux_net_sens)) +
        theme_ipsum() +
        coord_sf(crs = 2154, datum = NA) +
        scale_x_continuous(name = "") +
        scale_y_continuous(name = "") +
        theme(strip.text.y = element_text(angle = 360),
              text = element_text(family = "Roboto", color = "black"),
              panel.background = element_rect(fill = NA, color = NA),
              plot.background = element_rect(fill = NA, color = NA),
              axis.title = element_text(size = 9),
              axis.text = element_text(size = 8),
              axis.line =  element_line( color = "grey"),
              panel.grid.major = element_line(color = "#2b2b2b99",size = 0.1),
              panel.grid.minor = element_line(color = "#2b2b2b99", size = 0.05),
              legend.direction = "horizontal",
              legend.title=element_text(size=6), 
              legend.text=element_text(size=5),
              legend.position = "top")
      
      tooltip_css <- "background-color:#272B30;padding:2px;font-size: 120%;color: white;opacity:0.2"
      x <- girafe(ggobj = p_map_smnet, width = 1, height_svg = 6 )
      girafe_options(x, 
                     opts_tooltip(use_fill = FALSE, offx = 10, offy = -10, use_cursor_pos = TRUE, css = tooltip_css),
                     opts_hover(css = "stroke:black;r:5pt;"),
                     opts_selection(type = "none"),
                     opts_zoom(max = 2),
                     opts_toolbar(position = "topright", saveaspng = FALSE) )
      
      
    }
  })
  
  
  
    }


# exécuter l'application
shinyApp(ui = ui, server = server)