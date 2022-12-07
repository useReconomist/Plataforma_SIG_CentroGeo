# CentroGeo

# Proyecto SIG ----

# Tue Nov 29 15:04:01 2022 ------------------------------

# Autores: ----
# Zumaya Tapia Rodrigo  - @ - al.rzumaya@centrogeo.edu.mx 
# Noé Osorio García - @NoeOsorioPK - al.nosorio@centrogeo.edu.mx


# Las bases usadas corresponden a:
# agebs INEGI Margo Geoestadístico
# CONAPO POblación y marginación por ageb



# Encoding, Windows, linux o cambia si es MAC
# Sys.setlocale("LC_TIME", "es_ES")
Sys.setlocale("LC_ALL", "Spanish")


# Librerías usadas:
library(shiny)
library(tidyverse)
library(sf)

agebs_cdmx <- sf::st_read("agebs_cdmx2.gpkg") %>% 
  mutate(cve_mun=str_pad(string = cve_mun,width = 2,side = "left",pad = 0),
         cve_ent=str_pad(string = cve_ent,width = 2,side = "left",pad = 0),
         cvegeo=str_pad(string = cvegeo,width = 13,side = "left",pad = 0))

ageb_mun <- sf::st_read("base_mun.gpkg")

Encoding(agebs_cdmx$nom_ent) <- "UTF8"
Encoding(agebs_cdmx$nom_mun) <- "UTF8"
Encoding(agebs_cdmx$nom_loc) <- "UTF8"


trigger <- function(base){
  sf::st_join(base %>%
                select(id,lon,lat,everything()) %>% 
                mutate(lon=as.numeric(lon),
                       lat=as.numeric(lat)) %>% 
                sf::st_as_sf(coords=c("lon","lat")) %>% 
                sf::st_set_crs(4326),
              agebs_cdmx,
              join=sf::st_intersects) %>% 
    as_tibble()
}


# Define UI for application that draws a histogram
ui <- shinyUI(
  navbarPage(
    title = "CentroGeo, SIG",
    fluid = TRUE,
    collapsible = TRUE,
    tabPanel("General",
             fluidRow(
               tagList(
                 div(class = "container",
                     h1("Ubic-App",class = "title fit-h1"),
                     p(h4("Herramienta en línea para triggers, delimitado a los AGEBS de la ciudad de México disponibles en el MargoGeoestadístico del INEGI, para usar se requiero un archivo en formato
                       csv con 3 variables, id, lat, lon, la función integrará los puntos donde cae, 
                       la población de ese ageb, su marginación así como distintos indicadores."))))),
             column(12,
                    h4("Ingresa tu base en CSV"),
                    sidebarPanel(
                      fileInput("file1", "Archivos",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),
                      tags$hr(),
                      checkboxInput("header", "Header", TRUE)
                    ),
                    downloadButton("download", "Descarga el Archivo")),
             column(4,
                    tableOutput("archivo")),
             column(4,
                    plotOutput("mapa",
                               width = 700, height = 700))
    ),
    tabPanel("Acerca de",
             fluidRow(
               tagList(
                 div(class = "container",
                     h2("Autores"),
                     p("La Aplicación fue desarrollada por: ",
                       HTML("<ul><li> Zumaya Tapia Rodrigo - al.rzumaya@centrogeo.edu.mx </li><li> Noé Osorio García - al.nosorio@centrogeo.edu.mx </li></ul>"),
                      h2("Asesores"),
                      HTML("<ul><li> Mtra. Paulina Paredes:   pparedes@centrogeo.edu.mx </li><li> Mtro. Jesús Trujillo Almeida:   jtrujillo@centrogeo.edu.mx </li></ul>"),
                      h2("Datos"),
                      HTML("<ul><li> INEGI: Marco Geoestadístico, diciembre 2021 </li><li> INEGI: censo de población y vivienda 2020 </li> <li> CONAPO: Índice de marginación urbana 2020 </li></ul>"),
                      h2("Herramientas "),
                      HTML("<ul><li> QGIS: https://www.qgis.org/es/site/ </li><li> POSTGIS: https://postgis.net/ </li> <li> R: https://www.r-project.org/ </li> </ul>")
                      )
                     )
                 )
               )
             )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {

   
#  Reactividad  de los mapas
  b_mapa <- reactive({
    agebs_cdmx
  })
  
  b_mun <- reactive({
    ageb_mun
  })
  
  # input base

  b_base <- reactive({
    
    req(input$file1)
    
    ext <- tools::file_ext(input$file1$name)
    switch(ext,
           csv = vroom::vroom(input$file1$datapath, delim = ","),
           tsv = vroom::vroom(input$file1$datapath, delim = "\t"),
           validate("Archivo inválido; Por favor sube un archivo tipo '.csv'")
    )
  })
  
  
   # mapa viz
    output$mapa <- renderPlot({
        # generate bins based on input$bins from ui.R
      ggplot()+
        geom_sf(data=b_mapa(),aes(geometry=geom),
                fill="#78B7C5",
                col="#6BB1C1",
                size=.5,
                alpha=.7)+
        geom_sf(data=b_mun(),fill=NA,col="black",size=.6)+
        geom_sf(data=trigger(b_base()) %>% 
                  sf::st_as_sf(),col="red",size=1)+
        theme_void()
    })
    
    output$archivo <- renderTable({
  
      b_base() %>% 
        head(5)

    })
     
    output$download <- downloadHandler(
      filename = function() {
        paste0(str_remove(input$file1$name,".csv"),"_ageb_Pob_.csv")
      },
      content = function(file) {
        write_csv(trigger(b_base()) %>% as_tibble(), file)
      }
    )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
