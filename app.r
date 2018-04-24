library(foreign)
library(data.table)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

datos <- load("DatosGLOBALES1.RData")

datos <- data.frame(Website = as.character(DATOS[,"Website"]),
                    Provincia = as.character(DATOS[,"Provincia"]),
                    Latitud = as.numeric(DATOS[,"Latitud"]),
                    Longitud = as.numeric(DATOS[,"Longitud"]),
                    Tipo = as.character(DATOS[,"Tipo"]),
                    Precio = as.numeric(DATOS[,"Precio"]),
                    Dormitorios = as.numeric(DATOS[,"Dormitorios"]),
                    Camas = as.numeric(DATOS[,"Camas"]),
                    Banos = as.numeric(DATOS[,"Baños"]),
                    Proveedor = as.character(DATOS[,"Proveedor"]),stringsAsFactors = FALSE)

datos <- datos[-which(is.na(datos$Provincia)),]
datos$Website <- trimws(datos$Website)
datos$Provincia <- trimws(datos$Provincia)
datos$Tipo <- trimws(datos$Tipo)
datos$Precio <- trimws(datos$Precio)
datos$Dormitorios <- trimws(datos$Dormitorios)
datos$Camas <- trimws(datos$Camas)
datos$Banos <- trimws(datos$Banos)
datos$Proveedor <- trimws(datos$Proveedor)

vars <- c("Todas", sort(unique((datos$Provincia))))
vars1 <- c("Todos", sort(unique((datos$Proveedor))))

counts <- ddply(datos,.(datos$Proveedor,datos$Provincia),nrow)
counts <- na.omit(counts)
names(counts) <- c("Proveedor","Provincia","Cantidad")


ui <- navbarPage("Alojamientos Argentina", id="nav",

  tabPanel("Mapa Interactivo",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        #includeCSS("styles.css"),
        #includeScript("gomap.js")
      ),
      
      leafletOutput("map", width="100%", height="100%"),
      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    
                    h2("Filtro"),
                    
                    selectInput("Proveedores", "Proveedores", vars1),
                    selectInput("Provincia", "Provincia", vars)
                    
      )
      
    )
  ),
  tabPanel("Datos Generales", dataTableOutput("summary1")),
  tabPanel("Summary precios", dataTableOutput("summary3"),
           absolutePanel(id = "controls1", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                         width = 330, height = "auto",
                         
                         h2("Filtro"),
                         selectInput("Filtro0", "Proveedor", unique(datos$Proveedor)),
                         uiOutput('columns'),
                         uiOutput('columns1'), 
                         selectInput("Filtro4", "Bano/Camas", c("Bano","Camas"))
                         
                         
           ))
)

server <- function(input, output, session) {
  
}



shinyApp(ui, server)



