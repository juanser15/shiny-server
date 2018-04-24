#library(foreign)
library(shiny)
library(leaflet)
#library(RColorBrewer)
#library(scales)
#library(lattice)
library(dplyr)

datos <- load(file.path(www,"DatosGLOBALES1.RData"))

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
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = first(datos$Longitud), lat = first(datos$Latitud), zoom = 1)
  })
  
  
  
  
  observe({
    if(input$Proveedores == "Todos") {
      data = datos
    } else {
      data = datos[datos$Proveedor == input$Proveedores,]
    } 
    if(input$Provincia == "Todas") {
      data = data
    } else {
      data = data[data$Provincia == input$Provincia,]
    }
    if(sum(lengths(data)) != 0){
      POP =  paste("<strong>","Provincia:","</strong>",data$Provincia,"<br>",
                   "<strong>","Tipo de alojamiento:","</strong>",data$Tipo_Alojamiento,"<br>",
                   "<strong>","Proveedor:","</strong>",data$Proveedor,"<br>",
                   "<strong>","Website:","</strong>",paste0("<a href='",data$Website,"'>",data$Website,"</a>"))  
      Prov = data$Provincia} else {
        POP = NULL       
        Prov = NULL }
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      addTiles() %>% 
      addCircles(~Longitud, ~Latitud,
                 stroke=FALSE, fillOpacity=0.5, fillColor="blue",fill = "polygons",radius = 5000,
                 popup = POP ,
                 label = Prov)# %>%
  })
  
  output$summary1 <- renderDataTable({
    if(input$Proveedores == "Todos") {
      data = datos
    } else {
      data = datos[datos$Proveedor == input$Proveedores,]
    }
    
    if(input$Provincia == "Todas") {
      data = data
    } else {
      data = data[data$Provincia == input$Provincia,]
    }
    data  })
  
  
  output$summary2 <- renderDataTable({
    counts[counts$Proveedor == input$Proveedores,]  })
  
  output$columns <- renderUI({   
   Provincias <- unique(datos$Provincia[which(datos$Proveedor == input$Filtro0)])
       tagList(
    selectInput("Filtro2", "Provincias/Ciudades", choices = Provincias)
    )
  })
  output$columns1 <- renderUI({   
    Tipos <- unique(datos$Tipo[which(datos$Provincia == input$Filtro2)])
    tagList(
    selectInput("Filtro3", "Tipo", choices = Tipos)
    )
  })
  
  output$summary3 <- renderDataTable({
           DATA <- datos[which(datos$Proveedor == input$Filtro0 & datos$Provincia == input$Filtro2 & datos$Tipo == input$Filtro3),]
           DATA <- data.frame(Precio = as.numeric(DATA$Precio),
                              Dormitorios = as.numeric(DATA$Dormitorios),
                              Camas = as.numeric(DATA$Camas),
                              Banos = as.numeric(DATA$Banos),stringsAsFactors = FALSE)
           if(input$Filtro4 == "Camas"){
             output <-    DATA %>% 
             group_by(Camas) %>%
             summarise(Precio.ARS  = ceiling(mean(Precio)),
                       Dormitorios  = ceiling(mean(Dormitorios)),
                       Banos = ceiling(mean(Banos)))
           } else {
               output <-         DATA %>% 
                       group_by(Banos) %>%
                       summarise(Precio.ARS  = ceiling(mean(Precio)),
                                 Dormitorios  = ceiling(mean(Dormitorios)),
                                 Camas = ceiling(mean(Camas)))
           }
           
           data.table::data.table(output)
      })
}



shinyApp(ui, server)



