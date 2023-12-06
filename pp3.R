library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(gapminder)
library(shinythemes)
library(writexl)
library(utils)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Análisis Spotify"),
  
  dashboardSidebar(
    selectInput("anyo", "Año: ", choices = NULL, selected = NULL), 
    selectInput("genero", "Género: ", choices = NULL, selected = NULL),
    downloadButton("download1")
  ), 
  
  dashboardBody(
    navbarPage(
      title = "Explora los datos",
      tabPanel("Tabla resumen", 
               fluidRow(
                 p("Seleccione el ", strong("año"), "que desea observar y escoja el",strong("género.")),
                 p("La tabla presenta un resumen de los datos que usted ha seleccionado.") 
               ),
               fluidRow(
                 column(9,tableOutput("resumen")),
                 column(2)
               ) 
      ),  
      tabPanel("Gráficos interactivos", 
               p("Elija las variables que desea observar."),
               fluidRow(
                 column(6,selectInput(inputId = "variable_1",
                                      label = "Elija una de las siguientes variables",
                                      choices = c("bpm",  "dB" , "energy", "danceability", "liveness", "valence", "duration", "acousticness" ,"speechiness", "popularity")
                 )),
                 column(6,selectInput(inputId = "variable_2",
                                      label = "Elija una de las siguientes variables",
                                      choices = c("bpm",  "dB" , "energy", "danceability", "liveness", "valence", "duration", "acousticness" ,"speechiness", "popularity")))
               ),
               plotOutput("dispersion")
      ),
      theme = shinythemes::shinytheme("paper")
      
    )
  )
)

server <- function(input, output, session) {
  
  spotify_data <- read.csv2("datos/spotify_2000_2023.csv")
  
  observe({
    updateSelectInput(session, "anyo", choices = spotify_data$year)
    updateSelectInput(session, "genero", choices = spotify_data$top.genre)
  })
  
  output$resumen <- renderTable({
    tabla_genero <- spotify_data |> 
      filter(year == input$anyo, top.genre == input$genero) 
    
  })
  
  output$dispersion <- renderPlot({
    
    datos_filtrados <- spotify_data |> 
      filter(year == input$anyo, top.genre == input$genero)
    
    ggplot(data = datos_filtrados, aes_string(x = input$variable_1, y = input$variable_2)) +
      geom_point() +
      labs(x = input$variable_1, y = input$variable_2) +
      theme_minimal()
  })
  
  
  output$download1 <- downloadHandler(
    
    
    filename = function() {
      paste0("DatosSpotifyResumen", ".csv")
    },
    
    content = function(file) {
      tabla_genero <- spotify_data |> 
        filter(year == input$anyo, top.genre == input$genero) 
      write.csv(tabla_genero, file)
      
    }
  )
  
}

shinyApp(ui, server)