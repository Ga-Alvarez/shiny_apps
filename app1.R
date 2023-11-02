library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)  


ui <- navbarPage(
  title = "Análisis de datos des(empleo)",
  
  tabPanel(
    title = "Contexto",
    "Texto de bienvenida e introducción"
  ),
  tabPanel(
    title = "Tabla",
    "Modelos de aprendizaje automático", #cambiar nombre
    tableOutput("tabla_empleo")  
  ),
  navbarMenu(
    title = "Gráficos",
    tabPanel(
      title = "Análisis por región de empleos agricultura",
      plotOutput("grafico_barras")  
    ),
    tabPanel(
      title = "Análisis correlación",
      selectInput(inputId = "anyo", 
                  label = "Escoja un año", 
                  choices = unique(datos_empleo$anyo)),
      actionButton(inputId = "filtrar", 
                   label = "Filtrar"),
      plotOutput("grafico_dispersion")  
    ),
    tabPanel(
      title = "Analítica Avanzada",
      plotOutput("grafico")  
    )
  )
)

server <- function(input, output, session) {
  datos_empleo <- read_csv("datos/datos_empleo_genero.csv")
  
  output$tabla_empleo <- renderTable({
    head(datos_empleo, 30)
  })
  
  observe({
    output$grafico_barras <- renderPlot({
      resumen_datos <- datos_empleo  |> 
        group_by(pais_region) |> 
        summarise(total_agricultura = sum(empleo_agricultura_mujeres + empleo_agricultura_hombres, na.rm = TRUE))
      
      ggplot(data = resumen_datos) +
        geom_bar(mapping = aes(x = pais_region, 
                               y = total_agricultura), 
                 stat = "identity", 
                 fill = "lightgreen") +
        labs(title = "Cantidad de personas que trabajan en la agricultura", x = "Pais/región", y = "Cantidad de personas") +
        theme_minimal() +
        scale_y_continuous(breaks = seq(0, 400000, by = 50000))
    })
    
    observeEvent(input$filtrar, {
      datos_filtrados <- datos_empleo |> 
        filter(anyo == input$anyo)
      
      if (nrow(datos_filtrados) > 0) {
        grafico_disp <- ggplot(data = datos_filtrados) +
          geom_point(mapping = aes(x = desempleo_mujeres,
                                   y = desempleo_hombres), 
                     color = "blue") +
          labs(title = "Relación entre mujeres y hombres sin empleo", 
               x = "Mujeres desempleadas", 
               y = "Hombres desempleados") +
          theme_minimal()
        
        output$grafico_dispersion <- renderPlot({
          print(grafico_disp)
        })
      } else {
        output$grafico_dispersion <- renderText("No hay datos disponibles para el año seleccionado.")
      }
    })
  })
}

shinyApp(ui, server)


