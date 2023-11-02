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
    "Tabla de datos", #cambiar nombre
    fluidRow(
      column(4,selectInput(inputId = "region", label = "Escoja pais/región", choices = NULL,selected = NULL)),
      column(4,selectInput(inputId = "anyo", label = "Escoja el año", choices = NULL, selected = NULL)),
    ),
    fluidRow(
      column(4,actionButton(inputId = "update", label = "Filtrar tabla", icon = icon("table") )),
      column(4,actionButton(inputId = "reset", label = "Resetear Tabla", icon = icon("reset")))
    ),
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
     fluidRow(
       column(6,selectInput(inputId = "region",label = "Escoja una región", choices = unique(datos_empleo$pais_region))),
       column(6,actionButton(inputId = "filtrar", label = "Filtrar", icon = icon("barchart"))) 
       ),
     fluidRow(
       column(6,plotOutput("grafico_dispersion")),
       column(6,plotOutput("grafico_dispersion2"))
     ),
      
    ),
    tabPanel(
      title = "Analítica Avanzada",
      plotOutput("grafico")  
    )
  )
)

server <- function(input, output, session) {
  datos_empleo <- read_csv("datos/datos_empleo_genero.csv")
  #Tabla
  observe({ 
    updateSelectInput(session, "anyo", choices = unique(datos_empleo$anyo))
    updateSelectInput(session, "region",choices = unique(datos_empleo$pais_region))
  })
  
  observeEvent(input$update,{
    output$tabla_empleo <- renderTable(digits = 0, {
      filter(datos_empleo, pais_region == input$region & anyo == input$anyo)
    })
  })
  
  observeEvent(input$reset,{
    updateSelectInput(session, "anyo", choices = NULL, selected =  NULL)
    updateSelectInput(session, "region", choices = NULL,selected = NULL)
  }) #revisar como resetear la tabla para que aparezca en blanco
  
  #Grafico de barras
    
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
    
    
    #scatterplot 1
    observeEvent(input$filtrar, {
      datos_filtrados <- datos_empleo |> 
        filter(pais_region == input$region)
      
      if (nrow(datos_filtrados) > 0) {
        grafico_disp <- ggplot(data = datos_filtrados) +
          geom_point(mapping = aes(x = desempleo_educacion_mujeres,
                                   y = desempleo_mujeres), 
                     color = "blue", size = 6) +
          labs(title = "Relación entre mujeres desempleadas por país", 
               x = "Mujeres desempleadas con educación", 
               y = "Mujeres desempleados") +
          theme_minimal()
        
        output$grafico_dispersion <- renderPlot({
          print(grafico_disp)
        })
      } else {
        output$grafico_dispersion <- renderText("No hay datos disponibles para el año seleccionado.")
      }
    })
    #scatterplot 2
    observeEvent(input$filtrar, {
      datos_filtrados <- datos_empleo |> 
        filter(pais_region == input$region)
      
      if (nrow(datos_filtrados) > 0) {
        grafico_disp2 <- ggplot(data = datos_filtrados) +
          geom_point(mapping = aes(x = desempleo_educacion_hombres,
                                   y = desempleo_hombres), 
                     color = "red", size = 6) +
          labs(title = "Relación entre hombres desempleados por pais", 
               x = "Hombres desempleadas con educación", 
               y = "Hombres desempleados") +
          theme_minimal()
        
        output$grafico_dispersion2 <- renderPlot({
          print(grafico_disp2)
        })
      } else {
        output$grafico_dispersion2 <- renderText("No hay datos disponibles para el año seleccionado.")
      }
    })
  })
}

shinyApp(ui, server)


