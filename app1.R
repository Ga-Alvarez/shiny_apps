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
    "Tabla de datos", 
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
        column(6,actionButton(inputId = "filtrar", label = "Filtrar", icon = icon("dashboard"))) 
      ),
      fluidRow(
        column(6,plotOutput("grafico_dispersion")),
        column(6,plotOutput("grafico_dispersion2"))
      ),
      
    ),
    tabPanel(
      title = "Histograma",
      fluidRow(
        column(6,selectInput(inputId = "variable", label = "Escoje una variable", 
                             choices = c("empleo_informal_mujeres", "empleo_informal_hombres", "empleo_parcial_mujeres", "desempleo_educacion_mujeres", "desempleo_educacion_hombres", "desempleo_mujeres", "desempleo_hombres")
        ))
      ),
      plotOutput("histo")  
    )
  )
)

server <- function(input, output, session) {
  datos_empleo <- read_csv("datos/datos_empleo_genero.csv")
  
  observe({ 
    updateSelectInput(session, "anyo", choices = unique(datos_empleo$anyo))
    updateSelectInput(session, "region", choices = unique(datos_empleo$pais_region))
  })
  
  observeEvent(input$update, {
    datos_filtrados <- filter(datos_empleo, pais_region == input$region, anyo == input$anyo)
    output$tabla_empleo <- renderTable(digits = 0, {
      datos_filtrados
    })
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "anyo", choices = NULL, selected =  NULL)
    updateSelectInput(session, "region", choices = NULL, selected = NULL)
    output$tabla_empleo <- renderTable(NULL)
  })
  
  observe({
    output$grafico_barras <- renderPlot({
      resumen_datos <- datos_empleo |> 
        group_by(pais_region) |> 
        summarise(total_agricultura = sum(empleo_agricultura_mujeres + empleo_agricultura_hombres, na.rm = TRUE))
      
      ggplot(data = resumen_datos) +
        geom_bar(mapping = aes(x = pais_region, y = total_agricultura), stat = "identity", fill = "lightgreen") +
        labs(title = "Cantidad de personas que trabajan en la agricultura", x = "Pais/región", y = "Cantidad de personas") +
        theme_minimal() +
        scale_y_continuous(breaks = seq(0, 400000, by = 50000))
    })
    
    observeEvent(input$filtrar, {
      datos_filtrados <- datos_empleo |> 
        filter(pais_region == input$region)
      
      if (nrow(datos_filtrados) > 0) {
        grafico_disp <- ggplot(data = datos_filtrados) +
          geom_point(mapping = aes(x = desempleo_educacion_mujeres, y = desempleo_mujeres), color = "blue", size = 6) +
          labs(title = "Relación entre mujeres desempleadas por país", x = "Mujeres desempleadas con educación", y = "Mujeres desempleados") +
          theme_minimal()
        
        grafico_disp2 <- ggplot(data = datos_filtrados) +
          geom_point(mapping = aes(x = desempleo_educacion_hombres, y = desempleo_hombres), color = "red", size = 6) +
          labs(title = "Relación entre hombres desempleados por pais", x = "Hombres desempleadas con educación", y = "Hombres desempleados") +
          theme_minimal()
        
        output$grafico_dispersion <- renderPlot({
          print(grafico_disp)
        })
        
        output$grafico_dispersion2 <- renderPlot({
          print(grafico_disp2)
        })
      } else {
        output$grafico_dispersion <- renderText("No hay datos disponibles para el año seleccionado.")
        output$grafico_dispersion2 <- renderText("No hay datos disponibles para el año seleccionado.")
      }
    })
    
    output$histo <- renderPlot({
      ggplot(datos_empleo, aes_string(x = input$variable)) + 
        geom_histogram(binwidth = 5, fill = "blue", color = "white", alpha = 0.7, na.rm = FALSE) +
        labs(title = paste("Histograma de", input$variable), x = input$variable) +
        theme_minimal()
    })
  })
}

shinyApp(ui, server)


