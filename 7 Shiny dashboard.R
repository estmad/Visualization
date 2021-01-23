######################################################
# 1 Cargar librerias
######################################################

library(shiny)
library(shinydashboard)
library(readr)
library(reshape2)
library(plotly)

######################################################
# 2 Carga y manipulacion de datos
######################################################

datos <- read_csv("datos.csv", 
                  col_types = cols(date = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                   transactions = col_integer(), 
                                   sessions = col_integer()))
datos['dia'] <- as.Date(datos$date)

######################################################
# 3 UI
######################################################

#Preferentemente tener el ui en un fichero independiente
#Aca se modelan los datos, el input
#Tiene una pagina, un encabezado, una barra lateral y el cuerpo que contiene
ui <- dashboardPage(
  #############################################
  # Titulo
  
  dashboardHeader(
    title = "Shinydashboard"
  ),
  
  #############################################
  # Barra lateral
  #con un dobleclik se sombrea lo contenido dentro de un slidbar
  dashboardSidebar(
    # Tasa de rebote
    sliderInput(inputId = "bounce_rate",
                label = "Tasa de rebote",
                min = 0,
                max = 100,
                value = c(40,70),
                step = 5),
    
    # Tipo de dispositivo
    radioButtons(inputId = "device_type",
                 label = "Tipo de dispositivo",
                 choices = c("all","desktop", "mobile", "tablet")),
    
    # Selector de region
    selectInput(inputId = "region",
                label = "Region",
                choices = unique(datos$region),
                selected = NULL,
                multiple = TRUE),
    
    # Selector de fechas
    dateRangeInput(inputId = "rango_fechas",
                   label = "Rango de fechas",
                   start = min(datos$dia),
                   end = max(datos$dia))
    
  ),
  
  #############################################
  # Cuerpo
  dashboardBody(
    
    fluidRow(
      
      infoBox(
        title = "Numero de dias",
        icon = icon("anchor", lib = "font-awesome"),
        value = format(length(unique(datos$dia)), big.mark = ".", decimal.mark = ",")
      ),
      infoBox(
        title = "Sesiones",
        value = format(sum(datos$sessions), big.mark = ".", decimal.mark = ",")
      ),
      infoBox(
        title = "Transacciones",
        value = format(sum(datos$transactions), big.mark = ".", decimal.mark = ",")
      ),
      
    ),
    
    fluidRow(
      box(title = "Sesiones por region",
          status = "primary",
#          plotlyOutput("sesiones_por_region"),
          plotlyOutput(outputId = "grafico_1"),
          solidHeader = TRUE,
          collapsible = TRUE),
      box(title = "Transacciones por sistem operativo",
          status = "primary",
          plotlyOutput(outputId = "grafico_2"),
          solidHeader = TRUE,
          collapsible = TRUE)
      
    ),
    fluidRow(
      box(title = "Sesiones y transacciones por fecha",
          status = "primary",
          plotlyOutput(outputId = "grafico_3"),
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12)
    )
  )
)

######################################################
# 4 SERVER
######################################################

#Preferentemente tener el server en otro fichero independiente
#es el que decide como se va a renderizar
server <- function(input, output) { 
  
  output$grafico_1 <- renderPlotly({
    
    #filtrar conjunto de datos  
    
    #filtro tasa de rebote
    
    datos <- datos[datos$bounceRate >= input$bounce_rate[1] & datos$bounceRate <= input$bounce_rate[2],]  
    
    # filtro tipo de dispositivo
    
    if (input$device_type == "all"){
      datos <- datos
    } else {
      datos <- datos[datos$deviceCategory == input$device_type,]
    }
    
    # filtro region
    
    if (is.null(input$region)){
      datos <- datos
    } else {
      datos <- datos[datos$region %in% input$region,]
    }
    
    # filtro de fechas
    
    #if (input$rango_fechas[1] > input$rango_fechas[2]){
    #  print("Por favor introduzca un rango valido")
    #} else {
    #  datos <- datos[datos$region %in% input$region,]
    #}
    
    datos <- datos[datos$dia >= input$rango_fechas[1] & datos$dia <= input$rango_fechas[2],]
    
    ################################################################################
    
    # Agregacion de datos
    
    datos_1 <- dcast(data = datos,
                     formula =region~.,
                     fun.aggregate = sum,
                     value.var = "sessions")
    names(datos_1)[2] <- "sessions"
    datos_1 <- datos_1[datos_1$region != "(not set)",]
    row.names(datos_1) <- 1:nrow(datos_1)
    
    # Elaboracion del grafico
    
    fig_1 <- plot_ly(datos_1, 
                     x = ~sessions, 
                     y = ~region, 
                     type = 'bar', 
                     text = text,
                     marker = list(color = 'rgb(158,202,225)',
                                   line = list(color = 'rgb(8,48,107)',
                                               width = 1.5)))
    fig_1 <- fig_1 %>% layout(title = "Sesiones por region",
                              xaxis = list(title = ""),
                              yaxis = list(title = ""))
    fig_1
    
  })
    
  #######################################################
  
  output$grafico_2 <- renderPlotly({
    
    #filtro tasa de rebote
    
    datos <- datos[datos$bounceRate >= input$bounce_rate[1] & datos$bounceRate <= input$bounce_rate[2],]  
    
    # filtro tipo de dispositivo
    
    if (input$device_type == "all"){
      datos <- datos
    } else {
      datos <- datos[datos$deviceCategory == input$device_type,]
    }
    
    # filtro region
    
    if (is.null(input$region)){
      datos <- datos
    } else {
      datos <- datos[datos$region %in% input$region,]
    }
    
    # filtro de fechas
    
    # Agregacion de datos  
    
    datos_2 <- dcast(data = datos,
                     formula =operatingSystem~.,
                     fun.aggregate = sum,
                     value.var = "transactions")
    names(datos_2)[2] <- "transactions"
    datos_2 <- datos_2[datos_2$operatingSystem != "(not set)",]
    datos_2 <- datos_2[datos_2$transactions > 0,]
    row.names(datos_2) <- 1:nrow(datos_2)
    
    # Elaboracion de datos
    
    fig_2 <- plot_ly(datos_2, 
                     labels = ~operatingSystem, 
                     values = ~transactions, 
                     type = 'pie')
    fig_2 <- fig_2 %>% layout(title = 'Transacciones por sistema operativo',
                              xaxis = list(showgrid = FALSE, 
                                           zeroline = FALSE, 
                                           showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, 
                                           zeroline = FALSE, 
                                           showticklabels = FALSE))
    fig_2
    
  })
  
  #######################################################
    
  output$grafico_3 <- renderPlotly({
    
    #filtro tasa de rebote
    
    datos <- datos[datos$bounceRate >= input$bounce_rate[1] & datos$bounceRate <= input$bounce_rate[2],]  
    
    # filtro tipo de dispositivo
    
    if (input$device_type == "all"){
      datos <- datos
    } else {
      datos <- datos[datos$deviceCategory == input$device_type,]
    }
    
    # filtro region
    
    if (is.null(input$region)){
      datos <- datos
    } else {
      datos <- datos[datos$region %in% input$region,]
    }
    
    # filtro de fechas
    
    # Agregacion de datos 
    
    datos_3 <- dcast(data = datos,
                     formula =dia~.,
                     fun.aggregate = sum,
                     value.var = "sessions")
    names(datos_3)[2] <- "sessions"
    
    datos_4 <- dcast(data = datos,
                     formula =dia~.,
                     fun.aggregate = sum,
                     value.var = "transactions")
    names(datos_4)[2] <- "transactions"
    
    datos_5 <- merge(x = datos_3,
                     y = datos_4,
                     by.x = "dia",
                     by.y = "dia")  
    
    # Elaboracion de datos  
    
    fig_3 <- plot_ly(datos_5, 
                     x = ~dia, 
                     y = ~sessions, 
                     name = 'Sesiones', 
                     type = 'scatter', 
                     mode = 'lines',
                     yaxis = 'y') 
    fig_3 <- fig_3 %>% add_trace(y = ~transactions, 
                                 name = 'Trasacciones', 
                                 mode = 'lines',
                                 yaxis = 'y2')
    fig_3 <- fig_3 %>% layout(title = "Sesiones y transacciones por dia",
                              xaxis = list(title = "Dia"),
                              yaxis = list (title = "Sesiones",
                                            side = 'left'),
                              yaxis2 = list (title = "Transaccioes",
                                             side = 'right',
                                             overlaying = 'y'))
    fig_3
    
  }) 
  
}

# Para cargar desde otro fichero habria que llamarlo asi: source("server.R")

######################################################
# 5 SHINYAPP
######################################################

#esto lanza la aplicacion, une el ui con el server
shinyApp(ui, server)

######################################################
# 
######################################################
