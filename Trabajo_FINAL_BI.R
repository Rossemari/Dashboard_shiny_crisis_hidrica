#Cargar librerías
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(shinyWidgets)
library(plotly)
library(zoo)
library(lubridate)
library(DT)
library(leaflet)
library(sf)
library(RColorBrewer)
library(readxl)
library(openxlsx)
library(forecast)

summary(modelo_log)

#Ruta del archivo Excel
archivo_excel <- "C:/Users/rosse/Downloads/BI/datos_agua_chile.xlsx"

#Ruta del archivo geojson
regiones_sf <- st_read("C:/Users/rosse/Downloads/BI/regional.geojson.txt", quiet = TRUE)

#Leer el Excel
precipitaciones_mensuales <- read_excel(archivo_excel, sheet = "precipitaciones_mensuales")
caudales_mensuales <- read_excel(archivo_excel, sheet = "caudales_mensuales")
acceso_agua_urbana <- read_excel(archivo_excel, sheet = "acceso_agua_urbana")
recursos_renovables <- read_excel(archivo_excel, sheet = "recursos_renovables")
extraccion_agua <- read_excel(archivo_excel, sheet = "extraccion_agua")
productividad_agua <- read_excel(archivo_excel, sheet = "productividad_agua")
precipitaciones_anuales <- read_excel(archivo_excel, sheet = "precipitaciones_anuales")
valores_regiones <- read_excel(archivo_excel, sheet = "valores_regiones")

#Duplicar los datos para realizar el t.test o prueba de wilcoxon
precipitaciones_anuales_copia <- precipitaciones_anuales
extraccion_agua_copia <- extraccion_agua
productividad_agua_copia <- productividad_agua
recursos_renovables_copia <- recursos_renovables
acceso_agua_urbana_copia <- acceso_agua_urbana

#TRANSFORMACIONES

#Precipitaciones mensuales
names(precipitaciones_mensuales) <- c("fecha", "precipitaciones_mensuales")
precipitaciones_mensuales$fecha <- as.Date(as.yearmon(precipitaciones_mensuales$fecha))

#Caudales mensuales
names(caudales_mensuales) <- c("fecha", "caudales_m3_por_segundo")
caudales_mensuales$fecha <- as.Date(paste0(caudales_mensuales$fecha, "-01"))

#Acceso a agua urbana
acceso_agua_urbana <- acceso_agua_urbana[,-1:-2]
acceso_agua_urbana <- acceso_agua_urbana[,-2:-6]
names(acceso_agua_urbana) <- c("año", 
                               "agua_superficial", 
                               "servicios_de_agua_potable_no_mejorados",
                               "servicios_limitados_de_agua_potable",
                               "servicios_básicos_de_agua_potable",
                               "agua_potable_gestionados_de_forma_segura")

#Recursos renovables
recursos_renovables <- recursos_renovables[,-1:-2]
names(recursos_renovables) <- c("año", "recursos_internos_renovables_de_agua_dulce")

#Extracción de agua
extraccion_agua <- extraccion_agua[,-1:-2]
names(extraccion_agua) <- c("año", "extracción_agua_dulce_en_millones_de_metros_cúbicos")

#Productividad del agua
productividad_agua <- productividad_agua[,-1:-2]
names(productividad_agua) <- c("año", "PIB_por_m3_de_extracción_total_de_agua_dulce")

#Valores por región
valores_regiones <- valores_regiones[,-2:-5]
valores_regiones <- valores_regiones[,-7:-10]

#Precipitaciones anuales
precipitaciones_anuales <- precipitaciones_anuales[,-1:-2]
names(precipitaciones_anuales) <- c("año", "precipitaciones")

#Unir valores de regiones al GeoDataFrame
regiones_sf <- regiones_sf %>%
  left_join(valores_regiones, by = c("Region" = "REGION"))

###########################################
##2001 al 2021
precipitaciones_mensuales <- precipitaciones_mensuales[-1:-264,]
precipitaciones_mensuales <- precipitaciones_mensuales[-253:-288,]

caudales_mensuales <- caudales_mensuales[-253:-288,]

acceso_agua_urbana <- acceso_agua_urbana[-1,]
acceso_agua_urbana <- acceso_agua_urbana[-22,]

recursos_renovables <- recursos_renovables[-1:-40,]

extraccion_agua <- extraccion_agua[-1:-9,]

productividad_agua <- productividad_agua[-1:-9,]

precipitaciones_anuales <- precipitaciones_anuales[-1:-61,]
precipitaciones_anuales <- precipitaciones_anuales[-22:-24,]
#######################

#Renombrar variables
variables_climaticas <- c(
  "Temperatura media estival" = "Temed",
  "Temperatura media invernal" = "Tjmed",
  "Precipitación normal anual" = "PPA",
  "Precipitación anual mínima" = "PPA.MIN",
  "Precipitación anual máxima" = "PPA.MAX",
  "Estimación 2050 temperatura media estival" = "Temed.50",
  "Estimación 2050 temperatura media invernal" = "Tjmed.50",
  "Estimación 2050 precipitación normal anual" = "PPA.50",
  "Estimación 2050 precipitación anual mínima" = "PPA.50.MIN",
  "Estimación 2050 precipitación anual máxima" = "PPA.50.MAX"
)

variables_bases_datos <- list(
  prec_mens = "Precipitaciones mensuales",
  caudales = "Caudales mensuales",
  recursos = "Recursos renovables",
  extraccion = "Extracción agua",
  productividad = "Productividad agua",
  prec_anual = "Precipitaciones anuales",
  agua_urbana_superficial = "Acceso agua superficial",
  agua_urbana_segura = "Acceso agua seguro",
  agua_urbana_no_mejorada = "Acceso agua no mejorada",
  agua_urbana_basico = "Acceso agua basico"
)

####################################################
acceso_agua_urbana_no_mejorados <- acceso_agua_urbana[,c(-2,-4,-5,-6)]
acceso_agua_urbana_basico <- acceso_agua_urbana[,c(-2,-3,-4,-6)]
acceso_agua_urbana_seguro <- acceso_agua_urbana[,c(-2,-3,-4,-5)]

datos_modelo_lineal <- productividad_agua %>%
  left_join(precipitaciones_anuales, by = "año")%>%
  left_join(recursos_renovables, by = "año")%>%
  left_join(extraccion_agua, by = "año")%>%
  left_join(acceso_agua_urbana_seguro, by = "año")%>%
  left_join(acceso_agua_urbana_basico, by = "año")%>%
  left_join(acceso_agua_urbana_no_mejorados, by = "año")

#UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Crisis hídrica en Chile"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",  
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Análisis descriptivo", tabName = "analisis", icon = icon("chart-bar")),
      menuItem("Disponibilidad de agua", tabName = "disponibilidad", icon = icon("tint")),
      menuItem("Uso del agua", tabName = "uso", icon = icon("faucet")),
      menuItem("Acceso al agua", tabName = "acceso", icon = icon("hand-holding-water")),
      menuItem("Análisis avanzado", tabName = "visualizacion", icon = icon("chart-line")),
      menuItem("Recomendaciones", tabName = "recomendaciones", icon = icon("hands-helping")),
      
      #Rango de años
      selectInput("rango_global", "Selecciona el rango de años:",
                  choices = c("2001 - 2005", "2006 - 2010", "2011 - 2015", "2016 - 2021", "2001 - 2021"),
                  selected = "2001 - 2021"),
      
      conditionalPanel(
        condition = "input.tabs == 'acceso'",
        checkboxGroupInput("servicios_seleccionados", "Servicios de agua urbana:",
                           choices = c("Servicios limitados de agua potable" = "servicios_limitados_de_agua_potable",
                                       "Servicios básicos de agua potable" = "servicios_básicos_de_agua_potable",
                                       "Servicios de agua potable no mejorados" = "servicios_de_agua_potable_no_mejorados",
                                       "Uso de agua superficial" = "agua_superficial"),
                           selected = c("servicios_limitados_de_agua_potable", 
                                        "servicios_básicos_de_agua_potable", 
                                        "servicios_de_agua_potable_no_mejorados", 
                                        "agua_superficial"))
      ),
      
      #Botón para descargar el PDF
      br(),
      downloadButton("generar_informe", "📄 Descargar informe PDF")
    )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "inicio",
              h1("Dashboard de recursos hídricos en Chile"),
              p("Este dashboard interactivo presenta indicadores relacionados con la disponibilidad, uso y acceso al agua en Chile. Se utilizan múltiples bases de datos que abarcan diferentes períodos y tipos de información."),
              br(),
              h4("Descripción de las bases de datos utilizadas:"),
              tags$ul(
                tags$li("Precipitaciones mensuales en Chile."),
                tags$li("Precipitaciones anuales en Chile."),
                tags$li("Caudales de ríos en Chile por mes."),
                tags$li("Población urbana en Chile por año, según tipo de acceso a servicios de agua potable:"),
                tags$ul(
                  tags$li("Uso de agua superficial como fuente primaria."),
                  tags$li("Servicios de agua no mejorados."),
                  tags$li("Servicios limitados."),
                  tags$li("Servicios básicos."),
                  tags$li("Servicios gestionados de forma segura.")
                ),
                tags$li("Recursos internos renovables de agua dulce per cápita (m³)"),
                tags$li("Extracción total anual de agua dulce (miles de millones de m³)"),
                tags$li("Productividad del agua (PIB en dólares constantes de 2015 por m³ de agua extraída)"),
                tags$li("Base digital del clima en Chile: línea base (1980-2010) y proyección al año 2050")
              ),
              br(),
              h4("Fuentes de datos:"),
              tags$ul(
                tags$li("Precipitaciones mensuales y caudales de ríos mensuales: ",
                        tags$a(href = "https://globalwater.online/", 
                               "Global Water Monitor", target = "_blank")),
                tags$li("Acceso al agua urbana, recursos renovables, extracción y productividad del agua: ",
                        tags$a(href = "https://ourworldindata.org/", 
                               "Our World in Data", target = "_blank")),
                tags$li("Datos climáticos por región y su proyección al 2050: ",
                        tags$a(href = "https://mma.gob.cl/", 
                               "Ministerio del Medio Ambiente", target = "_blank"))
              )
              
      ),
      
      tabItem(tabName = "analisis",
              fluidRow(
                box(title = "Seleccionar base de datos", width = 3, status = "warning", solidHeader = TRUE,
                    selectInput("base_datos", "Base de datos:",
                                choices = c("Precipitaciones mensuales" = "prec_mens",
                                            "Caudales mensuales" = "caudales",
                                            "Recursos renovables" = "recursos",
                                            "Extracción agua" = "extraccion",
                                            "Productividad agua" = "productividad",
                                            "Precipitaciones anuales" = "prec_anual",
                                            "Acceso agua seguro" = "agua_urbana_segura",
                                            "Acceso agua no mejorada" = "agua_urbana_no_mejorada",
                                            "Acceso agua basico" = "agua_urbana_basico"))
                ),
                
                box(
                  title = "Variables y resumen estadístico",
                  width = 9,
                  status = "info",
                  solidHeader = TRUE,
                  DT::dataTableOutput("tabla_variables_resumen")
                )),
              
              fluidRow(
                box(
                  title = "Gráficos por variable",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  uiOutput("grafico_variable")
                )
              )
      ),
      
      
      #Disponibilidad de Agua
      tabItem(tabName = "disponibilidad",
              fluidRow(
                valueBoxOutput("kpi_precipitacion"),
                valueBoxOutput("kpi_caudal"),
                valueBoxOutput("kpi_renovables")
              ),
              fluidRow(
                box(
                  title = "Precipitaciones mensuales", width = 6, solidHeader = TRUE, status = "info",
                  plotlyOutput("plot_precipitaciones_mensuales")),
                box(title = "Precipitaciones anuales", width = 6, solidHeader = TRUE, status = "info",
                    plotlyOutput("plot_precipitaciones_anuales"))
              ),
              fluidRow(
                box(title = "Selecciona una variable climática", width = 5, solidHeader = TRUE, status = "warning",
                    selectInput("variable", "Variable climática:",
                                choices = variables_climaticas,
                                selected = "Temed")),
                box(title = "Mapa climático por región", width = 7, solidHeader = TRUE, status = "success",
                    leafletOutput("mapa", height = 600))
              )
              ,
              fluidRow(
                box(title = "Caudales mensuales", width = 6, solidHeader = TRUE, status = "primary",
                    plotlyOutput("plot_caudales_mensuales")),
                box(title = "Recursos renovables de agua dulce", width = 6, solidHeader = TRUE, status = "primary",
                    plotlyOutput("plot_renovables"))
              )
      ),
      
      #Uso del Agua
      tabItem(tabName = "uso",
              fluidRow(
                valueBoxOutput("kpi_extraccion"),
                valueBoxOutput("kpi_productividad")
              ),
              fluidRow(
                box(title = "Extracción total de agua dulce", width = 6, solidHeader = TRUE, status = "info",
                    plotlyOutput("plot_extraccion")),
                box(title = "Productividad del agua", width = 6, solidHeader = TRUE, status = "info",
                    plotlyOutput("plot_productividad"))
              )
      ),
      
      #Acceso al Agua
      tabItem(tabName = "acceso",
              fluidRow(
                valueBoxOutput("kpi_acceso_seguro"),
                valueBoxOutput("kpi_servicios_basicos")
              ),
              
              fluidRow(
                box(title = "Agua potable gestionada de forma segura", width = 6, solidHeader = TRUE, status = "primary",
                    plotlyOutput("plot_acceso_seguro")),
                box(title = "Servicios básicos y limitados", width = 6, solidHeader = TRUE, status = "primary",
                    plotlyOutput("plot_servicios_basicos"))
              )
      ),
      
      tabItem(tabName = "visualizacion",
              h1("Predicción de precipitaciones mensuales"),
              p("Esta sección entrega proyecciones mensuales de precipitaciones con base en series históricas, permitiendo anticipar escenarios de disponibilidad hídrica y apoyar la planificación territorial con un enfoque preventivo y adaptativo."),
              
              fluidRow(
                box(
                  title = "Serie histórica y predicción SARIMA (2025-2026)",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotOutput("grafico_precipitaciones_sarima", height = "350px")
                )
              ),
              
              
              
              fluidRow(
                box(
                  title = "Tabla de predicciones mensuales",
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
                  selectInput("anio_tabla", "Selecciona año para mostrar la tabla:", choices = c(2025, 2026), selected = 2025),
                  dataTableOutput("tabla_predicciones_sarima")
                )
              ),
              
              h1("Modelo de regresión múltiple sobre productividad del agua"),
              p("Este módulo permite explorar la relación conjunta entre variables de disponibilidad y extracción del agua sobre la productividad económica del recurso."),
              
              fluidRow(
                box(title = "Seleccionar una variable para visualizar su efecto parcial", width = 4, status = "warning", solidHeader = TRUE,
                    selectInput("variable_parcial", "Seleccionar variable para visualizar su efecto:",
                                choices = c("Acceso seguro" = "agua_potable_gestionados_de_forma_segura",
                                            "Acceso básico" = "servicios_básicos_de_agua_potable",
                                            "Extracción de agua" = "extracción_agua_dulce_en_millones_de_metros_cúbicos"))
                )
              ),
              
              fluidRow(
                box(title = "Gráfico del efecto parcial", width = 12,
                    solidHeader = TRUE, status = "primary",
                    plotlyOutput("grafico_parcial"))
              ),
              
              fluidRow(
                box(title = "Visualización del modelo", width = 12, solidHeader = TRUE, status = "primary",
                    plotlyOutput("grafico_efecto_modelo"))
              )
      ),
      
      tabItem(tabName = "recomendaciones",
              h1("Recomendaciones de gestión hídrica"),
              
              h3("Corto plazo (0–1 año)"),
              tags$ul(
                tags$li("Implementar sistemas de monitoreo inteligente en tiempo real: Instalar sensores IoT para medir la extracción de agua a nivel sectorial. Esto permitirá mejorar el control, evaluar eficiencia y ajustar políticas en base a evidencia actualizada."),
                tags$li("Capacitar a autoridades locales en eficiencia hídrica: Entrenar a gobiernos locales en el uso de indicadores como el PIB por metro cúbico extraído, promoviendo decisiones informadas y sostenibles."),
                tags$li("Establecer incentivos por productividad hídrica: Reconocer a sectores que logren generar mayor valor económico con menor volumen de agua, especialmente en agricultura e industria, reduciendo presión sobre el recurso sin frenar la actividad económica.")
              ),
              
              h3("Mediano plazo (1–3 años)"),
              tags$ul(
                tags$li("Reformular la asignación del recurso: Integrar criterios de eficiencia económica y sostenibilidad en los marcos de distribución del agua, priorizando usos que generen mayor retorno social y económico."),
                tags$li("Fomentar adopción tecnológica en sectores de alto consumo: Impulsar el uso de riego tecnificado, reutilización de aguas tratadas y tecnologías circulares en sectores estratégicos para mejorar la eficiencia operativa."),
                tags$li("Desarrollar dashboards públicos interactivos: Crear plataformas abiertas de monitoreo y seguimiento de indicadores clave (productividad, extracción, acceso al agua), fomentando la transparencia, la participación ciudadana y la fiscalización social.")
              )
      )
      
    ))
  
)

#Server
server <- function(input, output) {
  
  #DISPONIBILIDAD
  output$plot_precipitaciones_mensuales <- renderPlotly({
    
    # Parsear el rango de texto, por ejemplo "2010 - 2023"
    rangos <- strsplit(input$rango_global, " - ")[[1]]
    año_min <- as.numeric(rangos[1])
    año_max <- as.numeric(rangos[2])
    
    # Agregar columnas año y mes
    datos <- precipitaciones_mensuales %>%
      mutate(
        año = year(fecha),
        mes_num = month(fecha),
        mes = factor(month.abb[mes_num], levels = month.abb),
        precipitacion = precipitaciones_mensuales
      ) %>%
      filter(año >= año_min, año <= año_max)
    
    
    p <- ggplot(datos, aes(x = mes, y = precipitacion, group = año, color = factor(año))) +
      geom_line(linewidth = 0.8) +
      geom_point() +
      labs(
        x = "Mes",
        y = "Precipitación (mm)",
        color = "Año"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(
        breaks = pretty(datos$precipitacion, n = 6),
        labels = scales::label_number(big.mark = ".", decimal.mark = ",")
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Precipitaciones mensuales medidas en mm <br>comparadas por año en Chile",
          x = 0.5,
          y = 0.97,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 14)  
        )
      )
  })
  
  
  output$plot_precipitaciones_anuales <- renderPlotly({
    #Parsear el rango seleccionado
    rangos <- strsplit(input$rango_global, " - ")[[1]]
    año_min <- as.numeric(rangos[1])
    año_max <- as.numeric(rangos[2])
    
    #Filtrar datos según rango
    datos_filtrados <- precipitaciones_anuales %>%
      filter(año >= año_min, año <= año_max)
    
    p <- ggplot(datos_filtrados, aes(x = año, y = precipitaciones)) +
      geom_line(color = "turquoise3") +
      geom_point(color = "deepskyblue3") +
      labs(y = "Precipitación (mm)", x = "Año") +
      theme_minimal() +
      scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","))
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Precipitaciones anuales medidas en mm <br> por año en Chile",
          x = 0.5,
          y = 0.97,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 14)  #Ajusta tamaño
        )
      )
  })
  
  
  output$plot_caudales_mensuales <- renderPlotly({
    
    #Parsear el rango seleccionado
    rangos <- strsplit(input$rango_global, " - ")[[1]]
    año_min <- as.numeric(rangos[1])
    año_max <- as.numeric(rangos[2])
    
    #Filtrar datos según rango
    datos_filtrados <- caudales_mensuales %>%
      filter(year(fecha) >= año_min, year(fecha) <= año_max)
    
    
    p <- ggplot(datos_filtrados, aes(x = fecha, y = caudales_m3_por_segundo)) +
      geom_line(color = "turquoise4") +
      labs(x = "Fecha", y = "m³/s", title = "Caudales mensuales") +
      theme_minimal() +
      scale_y_continuous(
        breaks = pretty(datos_filtrados$caudales_m3_por_segundo, n = 5),
        labels = scales::label_number(big.mark = ".", decimal.mark = ",")
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Caudales de ríos medidos en m³/s<br> por mes en Chile",
          x = 0.5,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 14)  
        )
      )
  })
  
  
  output$plot_renovables <- renderPlotly({
    #Parsear el rango seleccionado
    rangos <- strsplit(input$rango_global, " - ")[[1]]
    año_min <- as.numeric(rangos[1])
    año_max <- as.numeric(rangos[2])
    
    #Filtrar datos según rango
    datos_filtrados <- recursos_renovables %>%
      filter(año >= año_min, año <= año_max)
    
    p <- ggplot(datos_filtrados, aes(x = año, y = recursos_internos_renovables_de_agua_dulce)) +
      geom_line(color = "springgreen4") +
      geom_point() +
      labs(x = "Año", y = "m³ por persona") +
      theme_minimal() +
      scale_y_continuous(
        breaks = pretty(datos_filtrados$recursos_internos_renovables_de_agua_dulce, n = 5),
        labels = scales::label_number(big.mark = ".", decimal.mark = ",")
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Recursos internos renovables de agua dulce per cápita<br> medido en m³ por persona por año en Chile",
          x = 0.5,
          y = 0.97,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 14)  #Ajusta tamaño
        )
      )
  })
  
  #USO DEL AGUA
  output$plot_extraccion <- renderPlotly({
    #Parsear el rango seleccionado
    rangos <- strsplit(input$rango_global, " - ")[[1]]
    año_min <- as.numeric(rangos[1])
    año_max <- as.numeric(rangos[2])
    
    #Filtrar datos según rango
    datos_filtrados <- extraccion_agua %>%
      filter(año >= año_min, año <= año_max)
    
    p <- ggplot(datos_filtrados, aes(x = año, y = extracción_agua_dulce_en_millones_de_metros_cúbicos)) +
      geom_line(color = "chocolate4") +
      geom_point() +
      labs(x = "Año", y = "Mil millones de m³") +
      theme_minimal() +
      scale_y_continuous(
        breaks = pretty(datos_filtrados$extracción_agua_dulce_en_millones_de_metros_cúbicos, n = 4),
        labels = scales::label_number(big.mark = ".", decimal.mark = ",")
      )
    
    ggplotly(p) %>% 
      layout(
        title = list(
          text = "Extracción de agua dulce en mil millones de m³<br>por año en Chile",
          x = 0.5,  
          y = 0.97,
          xanchor = "center",   
          yanchor = "top",
          font = list(size = 14)
        )
      )
  })
  
  output$plot_productividad <- renderPlotly({
    #Parsear el rango seleccionado
    rangos <- strsplit(input$rango_global, " - ")[[1]]
    año_min <- as.numeric(rangos[1])
    año_max <- as.numeric(rangos[2])
    
    #Filtrar datos según rango
    datos_filtrados <- productividad_agua %>%
      filter(año >= año_min, año <= año_max)
    
    p <- ggplot(datos_filtrados, aes(x = año, y = PIB_por_m3_de_extracción_total_de_agua_dulce)) +
      geom_line(color = "darkolivegreen") +
      geom_point() +
      labs(x = "Año", y = "USD 2015 por m³ de agua dulce extraída") +
      theme_minimal()  +
      scale_y_continuous(
        breaks = pretty(datos_filtrados$PIB_por_m3_de_extracción_total_de_agua_dulce, n = 6),
        labels = scales::label_number(big.mark = ".", decimal.mark = ",")
      )
    
    ggplotly(p) %>% 
      layout(
        title = list(
          text = "Productividad del agua dulce en Chile por años<br> medido en USD 2015 por m³",
          x = 0.5,
          y = 0.97,
          xanchor = "center",   
          yanchor = "top",
          font = list(size = 14)
        )
      )
  })
  
  #ACCESO AL AGUA
  output$plot_acceso_seguro <- renderPlotly({
    #Parsear el rango seleccionado
    rangos <- strsplit(input$rango_global, " - ")[[1]]
    año_min <- as.numeric(rangos[1])
    año_max <- as.numeric(rangos[2])
    
    #Filtrar datos según rango
    datos_filtrados <- acceso_agua_urbana %>%
      filter(año >= año_min, año <= año_max)
    
    p <- ggplot(datos_filtrados, aes(x = año, y = agua_potable_gestionados_de_forma_segura)) +
      geom_line(color = "blue3") +
      geom_point() +
      labs(x = "Año", y = "Población urbana") +
      theme_minimal() +
      scale_y_continuous(
        breaks = pretty(datos_filtrados$agua_potable_gestionados_de_forma_segura, n = 5),
        labels = scales::label_number(big.mark = ".", decimal.mark = ",")
      )
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Población urbana que tiene agua potable gestionada <br> de forma segura por año en Chile",
          x = 0.5,
          y = 0.97,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 14)  #Ajusta tamaño
        )
      )
  })
  
  
  output$plot_servicios_basicos <- renderPlotly({
    #Parsear el rango seleccionado
    rangos <- strsplit(input$rango_global, " - ")[[1]]
    año_min <- as.numeric(rangos[1])
    año_max <- as.numeric(rangos[2])
    
    #Filtrar datos según rango
    datos_filtrados <- acceso_agua_urbana %>%
      filter(año >= año_min, año <= año_max)
    
    #Mapas para etiquetas y colores
    servicios_labels <- c(
      servicios_básicos_de_agua_potable = "Básicos",
      servicios_limitados_de_agua_potable = "Limitados",
      servicios_de_agua_potable_no_mejorados = "No mejorada",
      agua_superficial = "Agua superficial"
    )
    colores <- c(
      "Básicos" = "darkgreen",
      "Limitados" = "orange3",
      "No mejorada" = "deepskyblue3",
      "Agua superficial" = "purple"
    )
    
    #Solo columnas seleccionadas
    cols_seleccionadas <- intersect(input$servicios_seleccionados, names(servicios_labels))
    
    #Validar que haya al menos un servicio seleccionado
    validate(
      need(length(cols_seleccionadas) > 0, "Selecciona al menos un tipo de servicio")
    )
    
    #Preparar datos en formato largo para ggplot
    datos_long <- datos_filtrados %>%
      dplyr::select(año, dplyr::all_of(cols_seleccionadas)) %>%
      tidyr::pivot_longer(cols = -año, names_to = "servicio", values_to = "valor")
    
    #Cambiar nombres técnicos por etiquetas bonitas
    datos_long$servicio <- servicios_labels[datos_long$servicio]
    
    #Graficar líneas y puntos para cada servicio
    p <- ggplot(datos_long, aes(x = año, y = valor, color = servicio)) +
      geom_line(linewidth = 1.2) +
      geom_point() +
      scale_color_manual(values = colores) +
      scale_y_continuous(
        breaks = pretty(datos_long$valor, n = 6),
        labels = scales::label_number(big.mark = ".", decimal.mark = ",")
      ) +
      labs(x = "Año", y = "Población urbana", color = "Tipo de servicio") +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(
        title = list(
          text = "Población urbana según el tipo de servicio de <br> agua potable por año en Chile",
          x = 0.5,
          y = 0.97,
          xanchor = "center",
          yanchor = "top",
          font = list(size = 14)  #Ajusta tamaño
        ),
        legend = list(
          y = 0.85,         #Baja la leyenda verticalmente (1 es el tope superior)
          x = 1.05,         #Puedes ajustar horizontalmente si quieres sacarla del gráfico
          xanchor = "left",
          yanchor = "top"
        ))
  })
  
  #CUADROS TIPO KPI
  output$kpi_precipitacion <- renderValueBox({
    valor <- round(mean(precipitaciones_anuales$precipitaciones), 1)
    valueBox(
      paste0(valor, " mm"),
      "Precipitación anual promedio",
      icon = icon("cloud-rain"),
      color = "aqua"
    )
  })
  
  output$kpi_caudal <- renderValueBox({
    valor <- round(mean(caudales_mensuales$caudales_m3_por_segundo), 1)
    valueBox(
      paste0(valor, " m³/s"),
      "Caudal medio mensual",
      icon = icon("tint"),
      color = "blue"
    )
  })
  
  output$kpi_renovables <- renderValueBox({
    valor <- round(mean(recursos_renovables$recursos_internos_renovables_de_agua_dulce), 1)
    valueBox(
      paste0(valor, " hm³/año"),
      "Recursos renovables promedio",
      icon = icon("water"),
      color = "navy"
    )
  })
  
  output$kpi_extraccion <- renderValueBox({
    valor <- round(mean(extraccion_agua$extracción_agua_dulce_en_millones_de_metros_cúbicos), 0)
    valueBox(
      paste0(valor, " hm³"),
      "Promedio de extracción anual",
      icon = icon("industry"),
      color = "green"
    )
  })
  
  output$kpi_productividad <- renderValueBox({
    valor <- round(mean(productividad_agua$PIB_por_m3_de_extracción_total_de_agua_dulce), 2)
    valueBox(
      valor,
      "Productividad del agua",
      icon = icon("chart-line"),
      color = "red"
    )
  })
  
  output$kpi_acceso_seguro <- renderValueBox({
    valor <- round(mean(acceso_agua_urbana$agua_potable_gestionados_de_forma_segura), 0)
    valueBox(
      paste0(valor),
      "Población con agua segura",
      icon = icon("hand-holding-water"),
      color = "green"
    )
  })
  
  output$kpi_servicios_basicos <- renderValueBox({
    valor <- round(mean(acceso_agua_urbana$servicios_básicos_de_agua_potable), 0)
    valueBox(
      paste0(valor),
      "Población con servicios básicos",
      icon = icon("building"),
      color = "blue"
    )
  })
  
  #HACER UN REACTIVE CON LOS DATOS PARA EL ANÁLISIS DESCRIPTIVO
  datos_seleccionados <- reactive({
    switch(input$base_datos,
           "prec_mens" = precipitaciones_mensuales,
           "caudales" = caudales_mensuales,
           "recursos" = {
             recursos_renovables$año <- as.Date(paste0(recursos_renovables$año, "-01-01"))
             recursos_renovables
           },
           "extraccion" = {
             extraccion_agua$año <- as.Date(paste0(extraccion_agua$año, "-01-01"))
             extraccion_agua
           },
           "productividad" = {
             productividad_agua$año <- as.Date(paste0(productividad_agua$año, "-01-01"))
             productividad_agua
           },
           "prec_anual" = {
             precipitaciones_anuales$año <- as.Date(paste0(precipitaciones_anuales$año, "-01-01"))
             precipitaciones_anuales
           },
           "agua_urbana" = {
             acceso_agua_urbana$año <- as.Date(paste0(acceso_agua_urbana$año, "-01-01"))
             acceso_agua_urbana
           }
    )
  })
  
  #MAPA ITERACTIVO
  output$mapa <- renderLeaflet({
    
    #Validar que la variable seleccionada exista
    req(input$variable)
    
    #Crear paleta de color según la variable seleccionada
    pal <- colorNumeric(palette = "YlGnBu", domain = regiones_sf[[input$variable]])
    
    #Crear mapa Leaflet
    leaflet(data = regiones_sf) %>%
      addTiles() %>%                            #Capa base
      addPolygons(                              #Polígonos con las regiones
        fillColor = ~pal(regiones_sf[[input$variable]]),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        #Etiqueta al pasar el cursor
        label = ~paste0(Region, "<br>", input$variable, ": ", regiones_sf[[input$variable]]),
        #Resaltar regiones
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      ) %>%
      #Leyenda del mapa
      addLegend(pal = pal,
                values = regiones_sf[[input$variable]],
                title = names(variables_climaticas)[variables_climaticas == input$variable],
                opacity = 1)
  })
  
  datos_seleccionados <- reactive({
    switch(input$base_datos,
           "prec_mens" = data.frame(precipitaciones = precipitaciones_mensuales$precipitaciones_mensuales),
           "caudales" = data.frame(caudales = caudales_mensuales$caudales_m3_por_segundo),
           "recursos" = data.frame(recursos = recursos_renovables$recursos_internos_renovables_de_agua_dulce),
           "extraccion" = data.frame(extraccion = extraccion_agua$extracción_agua_dulce_en_millones_de_metros_cúbicos),
           "productividad" = data.frame(productividad = productividad_agua$PIB_por_m3_de_extracción_total_de_agua_dulce),
           "prec_anual" = data.frame(precipitaciones = precipitaciones_anuales$precipitaciones),
           "agua_urbana_segura" = data.frame(segura = acceso_agua_urbana$agua_potable_gestionados_de_forma_segura),
           "agua_urbana_no_mejorada" = data.frame(no_mejorada = acceso_agua_urbana$servicios_de_agua_potable_no_mejorados),
           "agua_urbana_basico" = data.frame(basico = acceso_agua_urbana$servicios_básicos_de_agua_potable))
  })
  
  
  #TABLA DESCRIPTIVA POR BASE DE DATOS
  output$tabla_variables_resumen <- DT::renderDataTable({
    df <- datos_seleccionados()
    
    #Columnas numéricas
    num_cols <- sapply(df, is.numeric)
    df_num <- df[, num_cols, drop = FALSE]
    
    #Calcular resumen estilo summary pero con menos columnas y orden claro
    resumen <- data.frame(
      Min = sapply(df_num, min, na.rm = TRUE),
      Q1 = sapply(df_num, quantile, probs = 0.25, na.rm = TRUE),
      Median = sapply(df_num, median, na.rm = TRUE),
      Mean = sapply(df_num, mean, na.rm = TRUE),
      Q3 = sapply(df_num, quantile, probs = 0.75, na.rm = TRUE),
      Max = sapply(df_num, max, na.rm = TRUE),
      SD = sapply(df_num, sd, na.rm = TRUE),
      Missing = sapply(df_num, function(x) sum(is.na(x))),
      Leves = sapply(df_num, function(x) {
        q1 <- quantile(x, 0.25, na.rm = TRUE)
        q3 <- quantile(x, 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        sum(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr), na.rm = TRUE)
      }),
      Extremos = sapply(df_num, function(x) {
        q1 <- quantile(x, 0.25, na.rm = TRUE)
        q3 <- quantile(x, 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        sum(x < (q1 - 3 * iqr) | x > (q3 + 3 * iqr), na.rm = TRUE)
      })
    )
    
    #Redondear valores numéricos a 2 decimales
    resumen[,-1] <- round(resumen[,-1], 2)
    
    DT::datatable(
      resumen,
      options = list(
        pageLength = 5,
        lengthChange = FALSE,
        searching = FALSE,
        scrollX = TRUE,
        dom = 't',          #Solo tabla, sin barra de búsqueda ni paginación visible
        columnDefs = list(list(className = 'dt-center', targets = "_all")) #Centrar todo
      ),
      rownames = FALSE,
      class = "stripe hover"
    )
  })
  
  output$grafico_variable <- renderUI({
    df <- datos_seleccionados()
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    if (length(num_cols) == 0) {
      return(h4("No hay variables numéricas para graficar."))
    }
    
    plots_ui <- lapply(num_cols, function(var) {
      tagList(
        plotlyOutput(paste0("grafico_", var)),
        tags$hr()  #Línea divisoria opcional entre gráficos
      )
    })
    
    
    do.call(tagList, plots_ui)
  })
  
  observe({
    df <- datos_seleccionados()
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    lapply(num_cols, function(var) {
      output[[paste0("grafico_", var)]] <- renderPlotly({
        df <- datos_seleccionados()
        
        df_clean <- df %>%
          dplyr::filter(!is.na(.data[[var]]), is.finite(.data[[var]]))
        
        validate(
          need(nrow(df_clean) > 0, "No hay datos válidos para esta variable.")
        )
        
        # Limpiar y formatear el nombre de la variable para título y ejes
        var_limpio <- var
        var_limpio <- sub("^.*\\.", "", var_limpio)          # Quita todo antes del punto
        var_limpio <- gsub("_", " ", var_limpio)             # Reemplaza guiones bajos por espacios
        var_limpio <- tolower(var_limpio)                    # Pone todo en minúscula
        
        histo <- ggplot(df_clean, aes(x = .data[[var]])) +
          geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.6) +
          geom_density(color = "darkred") +
          theme_minimal() +
          labs(x = var, y = "Densidad")
        
        box <- ggplot(df_clean, aes(x = "valor", y = .data[[var]])) +
          geom_boxplot(fill = "orange", alpha = 0.6, outlier.color = "red") +
          coord_flip() +
          theme_minimal() +
          labs(x = NULL, y = var) +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
        
        histo_p <- ggplotly(histo, tooltip = c("x", "y"))
        box_p <- ggplotly(box, tooltip = c("x"))
        
        sp <- subplot(
          histo_p,
          box_p,
          nrows = 2,
          heights = c(0.7, 0.3),
          shareX = FALSE
        )
        
        sp <- sp %>% layout(
          title = list(
            text = paste("Distribución de", var_limpio),
            x = 0.5,
            xanchor = "center",
            font = list(size = 16)
          ),
          xaxis = list(title = NULL),
          yaxis = list(title = "Densidad"),
          xaxis2 = list(title = var_limpio),
          yaxis2 = list(title = NULL),
          margin = list(t = 60, b = 50, l = 60, r = 20)
        )
        
        
        sp
      })
      
      
      
      
    })
  })
  
  
  
##prediccion de precipitaciones mensuales
  
  
  #Cargar y preparar los datos desde 2010
  datos_sarima <- reactive({
    datos <- readxl::read_excel(archivo_excel, sheet = "precipitaciones_mensuales")
    datos <- datos %>%
      mutate(date = as.Date(paste0(date, "-01"))) %>%
      filter(date >= as.Date("2010-01-01"))
    return(datos)
  })
  
  # --- Crear serie temporal y ajustar modelo SARIMA completo
  modelo_sarima <- reactive({
    serie <- ts(datos_sarima()$value,
                start = c(lubridate::year(min(datos_sarima()$date)), 
                          lubridate::month(min(datos_sarima()$date))),
                frequency = 12)
    forecast::Arima(serie, order = c(1,0,1), seasonal = c(1,0,1))
  })
  
  # --- Realizar pronóstico para 36 meses (2025–2026)
  pronostico_sarima <- reactive({
    forecast::forecast(modelo_sarima(), h = 36)
  })
  
  tabla_predicciones <- reactive({
    fechas <- seq(as.Date("2025-01-01"), by = "month", length.out = 36)
    años <- lubridate::year(fechas)
    
    df <- data.frame(
      Fecha = fechas,
      año = años,
      Mes = lubridate::month(fechas, label = TRUE, abbr = TRUE),
      Precipitacion_Pronosticada = as.numeric(pronostico_sarima()$mean),
      LI_95 = pronostico_sarima()$lower[,2],
      LS_95 = pronostico_sarima()$upper[,2]
    ) %>%
      filter(año %in% c(2025, 2026))  # ← muestra solo 2025 y 2026
    
    return(df)
  })
  

    
  # --- Gráfico con predicción para los 3 años
  output$grafico_precipitaciones_sarima <- renderPlot({
    datos <- datos_sarima()
    df_pred <- tabla_predicciones()
    
    ggplot() +
      geom_line(data = datos, aes(x = date, y = value), color = "steelblue", size = 1) +
      geom_line(data = df_pred, aes(x = Fecha, y = Precipitacion_Pronosticada), color = "darkred", size = 1) +
      geom_ribbon(data = df_pred, aes(x = Fecha, ymin = LI_95, ymax = LS_95), fill = "red", alpha = 0.2) +
      scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y",
        expand = c(0.01, 0.01)
      ) +
      labs(
        title = "Serie histórica (2010-2024) y predicción mensual SARIMA (2025–2026)",
        x = "Fecha", y = "Precipitación (mm)"
      ) +
      theme_minimal()
  })
  
  
  
  output$tabla_predicciones_sarima <- DT::renderDataTable({
    df <- tabla_predicciones() %>%
      filter(año == input$anio_tabla) %>%
      dplyr::select(Mes, Precipitacion_Pronosticada, LI_95, LS_95)
    
    DT::datatable(
      df,
      options = list(
        pageLength = 12,
        lengthChange = FALSE,
        searching = FALSE,
        ordering = FALSE
      ),
      rownames = FALSE,
      class = "compact stripe hover",
      colnames = c("Mes", 
                   "Pronóstico (mm)", 
                   "Límite Inferior 95%", 
                   "Límite Superior 95%")
    )
  })
  
  
  
  
  
  ##################################################################################
  
  #REGRESIÓN MULTIPLE
  #Diccionario de nombres legibles
  nombres_bonitos <- c(
    "extracción_agua_dulce_en_millones_de_metros_cúbicos" = "Extracción de agua dulce (m³)",
    "agua_potable_gestionados_de_forma_segura" = "Agua potable gestionada de forma segura",
    "servicios_básicos_de_agua_potable" = "Servicios básicos de agua potable",
    "log_PIB_m3" = "Productividad del agua"
  )
  
  #Crear variable transformada (Box-Cox con lambda = -2)
  datos_modelo_lineal$log_PIB_m3 <- 1 / (datos_modelo_lineal$PIB_por_m3_de_extracción_total_de_agua_dulce^2.8)
  
  #Modelo de regresión múltiple con transformación
  modelo_log <- lm(log_PIB_m3 ~ 
                     extracción_agua_dulce_en_millones_de_metros_cúbicos +
                     agua_potable_gestionados_de_forma_segura +
                     servicios_básicos_de_agua_potable,
                   data = datos_modelo_lineal)
  
  output$grafico_parcial <- renderPlotly({
    req(input$variable_parcial)
    
    var <- input$variable_parcial
    
    #Generar secuencia de 100 valores para la variable seleccionada
    rango <- range(datos_modelo_lineal[[var]], na.rm = TRUE)
    x_seq <- seq(from = rango[1], to = rango[2], length.out = 100)
    
    #Crear data.frame de predicción (100 filas), con la variable seleccionada variando
    df_pred <- as.data.frame(matrix(nrow = 100, ncol = 0))
    for (v in names(modelo_log$model)[-1]) {
      if (v == var) {
        df_pred[[v]] <- x_seq
      } else {
        df_pred[[v]] <- rep(mean(datos_modelo_lineal[[v]], na.rm = TRUE), 100)
      }
    }
    
    #Predicciones con intervalos de confianza
    pred <- predict(modelo_log, newdata = df_pred, interval = "confidence")
    
    df_plot <- data.frame(
      x = df_pred[[var]],
      y = pred[, "fit"],
      ymin = pred[, "lwr"],
      ymax = pred[, "upr"]
    )
    
    # Obtener nombre bonito
    nombre_bonito_var <- nombres_bonitos[[var]]
    nombre_bonito_y <- nombres_bonitos[["log_PIB_m3"]]
    
    # Gráfico con puntos reales + curva + IC y eje X con valores completos
    p <- ggplot(df_plot, aes(x = x, y = y)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "skyblue", alpha = 0.2) +
      geom_point(data = datos_modelo_lineal,
                 aes_string(x = var, y = "log_PIB_m3"),
                 inherit.aes = FALSE,
                 shape = 21, fill = "red", color = "black", size = 2, alpha = 0.6) +
      labs(
        title = paste("Efecto de", nombre_bonito_var, "sobre la productividad del agua"),
        x = nombre_bonito_var,
        y = nombre_bonito_y
      ) +
      scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      theme_minimal()
    
    
    
    ggplotly(p)
  })
  
  #Renderizar el gráfico interactivo según variable seleccionada
  output$grafico_efecto_modelo <- renderPlotly({
    #Estandarizar variables predictoras y respuesta
    datos_est <- datos_modelo_lineal %>%
      dplyr::select(log_PIB_m3,
                    extracción_agua_dulce_en_millones_de_metros_cúbicos,
                    agua_potable_gestionados_de_forma_segura,
                    servicios_básicos_de_agua_potable) %>%
      as.data.frame() %>%
      mutate_all(scale)
    
    #Ajustar modelo con variables estandarizadas
    modelo_std <- lm(log_PIB_m3 ~ ., data = datos_est)
    
    #Extraer coeficientes (sin intercepto)
    coef_std <- coef(modelo_std)[-1]
    
    #Crear data.frame para graficar
    df_coef <- data.frame(
      Variable = names(coef_std),
      Coeficiente = as.numeric(coef_std)
    )
    
    #Gráfico con ggplotly
    library(ggplot2)
    library(plotly)
    
    # Reemplazar nombres técnicos por nombres bonitos
    df_coef$Variable_bonita <- nombres_bonitos[df_coef$Variable]
    
    # Gráfico de barras
    p <- ggplot(df_coef, aes(x = reorder(Variable_bonita, Coeficiente), y = Coeficiente, fill = Coeficiente > 0)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato")) +
      labs(title = "Importancia de variables en la regresión múltiple",
           x = "", y = "Coeficiente estandarizado") +
      theme_minimal()
    
    
    ggplotly(p)
    
    
  })
  
  #GENERAR INFORME EN PDF
  output$generar_informe <- downloadHandler(
    filename = function() {
      paste("informe crisis hidrica en Chile", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      
      #Generar el .Rmd dinámicamente
      rmd_content <- '
---
title: "Informe ejecutivo sobre predicción y uso eficiente del recurso hídrico en Chile"
author: "Rossemari Gajardo, Josefa Hasbun"
date: "2025-06-23"
output:
  pdf_document:
    toc: true
    number_sections: true
    toc_depth: 2
params:
  datos_precipitaciones_mensuales: NA
  datos_precipitaciones_anuales: NA
  datos_caudales: NA
  datos_renovables: NA
  datos_extraccion: NA
  datos_productividad: NA
  datos_acceso: NA
  datos_servicios: NULL
  servicios_seleccionados: NULL
  variable: NA 
  regiones_sf: NA
  datos_seleccionados: NA
  variable_seleccionada: NA
  año_min: NA         
  año_max: NA 
---

```{r setup, include=FALSE}
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(sf)
library(rlang)
library(knitr)
library(kableExtra)
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```

# Resumen ejecutivo

En el contexto de la creciente crisis hídrica que enfrenta Chile, caracterizada 
por una disminución progresiva de las precipitaciones y una alta presión sobre los 
recursos naturales, se vuelve fundamental comprender qué factores inciden realmente 
en la eficiencia del uso del agua dulce. Este informe analiza la productividad 
económica del recurso hídrico, entendida como el valor económico generado por cada 
metro cúbico utilizado, con el objetivo de orientar estrategias de gestión más 
eficaces y sostenibles.

Para abordar esta problemática, se desarrolló un análisis avanzado basado en dos 
componentes clave: una predicción de precipitaciones mensuales mediante un modelo 
SARIMA, y un modelo de regresión lineal múltiple para explicar la productividad 
del agua en función de variables hídricas y de acceso urbano. La predicción 
climática muestra un patrón estacional estable para los próximos dos años 
(2025–2026), con niveles máximos de precipitación entre mayo y agosto, y sin señales 
de una tendencia significativa al alza o a la baja. Esta estabilidad proyectada 
permite planificar con mayor certeza en el corto plazo.

Por otro lado, el modelo de regresión reveló que la eficiencia en el uso del agua 
está estrechamente vinculada con variables de gestión más que con la disponibilidad 
natural. Específicamente, se observó que una mayor cobertura de servicios de agua 
gestionados de forma segura se asocia con un incremento en la productividad económica 
del recurso. En cambio, niveles elevados de extracción de agua dulce y acceso limitado 
a servicios básicos se relacionan con una menor eficiencia. Estos hallazgos indican 
que la forma en que se distribuye y gestiona el recurso es más determinante que su 
cantidad climática.

En consecuencia, el estudio recomienda enfocar los esfuerzos en mejorar la calidad 
del acceso al agua potable, especialmente mediante infraestructura segura y eficiente; 
reducir gradualmente la sobreextracción en sectores críticos; e implementar monitoreo 
inteligente y políticas basadas en indicadores de eficiencia hídrica. Estos 
lineamientos permitirían no solo enfrentar con mayor resiliencia los escenarios 
de escasez, sino también optimizar el valor económico del recurso hídrico, garantizando 
su uso sostenible en el mediano y largo plazo.

# Contexto empresarial

La escasez hídrica se ha consolidado como una de las principales amenazas para 
el desarrollo económico sostenible, afectando especialmente a sectores 
estratégicos como la agricultura, la energía y la industria. En este escenario, 
tanto organismos públicos como privados se enfrentan al desafío de mantener la 
productividad económica sin comprometer la sostenibilidad del recurso hídrico. 
La presión sobre la infraestructura de agua urbana, junto con la variabilidad en 
la disponibilidad de agua dulce y los niveles de extracción, exige una evaluación 
profunda del impacto real de estos factores sobre el rendimiento económico 
del agua. Comprender esta relación es clave para tomar decisiones basadas en 
evidencia que promuevan un uso más eficiente y sustentable del recurso.

## Objetivo general

Evaluar el impacto de la disponibilidad, acceso y uso del recurso hídrico sobre 
su productividad económica en Chile, mediante análisis estadísticos inferenciales y 
modelos predictivos (modelos ARIMA para la proyección de precipitaciones y regresión 
lineal múltiple para explicar la productividad del agua), con el fin de proponer 
soluciones estratégicas que permitan enfrentar la crisis hídrica de manera eficiente, 
sustentable y basada en evidencia.

## Objetivos específicos

- Proyectar las precipitaciones mensuales para los próximos dos años (2025 y 2026), 
utilizando modelo SARIMA, para anticipar escenarios futuros de disponibilidad de agua dulce.

- Analizar, a través de regresión lineal múltiple, la relación entre la productividad económica 
del agua y variables explicativas como niveles de extracción, precipitaciones, recursos 
renovables y acceso urbano al agua potable.

- Generar recomendaciones concretas de política pública o estrategias de gestión hídrica 
basadas en los resultados obtenidos.

## Objetivos estratégicos del negocio

- Maximizar el valor económico generado por cada metro cúbico de agua extraída, 
expresado en términos de PIB.

- Aumentar la eficiencia en el uso del recurso hídrico en contextos de estrés y 
escasez, tanto en zonas rurales como urbanas.

- Diseñar políticas y decisiones operacionales basadas en evidencia empírica, que 
permitan anticiparse a escenarios críticos en la disponibilidad y productividad del agua.

## Métricas clave

- PIB generado por metro cúbico de agua utilizada (PIB/m³).

- Precipitaciones mensuales (mm), con proyecciones para 2025 y 2026 basadas en ARIMA.

- Volumen de extracción de agua anual.

- Disponibilidad de recursos renovables de agua.

- Porcentaje de población urbana con acceso a agua potable, categorizado en: no mejorada, básica y gestionada de forma segura.

Se eligió trabajar con series históricas entre los años 2001 y 2021, ya que este 
período concentra la información más continua, completa y homologada, lo que 
permite construir modelos de predicción confiables y realizar análisis 
estadísticos robustos para sustentar recomendaciones de gestión y planificación.

# Datos y análisis exploratorio

El análisis se construyó a partir de una base de datos consolidada que reúne indicadores 
climáticos, hídricos y socioeconómicos de Chile para el período 2001–2021. Esta 
base incluye información sobre precipitaciones (mensuales y anuales), extracción 
de agua dulce (expresada en millones de metros cúbicos), productividad del agua 
(medida como PIB generado por metro cúbico extraído), caudales, recursos renovables 
internos, y acceso urbano al agua potable desagregado por tipo de servicio. Además, 
se integraron datos regionales vinculados a un archivo geográfico .geojson, permitiendo 
visualizaciones espaciales.

Los datos, obtenidos de fuentes oficiales y de acceso público como Global Water
Monitor, Our World in Data y el Ministerio del Medio Ambiente, fueron organizados 
y depurados a través de diversas transformaciones: se seleccionó el periodo 2001–2021 
por su completitud y consistencia; se estandarizaron nombres de columnas para 
facilitar su manipulación en R; se ajustaron los formatos de fecha para análisis 
cronológicos; se eliminaron columnas irrelevantes o duplicadas; y se vincularon 
variables regionales con información geográfica para análisis territoriales.

## Análisis univariado

El análisis univariado permitió observar el comportamiento individual de los 
principales indicadores. La extracción de agua dulce mostró estabilidad en el 
tiempo, con una mediana cercana al valor máximo, reflejando una presión sostenida 
sobre los recursos hídricos, independientemente de las variaciones climáticas. 
Esta situación podría derivar en sobreexplotación si no es acompañada por 
mecanismos de renovación.

Los recursos renovables internos también se han mantenido relativamente constantes, 
pero su distribución espacial y temporal puede no coincidir con las necesidades 
de consumo humano o productivo. La productividad económica del agua, por su parte, 
evidencia una tendencia creciente, lo que sugiere avances en eficiencia. Sin embargo, 
cabe preguntarse si esta mejora responde a cambios estructurales, redistribución 
sectorial o a innovaciones tecnológicas.

En relación con el acceso a agua potable, se observó que si bien la mayoría de la 
población urbana cuenta con servicios gestionados de forma segura, persisten 
brechas importantes, especialmente en el acceso básico o no mejorado, que afectan 
a sectores más vulnerables. Finalmente, los registros de caudales y precipitaciones 
muestran variabilidad esperada, sin caídas abruptas, pero tampoco aumentos que 
compensen el incremento en la demanda, lo que sugiere una presión creciente sobre 
los ecosistemas hídricos.

## Análisis bivariado

En el análisis entre pares de variables se evidenciaron relaciones relevantes. La 
comparación entre precipitaciones y extracción de agua no mostró una relación 
directa, indicando que la extracción se mantiene constante incluso en años de 
menor disponibilidad natural, lo que plantea riesgos en términos de sostenibilidad. 
Algo similar ocurre entre precipitaciones y productividad del agua, donde la débil 
relación sugiere que la eficiencia no depende directamente del clima, sino de factores 
de gestión o tecnológicos.

Por otro lado, se observó una correlación positiva entre extracción de agua dulce 
y productividad, lo que podría interpretarse como una presión intensiva sobre el 
recurso para sostener la actividad económica. Esta relación puede ser efectiva 
desde una lógica productiva, pero riesgosa si no se regula el impacto ambiental.

## Análisis multivariado

La matriz de correlación permitió examinar de forma conjunta las relaciones entre 
las variables hídricas y económicas. Los resultados indicaron que la productividad 
del agua guarda una correlación negativa significativa con los recursos hídricos 
renovables (-0.976), lo que sugiere que una mayor disponibilidad natural no se 
traduce en un uso más eficiente del recurso. En contraste, la extracción de agua 
dulce muestra una correlación positiva (0.660) con la productividad, lo que refuerza 
la hipótesis de una estrategia basada en el uso intensivo más que en la sostenibilidad.

La precipitación anual presentó una correlación negativa más leve con la productividad 
(-0.344), lo que confirma que no es el factor determinante de eficiencia. Además, 
los indicadores vinculados al acceso al agua también mostraron asociaciones con 
la productividad y con los recursos renovables, revelando que en años de menor 
disponibilidad hídrica se intensifica la producción de agua potable, lo que podría 
aumentar la presión sobre los sistemas naturales.

En conjunto, el análisis exploratorio revela que Chile enfrenta una presión constante 
sobre sus recursos hídricos, con señales de eficiencia económica crecientes pero 
desalineadas respecto a la disponibilidad natural del recurso. Las decisiones de 
extracción y uso del agua no parecen responder a condiciones climáticas o de 
renovación, sino a imperativos económicos y de acceso. Estos hallazgos refuerzan 
la necesidad de avanzar hacia una gestión hídrica basada en evidencia, que incorpore 
criterios de eficiencia, sostenibilidad y equidad territorial, permitiendo anticiparse 
a escenarios críticos mediante una planificación inteligente del recurso.

# Selección y fundamentación de la técnica

Para abordar el problema desde una perspectiva cuantitativa sólida, se aplicaron 
dos enfoques analíticos complementarios:

- Modelos de predicción para series temporales (precipitaciones mensuales).

- Modelos de regresión lineal múltiple para explicar la productividad económica 
del agua en función de variables hídricas.

Antes de llegar a estas técnicas, se evaluaron otras alternativas. Para las 
precipitaciones se intentó utilizar modelos más simples como regresión lineal o 
modelos de suavizamiento exponencial, pero no cumplían los supuestos requeridos 
ni capturaban adecuadamente la estacionalidad de la serie. Finalmente, el modelo 
SARIMA fue el único que logró cumplir los supuestos técnicos y ajustarse 
adecuadamente al comportamiento cíclico del fenómeno. Además, se comprobó en la 
literatura que esta técnica es ampliamente utilizada en el estudio de variables 
climáticas como precipitación y temperatura.

Por otra parte, para analizar la productividad del agua se intentó aplicar 
inicialmente pruebas de hipótesis (como t-test y Mann-Whitney), utilizando cortes 
por mediana. Sin embargo, este enfoque es meramente exploratorio y no permite 
extraer conclusiones robustas ni generar modelos con capacidad explicativa o 
predictiva. En cambio, la regresión lineal múltiple permite cuantificar efectos 
parciales y evaluar la importancia relativa de cada variable predictora, lo que 
se ajusta a los requerimientos del dashboard.

## Modelo SARIMA para predicción de precipitaciones

Se utilizó un modelo SARIMA (Seasonal AutoRegressive Integrated Moving Average), 
recomendado especialmente para series temporales que presentan estacionalidad. Esta 
técnica permite incorporar tanto componentes autorregresivos, diferencias integradas 
y medias móviles, como también sus equivalentes estacionales, lo que la hace 
particularmente robusta para modelar fenómenos climáticos.

Justificación del uso del modelo SARIMA:

- Las precipitaciones mensuales presentan patrones cíclicos y estacionales que 
pueden ser capturados por los componentes estacionales del modelo.

- Permite realizar proyecciones a corto y mediano plazo con una estructura 
estadística sólida.

- Está validado en la literatura científica para el análisis de variables 
meteorológicas.

- Fue el único modelo evaluado que cumplió adecuadamente los supuestos estadísticos 
requeridos.

Verificación de supuestos del modelo SARIMA:

| Supuesto                                | Estadístico utilizado            | Valor-p | Conclusión      |
| --------------------------------------- | -------------------------------- | ------- | --------------- |
| Estacionariedad de la serie             | Prueba de Dickey-Fuller          |  <0,01  |Estacionaria (se rechaza H_0)|
| Ruido blanco en residuos                | Test de Ljung-Box                | 0,4895  |No hay autocorelación (se acepta H_0)|
| Ausencia de autocorrelación en residuos | Gráfico ACF/PACF                 |    —    | No se observa autocorrelación visible|
| Normalidad de los errores               | Prueba de Shapiro-Wilk / QQ plot |5.325e-06|No normales (se rechaza H_0)|

Tres de cuatro supuestos se cumplen, el único que no se cumple es de la normalidad
de los errores (p < 0,05 en Shapiro-Wilk), la cual no invalida el modelo SARIMA 
pero puede afectar las bandas de predicción.
La serie fue transformada para alcanzar estacionariedad, los residuos muestran 
independencia y no presentan autocorrelación significativa. Por lo tanto, el modelo 
se considera adecuado y confiable para la predicción de precipitaciones mensuales 
en los años 2025y 2026.

## Regresión lineal múltiple para modelar la productividad del agua

Con el objetivo de identificar los factores más influyentes en la productividad 
económica del agua se construyó un modelo de regresión lineal múltiple. En una 
primera etapa se consideraron múltiples variables predictoras relacionadas con el 
clima, la disponibilidad del recurso, el uso efectivo y el acceso urbano al agua. 
Estas incluyeron las precipitaciones anuales, los recursos hídricos renovables, el 
nivel de extracción de agua dulce, y los distintos tipos de acceso urbano al agua 
potable. Sin embargo, durante el proceso de ajuste se detectaron incumplimientos 
en los supuestos clásicos del modelo de regresión, principalmente asociados a la 
normalidad de los residuos, la homocedasticidad y la multicolinealidad entre 
predictores. Por esta razón, se procedió a reducir el número de variables, seleccionando 
únicamente aquellas que presentaban una relación significativa y estable con la 
productividad, y que no generaban distorsiones en los supuestos. Para mejorar el 
cumplimiento de los supuestos, se aplicó una transformación de Box-Cox a la variable 
respuesta, seleccionando un valor de lambda = –2.8, lo cual estabilizó la varianza 
y aproximó la distribución de los residuos a la normalidad.

Las variables predictoras seleccionadas son:

- Extracción de agua dulce (mil millones de m³)

- Acceso urbano a agua potable básica.

- Acceso urbano a agua potable gestionada de forma segura.

Además, se estandarizaron las variables explicativas para facilitar la interpretación 
de los coeficientes y visualizar su importancia relativa en el dashboard mediante 
gráficos de efectos parciales y relevancia estandarizada.

Justificación del uso de regresión múltiple:

- Permite explicar cuantitativamente cómo varía la productividad del agua según 
los distintos factores.

- Los resultados entregan herramientas concretas para la toma de decisiones estratégicas.

- El modelo cumplió con los supuestos fundamentales, lo que valida sus inferencias.

Verificación de supuestos del modelo de regresión lineal múltiple

| Supuesto                              | Estadístico utilizado            | Valor-p | Conclusión      |
| ------------------------------------- | -------------------------------- | ------- | --------------- |
| Linealidad global y parcial           | Gráficos de efecto parcial       | —       |Grafico no muestra patrones sistemáticos, por lo tanto si cumple|
| Independencia de los errores          | Test de Durbin-Watson            | 0.06896 |Puede haber leve autocorrelación positiva.|
| Homocedasticidad (varianza constante) | Test de Breusch-Pagan            | 0.6074  |No hay evidencia de heterocedasticidad|
| Normalidad de los errores             | Prueba de Shapiro-Wilk / QQ plot |  0.4004 |No se rechaza normalidad de los residuos. QQ plot se alinea bien.|
| Ausencia de multicolinealidad         | VIF (Variance Inflation Factor)  | —       |Grafico no muestra patrones sistemáticos, por lo tanto si cumple|

Los principales supuestos de la regresión múltiple fueron verificados y se 
encuentran cumplidos. Los residuos del modelo no presentan autocorrelación, 
mantienen varianza constante y se distribuyen de forma aproximadamente normal. Además, 
no se observan problemas de colinealidad entre los predictores. En consecuencia, 
el modelo es estadísticamente válido y adecuado para interpretar los efectos de 
las distintas variables hídricas sobre la productividad del agua.

# Implementación

El desarrollo del presente análisis se realizó íntegramente en el lenguaje de 
programación R, dada su robustez para el tratamiento de datos, modelamiento 
estadístico y visualización gráfica. Se trabajó en el entorno RStudio, una herramienta 
gratuita ampliamente utilizada tanto en entornos académicos como profesionales. 
Para la creación del producto interactivo final se utilizó el paquete Shiny, que 
permite construir dashboards dinámicos directamente desde R. Este enfoque fue 
seleccionado no solo por su flexibilidad, sino también por su capacidad de integrar, 
en un mismo entorno, procesamiento estadístico, modelamiento predictivo y 
visualización avanzada.

Para facilitar la futura replicación del análisis, la instalación del entorno 
requiere solo tres pasos: primero, descargar e instalar R desde el sitio oficial 
(CRAN); luego, instalar RStudio desde la plataforma Posit; y finalmente, instalar 
los paquetes necesarios, incluidos shiny, plotly, forecast, tseries, entre otros,
directamente desde R mediante comandos simples. De esta manera, cualquier usuario 
con conocimientos básicos puede ejecutar localmente la aplicación y explorar el 
análisis realizado.

El trabajo comenzó con la recolección y preparación de series históricas entre los 
años 2001 y 2021, integrando distintas fuentes de información relacionadas con el 
ciclo hídrico en Chile. Se incluyeron variables como precipitaciones anuales y mensuales, 
volumen de extracción de agua dulce, recursos hídricos renovables y población urbana 
según el tipo de acceso al agua, entre otras. Estas variables fueron normalizadas, 
analizadas y contrastadas con el PIB generado por el uso del agua, a fin de estimar 
su productividad económica como indicador principal.

En términos de modelamiento estadístico, se construyó un modelo SARIMA para predecir 
las precipitaciones mensuales en los años 2025, 2026. Este modelo fue seleccionado 
tras evaluar varias alternativas que no cumplían los supuestos requeridos, y fue 
validado a través de métricas de error y análisis de residuos. Su elección se 
sustenta también en evidencia empírica de su uso exitoso en estudios climáticos. 
Por otro lado, para analizar la productividad del agua, se aplicó una regresión 
lineal múltiple que permite evaluar el efecto parcial de variables como las precipitaciones, 
la extracción, los recursos renovables y el acceso al agua. Se estandarizaron los 
coeficientes para facilitar la interpretación visual, y se verificaron todos los 
supuestos del modelo, lo que asegura su validez técnica.

Ambos análisis fueron integrados en un dashboard interactivo desarrollado en Shiny, 
que facilita la exploración visual de los resultados y su interpretación. El dashboard 
está organizado mediante un panel lateral que guía al usuario a través de siete 
secciones: Inicio, Análisis descriptivo, Disponibilidad del agua, Uso del agua, 
Acceso al agua, Análisis avanzado y Recomendaciones. En las secciones de disponibilidad, 
uso y acceso al agua, el usuario puede seleccionar distintos rangos de años (2001–2005, 
2006–2010, 2011–2015, 2016–2021 o el período completo) para filtrar los gráficos y 
observar las variables mencionadas anteriormente según el periodo. En la sección 
de análisis avanzado se presentan tanto las proyecciones del modelo SARIMA como 
los resultados de la regresión múltiple, incluyendo gráficos de efecto parcial para 
cada variable seleccionable, y un gráfico resumen de importancia relativa estandarizada.

Como complemento, se incluyó un botón que permite descargar automáticamente un 
informe en formato PDF, el cual resume los principales hallazgos y visualizaciones 
del análisis, permitiendo su difusión en contextos institucionales, académicos o 
de toma de decisiones. Todo este conjunto de herramientas y visualizaciones busca 
facilitar una comprensión accesible pero rigurosa del fenómeno hídrico en Chile, 
apoyando así la construcción de estrategias de gestión basadas en evidencia.

Los enlaces de descarga de los programas utilizados, así como los códigos fuente 
desarrollados para la creación del dashboard, se encuentran disponibles en el 
anexo. Entre ellos se incluyen los sitios oficiales de instalación de R y RStudio, 
además de los archivos necesarios para ejecutar la aplicación.

# Resultados e interpretación

El análisis avanzado contempló dos componentes principales: la proyección de las 
precipitaciones mensuales mediante un modelo estadístico SARIMA, y la evaluación 
de la productividad económica del agua a través de un modelo de regresión lineal 
múltiple, con variables explicativas transformadas y estandarizadas. Ambos modelos 
presentaron un excelente ajuste y cumplieron con los supuestos necesarios para 
que sus conclusiones sean consideradas válidas y confiables.

En lo que respecta a la predicción de precipitaciones, el modelo SARIMA permitió 
generar estimaciones mensuales para los años 2025 y 2026. Los resultados 
muestran un patrón estacional claramente definido, con mayores niveles de precipitación 
entre los meses de mayo y agosto, y valores máximos recurrentes en junio, alcanzando 
aproximadamente los 123 mm. En contraste, los meses de enero, febrero y diciembre 
presentan los niveles más bajos, con registros estables en torno a los 61 a 65 mm. 
A lo largo de los dos años proyectados, no se evidencian cambios estructurales 
significativos ni una tendencia marcada al alza o a la baja, lo que sugiere una 
continuidad del comportamiento climático observado en las últimas décadas. En 
consecuencia, este escenario permite planificar desde una base relativamente estable, 
sin señales inmediatas de agravamiento del régimen de precipitaciones.

Por otro lado, se ajustó un modelo de regresión lineal múltiple para explicar la 
productividad económica del agua en función de tres variables predictoras: cobertura 
de acceso urbano a agua gestionada de forma segura, cobertura de servicios básicos 
de agua potable y volumen de extracción de agua dulce. El modelo transformado mostró 
que las tres variables son estadísticamente significativas con un nivel de confianza 
del 99.9%. La variable agua gestionada de forma segura presentó un coeficiente negativo, 
lo que indica que su incremento está asociado con una menor eficiencia en el uso 
del agua. Por el contrario, tanto la extracción de agua dulce como el acceso a 
servicios básicos de agua se relacionan con una mayor productividad, posiblemente 
porque reflejan una utilización más directa y flexible del recurso, aunque puedan 
implicar riesgos desde una perspectiva sanitaria o ambiental. El R² ajustado 
alcanzado fue de 0.9899, lo que refleja una capacidad explicativa sobresaliente.

Esta relación se reafirma en el modelo estandarizado, donde la variable más 
influyente es nuevamente la cobertura de agua gestionada de forma segura (impacto 
negativo), seguida por los servicios básicos de agua y la extracción (ambas con 
impactos positivos). Estos hallazgos, visualizados a través de gráficos de efecto 
parcial y de importancia relativa, permiten identificar con claridad qué dimensiones 
tienen mayor impacto sobre la eficiencia hídrica, destacando que no basta con tener 
agua disponible: la forma en que se accede y se gestiona resulta determinante.

En conjunto, los resultados obtenidos permiten concluir que la productividad 
económica del agua está más estrechamente relacionada con la forma en que se 
gestiona y distribuye el recurso, que con la cantidad de agua disponible por 
condiciones climáticas. Mientras que las precipitaciones proyectadas mantienen un 
comportamiento estacional estable sin variaciones estructurales relevantes, el 
modelo de regresión evidencia que factores como el acceso seguro al agua pueden 
limitar la productividad, mientras que un acceso básico ampliado y una extracción 
controlada inciden positivamente en la eficiencia del uso hídrico. Esto sugiere 
que las políticas más efectivas no deben centrarse exclusivamente en aumentar la 
disponibilidad, sino en mejorar la eficiencia operativa, regular la extracción y 
diseñar estrategias de acceso que maximicen el valor generado por cada metro 
cúbico de agua utilizado.

# Recomendaciones gerenciales

A partir de los resultados obtenidos en el análisis predictivo y en el modelo de 
regresión lineal múltiple, se concluye que la productividad económica del agua en 
Chile no está condicionada por la variación en las precipitaciones, sino por la 
forma en que se gestiona y se accede al recurso. En particular, se identificó que 
la mayor eficiencia hídrica se asocia con niveles controlados de extracción y con 
un mayor acceso básico al agua potable, mientras que una mayor cobertura de agua 
gestionada de forma segura se relaciona con una menor productividad, posiblemente 
por restricciones operativas, costos asociados o mayor rigidez en el uso.

En este contexto, se proponen las siguientes recomendaciones para enfrentar la 
crisis hídrica desde una perspectiva gerencial estratégica, considerando el escenario 
actual del país y sus proyecciones.

## Corto plazo (0-1 año)

- Incentivar la productividad hídrica mediante estímulos económicos diferenciados 
para sectores que logren generar mayor valor por metro cúbico utilizado (ej. 
agricultura, industria, energía). Esto ayuda a reducir la presión sobre el recurso 
sin frenar el desarrollo.

- Acelerar la implementación de sistemas de monitoreo en tiempo real (IoT y sensores) 
en las principales zonas de extracción, especialmente en regiones con estrés hídrico. 
Esta tecnología permite detectar sobreusos e informar modelos predictivos como los 
aplicados en este estudio.

- Capacitar a gobiernos regionales y municipios en el uso de herramientas de 
análisis de eficiencia hídrica, de modo que puedan incorporar criterios técnicos 
en decisiones de planificación, fiscalización y asignación.

## Mediano plazo (1-3 años)

- Reformular los marcos de asignación del agua, incorporando criterios de eficiencia 
económica y sostenibilidad, priorizando usos que generen mayor retorno social y 
económico por unidad de agua.

- Establecer normas de reporte obligatorio de eficiencia hídrica en sectores de 
alto consumo, como parte de una fiscalización más activa y basada en evidencia.

- Fomentar tecnologías de uso eficiente del agua, como riego tecnificado, sensores 
de humedad y reutilización de aguas tratadas.

- Desarrollar plataformas digitales públicas (como dashboards interactivos) para 
que ciudadanía, autoridades y empresas puedan monitorear en línea indicadores de 
productividad, acceso y extracción de agua, promoviendo transparencia y 
corresponsabilidad.

## Indicadores de seguimiento sugeridos

- PIB generado por metro cúbico extraído, desagregado por región y sector económico.

- Porcentaje de cobertura de servicios básicos de agua potable, especialmente en 
comunas con menor productividad hídrica.

- Volumen anual de extracción por sector productivo, con metas de reducción 
progresiva en zonas críticas.

- Porcentaje de adopción de tecnologías hídricas eficientes en sectores de mayor 
demanda.

- Porcentaje de cobertura nacional con monitoreo en tiempo real del uso del agua.

Estas recomendaciones apuntan a transformar la gestión hídrica en Chile desde un 
enfoque tradicional de administración hacia un modelo activo, basado en evidencia, 
eficiencia y equidad, donde la forma de acceso y la calidad de gestión del recurso 
son determinantes para enfrentar con éxito la crisis hídrica.

# Consideraciones éticas y limitaciones

El presente análisis se realizó exclusivamente a partir de fuentes oficiales y 
bases de datos públicas, sin involucrar datos personales ni sensibles, asegurando 
transparencia metodológica y cumplimiento ético.

Técnicamente, la calidad de los resultados depende de la integridad y cobertura 
de los registros utilizados, especialmente en variables hídricas que pueden verse 
afectadas por vacíos o cambios metodológicos en la recolección institucional. 
Aunque los modelos SARIMA y de regresión múltiple presentaron excelente ajuste y 
cumplimiento de supuestos, cualquier proyección o inferencia debe interpretarse 
con cautela, ya que no se controlan todas las fuentes externas de variabilidad 
(climática, política o económica).

Además, aunque se promueve la eficiencia económica del recurso, es fundamental 
recordar que decisiones basadas únicamente en estos criterios podrían invisibilizar 
dimensiones sociales y ambientales. Por ello, estas recomendaciones deben integrarse 
con procesos participativos, criterios de justicia hídrica y marcos normativos que 
aseguren el bienestar colectivo y la sustentabilidad ecosistémica. Finalmente, las 
predicciones aquí presentadas tienen un horizonte temporal limitado y deben ser 
actualizadas periódicamente mediante un sistema de monitoreo y revisión continua.

# Referencias

Box, G. E. P., Jenkins, G. M., Reinsel, G. C., & Ljung, G. M. (2015). Time Series 
Analysis: Forecasting and Control (5th ed.). Wiley. https://doi.org/10.1002/9781118619193

Helsel, D. R., & Hirsch, R. M. (2002). Statistical Methods in Water Resources. 
U.S. Geological Survey. https://pubs.usgs.gov/twri/twri4a3/

Ministerio de Obras Públicas. (2023). Diagnóstico de la situación hídrica en Chile. 
Dirección General de Aguas (DGA). https://dga.mop.gob.cl

OECD. (2022). Water Governance in Chile: Progress and Challenges. Organisation for 
Economic Co-operation and Development. https://doi.org/10.1787/9789264301137-en

Wooldridge, J. M. (2020). Introductory Econometrics: A Modern Approach (7th ed.). 
Cengage Learning.

Kutner, M. H., Nachtsheim, C. J., Neter, J., & Li, W. (2005). Applied Linear 
Statistical Models (5ª ed.). McGraw‑Hill/Irwin.

# Anexo

Links de las páginas web para descargar software R y RStudio:

- https://cran.r-project.org/

- https://posit.co/

Link de acceso al anexo de gráficos de análisis descriptivo y análisis avanzado

- https://drive.google.com/file/d/1k8ObRr3DVuk89yVXPUFtuvwb47GLyOiB/view?usp=sharing

Diccionario de variables

* Precipitaciones mensuales: Corresponden a la cantidad total de lluvia registrada 
cada mes de Chile. Se expresan en milímetros (mm) y reflejan el comportamiento 
estacional de las lluvias.

* Precipitaciones anuales: Representan la suma total de las precipitaciones durante 
cada año. Permiten observar tendencias de largo plazo y variaciones interanuales 
en el régimen hídrico.

* Temed – Temperatura media estival: Promedio de las temperaturas durante los meses 
de verano. Expresada en grados Celsius (°C).

* Tjmed – Temperatura media invernal: Promedio de las temperaturas en invierno. 
Expresado en °C.

* PPA – Precipitación normal anual: Promedio histórico de las precipitaciones 
acumuladas por año (mm).

* PPA.MIN – Precipitación anual mínima: Valor mínimo de precipitación registrada 
en un año, dentro del periodo observado (mm).

* PPA.MAX – Precipitación anual máxima: Valor máximo de precipitación registrada 
en un año (mm).

* Temed.50 – Estimación 2050 temperatura media estival: Proyección de la temperatura 
promedio en verano para el año 2050, en °C.

* Tjmed.50 – Estimación 2050 temperatura media invernal: Proyección de la temperatura 
promedio invernal en 2050, en °C.

* PPA.50 – Estimación 2050 precipitación normal anual: Proyección del promedio 
anual de precipitaciones para el año 2050 (mm).

* PPA.50.MIN – Estimación 2050 precipitación anual mínima: Valor mínimo proyectado 
de precipitaciones anuales al 2050 (mm).

* PPA.50.MAX – Estimación 2050 precipitación anual máxima: Valor máximo proyectado 
de precipitaciones anuales al 2050 (mm).

* Caudales mensuales: Volumen de agua que fluye por los ríos mensualmente, en metros 
cúbicos por segundo (m³/s). Refleja la dinámica superficial del agua.

* Recursos internos renovables de agua dulce per cápita: Volumen anual de agua dulce 
disponible internamente por persona, medido en metros cúbicos (m³/persona/año).

* Extracción de agua dulce: Volumen de agua dulce utilizada anualmente para 
actividades humanas (domésticas, agrícolas, industriales). Medido en miles de millones 
de m³.

* Productividad del agua: Valor económico generado por unidad de agua usada. Expresado 
en dólares de 2015 por m³. Indica eficiencia hídrica.

* Acceso al agua potable gestionada de manera segura: Población urbana con acceso 
a agua potable segura, es decir, de calidad, continua y disponible.

* Acceso al agua potable según tipo de servicio: Clasificación del acceso urbano 
según el tipo de fuente (mejorada, limitada, no mejorada o superficial). Permite 
medir mejoras en cobertura y calidad sanitaria.

'
      
      #Guardar .Rmd temporal
      tempReport <- file.path(tempdir(), "informe_temp.Rmd")
      writeLines(rmd_content, tempReport, useBytes = TRUE)
      
      # Procesar rango_global y extraer año_min y año_max
      rango_global <- input$rango_global
      
      # Si viene como string único, separar:
      if (!is.null(rango_global) && length(rango_global) == 1) {
        rango_global <- strsplit(gsub("\\s+", "", rango_global), "-")[[1]]
      }
      
      año_min <- as.numeric(rango_global[1])
      año_max <- as.numeric(rango_global[2])
      
      if (is.na(año_min) | is.na(año_max)) {
        stop("Rango global inválido, debe ser algo como '2001-2005'")
      }
      
      # Filtrar datos según años
      datos_precipitaciones_mensuales <- precipitaciones_mensuales %>%
        mutate(año = year(fecha), mes = month(fecha, label = TRUE, abbr = TRUE)) %>%
        filter(año >= año_min & año <= año_max)
      
      datos_precipitaciones_anuales <- precipitaciones_anuales %>%
        filter(año >= año_min & año <= año_max)
      
      datos_caudales <- caudales_mensuales %>%
        filter(year(fecha) >= año_min & year(fecha) <= año_max)
      
      datos_renovables <- recursos_renovables %>%
        filter(año >= año_min & año <= año_max)
      
      datos_extraccion <- extraccion_agua %>%
        filter(año >= año_min & año <= año_max)
      
      datos_productividad <- productividad_agua %>%
        filter(año >= año_min & año <= año_max)
      
      datos_acceso <- acceso_agua_urbana %>%
        filter(año >= año_min & año <= año_max)
      
      datos_servicios <- acceso_agua_urbana %>%
        filter(año >= año_min & año <= año_max)
      
      # Renderizar informe pasando los parámetros ya separados
      rmarkdown::render(
        input = tempReport,
        output_format = "pdf_document",
        output_file = file,
        params = list(
          datos_precipitaciones_mensuales = datos_precipitaciones_mensuales,
          datos_precipitaciones_anuales = datos_precipitaciones_anuales,
          datos_caudales = datos_caudales,
          datos_renovables = datos_renovables,
          datos_extraccion = datos_extraccion,
          datos_productividad = datos_productividad,
          datos_acceso = datos_acceso,
          datos_servicios = datos_servicios,
          servicios_seleccionados = input$servicios_seleccionados,
          regiones_sf = regiones_sf,
          variable = input$variable,
          datos_seleccionados = datos_seleccionados(),
          variable_seleccionada = input$variable_seleccionada,
          año_min = año_min,
          año_max = año_max
        ),
        envir = new.env(parent = globalenv())
      )
      
    }
  )}


#Lanzar aplicación
shinyApp(ui= ui, server = server)

