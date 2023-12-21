library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
library(shinyBS)
library(shinyWidgets)
library(DT)
library(lubridate)
library(DBI)
library(odbc)
library(RPostgres)
library(pool)
library(readr)

indicadores <- c("Número de instalaciones",
                 "Tasa de instalaciones públicas y privadas",
                 "Utilización de instalaciones deportivas",
                 "Número de turistas deportivos",
                 "Número de eventos deportivos",
                 "Número de asistentes a eventos deportivos",
                 "Ingresos totales por turismo deportivo",
                 "Empleo generado",
                 "Inversión en infraestructuras deportivas",
                 "Inversión en la promoción del turismo deportivo",
                 "Inversión en eventos deportivos",
                 "Ingresos por las ventas de equipamiento deportivo",
                 "Ingresos en los negocios locales",
                 "Ingresos por subvenciones y ayudas en el deporte",
                 "Ingresos tributarios relacionados con el turismo deportivo",
                 "Número de clubes y asociaciones deportivas",
                 "Número de miembros de clubes y asociaciones deportivas",
                 "Balance de ingresos y gastos en turismo deportivo",
                 "Inversión total en turismo deportivo",
                 "Número de clubes por deporte",
                 "Número de instalaciones por mil habitantes",
                 "Nivel de ruido",
                 "Índice de calidad del aire",
                 "Participación local en eventos deportivos",
                 "Generación de residuos urbanos",
                 "Emisiones de Gases de Efecto Invernadero",
                 "Número de programas deportivos juveniles y comunitarios")

fuente <- c("Ayuntamiento. Censo de instalaciones deportivas.",
            "Ayuntamiento. Censo de instalaciones deportivas.",
            "Ayuntamiento. Registro de acceso a instalaciones o de reservas preguntando por código postal de residencia o si es turista.",
            "Compañías telefónicas, establecimientos hoteleros o compañías de transporte.",
            "Ayuntamiento. Registro de eventos deportivos del ayuntamiento.",
            "Organización del evento. Registro de asistencia de la organización del evento y registro de participación. Instalación de cámaras o sensores en el municipio para el conteo en los eventos en recintos abiertos.",
            "Ayuntamiento. Registro de ingresos por turismo deportivo del ayuntamiento o datos de transacciones de tarjetas de crédito.",
            "Ayuntamiento, organización del evento o comercios locales. Registro de personas contratadas por la organización del evento, comercios locales o el ayuntamiento.",
            "Ayuntamiento. Registro de gastos del ayuntamiento.",
            "Ayuntamiento. Registro de gastos del ayuntamiento.",
            "Ayuntamiento. Registro de gastos del ayuntamiento.",
            "Comercios de venta de equipamiento deportivo. Registro de ingresos de los negocios de venta de equipamiento deportivo.",
            "Negocios locales. Registro de ingresos de los negocios.",
            "Ayuntamiento. Registro de ingresos del ayuntamiento.",
            "Ayuntamiento. Registro de ingresos del ayuntamiento.",
            "Ayuntamiento. Registro de entidades deportivas.",
            "Clubes y asociaciones deportivas. Registro de miembros.",
            "Ayuntamiento. Registros de inversiones e ingresos.",
            "Ayuntamiento. Registro de inversiones en turismo deportivo del ayuntamiento.",
            "Ayuntamiento. Registro de entidades deportivas.",
            "Clubes y asociaciones deportivas. Registro de miembros.",
            "Ayuntamiento. Registro de instalaciones deportivas y de habitantes.",
            "Ayuntamiento. Sensores de nivel de ruido.",
            "Ayuntamiento. Sensores de calidad del aire.",
            "Ayuntamiento. Consejería de medio ambiente.",
            "Ayuntamiento. Consejería de medio ambiente.",
            "Ayuntamiento. Listado de programas deportivos."

)

metodo <- c("Obtención directa.",
            "Cociente entre el número de instalaciones públicas y privadas.",
            "Obtención directa o conteo de usuarios no residentes mediante abonos de turistas o preguntando directamente.",
            "Obtener mediante datos de teléfonos móviles, de reservas de hoteles y vuelos o encuestando a los asistentes a un evento.",
            "Obtención directa.",
            "Obtención directa. Conteo mediante cámaras o sensores en eventos abiertos.",
            "Método cuantitativo. Suma de todos los tipos de ingresos relacionados con el turismo deportivo.",
            "Obtención directa.",
            "Obtención directa.",
            "Obtención directa.",
            "Obtención directa.",
            "Obtención directa.",
            "Obtención directa.",
            "Obtención directa.",
            "Obtención directa.",
            "Método cuantitativo. Suma del listado de clubes y asociaciones deportivas.",
            "Obtención directa.",
            "Método cuantitativo. Diferencia entre ingresos totales e inversión total.",
            "Método cuantitativo. Suma de todos los tipos de inversiones relacionados con el turismo deportivo.",
            "Método cuantitativo. Suma de listado de clubes y asociaciones agrupados por deporte.",
            "Método cuantitativo. Suma de listado de miembros agrupados por deporte.",
            "Método cuantitativo. Cociente entre el número de instalaciones deportivas y el número de habitantes multiplicado por 1000.",
            "Los datos de este indicador se obtienen mediante sensores instalados en los puntos de interés del municipio como pueden ser instalaciones deportivas en las que tienen lugar los eventos. Una vez obtenidos los datos se calcula la media de entre las 04:00-04:59, 12:00-12:59 y 20:00-20:59.",
            "Los datos de este indicador se obtienen mediante sensores instalados en los puntos de interés del municipio como pueden ser instalaciones deportivas en las que tienen lugar los eventos. Las mediciones de los contaminantes se utilizan siguiendo las indicaciones de la EPA para calcular el índice.",
            "Obtención directa.",
            "Los datos de este indicador se obtienen mediante sensores instalados en los puntos de interés del municipio como pueden ser instalaciones deportivas en las que tienen lugar los eventos. Para obtener el dato en toneladas de CO2 equivalente se multiplica el valor medido del gas por su factor de emisión.",
            "Obtención directa."

)

df_indicadores <- data.frame(Indicador = indicadores,
                             FuenteDeObtencion = fuente,
                             MetodoDeCalculo = metodo)


municipios <- read_csv("municipios.csv")

tabla1 <- read_csv("tabla1.csv")
tabla2 <- read_csv("tabla2.csv")
tabla3 <- read_csv("tabla3.csv")
tabla4 <- read_csv("tabla4.csv")
tabla5 <- read_csv("tabla5.csv")
tabla6 <- read_csv("tabla6.csv")
tabla7 <- read_csv("tabla7.csv")
tabla8 <- read_csv("tabla8.csv")
tabla9 <- read_csv("tabla9.csv")
tabla10 <- read_csv("tabla10.csv")
tabla11 <- read_csv("tabla11.csv")
tabla12 <- read_csv("tabla12.csv")
tabla13 <- read_csv("tabla13.csv")
tabla14 <- read_csv("tabla14.csv")
tabla15 <- read_csv("tabla15.csv")
tabla16 <- read_csv("tabla16.csv")
tabla17 <- read_csv("tabla17.csv")
tabla18 <- read_csv("tabla18.csv")
tabla19 <- read_csv("tabla19.csv")
tabla20 <- read_csv("tabla20.csv")
tabla21 <- read_csv("tabla21.csv")
tabla22 <- read_csv("tabla22.csv")
tabla23 <- read_csv("tabla23.csv")
tabla24 <- read_csv("tabla24.csv")
tabla25 <- read_csv("tabla25.csv")
tabla26 <- read_csv("tabla26.csv")
tabla27 <- read_csv("tabla27.csv")
tabla28 <- read_csv("tabla28.csv")
tabla29 <- read_csv("tabla29.csv")

onStop(function() {
  poolClose(con)
})

ui <- dashboardPage(title = "Turismo deportivo",
                    dashboardHeader(title = NULL, titleWidth = 0,
                                    tags$li(class = "dropdown navmenuR",
                                            actionButton("social", "Social"),
                                            actionButton("economico", "Económico"),
                                            actionButton("ambiental", "Medioambiental"),
                                            actionButton("metodologia", "Metodología")),
                                    leftUi = tagList(
                                      img(src = "icono.webp", class = "logoHeader")
                                    )
                    ),
                    dashboardSidebar(disable = T, minified = F, collapsed = T,
                                     sidebarMenu(id="tabs",
                                                 menuItem("Inicio", tabName = "Inicio"),
                                                 menuItem("Social", tabName="Social"),
                                                 menuItem("Impacto económico", tabName="Economico"),
                                                 menuItem("Medioambiental", tabName="Ambiental"),
                                                 menuItem("Metodología", tabName="Metodologia")
                                     )
                    ),
                    dashboardBody(
                      includeCSS("www/style.css"),
                      useShinyjs(),
                      tabItems(
                        tabItem(tabName = "Inicio",
                                div(class = "inicioWrapper",
                                    div(class = "inicioTitulo", "Turismo deportivo")
                                    )
                        ),
                        # Social --------
                        tabItem(tabName = "Social",
                                div(class = "barraSup",
                                    div(style = "display: flex; align-items: center; gap: 1rem;",
                                        div(class = "rectangulo"),
                                        h3("Indicadores sociales")
                                    ),
                                    div(class = "seleccionadores",
                                        pickerInput(
                                          inputId = "municipioSocial",
                                          label = "Municipio",
                                          selected = municipios$nombre[1],
                                          choices = municipios$nombre,
                                          options = list(`live-search` = TRUE),
                                          choicesOpt = list(
                                            tokens = municipios$nombre_provincia
                                          )
                                        ),
                                        pickerInput(
                                          inputId = "añoSocial",
                                          label = "Año",
                                          choices = c(2023)
                                        )
                                    )
                                ),
                                column(width = 6, id = "columna",
                                       fluidRow(id="fila",
                                                div(id = "Turistas",
                                                    uiOutput("numTuristasInd")
                                                ),
                                                div(id = "Empleo",
                                                    uiOutput("empleoInd")
                                                ),
                                                div(id = "Programas",
                                                    uiOutput("programasInd")
                                                )
                                       ),
                                       fluidRow(id="fila",
                                                box(width = 12, id="ecoSocial", title = textOutput("tituloBarras"),
                                                    plotlyOutput("barras")
                                                )
                                       ),
                                       uiOutput("instalacionesUI"),
                                       fluidRow(id="fila",
                                                box(width = 12, id="ecoSocial", title = "Uso de instalaciones",
                                                    plotlyOutput("usoInstalaciones")
                                                )
                                       )
                                ),
                                column(width = 6, id = "columna",
                                       fluidRow(id="fila",
                                                box(width = 4, id = "fooSocial", title = NULL, headerBorder = F,
                                                    div(class = "centro",
                                                        p("Eventos deportivos", class = "numAmb"),
                                                        uiOutput("numEventosInd"),
                                                        p("", class = "varAmb")
                                                    )
                                                ),
                                                box(width = 8,  id = "fooSocialZ", title = NULL, headerBorder = F,
                                                    column(width = 4, style = "padding-right: 7.5px; margin: auto; position: relative; top: calc(50% - 27px);",
                                                           pickerInput(
                                                             inputId = "selectEvento",
                                                             label = "Seleccionar Evento",
                                                             selected = NULL,
                                                             choices = c(),
                                                             options = list(
                                                               `live-search` = TRUE)
                                                           )
                                                           ),
                                                    column(width = 8, style = "padding-left: 7.5px;",
                                                           uiOutput("infoEventos")
                                                           )

                                                    )
                                       ),
                                       fluidRow(id="fila",
                                                box(width = 12, id="ecoSocial", title = "Número de eventos",
                                                    plotlyOutput("numEventos")
                                                    )
                                       ),
                                       fluidRow(id = "fila",
                                                uiOutput("clubesUI"),
                                                column(width = 6, id = "columna",
                                                       box(width = 12, id = "ecoSocial2", title = "Número de clubes por deporte",
                                                           plotlyOutput("numClubes")
                                                       )
                                                )
                                       )
                                )

                        ),
                        # Economico --------
                        tabItem(tabName = "Economico",
                                div(class = "barraSup",
                                    div(style = "display: flex; align-items: center; gap: 1rem;",
                                        div(class = "rectangulo"),
                                        h3("Impacto económico")
                                    ),
                                    div(class = "seleccionadores",
                                        airMonthpickerInput(
                                          inputId = "fechasEco",
                                          value = c(Sys.Date(), Sys.Date()-years(2)),
                                          label = "Rango de fechas",
                                          language = "es",
                                          dateFormat = "M yyyy",
                                          addon = "left",
                                          range = T
                                        ),
                                        pickerInput(
                                          inputId = "gruposEco",
                                          label = "Agrupación",
                                          choices = c("Mensual", "Cuatrimestral", "Anual")
                                        ),
                                        pickerInput(
                                          inputId = "municipioEco",
                                          label = "Municipio",
                                          selected = municipios$nombre[1],
                                          choices = municipios$nombre,
                                          options = list(`live-search` = TRUE),
                                          choicesOpt = list(
                                            tokens = municipios$nombre_provincia
                                          )
                                        )
                                    )
                                ),
                                fluidRow(id = "fila",
                                             uiOutput("inversionUI"),
                                         box(id = "ecoEco", width = 8,
                                             title = "Inversión en turismo deportivo",
                                             plotlyOutput("inversion")
                                         )
                                ),
                                fluidRow(id = "fila",
                                         uiOutput("ingresosUI"),
                                         box(id = "ecoEco", width = 8,
                                             title = "Ingresos por turismo deportivo",
                                             plotlyOutput("ingresos")
                                         )
                                ),
                        ),
                        # Ambiental --------
                        tabItem(tabName = "Ambiental",
                                div(class = "barraSup",
                                    div(style = "display: flex; align-items: center; gap: 1rem;",
                                        div(class = "rectangulo"),
                                        h3("Impacto medioambiental")
                                    ),
                                    div(class = "seleccionadores",
                                        airMonthpickerInput(
                                          inputId = "fechasAmb",
                                          value = c(Sys.Date(), Sys.Date()-years(1)),
                                          label = "Rango de fechas",
                                          language = "es",
                                          dateFormat = "M yyyy",
                                          addon = "left",
                                          range = T
                                        ),
                                        pickerInput(
                                          inputId = "municipioAmb",
                                          label = "Municipio",
                                          selected = municipios$nombre[1],
                                          choices = municipios$nombre,
                                          options = list(`live-search` = TRUE),
                                          choicesOpt = list(
                                            tokens = municipios$nombre_provincia
                                          )
                                        )
                                    )
                                ),
                                fluidRow(id = "fila",
                                         uiOutput("indiceAire"),
                                         box(width = 9, id = "ecoAmb", title = "Emisiones de Gases de Efecto Invernadero",
                                             plotlyOutput("efectoInvernadero")
                                         )
                                ),
                                uiOutput("indicadoresAmb"),
                                fluidRow(id = "fila",
                                         box(width = 6, id = "ecoAmb", title = "Nivel de ruido",
                                             plotlyOutput("ruido")
                                         ),
                                         box(width = 6, id = "ecoAmb", title = "Cantidad de residuos generados",
                                             plotlyOutput("residuos")
                                         )
                                )
                        ),
                        # Metodología --------
                        tabItem(tabName = "Metodologia",
                                box(width = 12, id = "fooMetodologia", title = NULL, headerBorder = F,
                                    DT::dataTableOutput("metodologiaTabla")
                                )
                        )
                      ),
                      includeScript(path = "www/script.js")
                    )
)

server <- function(input, output, session) {

# Cambio de pestaña --------

  observeEvent(input$indicadores,{
    updateTabItems(session, 'tabs', "Indicadores")
  })

  observeEvent(input$social,{
    session$sendCustomMessage("msg", 5)
    updateTabItems(session, 'tabs', "Social")
  })

  observeEvent(input$ambiental,{
    updateTabItems(session, 'tabs', "Ambiental")
  })

  observeEvent(input$instalaciones,{
    updateTabItems(session, 'tabs', "Instalaciones")
  })

  observeEvent(input$economico,{
    updateTabItems(session, 'tabs', "Economico")
  })

  observeEvent(input$clubes,{
    updateTabItems(session, 'tabs', "Clubes")
  })

  observeEvent(input$metodologia,{
    updateTabItems(session, 'tabs', "Metodologia")
  })

# UI Social --------

  output$numTuristasInd <- renderUI({
    box(width = 4, id = "fooSocial", title = NULL, headerBorder = F,  class = "seleccionado",
        div(class = "centro",
            p("Turistas deportivos", class = "numAmb"),
            p(tabla1%>%
                dplyr::filter(nombre == input$municipioSocial)%>%
                arrange(desc(fecha))%>%
                select(num_turistas)%>%
                head(1)%>%
                .$num_turistas,
              class = "prcAmb"),
            p("", class = "varAmb")
        )
    )
  })

  output$empleoInd <- renderUI({
    box(width = 4, id = "fooSocial", title = NULL, headerBorder = F,
        div(class = "centro",
            p("Empleo generado", class = "numAmb"),
            p(tabla2%>%
                dplyr::filter(nombre == input$municipioSocial)%>%
                arrange(desc(fecha))%>%
                select(num_empleos)%>%
                head(1)%>%
                .$num_empleos,
              class = "prcAmb"),
            p("", class = "varAmb")
        )
    )
  })

  output$programasInd <- renderUI({
    box(width = 4, id = "fooSocial", title = NULL, headerBorder = F,
        div(class = "centro",
            p("Programas deportivos", class = "numAmb"),
            p(tabla3%>%
                mutate(year = year(fecha), month = month(fecha))%>%
                dplyr::filter(nombre == input$municipioSocial, year == max(year))%>%
                dplyr::filter(month == max(month))%>%
                nrow(),
              class = "prcAmb"),
            p("", class = "varAmb")
        )
    )
  })

  # Indicador número de eventos deportivos
  output$numEventosInd <- renderUI({
    p(tabla4%>%
        mutate(year = year(fecha_fin), month = month(fecha_fin))%>%
        dplyr::filter(nombre == input$municipioSocial, year == max(year))%>%
        dplyr::filter(month == max(month))%>%
        nrow(),
      class = "prcAmb")
  })

  # Filtro por evento
  observeEvent(list(input$municipioSocial, input$añoSocial), {
    updatePickerInput(session = session, inputId = "selectEvento",
                      choices = tabla5%>%
                        mutate(year = year(fecha_fin))%>%
                        dplyr::filter(nombre == input$municipioSocial, year == input$añoSocial)%>%
                        pull(evento_nombre)
                      )
  })

  #Filtro de año
  observeEvent(input$municipioSocial,{
    updatePickerInput(session = session, inputId = "añoSocial",
                      choices = unique(tabla6%>%
                                         dplyr::filter(nombre == input$municipioSocial)%>%
                                         mutate(year = year(fecha))%>%
                                         select(year))$year
                      )
  })

  # Indicador número de espectadores en eventos deportivos
  output$infoEventos <- renderUI({
    evento <- tabla7
    part <- evento%>%
      dplyr::filter(evento_nombre == input$selectEvento)%>%
      pull(participantes)
    espec <- evento%>%
      dplyr::filter(evento_nombre == input$selectEvento)%>%
      pull(espectadores)
    div(class = "centro2",
        p("Participantes", class = "numAmb letra", style = "grid-row: 1; grid-column: 1;"),
        p(part,
          class = "prcAmb", style = "grid-row: 2; grid-column: 1;"),
        p("Espectadores", class = "numAmb letra", style = "grid-row: 1; grid-column: 2;"),
        p(espec,
          class = "prcAmb", style = "grid-row: 2; grid-column: 2;")
    )
  })

  # Indicadores instalaciones deportivas
  output$instalacionesUI <- renderUI({
    instalaciones <- tabla8
    instalaciones2 <- tabla9
    fluidRow(id="fila",
             box(width = 4, id = "fooSocial", title = NULL, headerBorder = F,
                 div(class = "centro",
                     p("Instalaciones deportivas", class = "numAmb"),
                     p(instalaciones2%>%
                         dplyr::filter(nombre == input$municipioSocial)%>%
                         nrow(),
                       class = "prcAmb"),
                     p("", class = "varAmb")
                 )
             ),
             box(width = 4, id = "fooSocial", title = NULL, headerBorder = F,
                 div(class = "centro",
                     p("Tasa de instalaciones", class = "numAmb"),
                     p(round((instalaciones%>%
                                dplyr::filter(nombre == input$municipioSocial, tipo_propiedad == "Publica")%>%
                                nrow())/
                               (instalaciones%>%
                                  dplyr::filter(nombre == input$municipioSocial, tipo_propiedad == "Privada")%>%
                                  nrow()),2),
                       class = "prcAmb"),
                     p("", class = "varAmb")
                 )
             ),
             box(width = 4, id = "fooSocial", title = NULL, headerBorder = F,
                 div(class = "centro",
                     p("Uso de instalaciones", class = "numAmb"),
                     p(tabla10%>%
                         mutate(year = year(fecha), month = month(fecha))%>%
                         dplyr::filter(nombre == input$municipioSocial, year == max(year))%>%
                         dplyr::filter(month == max(month))%>%
                         pull(usuarios),
                       class = "prcAmb"),
                     p("", class = "varAmb")
                 )
             )
    )

  })

  # Indicador número de clubes deportivos
  output$clubesUI <- renderUI({
    club <- tabla11
    habitantes <- tabla12
    column(width = 6, id = "columna",
           box(width = 12, id = "fooSocial2", title = NULL, headerBorder = F,
               div(class = "centro",
                   p("Clubes deportivos", class = "numSoc"),
                   p(club%>%
                       dplyr::filter(nombre == input$municipioSocial)%>%
                       nrow(),
                     class = "prcSoc"),
                   p("Clubes por 1000 habitantes", class = "numSoc2"),
                   p(round(
                     (club%>%
                       dplyr::filter(nombre == input$municipioSocial)%>%
                       nrow()/habitantes%>%
                       dplyr::filter(nombre == input$municipioSocial)%>%
                       pull(habitantes))*1000,2),
                     class = "prcSoc2")
               )
           ),
           box(width = 12, id = "fooSocial2", title = NULL, headerBorder = F,
               div(class = "centro",
                   p("Miembros", class = "numSoc"),
                   p(club%>%
                       dplyr::filter(nombre == input$municipioSocial)%>%
                       pull(miembros)%>%
                       sum(),
                     class = "prcSoc"),
                   p("Porcentaje de población", class = "numSoc2"),
                   p(paste0(round((club%>%
                       dplyr::filter(nombre == input$municipioSocial)%>%
                       pull(miembros)%>%
                       sum()/habitantes%>%
                       dplyr::filter(nombre == input$municipioSocial)%>%
                       pull(habitantes))*100,2), "%"),
                     class = "prcSoc2")
               )
           )
    )
  })

# Gráficos Social --------

  # Indicadores
  graficosBarras <- reactiveValues()

  graficosBarras$Turistas <- reactive( tabla13%>%
      mutate(year = year(fecha), Mes = month(fecha, label = T))%>%
      dplyr::filter(nombre == input$municipioSocial, year == as.numeric(input$añoSocial))%>%
      ggplot()+
      suppressWarnings(geom_bar(aes(x=Mes, y=num_turistas, text = paste0("Turistas deportivos: ", num_turistas, "<br>Mes: ", Mes)), stat = "identity", fill = "#FFAE00"))+
      theme_bw()+
      labs(y = "Turistas deportivos", x = "Mes")
  )

  graficosBarras$Empleos <- reactive( tabla14%>%
                                        mutate(year = year(fecha), Mes = month(fecha, label = T))%>%
                                        dplyr::filter(nombre == input$municipioSocial, year == as.numeric(input$añoSocial))%>%
                                        ggplot()+
                                        suppressWarnings(geom_bar(aes(x=Mes, y=num_empleos, text = paste0("Empleos generados: ", num_empleos, "<br>Mes: ", Mes)), stat = "identity", fill = "#B3B3B3"))+
                                        theme_bw()+
                                        labs(y = "Empleos", x = "Mes")
  )

  graficosBarras$Programas <- reactive(
    tabla15%>%
      mutate(year = year(fecha), month = month(fecha, label = T))%>%
      dplyr::filter(nombre == input$municipioSocial, year == input$añoSocial)%>%
      group_by(month, nombre_deporte)%>%
      summarise(num_programas = n(), .groups = 'drop')%>%
    ggplot()+
      suppressWarnings(geom_bar(aes(x=month, y=num_programas, group = nombre_deporte, fill = nombre_deporte, text = paste0("Programas deportivos: ", num_programas, "Deporte: ", nombre_deporte, "<br>Mes: ", month)), stat = "identity"))+
      scale_fill_manual(values = c("#FFAE00", "#6A8D90", "#B3B3B3", "#C7B8E7", "#A0D1FF", "#EFB5B9", "#D9B300", "#479AA1"))+
      theme_bw()+
      labs(y = "Programas deportivos", x = "Mes")
  )

  graficosBarras$out <- reactive( tabla16%>%
                                    mutate(year = year(fecha), Mes = month(fecha, label = T))%>%
                                    dplyr::filter(nombre == input$municipioSocial, year == as.numeric(input$añoSocial))%>%
                                    ggplot()+
                                    suppressWarnings(geom_bar(aes(x=Mes, y=num_turistas, text = paste0("Turistas deportivos: ", num_turistas, "<br>Mes: ", Mes)), stat = "identity", fill = "#FFAE00"))+
                                    theme_bw()+
                                    labs(y = "Turistas deportivos", x = "Mes")
  )

  output$barras <- renderPlotly({
    ggplotly(
      graficosBarras$out()
    , tooltip = "text")%>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
  })

  titulo <- reactiveValues(
    texto = "Número de turistas deportivos el último año"
  )

  output$tituloBarras <- renderText({
    titulo$texto
  })

  # Número de eventos deportivos
  output$numEventos <- renderPlotly({
    ggplotly(
      tabla17%>%
        mutate(year = year(fecha_fin), month = month(fecha_fin, label = T))%>%
        dplyr::filter(nombre == input$municipioSocial, year == input$añoSocial)%>%
        group_by(month)%>%
        summarise(num_eventos = n())%>%
      ggplot()+
        suppressWarnings(geom_bar(aes(x=month, y=num_eventos, text = paste0("Eventos deportivos: ", num_eventos, "<br>Mes: ", month)), stat = "identity", fill = "#6A8D90"))+
        theme_bw()+
        labs(y = "Eventos deportivos", x = "Mes")
      , tooltip = "text")%>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
  })

  # Instalaciones
  output$usoInstalaciones <- renderPlotly({
    ggplotly(
      tabla18%>%
        mutate(year = year(fecha), month = month(fecha, label = T))%>%
        dplyr::filter(nombre == input$municipioSocial, year == input$añoSocial)%>%
        ggplot()+
        suppressWarnings(geom_bar(aes(x = month, y = usuarios, text = paste0("Número de ususarios: ", usuarios, "<br>Mes: ", month)), stat = "identity", fill = "#479AA1"))+
        theme_bw()+
        labs(y = "Usuarios", x = "Mes")
      , tooltip = "text")%>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
  })

  # Clubes
  output$numClubes <- renderPlotly({
    tabla19%>%
      dplyr::filter(nombre == input$municipioSocial)%>%
      group_by(nombre_deporte)%>%
      summarise(Clubes = n())%>%
    plot_ly(values = ~Clubes, labels = ~nombre_deporte, type = "pie")%>%
      layout(showlegend = F, margin = list(l = 10, r = 10, t = 20, b = 20))%>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestPie"))
  })

  # Título del gráfico de indicadores generales
    onclick("Turistas",{
            graficosBarras$out <- graficosBarras$Turistas
            titulo$texto <- "Número de turistas deportivos"
    })

    onclick("Programas",{
            graficosBarras$out <- graficosBarras$Programas
            titulo$texto <- "Número de programas deportivos por deporte"
    })

    onclick("Empleo",{
            graficosBarras$out <- graficosBarras$Empleos
            titulo$texto <- "Empleos genereados"
    })

# UI económico --------

  # Indicadore ingresos totales este año
  output$ingresosUI <- renderUI({
    ing <-tabla20%>%
      mutate(year = year(fecha), month = month(fecha))%>%
      dplyr::filter(nombre == input$municipioEco, month <= month(max(fecha)))%>%
      group_by(year)%>%
      summarize(n = sum(cantidad))
    este <- ing%>%
      dplyr::filter(year == max(year))%>%
      pull(n)
    anterior <- ing%>%
      dplyr::filter(year == max(year)-1)%>%
      pull(n)
    if(este > anterior){
      box(width = 4, id = "fooEco", title = NULL, headerBorder = F,
          div(class = "encabezadoIndicador",
              div(class = "tituloIndicadorEco",
                  "Ingresos totales"
              )
          ),
          div(class="cuerpoIndicador",
              img(src="ingresos.webp", class = "imgIndicador"),
              p(paste0(format(este, big.mark = ".", decimal.mark = ","), "€"), class = "numEco"),
              p(paste0(round((este - anterior)/anterior*100, 2), "%"), class = "prcEco", tags$i(class = "fa-solid fa-arrow-up", style = "color: #26a269;")),
              p("Variación anual", class = "varEco")
          )
      )
    } else {
      box(width = 4, id = "fooEco", title = NULL, headerBorder = F,
          div(class = "encabezadoIndicador",
              div(class = "tituloIndicadorEco",
                  "Ingresos totales"
              )
          ),
          div(class="cuerpoIndicador",
              img(src="ingresos.webp", class = "imgIndicador"),
              p(paste0(format(este, big.mark = ".", decimal.mark = ","), "€"), class = "numEco"),
              p(paste0(round((este - anterior)/anterior*100, 2), "%"), class = "prcEco", tags$i(class = "fa-solid fa-arrow-down", style = "color: #a2263b;")),
              p("Variación anual", class = "varEco")
          )
      )
    }
  })

  # Indicadore inversión total este año
  output$inversionUI <- renderUI({
    ing <- tabla21%>%
      mutate(year = year(fecha), month = month(fecha))%>%
      dplyr::filter(nombre == input$municipioEco, month <= month(max(fecha)))%>%
      group_by(year)%>%
      summarize(n = sum(cantidad))
    este <- ing%>%
      dplyr::filter(year == max(year))%>%
      pull(n)
    anterior <- ing%>%
      dplyr::filter(year == max(year)-1)%>%
      pull(n)
    if(este > anterior){
      box(width = 4, id = "fooEco", title = NULL, headerBorder = F,
          div(class = "encabezadoIndicador",
              div(class = "tituloIndicadorEco",
                  "Inversión total"
              )
          ),
          div(class="cuerpoIndicador",
              img(src="inversion.webp", class = "imgIndicador"),
              p(paste0(format(este, big.mark = ".", decimal.mark = ","), "€"), class = "numEco"),
              p(paste0(round((este - anterior)/anterior*100, 2), "%"), class = "prcEco", tags$i(class = "fa-solid fa-arrow-up", style = "color: #26a269;")),
              p("Variación anual", class = "varEco")
          )
      )
    } else {
      box(width = 4, id = "fooEco", title = NULL, headerBorder = F,
          div(class = "encabezadoIndicador",
              div(class = "tituloIndicadorEco",
                  "Inversión total"
              )
          ),
          div(class="cuerpoIndicador",
              img(src="inversion.webp", class = "imgIndicador"),
              p(paste0(format(este, big.mark = ".", decimal.mark = ","), "€"), class = "numEco"),
              p(paste0(round((este - anterior)/anterior*100, 2), "%"), class = "prcEco", tags$i(class = "fa-solid fa-arrow-down", style = "color: #a2263b;")),
              p("Variación anual", class = "varEco")
          )
      )
    }
  })

# Gráficos económico --------

  # Gráfico de líneas inversion
  output$inversion <- renderPlotly({
    inversion <- tabla22
    if(input$gruposEco == "Mensual"){
      ggplotly(
        inversion%>%
          mutate(year = year(fecha), quarter = quarter(fecha), month = month(fecha))%>%
          dplyr::filter(nombre == input$municipioEco, fecha <= input$fechasEco[2], fecha >= input$fechasEco[1])%>%
          group_by(fecha, tipo_inversion)%>%
          summarise(n = sum(cantidad), .groups = 'drop')%>%
          ggplot()+
          geom_area(aes(x=fecha, y=n, fill = tipo_inversion), colour = "black", alpha = 0.75)+
          suppressWarnings(geom_point(aes(x=fecha, y=n, fill = tipo_inversion, text = paste0("Cantidad (€); ", n, "<br>Tipo de inversión: ", tipo_inversion, "<br>Fecha: ", fecha)), position = position_stack(), colour="black", pch=21, size=2))+
          scale_fill_manual(values = c("#FFAE00", "#6A8D90", "#B3B3B3", "#C7B8E7", "#A0D1FF", "#EFB5B9", "#D9B300", "#479AA1"))+
          theme_bw()+
          theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
          labs(y = "Cantidad", x = "Fecha")
        , tooltip = "text")%>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    } else if(input$gruposEco == "Cuatrimestral"){
      ggplotly(
        inversion%>%
          mutate(year = year(fecha), quarter = floor_date(fecha, unit = "year") + months(3*quarter(fecha)-1))%>%
          dplyr::filter(nombre == input$municipioEco, fecha <= input$fechasEco[2], fecha >= input$fechasEco[1])%>%
          group_by(quarter, tipo_inversion)%>%
          summarise(n = sum(cantidad), year = max(year), .groups = 'drop')%>%
          ggplot()+
          geom_area(aes(x=quarter, y=n, fill = tipo_inversion), colour = "black", alpha = 0.75)+
          suppressWarnings(geom_point(aes(x=quarter, y=n, fill = tipo_inversion, text = paste0("Cantidad (€); ", n, "<br>Tipo de inversión: ", tipo_inversion, "<br>Cuatrimestre: ", year, " Q", quarter(quarter))), position = position_stack(), colour="black", pch=21, size=2))+
          scale_fill_manual(values = c("#FFAE00", "#6A8D90", "#B3B3B3", "#C7B8E7", "#A0D1FF", "#EFB5B9", "#D9B300", "#479AA1"))+
          theme_bw()+
          theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
          labs(y = "Cantidad", x = "Fecha")
        , tooltip = "text")%>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    } else {
      ggplotly(
        inversion%>%
          mutate(year = year(fecha), quarter = floor_date(fecha, unit = "year") + months(3*quarter(fecha)-1))%>%
          dplyr::filter(nombre == input$municipioEco, fecha <= input$fechasEco[2], fecha >= input$fechasEco[1])%>%
          group_by(year, tipo_inversion)%>%
          summarise(n = sum(cantidad), .groups = 'drop')%>%
          ggplot()+
          geom_area(aes(x=year, y=n, fill = tipo_inversion), colour = "black", alpha = 0.75)+
          suppressWarnings(geom_point(aes(x=year, y=n, fill = tipo_inversion, text = paste0("Cantidad (€); ", n, "<br>Tipo de inversión: ", tipo_inversion, "<br>Año: ", year)), position = position_stack(), colour="black", pch=21, size=2))+
          scale_fill_manual(values = c("#FFAE00", "#6A8D90", "#B3B3B3", "#C7B8E7", "#A0D1FF", "#EFB5B9", "#D9B300", "#479AA1"))+
          theme_bw()+
          theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
          labs(y = "Cantidad", x = "Fecha")
        , tooltip = "text")%>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    }
  })

  # Gráfico de líneas ingresos
  output$ingresos <- renderPlotly({
    ingresos <- tabla23
    if(input$gruposEco == "Mensual"){
      ggplotly(
        ingresos%>%
          mutate(year = year(fecha), quarter = quarter(fecha), month = month(fecha))%>%
          dplyr::filter(nombre == input$municipioEco, fecha <= input$fechasEco[2], fecha >= input$fechasEco[1])%>%
          group_by(fecha, nombre_origen)%>%
          summarise(n = sum(cantidad), .groups = 'drop')%>%
          ggplot()+
          geom_area(aes(x=fecha, y=n, fill = nombre_origen), colour = "black", alpha = 0.75)+
          suppressWarnings(geom_point(aes(x=fecha, y=n, fill = nombre_origen, text = paste0("Cantidad (€); ", n, "<br>Origen de ingresos: ", nombre_origen, "<br>Fecha: ", fecha)), position = position_stack(), colour="black", pch=21, size=2))+
          scale_fill_manual(values = c("#FFAE00", "#6A8D90", "#B3B3B3", "#C7B8E7", "#A0D1FF", "#EFB5B9", "#D9B300", "#479AA1"))+
          theme_bw()+
          theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
          labs(y = "Cantidad", x = "Fecha")
        , tooltip = "text")%>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    } else if(input$gruposEco == "Cuatrimestral"){
      ggplotly(
        ingresos%>%
          mutate(year = year(fecha), quarter = floor_date(fecha, unit = "year") + months(3*quarter(fecha)-1))%>%
          dplyr::filter(nombre == input$municipioEco, fecha <= input$fechasEco[2], fecha >= input$fechasEco[1])%>%
          group_by(quarter, nombre_origen)%>%
          summarise(n = sum(cantidad), year = max(year), .groups = 'drop')%>%
          ggplot()+
          geom_area(aes(x=quarter, y=n, fill = nombre_origen), colour = "black", alpha = 0.75)+
          suppressWarnings(geom_point(aes(x=quarter, y=n, fill = nombre_origen, text = paste0("Cantidad (€); ", n, "<br>Origen de ingresos: ", nombre_origen, "<br>Cuatrimestre: ", year, " Q", quarter(quarter))), position = position_stack(), colour="black", pch=21, size=2))+
          scale_fill_manual(values = c("#FFAE00", "#6A8D90", "#B3B3B3", "#C7B8E7", "#A0D1FF", "#EFB5B9", "#D9B300", "#479AA1"))+
          theme_bw()+
          theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
          labs(y = "Cantidad", x = "Fecha")
        , tooltip = "text")%>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    } else {
      ggplotly(
        ingresos%>%
          mutate(year = year(fecha), quarter = floor_date(fecha, unit = "year") + months(3*quarter(fecha)-1))%>%
          dplyr::filter(nombre == input$municipioEco, fecha <= input$fechasEco[2], fecha >= input$fechasEco[1])%>%
          group_by(year, nombre_origen)%>%
          summarise(n = sum(cantidad), .groups = 'drop')%>%
          ggplot()+
          geom_area(aes(x=year, y=n, fill = nombre_origen), colour = "black", alpha = 0.75)+
          suppressWarnings(geom_point(aes(x=year, y=n, fill = nombre_origen, text = paste0("Cantidad (€); ", n, "<br>Origen de ingresos: ", nombre_origen, "<br>Fecha: ", year)), position = position_stack(), colour="black", pch=21, size=2))+
          scale_fill_manual(values = c("#FFAE00", "#6A8D90", "#B3B3B3", "#C7B8E7", "#A0D1FF", "#EFB5B9", "#D9B300", "#479AA1"))+
          theme_bw()+
          theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
          labs(y = "Cantidad", x = "Fecha")
        , tooltip = "text")%>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    }
  })

# UI Ambiental --------

  # Indicador calidad del aire
  output$indiceAire <- renderUI({
    calidad <- tabla24%>%
      dplyr::filter(nombre == input$municipioAmb, fecha == max(fecha))%>%
      pull(indice_aire)

      if(calidad <= 50){
        box(width = 3, id = "foo", title = NULL, headerBorder = F,
            div(class = "encabezadoIndicador",
                div(class = "tituloIndicador",
                    "Índice de Calidad del Aire"
                )
            ),
            div(class="cuerpoIndicador",
                img(src="aire.webp", class = "imgIndicador"),
                p(calidad, class = "num", id = "valorCalidad"),
                p("Buena", class = "prc", style = "color: rgb(0, 228, 0);", id = "textoCalidad")
            )
        )
      } else if (calidad <= 100){
        box(width = 3, id = "foo", title = NULL, headerBorder = F,
            div(class = "encabezadoIndicador",
                div(class = "tituloIndicador",
                    "Índice de Calidad del Aire"
                )
            ),
            div(class="cuerpoIndicador",
                img(src="aire.webp", class = "imgIndicador"),
                p(calidad, class = "num", id = "valorCalidad"),
                p("Moderada", class = "prc", style = "color: rgb(255, 255, 0);", id = "textoCalidad")
            )
        )
      } else if (calidad <= 150){
        box(width = 3, id = "foo", title = NULL, headerBorder = F,
            div(class = "encabezadoIndicador",
                div(class = "tituloIndicador",
                    "Índice de Calidad del Aire"
                )
            ),
            div(class="cuerpoIndicador",
                img(src="aire.webp", class = "imgIndicador"),
                p(calidad, class = "num", id = "valorCalidad"),
                p("Perjudicial para Grupos Sensibles", class = "prc", style = "color: rgb(255, 126, 0);", id = "textoCalidad")
            )
        )
      } else if (calidad <= 200){
        box(width = 3, id = "foo", title = NULL, headerBorder = F,
            div(class = "encabezadoIndicador",
                div(class = "tituloIndicador",
                    "Índice de Calidad del Aire"
                )
            ),
            div(class="cuerpoIndicador",
                img(src="aire.webp", class = "imgIndicador"),
                p(calidad, class = "num", id = "valorCalidad"),
                p("Perjudicial", class = "prc", style = "color: rgb(255, 0, 0);", id = "textoCalidad")
            )
        )
      } else if (calidad <= 300){
        box(width = 3, id = "foo", title = NULL, headerBorder = F,
            div(class = "encabezadoIndicador",
                div(class = "tituloIndicador",
                    "Índice de Calidad del Aire"
                )
            ),
            div(class="cuerpoIndicador",
                img(src="aire.webp", class = "imgIndicador"),
                p(calidad, class = "num", id = "valorCalidad"),
                p("Muy Perjudicial", class = "prc", style = "color: rgb(143, 63, 151);", id = "textoCalidad")
            )
        )
      } else {
        box(width = 3, id = "foo", title = NULL, headerBorder = F,
            div(class = "encabezadoIndicador",
                div(class = "tituloIndicador",
                    "Índice de Calidad del Aire"
                )
            ),
            div(class="cuerpoIndicador",
                img(src="aire.webp", class = "imgIndicador"),
                p(calidad, class = "num", id = "valorCalidad"),
                p("Peligrosa", class = "prc", style = "color: rgb(126, 0, 35);", id = "textoCalidad")
            )
        )
      }
  })

  # Indicadores de nivel de ruido y cantidad de residuos
  output$indicadoresAmb <- renderUI({
    ruido <-tabla25%>%
      dplyr::filter(nombre == input$municipioAmb, fecha == max(fecha))
    residuos <- tabla26%>%
      dplyr::filter(nombre == input$municipioAmb, fecha == max(fecha))

    fluidRow(id = "fila",
             box(width = 2, title = NULL, headerBorder = F, id = "fooAmb",
                 div(class = "centro",
                     p("Nivel de ruido 4h", class = "numAmb"),
                     p(ruido$ruido4, class = "prcAmb"),
                     p("decibelios", class = "varAmb")
                 )
             ),
             box(width = 2, title = NULL, headerBorder = F, id = "fooAmb",
                 div(class = "centro",
                     p("Nivel de ruido 12h", class = "numAmb"),
                     p(ruido$ruido12, class = "prcAmb"),
                     p("decibelios", class = "varAmb")
                 )
             ),
             box(width = 2, title = NULL, headerBorder = F, id = "fooAmb",
                 div(class = "centro",
                     p("Nivel de ruido 20h", class = "numAmb"),
                     p(ruido$ruido20, class = "prcAmb"),
                     p("decibelios", class = "varAmb")
                 )
             ),
             box(width = 2, title = NULL, headerBorder = F, id = "fooAmb",
                 div(class = "centro",
                     p("Residuos de envases", class = "numAmb"),
                     p(residuos$envases, class = "prcAmb"),
                     p("toneladas", class = "varAmb")
                 )
             ),
             box(width = 2, title = NULL, headerBorder = F, id = "fooAmb",
                 div(class = "centro",
                     p("Residuos de cartón", class = "numAmb"),
                     p(residuos$papel, class = "prcAmb"),
                     p("toneladas", class = "varAmb")
                 )
             ),
             box(width = 2, title = NULL, headerBorder = F, id = "fooAmb",
                 div(class = "centro",
                     p("Residuos orgánicos", class = "numAmb"),
                     p(residuos$organico, class = "prcAmb"),
                     p("toneladas", class = "varAmb")
                 )
             )
    )
  })

# Gráficos Ambiental --------

  #Gráfico de líneas de Gases de Efecto Invernadero
  output$efectoInvernadero <- renderPlotly({
    ggplotly(
      tabla27%>%
        dplyr::filter(nombre == input$municipioAmb, fecha <= input$fechasAmb[2], fecha >= input$fechasAmb[1])%>%
        pivot_longer(cols = -c(fecha, nombre), names_to = "Contaminante")%>%
      ggplot()+
        geom_line(aes(x=fecha, y=value, color = Contaminante))+
        suppressWarnings(geom_point(aes(x=fecha, y=value, color = Contaminante, text = paste0("Concentración (ppm); ", value, "<br>Contaminante: ", Contaminante, "<br>Fecha: ", fecha))))+
        scale_color_manual(values = c("#FFAE00", "#6A8D90", "#B3B3B3", "#C7B8E7", "#A0D1FF", "#EFB5B9", "#D9B300", "#479AA1"))+
        theme_bw()+
        theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
        labs(y = "Medición (ppm)", x = "Fecha")
      , tooltip = "text")%>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
  })

  #Gráfico de líneas de niveles de ruido
  output$ruido <- renderPlotly({
    ggplotly(
      tabla28%>%
        dplyr::filter(nombre == input$municipioAmb, fecha <= input$fechasAmb[2], fecha >= input$fechasAmb[1])%>%
        pivot_longer(cols = -c(fecha, nombre), names_to = "Hora")%>%
        ggplot()+
        geom_line(aes(x=fecha, y=value, color = Hora))+
        suppressWarnings(geom_point(aes(x=fecha, y=value, color = Hora, text = paste0("Nivel de ruido (dB); ", value, "<br>Hora de la medición: ", Hora, "<br>Fecha: ", fecha))))+
        scale_color_manual(values = c("#EFB5B9", "#D9B300", "#479AA1"))+
        theme_bw()+
        theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
        labs(y = "Medición (dB)", x = "Fecha")
      , tooltip = "text")%>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
  })

  #Gráfico de líneas de cantidad de residuos
  output$residuos <- renderPlotly({
    ggplotly(
      tabla29%>%
        dplyr::filter(nombre == input$municipioAmb, fecha <= input$fechasAmb[2], fecha >= input$fechasAmb[1])%>%
        pivot_longer(cols = -c(fecha, nombre), names_to = "Tipo")%>%
        ggplot()+
        geom_area(aes(x=fecha, y=value, fill = Tipo), colour = "black", alpha = 0.75)+
        suppressWarnings(geom_point(aes(x=fecha, y=value, fill = Tipo, text = paste0("Cantidad de residuos (T); ", value, "<br>Tipo de residuo: ", Tipo, "<br>Fecha: ", fecha)), position = position_stack(), colour="black",pch=21, size=2))+
        scale_fill_manual(values = c("#B3B3B3", "#C7B8E7", "#FFAE00"))+
        theme_bw()+
        theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
        labs(y = "Cantidad (T)", x = "Fecha")
    , tooltip = "text")%>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "lasso2d", "hoverClosestCartesian", "hoverCompareCartesian"))
  })

# Metodología --------
  output$metodologiaTabla <- DT::renderDataTable(
    df_indicadores, options = list(pageLength = -1, dom = "t"), rownames = F, selection = c("none")
  )
}

shinyApp(ui = ui, server = server)
