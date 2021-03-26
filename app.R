library(shinydashboard)
library(mongolite)
library(plotly)
library(tidyr)
library(zoo)
library(ggplot2)
library(shiny)
library(dplyr)
library(aod)
library(leaflet)
library(rgdal)
library(haven)
library(ggplot2)
library(viridis)
library(data.table)
library(rgdal)
library(sf)
library(plotly)
library(scales)
library(proj4)
library(tidyr)
library(purrr)
library(varhandle)

#rm(list=ls())  
# 
# paq = c("haven", "ggplot2", "viridis", "dplyr", 
#         "data.table" , "rgdal", "sf",
#         "plotly", "scales",  "leaflet", 
#         "proj4", "tidyr", "purrr", "varhandle")
# install.packages(paq)
# lapply(paq, library, character.only = TRUE) 

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "purple"



dd = read.csv2("data/enoe2020.csv")
dd = dd %>%
  mutate(Sex = case_when(sex == 1 ~ "Hombre", sex == 2 ~ "Mujer"))

# imssData <- read.csv("data/data_imss.csv", header = TRUE)
# 
# AllDataENOE <- read.csv("data/data_enoe.csv", header = TRUE)
# AllDataENOE <- AllDataENOE[AllDataENOE$clase2 <= 3 & 
#                              AllDataENOE$clase2 != 0 & 
#                              AllDataENOE$eda >= 15 & 
#                              AllDataENOE$eda <= 65 &
#                              AllDataENOE$niv_ins <= 4, ]
# 
# allImssCovidData <- read.csv("data/data_imss_covid.csv", header = TRUE)
# 
# 
# allCovidData <- read.csv("data/data_covid.csv", header = TRUE) 
# monthCovImss.nacional <- read.csv("data/monthcovimss_nacional.csv", header = TRUE) 
# monthCovidImss.data <- read.csv("data/monthcovidimss_data.csv", header = TRUE) 
# mexico <- rgdal::readOGR("data/mexico.json")


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Análisis Exploratorio", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Modelos", icon = icon("bar-chart-o"),
             menuSubItem("Regresión Logística", tabName = "subitem1"),
             menuSubItem("Calculadora de Desempleo", tabName = "subitem2")
    ),
    menuItem("Código fuente App", icon = icon("file-code-o"),
             href = "https://github.com/sydmizar/stemdatagirls/app.R"
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            fluidRow(
              box(
                title = "Mujeres en STEM",
                width = "100%",
                solidHeader = TRUE,
                background = "purple",
                "Introduccion"
              )
            ),
            fluidRow(
              box(
                title = "Población en STEM y años de escolaridad",
                status = "primary",
                plotlyOutput("aniosescolaridad", height = 400),
                height = 460
                # width = "100%"
              ),
              box(
                title = "Población en STEM y estado civil",
                status = "primary",
                plotlyOutput("estadocivil", height = 400),
                height = 460
                # width = "100%"
              )
              #estadocivil, sectoreconomico, condocupacion
            ),
            fluidRow(
              box(
                title = "Población en STEM y unidad económica",
                status = "primary",
                plotlyOutput("unidadeconomica", height = 400),
                height = 460
                # width = "100%"
              ),
              box(
                title = "Población en STEM y rango de edad",
                status = "primary",
                plotlyOutput("rangoedad", height = 400),
                height = 460
                # width = "100%"
              )
            ),
            fluidRow(
              box(
                title = "Población en STEM y prestaciones sin considerar las de salud",
                status = "primary",
                plotlyOutput("prestacionsinsalud", height = 400),
                height = 460
                # width = "100%"
              ),
              box(
                title = "Población en STEM y prestaciones de salud",
                status = "primary",
                plotlyOutput("prestacionsalud", height = 400),
                height = 460
                # width = "100%"
              )
            ),
            fluidRow(
              box(
                title = "Población en STEM y horas trabajadas en la semana",
                status = "primary",
                plotlyOutput("horas", height = 400),
                height = 460
                # width = "100%"
              )
            )
            # fluidRow(
            #   box(
            #     title = "Factores de Impacto en el Desempleo y la Recuperación Económica en México",
            #     width = "100%",
            #     solidHeader = TRUE,
            #     background = "light-blue",
            #     "Debido a la pandemia SARS-COV-2 y el distanciamiento social implementado por los gobiernos, se ha presentado la mayor caída del empleo que se haya registrado en México y en el mundo. Entre Marzo y Julio de 2020 se perdieron más de un millón 117 mil 854 trabajos tanto formales como informales."
            #   )
            # ),
            # 
            # fluidRow(
            #   box(
            #     title = "Evolución del empleo en México 2019 - 2020",
            #     status = "primary",
            #     plotlyOutput("evolucion", height = 400),
            #     height = 460
            #     # width = "100%"
            #   ),
            #   box(
            #     title = "Evolución de número de empleados y casos diarios detectados con COVID-19",
            #     status = "primary",
            #     plotlyOutput("covidimss", height = 400),
            #     height = 460
            #     # width = "100%"
            #   )
            # ),
            # 
            # fluidRow(
            #   box(
            #     title = "Número de casos positivos con COVID-19 por entidad",
            #     status = "primary",
            #     leafletOutput("mapMexico"),
            #     #height = 700,
            #     width = "100%"
            #   )
            # ),
            # 
            # fluidRow(
            #   box(
            #     title = "Desempleo abierto por género y trimestre",
            #     status = "primary",
            #     plotlyOutput("generoperiodo", height = 400),
            #     height = 460
            #     # width = "100%"
            #   ),
            #   box(
            #     title = "Población con o sin empleo por trimestre",
            #     status = "primary",
            #     selectInput("periodografica", "",
            #                 choices = c("Enero-Marzo" = 120, "Abril-Junio" = 220, "Julio-Septiembre" = 320),
            #                 selected = "120"
            #     ),
            #     plotlyOutput("periodoempleo", height = 300),
            #     height = 460
            #     # width = "100%"
            #   )
            # ),
            # 
            # fluidRow(
            #   box(
            #     title = "Población económicamente activa y inactiva por nivel educativo",
            #     status = "primary",
            #     plotlyOutput("niveleduc", height = 400),
            #     height = 460
            #     # width = "100%"
            #   ),
            #   box(
            #     title = "Población económicamente activa e inactiva por edad y nivel educativo",
            #     status = "primary",
            #     plotlyOutput("edadnivel", height = 400),
            #     height = 460
            #     # width = "100%"
            #   )
            # )
            
        
    ),
    tabItem("subitem1"
            #     fluidRow(
            #   box(
            #     title = "Probabilidad asociada al desempleo abierto según características sociodemográficas",
            #     width = "100%",
            #     solidHeader = TRUE,
            #     background = "light-blue",
            #     "La regresión logística (logistic regression) calcula la probabilidad de pertenecer al desempleo abierto según características sociodemográficas, tales como: edad, sexo y nivel educativo."
            #   )
            # ),
            # fluidRow(
            #     box(
            #       title = "Periodo trimestral: ",
            #       solidheader = TRUE, status = "warning",
            #       width = "100%",
            #       selectInput("periodo", "",
            #                   choices = c("Enero-Marzo" = 120, "Abril-Junio" = 220, "Julio-Septiembre" = 320),
            #                   selected = "120"
            #       )
            #     )
            # ),
            # 
            # fluidRow(
            #   box(
            #     title = "Probabilidad de ser desempleado abierto, según edad y nivel educativo",
            #     status = "primary",
            #     width = "100%",
            #     plotlyOutput("regLogit", height = 400),
            #     height = 460
            #   )
            # ),
            # fluidRow(
            #   box(
            #     width = "100%",
            #     verbatimTextOutput("mylogit")
            #   )
            # ),
            # fluidRow(
            #   box(
            #     width = "100%",
            #     verbatimTextOutput("wald")
            #   )
            # )
            
    ),
    tabItem("subitem2"
            
            # fluidRow(
            #   box(
            #     title = "",
            #     solidheader = TRUE, status = "warning",
            #     numericInput("edad", "Edad: ", value = 15, min = 15, max = 65, step = 1),
            #     selectInput("genero", "Género: ",
            #                 choices = c("Hombre" = 1, "Mujer" = 2),
            #                 selected = "1"
            #     ),
            #     selectInput("nivins", "Nivel Educativo: ",
            #                 choices = c("Primaria Incompleta" = 1, "Primaria Completa" = 2, "Secundaria Completa" = 3, "Medio superior y superior" = 4),
            #                 selected = "1"
            #     ),
            #   ),
            #   box(
            #     title = "Probabilidad de Desempleo",
            #     solidheader = TRUE,
            #     status = "warning",
            #     textOutput("probddesempleo")
            #   ),
            #   valueBoxOutput("rate", width = 6)
            # )
            
            )
  )
)

header <- dashboardHeader(
  title = "Mujeres en STEM",
  titleWidth = 250
  # messages,
  # notifications,
  # tasks
)

ui <- dashboardPage(header, sidebar, body, skin = skin)

server <- function(input, output, session) {

  dataenoe <- reactive({
    DataENOE120 <- AllDataENOE[AllDataENOE$per == input$periodo, ]
    
    # Varible dicotomica de desempleo 
    DataENOE120$clase2[DataENOE120$clase2 == 1] <- 0 # No desempleados
    DataENOE120$clase2[DataENOE120$clase2 == 2 | DataENOE120$clase2 == 3] <- 1 # Desempleados abiertos
    
    # Variable dicotomica sexo
    DataENOE120$sex[DataENOE120$sex == 1] <- 0 # Hombr
    DataENOE120$sex[DataENOE120$sex == 2] <- 1 # Mujer
    
    
    # Variable categ?rica
    DataENOE120$niv_ins <- factor(DataENOE120$niv_ins)
    
    # DataENOE120$niv_ins <- factor(DataENOE120$niv_ins, levels=c(1, 2, 3, 4),
    #                             labels = c('Primaria incompleta','Primaria completa','Secundaria incompleta', 'Medio superior y superior'))

    
    return(DataENOE120)
    
  })
  
  
  mylogit <- reactive({
    # Logistic regression
    DataENOE120 <- dataenoe()
    mylogit120 <- glm(clase2 ~ sex + eda + niv_ins, data = DataENOE120, family = "binomial")
    
    return(mylogit120)
  })
  
  probdec <- reactive({
    mylogit120 <- mylogit()
    DataENOE120 <- dataenoe()
    
    # Probabilidad de estar desempleado a nivel nacional: 0.1117514
    
    probdec120 <- with(DataENOE120, data.frame(sex = mean(sex), eda = rep(seq(from = 15, to = 65, length.out = 10),
                                                                          4), niv_ins = factor(rep(1:4, each = 10))))
    
    probdec120n <- cbind(probdec120, predict(mylogit120, newdata = probdec120, type = "link",
                                             se = TRUE))
    probdec120n<- within(probdec120n, {
      PredictedProb <- plogis(fit)
      LL <- plogis(fit - (1.96 * se.fit))
      UL <- plogis(fit + (1.96 * se.fit))
    })
    return(probdec120n)
  })
  
  probdesempleo <- reactive({
    DataENOE120 <- dataenoe()
    mylogit120 <- mylogit()
    probdec120 <- with(DataENOE120, data.frame(sex = as.numeric(input$genero), eda = input$edad, niv_ins = input$nivins))
    probdec120n <- cbind(probdec120, predict(mylogit120, newdata = probdec120, type = "link",
                                             se = TRUE))
    probdec120n<- within(probdec120n, {
      PredictedProb <- plogis(fit)
      LL <- plogis(fit - (1.96 * se.fit))
      UL <- plogis(fit + (1.96 * se.fit))
    })
    
    
    return(probdec120n)
  })
  
  reglin <- reactive({
    trainCovImss.data <- monthCovidImss.data %>% dplyr::mutate(mun = as.factor(imun), 
                                                               casos = casos_diarios_prom, 
                                                               muertes = muertos_diarios_prom, 
                                                               hosp = hospitalizados_diarios_prom,
                                                               tasa = tasa_empleabilidad
    ) %>% 
      dplyr::select(mun, casos, muertes, hosp, tasa)
    
    # Working on this
    covImss.lm <- lm(tasa ~ . - mun, trainCovImss.data) # bad results
    return(covImss.lm)
  })
  
  output$aniosescolaridad <- renderPlotly({
    dd %>% 
      group_by(Sex, anios_esc) %>% 
      filter(anios_esc>=0L & anios_esc<=98L) %>% 
      summarise(total=sum(wt=fac_tri.x),
                ingreso=mean(ingocup)) %>%  
      mutate(Ingreso=round(ingreso,2)) %>%
      mutate(Poblacion=round(total,0)) %>% 
      mutate(text = paste("Sexo: ", Sex,
                          "\nIngreso mensual promedio : ", Ingreso, 
                          "\nPoblación: ", Poblacion, 
                          "\nAños de escolaridad formal: ", anios_esc, sep="")) %>%
      ggplot(aes(x=as_factor(anios_esc), y=Ingreso, size = Poblacion, color = as_factor(Sex), text=text)) +
      geom_point() +
      scale_colour_manual(values = c("cadetblue3","blueviolet") ) +
      labs( color = "Sexo",
            y="Ingreso mensual promedio",
            x= "Años de educación formal adquirida",
            caption = "ENOE 2020. INEGI") +
      theme_minimal() +
      theme(legend.position="left") -> sex_esc 
    ggplotly(sex_esc, tooltip = "text")
  })
  
  output$estadocivil <- renderPlotly({
    dd %>% 
      filter(e_con<=6) %>%
      group_by(Sex, e_con) %>% 
      summarise(total=sum(wt=fac_tri.x),
                ingreso=mean(ingocup)) %>%  
      mutate(Ingreso=round(ingreso,2)) %>%
      mutate(Poblacion=round(total,0)) %>% 
      mutate(text = paste("Sexo: ", Sex,
                          "\nIngreso mensual promedio : ", Ingreso, 
                          "\nPoblación: ", Poblacion, 
                          "\nEstado civil: ", e_con, sep="")) %>%
      ggplot(aes(x=as_factor(e_con), y=Ingreso, size = Poblacion, color = as_factor(Sex), text=text)) +
      geom_point() +
      scale_colour_manual(values = c("cadetblue3","blueviolet") ) +
      labs(color = "Sexo",
           y="Ingreso mensual promedio",
           x= "Estado civil",
           caption = "ENOE 2020. INEGI") +
      scale_x_discrete(labels=c("1" = "Unión Libre", "2" = "Separado(a)",
                                "3" = "Divorciado(a)", "4" = "Viudo(a)",
                                "5" = "Casado(a)", "6" = "Soltero(a)")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position="left") -> sex_edociv
    ggplotly(sex_edociv, tooltip = "text")
  })
    
  output$unidadeconomica <- renderPlotly({
    dd %>% 
      filter(tue2>=1,
             tue2<=7) %>%
      group_by(Sex, tue2) %>% 
      summarise(total=sum(wt=fac_tri.x),
                ingreso=mean(ingocup)) %>%  
      mutate(Ingreso=round(ingreso,2)) %>%
      mutate(Poblacion=round(total,0)) %>% 
      mutate(text = paste("Sexo: ", Sex,
                          "\nIngreso mensual promedio : ", Ingreso, 
                          "\nPoblación: ", Poblacion, 
                          "\nUnidad económica: ", tue2, sep="")) %>%
      ggplot(aes(x=as_factor(tue2), y=Ingreso, size = Poblacion, color = as_factor(Sex), text=text)) +
      geom_point() +
      scale_colour_manual(values = c("cadetblue3","blueviolet") ) +
      labs(color = "Sexo",
           y="Ingreso mensual promedio",
           x= "Unidad económica",
           caption = "ENOE 2020. INEGI") +
      scale_x_discrete(labels=c("1" = "Sociedad y corporativas", "2" = "No en sociedad",
                                "3" = "Privadas", "4" = "Públicas",
                                "5" = "Sector informal", "6" = "Trabajo doméstico rem.",
                                "7" = "Agricultura autosubsistencia")) +
      theme_minimal() +
      theme( axis.text.x = element_text(angle = 45, hjust = 1),
             legend.position="left") -> sex_ueco
    ggplotly(sex_ueco, tooltip = "text")
    
  })
  
  output$rangoedad <- renderPlotly({
    dd %>% 
      filter( eda7c>=1,
              eda7c<=6) %>% 
      group_by(Sex, eda7c) %>% 
      summarise(total=sum(wt=fac_tri.x),
                ingreso=mean(ingocup)) %>%  
      mutate(Ingreso=round(ingreso,2)) %>%
      mutate(Poblacion=round(total,0)) %>% 
      mutate(text = paste("Sexo: ", Sex,
                          "\nIngreso mensual promedio : ", Ingreso, 
                          "\nPoblación: ", Poblacion, 
                          "\nRango de edad: ", eda7c, sep="")) %>%
      ggplot(aes(x=as_factor(eda7c), y=Ingreso, size = Poblacion, color = as_factor(Sex), text=text)) +
      geom_point() +
      scale_colour_manual(values = c("cadetblue3","blueviolet") ) +
      labs( color = "Sexo",
            y="Ingreso mensual promedio",
            x= "Rango de edad",
            caption = "ENOE 2020. INEGI") +
      scale_x_discrete(labels=c("1" = "15 a 19", "2" = "20 a 29",
                                "3" = "30 a 39", "4" = "40 a 49",
                                "5" = "50 a 59", "6" = "60 y más")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="left") -> sex_edad
    ggplotly(sex_edad, tooltip = "text")
  })
  
  output$prestacionsinsalud <- renderPlotly({
    dd %>% 
      filter( pre_asa>=1,
              pre_asa<=2) %>%
      group_by(Sex, pre_asa) %>% 
      summarise(total=sum(wt=fac_tri.x),
                ingreso=mean(ingocup)) %>%  
      mutate(Ingreso=round(ingreso,2)) %>%
      mutate(Población=round(total,0)) %>% 
      mutate(text = paste("Sexo: ", Sex,
                          "\nIngreso mensual promedio : ", Ingreso, 
                          "\nPoblación: ", Población, 
                          "\nPrestaciones: ", pre_asa, sep="")) %>%
      ggplot(aes(x=as_factor(pre_asa), y=Ingreso, size = Población, color = as_factor(Sex), text=text)) +
      geom_point() +
      scale_colour_manual(values = c("cadetblue3","blueviolet") ) +
      labs( color = "Sexo",
            y="Ingreso mensual promedio",
            x= "Prestaciones",
            caption = "ENOE 2020. INEGI") +
      scale_x_discrete(labels=c("1" = "Con prestaciones", "2" = "Sin prestaciones")) +
      theme_minimal() +
      theme( axis.text.x = element_text(angle = 0, hjust = 1),
             legend.position="left") -> sex_prestnosalud
    ggplotly(sex_prestnosalud, tooltip = "text")
  })
  
  output$prestacionsalud <- renderPlotly({
    dd %>% 
      filter(medica5c>=1,
             medica5c<=4) %>%
      group_by(Sex, medica5c) %>% 
      summarise(total=sum(wt=fac_tri.x),
                ingreso=mean(ingocup)) %>%  
      mutate(Ingreso=round(ingreso,2)) %>%
      mutate(Población=round(total,0)) %>% 
      mutate(text = paste("Sexo: ", Sex,
                          "\nIngreso mensual promedio : ", Ingreso, 
                          "\nPoblación: ", Población, 
                          "\nPrestaciones de salud: ", medica5c, sep="")) %>%
      ggplot(aes(x=as_factor(medica5c), y=Ingreso, size = Población, color = as_factor(Sex), text=text)) +
      geom_point() +
      scale_colour_manual(values = c("cadetblue3","blueviolet") ) +
      labs( color = "Sexo",
            y="Ingreso mensual promedio",
            x= "Prestaciones de salud",
            caption = "ENOE 2020. INEGI") +
      scale_x_discrete(labels=c("1" = "Sin prestaciones", "2" = "Solo Inst. Salud",
                                "3" = "Inst. Salud y otras", "4" = "No acceso a inst. salud, pero sí otras")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 20, hjust = 1),
            legend.position="left") -> sexo_salud
    ggplotly(sexo_salud, tooltip = "text")
  })
  
  output$horas <- renderPlotly({
    dd %>% 
      filter(per.x == 120) %>%
      group_by(Sex, hrsocup) %>% 
      summarise(total=sum(wt=fac_tri.x),
                ingreso=mean(ingocup),
                horas=mean(hrsocup)) %>% 
      mutate(Ingreso=round(ingreso,2)) %>%
      mutate(Horas=round(horas,0)) %>%
      mutate(Población=round(total,0)) %>% #Aquí termina la transformación de variables
      mutate(text = paste("Sexo: ", Sex,
                          "\nIngreso mensual promedio : ", Ingreso, 
                          "\nPoblación: ", Población, 
                          "\nHoras trabajadas promedio: ", Horas , sep="")) %>%
      ggplot(aes(x=as_factor(Horas), y=Ingreso, size = Población, color = as_factor(Sex), text=text)) +
      geom_point() +
      scale_colour_manual(values = c("cadetblue3","blueviolet") ) +
      labs( color = "Sexo",
            y="Ingreso mensual promedio",
            x= "Horas trabajadas",
            caption = "ENOE 2020. INEGI") +
      theme_minimal() +
      theme( axis.text.x = element_text(angle = 45, hjust = 1),
             legend.position="left") -> sexo_horas
    ggplotly(sexo_horas, tooltip = "text")
  })
    
  output$evolucion <- renderPlotly({
    imssData <- imssData %>% separate(mes, into = c('anio', 'mes'), sep = '-')
    imssData$date_month <-as.Date(as.yearmon(paste(imssData$anio, "/", imssData$mes, sep=""), format="%Y/%m"))
    
    # Agrupado de los datos por el atributo fecha
    data_chart1 <- imssData %>% group_by(date_month) %>% dplyr::summarise(asegurados = sum(asegurados))
    
    # Visualización del empleo en México y su evolución mensual
    # Se resalta la mayor caída de empleos registrada en México, ocasionada principalmente por la pandemia COVID-19. 
    # Donde la tasa de ocupación entre Febrero y Julio del 2020 cayó % perdiendo mas de X millones de puestos formales como informales.
    
    plot_ly(data = data_chart1, x = ~date_month, y = ~asegurados, mode = 'lines', line = list(color = 'rgb(205, 12, 24)', width = 4)) %>% layout(title = "", xaxis = list(title = ""), yaxis = list (title = "Empleados"))
    
  })
  
  output$covidimss <- renderPlotly({
    imsscovid_chart1 <- allImssCovidData %>% group_by(mes) %>% summarise(casos = sum(casos_diarios), asegurados = sum(asegurados))
    imsscovid_chart1
    
    ay2 <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Casos positivos"
    )
    
    fig_imsscovid1 <- imsscovid_chart1 %>% plot_ly() %>% add_lines(x = ~mes, y = ~asegurados, name='Empleados') %>% add_lines(x = ~mes, y = ~casos, name='Casos positivos', yaxis = "y2") %>% layout(title = "", yaxis = list (title = "Número de empleados"), yaxis2 = ay2, xaxis = list(title="Número del mes"))
    fig_imsscovid1
  })
  
  output$generoperiodo <- renderPlotly({
    # niv_ins == 4, 
    enoe_chart1 <- AllDataENOE %>% filter(clase2 == 2 | clase2 == 3, per != 319) %>% group_by(per,sex) %>% count(sex) %>% mutate(per = as.character(per))
    enoe_chart1 <- enoe_chart1 %>% mutate(sex = replace(sex,sex==1,'Hombre')) %>% mutate(sex = replace(sex,sex==2,'Mujer'))
    fig_enoe1 <- enoe_chart1 %>% plot_ly(x = ~per,y = ~n,type = 'bar', split = ~sex) %>% layout(yaxis = list(title = "Desempleo abierto"), xaxis = list(title = "Periodo trimestral"))
    fig_enoe1
  })
  
  output$periodoempleo <- renderPlotly({
    enoe_chart3 <- AllDataENOE %>% filter(per != 319) %>% mutate(clase2 = replace(clase2,clase2 == 1,'Con empleo')) %>% mutate(clase2 = replace(clase2,clase2 == 2 | clase2 ==3,'Sin empleo'))
    enoe_chart3 <- enoe_chart3 %>% group_by(per,niv_ins,clase2) %>% count(clase2) %>% mutate(per = as.character(per))
    # fig_enoe3 <- enoe_chart3 %>% plot_ly(x = ~per,y = ~n,type = 'bar',split = ~clase2)
    # fig_enoe3
    
    fig_enoe6 <- enoe_chart3 %>% filter(per == input$periodografica) %>% plot_ly(type = 'pie', labels = ~clase2, values = ~n) 
    fig_enoe6
    
  })
  
  output$niveleduc <- renderPlotly({
    enoe_chart2 <- AllDataENOE %>% filter(per != 319) %>% group_by(niv_ins) %>% count(niv_ins)
    enoe_chart2 <- enoe_chart2 %>% mutate(niv_ins = replace(niv_ins,niv_ins == '1','Primaria incompleta'))
    enoe_chart2 <- enoe_chart2 %>% mutate(niv_ins = replace(niv_ins,niv_ins == '2','Primaria completa'))
    enoe_chart2 <- enoe_chart2 %>% mutate(niv_ins = replace(niv_ins,niv_ins == '3','Secundaria completa'))
    enoe_chart2 <- enoe_chart2 %>% mutate(niv_ins = replace(niv_ins,niv_ins == '4','Medio superior y superior'))
    fig_enoe2 <- enoe_chart2 %>% plot_ly(labels = ~niv_ins, values = ~n, type = 'pie')
    fig_enoe2
  })
  
  output$edadnivel <- renderPlotly({
    enoe_chart4 <- AllDataENOE %>% filter(per != 319) %>% select(eda,niv_ins) %>% group_by(niv_ins)
    enoe_chart4 <- enoe_chart4 %>% mutate(niv_ins = replace(niv_ins,niv_ins == '1','Primaria incompleta'))
    enoe_chart4 <- enoe_chart4 %>% mutate(niv_ins = replace(niv_ins,niv_ins == '2','Primaria completa'))
    enoe_chart4 <- enoe_chart4 %>% mutate(niv_ins = replace(niv_ins,niv_ins == '3','Secundaria completa'))
    enoe_chart4 <- enoe_chart4 %>% mutate(niv_ins = replace(niv_ins,niv_ins == '4','Medio superior y superior'))
    fig_enoe5 <- enoe_chart4 %>% plot_ly(x = ~niv_ins, y = ~eda, type = 'box') %>% layout(yaxis = list(title = "Edad"), xaxis = list(title = "Nivel educativo"))
    fig_enoe5
  })
  
  # output$x <- renderPlotly({
  #   
  # })
  
  
  output$regLogit <- renderPlotly({
    enoe_chart4 <- probdec()
    ggplotly(ggplot(enoe_chart4, aes(x = eda, y = PredictedProb)) + labs(y="Probabilidad", x = "Edad", fill = "Intervalo de confianza", col = "Nivel educativo") + geom_ribbon(aes(ymin = LL, ymax = UL, fill = niv_ins), alpha = 0.2) + geom_line(aes(colour = niv_ins), size = 1))
    
  })
  
  output$wald <- renderPrint({
    # Prueba de Wald: Para saber el efecto de la variable categ?rica
    logit <- mylogit()
    wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 4:6)
    # H0: El efecto de la variable categ?rica no es estad?sticamente significativo
    # Resultado: Pvalue< 0.05, por tanto, se rechaza H0.
  })
  
  output$coefconfint <- renderPrint({
    logit <- mylogit()
    # Radios de probabilidad e intervalos de confianza al 95%
    # exp(cbind(OR = coef(logit), confint(logit)))
  })
    
    
  output$mylogit <- renderPrint({
    summary(mylogit())
  })
  
  output$mylinreg <- renderPrint({
    summary(reglin())
  })
  
  output$probddesempleo <- renderText({
    paste("La probabilidad de estar desempleado en México en el trimestre ", input$periodo," con tu perfil es: ", probdesempleo()$PredictedProb*100, "\n(Nota: Se puede modificar el trimestre en la pestaña de regresión logística)", sep = "")
  })
  
  # output$trimestre <- renderText({
  #   paste("El modelo corresponde al trimestre: ", input$periodo, sep = )
  # })
  
  output$rate <- renderValueBox({
  
    valueBox(
      value = formatC(probdesempleo()$PredictedProb*100, digits = 1, format = "f"),
      subtitle = if (probdesempleo()$PredictedProb*100 >= 50) "Probabilidad alta" else "Probabilidad baja",
      icon = icon("area-chart"),
      color = if (probdesempleo()$PredictedProb*100 >= 50) "red" else "green"
    )
  })
  
  
  output$empleabilidad <- renderPlotly({
    ay1 <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Tasa empleabilidad"
    )
    
    ay2 <- list(
      tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "Promedio confirmados"
    )
    
    fig_imsscovid <- monthCovImss.nacional %>% plot_ly() %>% add_lines(x = ~monYear, y = ~tasa, name='Tasa de empleabilidad') %>% add_lines(x = ~monYear, y = ~casos, name='Promedio de casos diarios', yaxis = "y2") %>% layout(yaxis = list(title = "Tasa"), yaxis1 = ay1, yaxis2 = ay2, xaxis = list(title=""))

    fig_imsscovid
  })
  
  output$mapMexico <- renderLeaflet({
    allCovidData$cve_ent <- substr(allCovidData$imun,1,nchar(allCovidData$imun)-3)
    
    covid_chart1 <- allCovidData %>% group_by(cve_ent) %>% summarise(casos = sum(casos_diarios))
    covid_chart1 <- na.omit(covid_chart1)
    covid_chart1$fips <- sprintf("%02d", as.numeric(covid_chart1$cve_ent))
    covid_chart1$fips <- paste0("MX", covid_chart1$fips)
    
    mapamexico <- merge(mexico, covid_chart1, by = "fips")
    pal <- colorNumeric("viridis", NULL)
    
    leaflet(mapamexico) %>%
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1, fillColor = ~pal(log10(casos)),
    label = ~paste0(name, ": ", formatC(casos, big.mark = ","))) %>%
    addLegend(pal = pal, values = ~log10(casos), opacity = 1.0,
              labFormat = labelFormat(transform = function(x) round(10^x)))
  
  })
  
}

shinyApp(ui, server)
