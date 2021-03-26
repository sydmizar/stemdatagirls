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

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "purple"



dd = read.csv2("data/enoe2020.csv")
dd = dd %>%
  mutate(Sex = case_when(sex == 1 ~ "Hombre", sex == 2 ~ "Mujer"))

tdesempleo_abierto_edad = read.csv("data/tdesempleo_abierto_edad.csv")
templeados_edad = read.csv("data/templeados_edad.csv")
templeados_anio = read.csv("data/templeados_anio.csv")
tdesempleados_anio = read.csv("data/tdesempleados_anio.csv")
prob_sexo = read.csv("data/reglog_sexo.csv")
prob_niv_ins = read.csv("data/reglog_niv_ins.csv")


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Brecha Salarial", tabName = "brecha", icon = icon("bar-chart-o")),
    menuItem("Análisis Exploratorio", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Código fuente App", icon = icon("file-code-o"),
             href = "https://github.com/sydmizar/stemdatagirls")
    )
)

body <- dashboardBody(
  tabItems(
    tabItem("brecha",
            fluidRow(
              htmlOutput("frame")
            )
    ),
    tabItem("dashboard",
            fluidRow(
              box(
                title = "Mujeres en STEM",
                width = "100%",
                solidHeader = TRUE,
                background = "purple"
              )
            ),
            # fluidRow(
            #   box(
            #     title = "Total de Empleados por actividad económica",
            #     status = "primary",
            #     plotlyOutput("actividadeconomica", height = 400),
            #     height = 460,
            #     width = "100%"
            #   )
            # ),
            fluidRow(
              box(
                title = "Total de trabajadores en STEM por año",
                status = "primary",
                plotlyOutput("empleadosanio", height = 400),
                height = 460
                # width = "100%"
              ),
              box(
                title = "Total de desempleados abiertos por año",
                status = "primary",
                plotlyOutput("desempleadosanio", height = 400),
                height = 460
                # width = "100%"
              )
            ),
            fluidRow(
              box(
                title = "Total de trabajadores en STEM por edad",
                status = "primary",
                plotlyOutput("empleadosedad", height = 400),
                height = 460,
                width = "100%"
              )
            ),
            fluidRow(
              box(
                title = "Total de desempleados abiertos por edad",
                status = "primary",
                plotlyOutput("desempleoedad", height = 400),
                height = 460,
                width = "100%"
              )
            ),
            fluidRow(
              box(
                title = "Probabilidad de desempleo por sexo",
                status = "primary",
                plotlyOutput("relsexo", height = 400),
                height = 460
                # width = "100%"
              ),
              box(
                title = "Probabilidad de desempleo por nivel educativo",
                status = "primary",
                plotlyOutput("relnivins", height = 400),
                height = 460
                # width = "100%"
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
                height = 460,
                width = "100%"
              )
            )
    )
  )
)

header <- dashboardHeader(
  title = "Mujeres en STEM",
  titleWidth = 250
)

ui <- dashboardPage(header, sidebar, body, skin = skin)

server <- function(input, output, session) {
  
  output$frame <- renderUI({
    test <- "https://app.powerbi.com/view?r=eyJrIjoiMGNmZTA3NGItMTZmMC00MDE3LWJjNTEtYmU5NDk5ZjlmZDZmIiwidCI6IjM5MWY3M2I3LWE1OTAtNDNhZC05OGYwLThiMDUwOThhY2NjOCIsImMiOjR9"
    my_test <- tags$iframe(src=test, style='width:100%;height:100vh;')
    print(my_test)
    my_test
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
      mutate(Poblacion=round(total,0)) %>% 
      mutate(text = paste("Sexo: ", Sex,
                          "\nIngreso mensual promedio : ", Ingreso, 
                          "\nPoblación: ", Poblacion, 
                          "\nPrestaciones: ", pre_asa, sep="")) %>%
      ggplot(aes(x=as_factor(pre_asa), y=Ingreso, size = Poblacion, color = as_factor(Sex), text=text)) +
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
      mutate(Poblacion=round(total,0)) %>% 
      mutate(text = paste("Sexo: ", Sex,
                          "\nIngreso mensual promedio : ", Ingreso, 
                          "\nPoblación: ", Poblacion, 
                          "\nPrestaciones de salud: ", medica5c, sep="")) %>%
      ggplot(aes(x=as_factor(medica5c), y=Ingreso, size = Poblacion, color = as_factor(Sex), text=text)) +
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
      mutate(Poblacion=round(total,0)) %>% 
      mutate(text = paste("Sexo: ", Sex,
                          "\nIngreso mensual promedio : ", Ingreso, 
                          "\nPoblación: ", Poblacion, 
                          "\nHoras trabajadas promedio: ", Horas , sep="")) %>%
      ggplot(aes(x=as_factor(Horas), y=Ingreso, size = Poblacion, color = as_factor(Sex), text=text)) +
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
  
  output$desempleoedad <- renderPlotly({
    tdesempleo_abierto_edad %>% 
      mutate(text = paste("Total de Desempleados Abiertos ", tdhr, sep=""))%>%
      mutate(SEX = replace(SEX, SEX == 1,'Hombre'))%>%
      mutate(SEX = replace(SEX, SEX == 2,'Mujer'))%>%
      ggplot(aes(x = as.factor(EDA), y = tdhr, fill=as.factor(SEX), text=text)) +
      geom_col(position = "dodge") + 
      scale_y_continuous(labels = scales::comma) +
      #geom_text(aes(label = tdhr), vjust = -0.2, colour = "black",position = position_dodge(.9))+
      scale_fill_manual(values = c("cadetblue3","blueviolet")) +
      labs(title = "",
           fill = "Sexo",
           y="Total de Desempleados Abiertos",
           x="Edad") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
                           legend.position="left") -> cdesempleo_abierto_edad
    ggplotly(cdesempleo_abierto_edad, tooltip = "text")
  })
  
  output$empleadosedad <- renderPlotly({
    templeados_edad %>%
      mutate(text = paste("Total de trabajadores en STEM: ", tdhr, sep=""))%>%
      mutate(SEX = replace(SEX, SEX == 1,'Hombre'))%>%
      mutate(SEX = replace(SEX, SEX == 2,'Mujer'))%>%
      ggplot(aes(x = as.factor(EDA), y = tdhr, fill=as.factor(SEX), text=text)) +
      geom_col(position = "dodge") + 
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      #geom_text(aes(label = tdhr), vjust = -0.2, colour = "black",position = position_dodge(.9))+
      scale_fill_manual(values = c("cadetblue3","blueviolet")) +
      labs(title = "",
           fill = "Sexo",
           y="Total de trabajadores en STEM",
           x="Edad") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
                             legend.position="left") -> cempleados_edad
    ggplotly(cempleados_edad, tooltip = "text")
  })
  
  output$empleadosanio <- renderPlotly({
    templeados_anio %>%
      mutate(text = paste("Total de trabajadores en STEM: ", tdhr, sep=""))%>%
      mutate(SEX = replace(SEX, SEX == 1,'Hombre'))%>%
      mutate(SEX = replace(SEX, SEX == 2,'Mujer'))%>%
      ggplot(aes(x = as.factor(ANIO), y = tdhr, fill=as.factor(SEX), text=text)) +
      geom_col(position = "dodge") + 
      scale_y_continuous(labels = scales::comma) +
      #geom_text(aes(label = tdhr), vjust = -0.2, colour = "black",position = position_dodge(.9))+
      scale_fill_manual(values = c("cadetblue3","blueviolet")) +
      labs(title = "",
           fill = "Sexo",
           y="Total de trabajadores STEM",
           x="Año") -> cempleados_anio
    ggplotly(cempleados_anio, tooltip = "text")
  })
  
  output$desempleadosanio <- renderPlotly({
    tdesempleados_anio %>% 
      mutate(text = paste("Total de trabajadores en STEM: ", tdhr, sep=""))%>%
      mutate(SEX = replace(SEX, SEX == 1,'Hombre'))%>%
      mutate(SEX = replace(SEX, SEX == 2,'Mujer'))%>%
      ggplot(aes(x = as.factor(ANIO), y = tdhr, fill=as.factor(SEX), text=text)) +
      geom_col(position = "dodge") + 
      scale_y_continuous(labels = scales::comma) +
      #geom_text(aes(label = tdhr), vjust = -0.2, colour = "black",position = position_dodge(.9))+
      scale_fill_manual(values = c("cadetblue3","blueviolet")) +
      labs(title = "",
           fill = "Sexo",
           y="Total de desempleados abiertos",
           x="Año") -> cdesempleados_anio
    ggplotly(cdesempleados_anio, tooltip = "text")
    
  })
  
  output$actividadeconomica <- renderPlotly({
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '1','Agricultura, ganadería, aprovechamiento forestal, pesca y caza'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '2','Minería'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '3','Generación y distribución de electricidad, suministro de agua y gas'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '4','Construcción'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '5','Industrias manufactureras'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '6','Comercio al por mayor'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '7','Comercio al por menor'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '8','Transportes, correos y almacenamiento'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '9','Información en medios masivos'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '10','Servicios financieros y de seguros'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '11','Servicios inmobiliarios y de alquiler de bienes'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '12','Servicios profesionales, científicos y técnicos'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '13','Corporativos'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '14','Servicios de apoyo a los negocios y manejo de desechos'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '15','Servicios educativos'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '16','Servicios de salud y de asistencia social'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '17','Servicios de esparcimiento culturales y deportivos'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '18','Servicios de hospedaje y preparación de alimentos y bebidas'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '19','Otros servicios, excepto actividades gubernamentales'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '20','Actividades gubernamentales y de organismos internacionales'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SCIAN = replace(SCIAN, SCIAN == '21','No especificado'))
    
    chart_scian_sex <- chart_scian_sex %>% mutate(SEX = replace(SEX, SEX == 1,'Hombre'))
    chart_scian_sex <- chart_scian_sex %>% mutate(SEX = replace(SEX, SEX == 2,'Mujer'))
    
    chart_scian_sex %>% ggplot(aes(x = as.factor(SCIAN), y = tdhr, fill=as.factor(SEX))) +
      geom_col(position = "dodge") + 
      scale_y_continuous(labels = scales::comma) +
      #geom_text(aes(label = tdhr), vjust = -0.2, colour = "black",position = position_dodge(.9))+
      scale_fill_manual(values = c("cadetblue3","blueviolet")) +
      labs(title = "",
           fill = "Sexo",
           y="Total de empleados",
           x="Actividades económicas") + coord_flip() -> chart_scian
    ggplotly(chart_scian, tooltip = "text")
    
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
  
  output$relnivins <- renderPlotly({
    chart_probability <- prob_niv_ins %>% mutate(NIV_INS = replace(NIV_INS, NIV_INS == '1','Primaria incompleta')) %>% 
      mutate(NIV_INS = replace(NIV_INS,NIV_INS == '2','Primaria completa')) %>% 
      mutate(NIV_INS = replace(NIV_INS,NIV_INS == '3','Secundaria completa')) %>% 
      mutate(NIV_INS = replace(NIV_INS,NIV_INS == '4','Medio superior y superior'))
    
    ggplotly(ggplot(chart_probability, aes(x = EDA, y = PredictedProb)) 
             # + ggtitle("Desempleo abierto 2020.1") 
             + geom_ribbon(aes(ymin = LL, ymax = UL, fill = NIV_INS), alpha = 0.2) 
             + geom_line(aes(colour = NIV_INS), size = 1) 
             + labs(title = "Desempleo abierto 2015 - 2020",
                    fill = "Nivel educativo",
                    y="Probabilidad de desempleo",
                    x="Edad")
    ) 
  })
  
  output$relsexo <- renderPlotly({
    chart_probability <- prob_sexo %>% mutate(SEX = replace(SEX, SEX == '1','Hombre')) %>% 
      mutate(SEX = replace(SEX,SEX == '2','Mujer')) 
    
    ggplotly(ggplot(chart_probability, aes(x = EDA, y = PredictedProb)) 
             # + ggtitle("Desempleo abierto 2020.1") 
             + geom_ribbon(aes(ymin = LL, ymax = UL, fill = SEX), alpha = 0.5) 
             #+ geom_line(aes(colour = SEX), size = 1)
             + scale_fill_manual(values = c("cadetblue3", "blueviolet"))
             + labs(title = "Desempleo abierto 2015 - 2020",
                    fill = "Sexo",
                    y="Probabilidad de desempleo",
                    x="Edad")
    ) 
     
  })

}

shinyApp(ui, server)
