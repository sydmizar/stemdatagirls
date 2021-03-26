#### Data stem #####
rm(list=ls())  

paq = c("haven", "ggplot2", "viridis", "dplyr", "data.table" , "rgdal", "sf",
        "plotly", "scales",  "leaflet", "proj4", "tidyr", "purrr", "varhandle")
install.packages(paq)
lapply(paq, library, character.only = TRUE) 

dd = read.csv2("/Users/lilialopez/Desktop/enoe2020.csv")

dd = dd %>%
        mutate(Sex = case_when(sex == 1 ~ "Hombre", sex == 2 ~ "Mujer"))

#Población en STEM y años de escolaridad
dd %>% 
  group_by(Sex, anios_esc) %>% 
  filter(anios_esc>=0L & anios_esc<=98L) %>% 
  summarise(total=sum(wt=fac_tri.x),
            ingreso=mean(ingocup)) %>%  
  mutate(Ingreso=round(ingreso,2)) %>%
  mutate(Población=round(total,0)) %>% 
  mutate(text = paste("Sexo: ", Sex,
                      "\nIngreso mensual promedio : ", Ingreso, 
                      "\nPoblación: ", Población, 
                      "\nAños de escolaridad formal: ", anios_esc, sep="")) %>%
  ggplot(aes(x=as_factor(anios_esc), y=Ingreso, size = Población, color = as_factor(Sex), text=text)) +
  geom_point() +
  scale_colour_manual(values = c("cadetblue3","blueviolet") ) +
  labs( color = "Sexo",
        y="Ingreso mensual promedio",
        x= "Años de educación formal adquirida",
        caption = "ENOE 2020. INEGI") +
  theme_minimal() +
  theme(legend.position="left") -> sex_esc 
ggplotly(sex_esc, tooltip = "text")
  

#Población en STEM y estado civil
dd %>% 
  filter(e_con<=6) %>%
  group_by(Sex, e_con) %>% 
  summarise(total=sum(wt=fac_tri.x),
            ingreso=mean(ingocup)) %>%  
  mutate(Ingreso=round(ingreso,2)) %>%
  mutate(Población=round(total,0)) %>% 
  mutate(text = paste("Sexo: ", Sex,
                      "\nIngreso mensual promedio : ", Ingreso, 
                      "\nPoblación: ", Población, 
                      "\nEstado civil: ", e_con, sep="")) %>%
  ggplot(aes(x=as_factor(e_con), y=Ingreso, size = Población, color = as_factor(Sex), text=text)) +
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

#Población en STEM y unidad económica
dd %>% 
  filter(tue2>=1,
         tue2<=7) %>%
  group_by(Sex, tue2) %>% 
  summarise(total=sum(wt=fac_tri.x),
            ingreso=mean(ingocup)) %>%  
  mutate(Ingreso=round(ingreso,2)) %>%
  mutate(Población=round(total,0)) %>% 
  mutate(text = paste("Sexo: ", Sex,
                      "\nIngreso mensual promedio : ", Ingreso, 
                      "\nPoblación: ", Población, 
                      "\nUnidad económica: ", tue2, sep="")) %>%
  ggplot(aes(x=as_factor(tue2), y=Ingreso, size = Población, color = as_factor(Sex), text=text)) +
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


#Población en STEM y rango de edad
dd %>% 
  filter( eda7c>=1,
          eda7c<=6) %>% 
  group_by(Sex, eda7c) %>% 
  summarise(total=sum(wt=fac_tri.x),
            ingreso=mean(ingocup)) %>%  
  mutate(Ingreso=round(ingreso,2)) %>%
  mutate(Población=round(total,0)) %>% 
  mutate(text = paste("Sexo: ", Sex,
                      "\nIngreso mensual promedio : ", Ingreso, 
                      "\nPoblación: ", Población, 
                      "\nRango de edad: ", eda7c, sep="")) %>%
  ggplot(aes(x=as_factor(eda7c), y=Ingreso, size = Población, color = as_factor(Sex), text=text)) +
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
  
#Población en STEM y prestaciones sin considerar las de salud
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

#Población en STEM y prestaciones de salud
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


#Población en STEM y horas trabajadas en la semana
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
