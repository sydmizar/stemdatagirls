#### Data stem #####
rm(list=ls())  

paq = c("haven", "ggplot2", "viridis", "dplyr", "data.table" , "rgdal", "sf",
        "plotly", "scales",  "leaflet", "proj4", "tidyr", "purrr", "varhandle")
install.packages(paq)
lapply(paq, library, character.only = TRUE) 

dd = read.csv2("/Users/lilialopez/Desktop/enoe2020.csv")

#Población en STEM y años de escolaridad
dd %>% 
  group_by(sex, anios_esc) %>% 
  filter(per.x == 120,anios_esc>=0L & anios_esc<=98L) %>% 
  summarise(total=sum(wt=fac_tri.x),
            ingreso=mean(ingocup)) %>%  
  mutate(Ingreso=round(ingreso,2)) %>%
  mutate(Población=round(total,0)) %>% #Aquí termina la transformación de variables
  mutate(text = paste("Sexo: ", sex,
                      "\nIngreso mensual : ", Ingreso, 
                      "\nPoblación: ", Población, 
                      "\nAños de escolaridad formal: ", anios_esc, sep="")) %>%
  ggplot(aes(x=as_factor(anios_esc), y=Ingreso, size = Población, color = as_factor(sex), text=text)) +
  geom_point(alpha=1) +
  scale_size(range = c(1.4, 19), name="Sexo") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  labs(y="Ingreso mensual promedio",
       x= "Años de educación formal adquirida") +
  theme_minimal() +
  theme(legend.position="left") -> sex_esc 
ggplotly(sex_esc, tooltip = "text")


#Población en STEM y estado civil
dd %>% 
  filter(per.x == 120) %>%
  group_by(sex, e_con) %>% 
  summarise(total=sum(wt=fac_tri.x),
            ingreso=mean(ingocup)) %>%  
  mutate(Ingreso=round(ingreso,2)) %>%
  mutate(Población=round(total,0)) %>% #Aquí termina la transformación de variables
  mutate(text = paste("Sexo: ", sex,
                      "\nIngreso mensual : ", Ingreso, 
                      "\nPoblación: ", Población, 
                      "\nEstado civil: ", e_con, sep="")) %>%
  ggplot(aes(x=as_factor(e_con), y=Ingreso, size = Población, color = as_factor(sex), text=text)) +
  geom_point(alpha=1) +
  scale_size(range = c(1.4, 19), name="Sexo") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  labs(y="Ingreso mensual promedio",
       x= "Estado civil") +
  theme_minimal() +
  theme(legend.position="left") -> sex_edociv
ggplotly(sex_edociv, tooltip = "text")


#Población en STEM y sector económico
dd %>% 
  filter(per.x == 120,
         rama>=1,
         rama<=6) %>%
  group_by(sex, rama) %>% 
  summarise(total=sum(wt=fac_tri.x),
            ingreso=mean(ingocup)) %>%  
  mutate(Ingreso=round(ingreso,2)) %>%
  mutate(Población=round(total,0)) %>% #Aquí termina la transformación de variables
  mutate(text = paste("Sexo: ", sex,
                      "\nIngreso mensual : ", Ingreso, 
                      "\nPoblación: ", Población, 
                      "\nSector económico: ", rama, sep="")) %>%
  ggplot(aes(x=as_factor(rama), y=Ingreso, size = Población, color = as_factor(sex), text=text)) +
  geom_point(alpha=1) +
  scale_size(range = c(1.4, 19), name="Sexo") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  labs(y="Ingreso mensual promedio",
       x= "Sector económico") +
  theme_minimal() +
  theme(legend.position="left") -> sex_secteco
ggplotly(sex_secteco, tooltip = "text")


#Población en STEM y condición de ocupación
dd %>% 
  filter(per.x == 120,
         c_ocu11c>=1,
         c_ocu11c<=10) %>%
  group_by(sex, c_ocu11c) %>% 
  summarise(total=sum(wt=fac_tri.x),
            ingreso=mean(ingocup)) %>%  
  mutate(Ingreso=round(ingreso,2)) %>%
  mutate(Población=round(total,0)) %>% #Aquí termina la transformación de variables
  mutate(text = paste("Sexo: ", sex,
                      "\nIngreso mensual : ", Ingreso, 
                      "\nPoblación: ", Población, 
                      "\nCondición de ocupación: ", c_ocu11c, sep="")) %>%
  ggplot(aes(x=as_factor(c_ocu11c), y=Ingreso, size = Población, color = as_factor(sex), text=text)) +
  geom_point(alpha=1) +
  scale_size(range = c(1.4, 19), name="Sexo") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  labs(y="Ingreso mensual promedio",
       x= "Condición de ocupación") +
  theme_minimal() +
  theme(legend.position="left") -> sex_ocupa
ggplotly(sex_ocupa, tooltip = "text")

#Población en STEM y unidad económica
dd %>% 
  filter(per.x == 120,
         tue2>=1,
         tue2<=7) %>%
  group_by(sex, tue2) %>% 
  summarise(total=sum(wt=fac_tri.x),
            ingreso=mean(ingocup)) %>%  
  mutate(Ingreso=round(ingreso,2)) %>%
  mutate(Población=round(total,0)) %>% #Aquí termina la transformación de variables
  mutate(text = paste("Sexo: ", sex,
                      "\nIngreso mensual : ", Ingreso, 
                      "\nPoblación: ", Población, 
                      "\nUnidad económica: ", tue2, sep="")) %>%
  ggplot(aes(x=as_factor(tue2), y=Ingreso, size = Población, color = as_factor(sex), text=text)) +
  geom_point(alpha=1) +
  scale_size(range = c(1.4, 19), name="Sexo") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  labs(y="Ingreso mensual promedio",
       x= "Unidad económica") +
  theme_minimal() +
  theme(legend.position="left") -> sex_ueco
ggplotly(sex_ueco, tooltip = "text")


#Población en STEM y rango de edad
dd %>% 
  filter( per.x == 120,
          eda7c>=1,
          eda7c<=6) %>% 
  group_by(sex, eda7c) %>% 
  summarise(total=sum(wt=fac_tri.x),
            ingreso=mean(ingocup)) %>%  
  mutate(Ingreso=round(ingreso,2)) %>%
  mutate(Población=round(total,0)) %>% #Aquí termina la transformación de variables
  mutate(text = paste("Sexo: ", sex,
                      "\nIngreso mensual : ", Ingreso, 
                      "\nPoblación: ", Población, 
                      "\nRango de edad: ", eda7c, sep="")) %>%
  ggplot(aes(x=as_factor(eda7c), y=Ingreso, size = Población, color = as_factor(sex), text=text)) +
  geom_point(alpha=1) +
  scale_size(range = c(1.4, 19), name="Sexo") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  labs(y="Ingreso mensual promedio",
       x= "Rango de edad") +
  theme_minimal() +
  theme(legend.position="left") -> sex_edad
ggplotly(sex_edad, tooltip = "text")

#Población en STEM y prestaciones sin considerar las de salud
dd %>% 
  filter( per.x == 120,
          pre_asa>=1,
          pre_asa<=2) %>%
  group_by(sex, pre_asa) %>% 
  summarise(total=sum(wt=fac_tri.x),
            ingreso=mean(ingocup)) %>%  
  mutate(Ingreso=round(ingreso,2)) %>%
  mutate(Población=round(total,0)) %>% #Aquí termina la transformación de variables
  mutate(text = paste("Sexo: ", sex,
                      "\nIngreso mensual : ", Ingreso, 
                      "\nPoblación: ", Población, 
                      "\nPrestaciones: ", pre_asa, sep="")) %>%
  ggplot(aes(x=as_factor(pre_asa), y=Ingreso, size = Población, color = as_factor(sex), text=text)) +
  geom_point(alpha=1) +
  scale_size(range = c(1.4, 19), name="Sexo") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  labs(y="Ingreso mensual promedio",
       x= "Prestaciones") +
  theme_minimal() +
  theme(legend.position="left") -> sex_prestnosalud
ggplotly(sex_prestnosalud, tooltip = "text")

#Población en STEM y prestaciones de salud
dd %>% 
  filter(per.x == 120) %>%
  group_by(sex, medica5c) %>% 
  summarise(total=sum(wt=fac_tri.x),
            ingreso=mean(ingocup)) %>%  
  mutate(Ingreso=round(ingreso,2)) %>%
  mutate(Población=round(total,0)) %>% #Aquí termina la transformación de variables
  mutate(text = paste("Sexo: ", sex,
                      "\nIngreso mensual : ", Ingreso, 
                      "\nPoblación: ", Población, 
                      "\nPrestaciones de salud: ", medica5c, sep="")) %>%
  ggplot(aes(x=as_factor(medica5c), y=Ingreso, size = Población, color = as_factor(sex), text=text)) +
  geom_point(alpha=1) +
  scale_size(range = c(1.4, 19), name="Sexo") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  labs(y="Ingreso mensual promedio",
       x= "Prestaciones de salud") +
  theme_minimal() +
  theme(legend.position="left") -> sexo_salud
ggplotly(sexo_salud, tooltip = "text")


#Población en STEM por entidad
dd %>% 
  filter(per.x == 120) %>%
  group_by(sex, ent) %>% 
  summarise(total=sum(wt=fac_tri.x),
            ingreso=mean(ingocup)) %>%  
  mutate(Ingreso=round(ingreso,2)) %>%
  mutate(Población=round(total,0)) %>% #Aquí termina la transformación de variables
  mutate(text = paste("Sexo: ", sex,
                      "\nIngreso mensual : ", Ingreso, 
                      "\nPoblación: ", Población, 
                      "\nEstado de la República: ", ent, sep="")) %>%
  ggplot(aes(x=as_factor(ent), y=Ingreso, size = Población, color = as_factor(sex), text=text)) +
  geom_point(alpha=1) +
  scale_size(range = c(1.4, 19), name="Sexo") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  labs(y="Ingreso mensual promedio",
       x= "Estado de la República") +
  theme_minimal() +
  theme(legend.position="left") -> sexo_estado
ggplotly(sexo_estado, tooltip = "text")


#Población en STEM y horas trabajadas en la semana
dd %>% 
  filter(per.x == 120) %>%
  group_by(sex, hrsocup) %>% 
  summarise(total=sum(wt=fac_tri.x),
            ingreso=mean(ingocup),
            horas=mean(hrsocup)) %>% 
  mutate(Ingreso=round(ingreso,2)) %>%
  mutate(Horas=round(horas,0)) %>%
  mutate(Población=round(total,0)) %>% #Aquí termina la transformación de variables
  mutate(text = paste("Sexo: ", sex,
                      "\nIngreso mensual : ", Ingreso, 
                      "\nPoblación: ", Población, 
                      "\nHoras trabajadas: ", Horas , sep="")) %>%
  ggplot(aes(x=as_factor(Horas), y=Ingreso, size = Población, color = as_factor(sex), text=text)) +
  geom_point(alpha=1) +
  scale_size(range = c(1.4, 19), name="Sexo") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  labs(y="Ingreso mensual promedio",
       x= "Horas trabajadas") +
  theme_minimal() +
  theme(legend.position="left") -> sexo_horas
ggplotly(sexo_horas, tooltip = "text")
