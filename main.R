# Carga de paquetes
suppressWarnings(library(mongolite)) # get data from MongoDB
suppressWarnings(library(dplyr)) # data manipulation
suppressWarnings(library(aod)) # logistic regression odds
suppressWarnings(library(ggplot2)) # graphs
suppressWarnings(library(foreign))
suppressWarnings(library(rjson))
suppressWarnings(library(reshape2))
suppressWarnings(library(tidyr))
suppressWarnings(library(plotly))
suppressWarnings(library(zoo))
suppressWarnings(library(ISLR))
suppressWarnings(library(stringi))

# Datos de conexion a MongoDB
url_path = 'mongodb+srv://Henry:3eXoszlAIBpQzGGA@proyectobedu.jr6fz.mongodb.net/test'

# Definicion de carpeta de trabajo y conexion a base de datos MongoDB
path <- "C:/Users/BALAMLAPTOP2/Documents/GitHub/stemdatagirls"
setwd(path)

# Definicion de directorio de salida durante el proceso de descomprimir archivos se debe respetar 'enoe_sdem'
outDir <- "C:\\Users\\BALAMLAPTOP2\\Documents\\GitHub\\stemdatagirls\\data"

# Extrae los archivos .dbf de los archivos comprimidos ZIP
for (zfile in list.files(pattern = "*.zip$", recursive = TRUE)) {
  unzip(zfile, exdir = outDir)
}

colnames(rawdata[[1]]) <- c("R_DEF","LOC","MUN","EST","EST_D","EST_D_MEN","AGEB","T_LOC","T_LOC_MEN","CD_A","ENT","CON","UPM","D_SEM","N_PRO_VIV","V_SEL","N_HOG","H_MUD","N_ENT","PER","N_REN","C_RES","PAR_C","SEX","EDA","NAC_DIA","NAC_MES","NAC_ANIO","L_NAC_C","CS_P12","CS_P13_1","CS_P13_2","CS_P14_C","CS_P15","CS_P16","CS_P17","N_HIJ","E_CON","CS_AD_MOT","CS_P20_DES","CS_AD_DES","CS_NR_MOT","CS_P22_DES","CS_NR_ORI","UR","ZONA","SALARIO","FAC","FAC_MEN","CLASE1","CLASE2","CLASE3","POS_OCU","SEG_SOC","RAMA","C_OCU11C","ING7C","DUR9C","EMPLE7C","MEDICA5C","BUSCAR5C","RAMA_EST1","RAMA_EST2","DUR_EST","AMBITO1","AMBITO2","TUE1","TUE2","TUE3","BUSQUEDA","D_ANT_LAB","D_CEXP_EST","DUR_DES","SUB_O","S_CLASIFI","REMUNE2C","PRE_ASA","TIP_CON","DISPO","NODISPO","C_INAC5C","PNEA_EST","NIV_INS","EDA5C","EDA7C","EDA12C","EDA19C","HIJ5C","DOMESTICO","ANIOS_ESC","HRSOCUP","INGOCUP","ING_X_HRS","TPG_P8A","TCCO","CP_ANOC","IMSSISSSTE","MA48ME1SM","P14APOYOS","SCIAN","T_TRA","EMP_PPAL","TUE_PPAL","TRANS_PPAL","MH_FIL2","MH_COL","SEC_INS","TIPO","MES_CAL","CA","DEQUIEN")
colnames(rawdata[[2]]) <- c("R_DEF","LOC","MUN","EST","EST_D","EST_D_MEN","AGEB","T_LOC","T_LOC_MEN","CD_A","ENT","CON","UPM","D_SEM","N_PRO_VIV","V_SEL","N_HOG","H_MUD","N_ENT","PER","N_REN","C_RES","PAR_C","SEX","EDA","NAC_DIA","NAC_MES","NAC_ANIO","L_NAC_C","CS_P12","CS_P13_1","CS_P13_2","CS_P14_C","CS_P15","CS_P16","CS_P17","N_HIJ","E_CON","CS_AD_MOT","CS_P20_DES","CS_AD_DES","CS_NR_MOT","CS_P22_DES","CS_NR_ORI","UR","ZONA","SALARIO","FAC","FAC_MEN","CLASE1","CLASE2","CLASE3","POS_OCU","SEG_SOC","RAMA","C_OCU11C","ING7C","DUR9C","EMPLE7C","MEDICA5C","BUSCAR5C","RAMA_EST1","RAMA_EST2","DUR_EST","AMBITO1","AMBITO2","TUE1","TUE2","TUE3","BUSQUEDA","D_ANT_LAB","D_CEXP_EST","DUR_DES","SUB_O","S_CLASIFI","REMUNE2C","PRE_ASA","TIP_CON","DISPO","NODISPO","C_INAC5C","PNEA_EST","NIV_INS","EDA5C","EDA7C","EDA12C","EDA19C","HIJ5C","DOMESTICO","ANIOS_ESC","HRSOCUP","INGOCUP","ING_X_HRS","TPG_P8A","TCCO","CP_ANOC","IMSSISSSTE","MA48ME1SM","P14APOYOS","SCIAN","T_TRA","EMP_PPAL","TUE_PPAL","TRANS_PPAL","MH_FIL2","MH_COL","SEC_INS","TIPO","MES_CAL","CA")


# Lectura de todos los archivos .dbf en la carpeta del proyecto
rawdata <- lapply(list.files(pattern = "*.dbf$", recursive = TRUE), read.dbf)

# Extraccion de atributos considerados para el modelo de regresion logistica y lineal. 
selecteddata <- lapply(rawdata, select, c("R_DEF","LOC","MUN","EST","AGEB","CD_A","ENT","CON","UPM","D_SEM","N_PRO_VIV","V_SEL","N_HOG","H_MUD","N_ENT","PER","N_REN","C_RES","PAR_C","SEX","EDA","NAC_DIA","NAC_MES","NAC_ANIO","L_NAC_C","CS_P12","CS_P13_1","CS_P13_2","CS_P14_C","CS_P15","CS_P16","CS_P17","N_HIJ","E_CON","CS_AD_MOT","CS_P20_DES","CS_AD_DES","CS_NR_MOT","CS_P22_DES","CS_NR_ORI","UR","ZONA","SALARIO","FAC","CLASE1","CLASE2","CLASE3","POS_OCU","SEG_SOC","RAMA","C_OCU11C","ING7C","DUR9C","EMPLE7C","MEDICA5C","BUSCAR5C","RAMA_EST1","RAMA_EST2","DUR_EST","AMBITO1","AMBITO2","TUE1","TUE2","TUE3","BUSQUEDA","D_ANT_LAB","D_CEXP_EST","DUR_DES","SUB_O","S_CLASIFI","REMUNE2C","PRE_ASA","TIP_CON","DISPO","NODISPO","C_INAC5C","PNEA_EST","NIV_INS","EDA5C","EDA7C","EDA12C","EDA19C","HIJ5C","DOMESTICO","ANIOS_ESC","HRSOCUP","INGOCUP","ING_X_HRS","TPG_P8A","TCCO","CP_ANOC","IMSSISSSTE","MA48ME1SM","P14APOYOS","SCIAN","T_TRA","EMP_PPAL","TUE_PPAL","TRANS_PPAL","MH_FIL2","MH_COL","SEC_INS"))

for (i in 1:length(selecteddata)){
  per <- as.character(selecteddata[[i]][1,'PER'])
  selecteddata[[i]]$TRI <- substring(per,1,1)
  selecteddata[[i]]$ANIO <- as.numeric(paste("20", substring(per,2,3), sep = ""))
}



for (i in 1:length(rawdata)){
  if("FAC" %in% colnames(rawdata[[i]])){
    print("OK")
  } else {
    print(as.character(rawdata[[i]][1,'PER']))
  }
}


for (j in 19:20) {
  data_enoe <- NA
  for (i in 1:length(selecteddata)){
    if (grepl(paste(as.character(j),'$', sep = ""), as.character(selecteddata[[i]][1,'PER']))){
      data_enoe <- rbind(data_enoe, selecteddata[[i]])
    }
  }
  write.csv(data_enoe,paste("data_enoe_",as.character(j),".csv", sep = ""),fileEncoding = "UTF-8")
}

# Construccion de data frame y cambio de nombres
data_enoe <- do.call(rbind, selecteddata)
colnames(data_enoe) <- c("cve_ent", "cve_mun", "sex", "eda", "niv_ins", "rama", "clase2", "per")

# Se omiten valores NaN dentro de la base de datos.
data_enoe <- na.omit(data_enoe)

# Se omiten los registros de las personas no economicamente activas y no dispibles [clase2 == 4], 
# a los menores de 15 aÃ±os y mayores de 65 aÃ±os [eda <= 15 & eda >= 65] y aquellos sin nivel educativo registrado [niv_ins == 5]

data_enoe <- data_enoe[data_enoe$clase2 <= 3 & 
                         data_enoe$clase2 != 0 & 
                         data_enoe$eda >= 15 & 
                         data_enoe$eda <= 65 &
                         data_enoe$niv_ins <= 4, ]

# Se establece una conexion a MongoDB y se cargan todos los datos en la coleccion 'data_enoe'
mongo <- mongo(collection = "data_enoe", db = "bedu18", url = url_path, verbose = TRUE)
mongo$insert(data_enoe)

selecteddata[[1]] %>%
  filter(TUE2==6,
         EDA7C>=1,
         EDA7C<=6) %>% 
  group_by(SEX, EDA7C) %>% 
  dplyr::summarise(tdhr= sum(wt=FAC)) %>% 
  mutate(text = paste("Total de trabajadoras del hogar: ", tdhr, sep=""))%>%
  ggplot(aes(x = as.factor(EDA7C), y = tdhr, fill=as.factor(SEX), text=text)) +
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = scales::comma) +
  #geom_text(aes(label = tdhr), vjust = -0.2, colour = "black",position = position_dodge(.9))+
  scale_fill_manual(values = c("cadetblue3","blueviolet")) +
  labs(title = "",
       fill = "Sexo",
       y="",
       x="")

nrows <- 0
for (i in 1:length(selecteddata)){
  nrows <- nrows + dim(selecteddata[[i]])[1]
}
data_enoe <- data_enoe[data_enoe$clase2 <= 3 & 
                         data_enoe$clase2 != 0 & 
                         data_enoe$eda >= 15 & 
                         data_enoe$eda <= 65 &
                         data_enoe$niv_ins <= 4, ]
#data_enoe_filtered <- NA

for (i in 1:length(selecteddata)){
  if (selecteddata[[i]][1,'ANIO'] %in% c(2015,2016,2017,2018,2019,2020)){
    print(i)
    if (i == 1){
      data_enoe_filtered <- selecteddata[[i]] %>% 
        filter(CLASE2 <= 3, CLASE2 != 0, NIV_INS <= 4) %>% 
        group_by(ANIO, MUN, ENT, SEX, E_CON, NIV_INS, EDA19C, HIJ5C, SCIAN, CLASE2) %>% 
        dplyr::summarise(FAC= sum(wt=FAC), avgsalario = mean(SALARIO))
    } else {
      aux <- selecteddata[[i]] %>% 
        filter(CLASE2 <= 3, CLASE2 != 0, NIV_INS <= 4) %>%
        group_by(ANIO, MUN, ENT, SEX, E_CON, NIV_INS, EDA19C, HIJ5C, SCIAN, CLASE2) %>% 
        dplyr::summarise(FAC= sum(wt=FAC), avgsalario = mean(SALARIO))
      data_enoe_filtered <- rbind(data_enoe_filtered, aux)
    }
  }
}

for (i in 1:length(selecteddata)){
  if (selecteddata[[i]][1,'ANIO'] %in% c(2015,2016,2017,2018,2019,2020)){
    print(i)
    if (i == 1){
      data_enoe_filtered <- selecteddata[[i]] %>% 
        filter(CLASE2 <= 3, CLASE2 != 0, NIV_INS <= 4) %>% 
        group_by(ANIO, EDA, SEX, E_CON, NIV_INS, SCIAN, CLASE2) %>% 
        dplyr::summarise(FAC= sum(wt=FAC), avgsalario = mean(SALARIO))
    } else {
      aux <- selecteddata[[i]] %>% 
        filter(CLASE2 <= 3, CLASE2 != 0, NIV_INS <= 4) %>%
        group_by(ANIO, EDA, SEX, E_CON, NIV_INS, SCIAN, CLASE2) %>% 
        dplyr::summarise(FAC= sum(wt=FAC), avgsalario = mean(SALARIO))
      data_enoe_filtered <- rbind(data_enoe_filtered, aux)
    }
  }
}


write.csv(data_enoe_filtered, 'data_enoe_5years.csv')
View(data_enoe_filtered %>% group_by(ANIO) %>% 
  dplyr::summarise(n= n()))


data_enoe_filtered <- na.omit(data_enoe_filtered)

data_enoe_filtered$CLASE2[data_enoe_filtered$CLASE2 == 1] <- 0 # No desempleados

data_enoe_filtered$CLASE2[data_enoe_filtered$CLASE2 == 2 | data_enoe_filtered$CLASE2 == 3] <- 1 # Desempleados abiertos

# Variable dicotomica sexo

data_enoe_filtered$SEX[data_enoe_filtered$SEX == 1] <- 0 # Hombre

data_enoe_filtered$SEX[data_enoe_filtered$SEX == 2] <- 1 # Mujer

# Visualizaciones
# TOTAL DE TRABAJADORES EN STEM 2015 - 2020
data_enoe_filtered$SEX <- as.numeric(data_enoe_filtered$SEX)
data_enoe_filtered %>%
  filter(CLASE2 == 1) %>% 
  group_by(SEX, EDA) %>% 
  dplyr::summarise(tdhr= sum(wt=FAC)) %>% 
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
       x="Edad")

data_enoe_filtered %>%
  filter(SCIAN %in% c(1,2,3,4,5,9,10,11,12,13,16), CLASE2 == 0) %>% 
  group_by(SEX, EDA) %>% 
  dplyr::summarise(tdhr= sum(wt=FAC)) %>% 
  mutate(text = paste("Total de trabajadores en STEM: ", tdhr, sep=""))%>%
  mutate(SEX = replace(SEX, SEX == 1,'Hombre'))%>%
  mutate(SEX = replace(SEX, SEX == 2,'Mujer'))%>%
  ggplot(aes(x = as.factor(EDA), y = tdhr, fill=as.factor(SEX), text=text)) +
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = scales::comma) +
  #geom_text(aes(label = tdhr), vjust = -0.2, colour = "black",position = position_dodge(.9))+
  scale_fill_manual(values = c("cadetblue3","blueviolet")) +
  labs(title = "",
       fill = "Sexo",
       y="Total de trabajadores en STEM",
       x="Edad")

data_enoe_filtered %>%
  filter(SCIAN %in% c(1,2,3,4,5,9,10,11,12,13,16), CLASE2 == 0) %>% 
  group_by(SEX, ANIO) %>% 
  dplyr::summarise(tdhr= sum(wt=FAC)) %>% 
  mutate(text = paste("Total de trabajadores en STEM: ", tdhr, sep=""))%>%
  mutate(SEX = replace(SEX, SEX == 1,'Masculino'))%>%
  mutate(SEX = replace(SEX, SEX == 2,'Femenino'))%>%
  ggplot(aes(x = as.factor(ANIO), y = tdhr, fill=as.factor(SEX), text=text)) +
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = scales::comma) +
  #geom_text(aes(label = tdhr), vjust = -0.2, colour = "black",position = position_dodge(.9))+
  scale_fill_manual(values = c("blueviolet","cadetblue3")) +
  labs(title = "",
       fill = "Género",
       y="Total de trabajadores STEM",
       x="Año")

data_enoe_filtered %>%
  filter(CLASE2 == 1) %>% 
  group_by(SEX, ANIO) %>% 
  dplyr::summarise(tdhr= sum(wt=FAC)) %>% 
  mutate(text = paste("Total de trabajadores en STEM: ", tdhr, sep=""))%>%
  mutate(SEX = replace(SEX, SEX == 1,'Masculino'))%>%
  mutate(SEX = replace(SEX, SEX == 2,'Femenino'))%>%
  ggplot(aes(x = as.factor(ANIO), y = tdhr, fill=as.factor(SEX), text=text)) +
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = scales::comma) +
  #geom_text(aes(label = tdhr), vjust = -0.2, colour = "black",position = position_dodge(.9))+
  scale_fill_manual(values = c("blueviolet","cadetblue3")) +
  labs(title = "",
       fill = "Género",
       y="Total de desempleados abiertos",
       x="Año")

data_enoe_filtered %>%
  filter(CLASE2 == 0) %>% 
  group_by(SEX, SCIAN) %>% 
  dplyr::summarise(tdhr= sum(wt=FAC)) %>% 
  mutate(text = paste("Total de trabajadores en STEM: ", tdhr, sep=""))%>%
  ggplot(aes(x = as.factor(SCIAN), y = tdhr, fill=as.factor(SEX), text=text)) +
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = scales::comma) +
  #geom_text(aes(label = tdhr), vjust = -0.2, colour = "black",position = position_dodge(.9))+
  scale_fill_manual(values = c("cadetblue3","blueviolet")) +
  labs(title = "",
       fill = "Sexo",
       y="Total de trabajadores",
       x="Actividad económica")

chart_scian_sex <- data_enoe_filtered %>% filter(CLASE2 == 0, SCIAN != 21) %>% group_by(SEX, SCIAN) %>% dplyr::summarise(tdhr= sum(wt=FAC))
chart_scian_sex$SCIAN <- as.character(chart_scian_sex$SCIAN)
# chart_scian_sex$SEX <- as.character(chart_scian_sex$SEX)
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

chart_scian_sex <- chart_scian_sex %>% mutate(SEX = replace(SEX, SEX == 1,'Masculino'))
chart_scian_sex <- chart_scian_sex %>% mutate(SEX = replace(SEX, SEX == 2,'Femenino'))

chart_scian_sex %>% ggplot(aes(x = as.factor(SCIAN), y = tdhr, fill=as.factor(SEX))) +
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = scales::comma) +
  #geom_text(aes(label = tdhr), vjust = -0.2, colour = "black",position = position_dodge(.9))+
  scale_fill_manual(values = c("blueviolet", "cadetblue3")) +
  labs(title = "",
       fill = "Género",
       y="Total de empleados",
       x="Actividades económicas") + coord_flip()
#  + scale_x_discrete(guide = guide_axis(angle = 90))

# Variable categorica
data_enoe_filtered <- sapply(data_enoe_filtered, as.numeric)

data_enoe_filtered <- as.data.frame(data_enoe_filtered)
data_enoe_filtered$EDA <- as.numeric(data_enoe_filtered$EDA)

data_enoe_filtered$NIV_INS <- factor(data_enoe_filtered$NIV_INS)
data_enoe_filtered$E_CON <- factor(data_enoe_filtered$E_CON)
data_enoe_filtered$SCIAN <- factor(data_enoe_filtered$SCIAN)
data_enoe_filtered$SEX <- as.numeric(data_enoe_filtered$SEX)

# Caracteristicas iniciales

names(data_enoe_filtered)

str(data_enoe_filtered)

head(data_enoe_filtered)

summary(data_enoe_filtered)

# Regresion Logistica

mylogit120 <- glm(CLASE2 ~ SEX + EDA + NIV_INS, data = data_enoe_filtered, family = "binomial")

summary(mylogit120)

# Prueba de Wald: Para saber el efecto de la variable categorica

wald.test(b = coef(mylogit120), Sigma = vcov(mylogit120), Terms = 4:6)

# H0: El efecto de la variable categorica no es estadisticamente significativo
# Resultado: P-value< 0.15, por tanto, se rechaza H0.

# Radios de probabilidad e intervalos de confianza al 95%

# exp(cbind(OR = coef(mylogit120), confint(mylogit120)))

# Calculo de probabilidades

probmean120 <- with(data_enoe_filtered, data.frame(SEX = mean(SEX), EDA = mean(EDA), NIV_INS = factor(1:4)))

probmean120$NIV_INSP <- predict(mylogit120, newdata = probmean120, type = "response")

probmean120

mean(as.numeric(probmean120$NIV_INSP))
# Probabilidad de estar desempleado a nivel nacional: 0.1387203

probdec120 <- with(data_enoe_filtered, data.frame(SEX = mean(SEX), EDA = rep(seq(from = 15, to = 65, length.out = 10),
                                                                      4), NIV_INS = factor(rep(1:4, each = 10))))

probdec120n <- cbind(probdec120, predict(mylogit120, newdata = probdec120, type = "link",
                                         se = TRUE))
probdec120n<- within(probdec120n, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

probdec120n

# Grafica de probabilidades 
probdec120n$NIV_INS <- as.character(probdec120n$NIV_INS)
chart_probability <- probdec120n %>% mutate(NIV_INS = replace(NIV_INS, NIV_INS == '1','Primaria incompleta')) %>% 
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

####### MODELO DIFERENTE
data_enoe_filtered$SEX <- as.factor(data_enoe_filtered$SEX)

mylogit120 <- glm(CLASE2 ~ SEX + EDA, data = data_enoe_filtered, family = "binomial")

summary(mylogit120)

# Calculo de probabilidades

probmean120 <- with(data_enoe_filtered, data.frame(SEX = factor(1:2), EDA = mean(EDA)))

probmean120$SEX <- predict(mylogit120, newdata = probmean120, type = "response")

probmean120

mean(as.numeric(probmean120$NIV_INSP))
# Probabilidad de estar desempleado a nivel nacional: 0.1387203

probdec120 <- with(data_enoe_filtered, data.frame(SEX = factor(1:2), EDA = rep(seq(from = 15, to = 65, length.out = 10),
                                                                             4)))

probdec120n <- cbind(probdec120, predict(mylogit120, newdata = probdec120, type = "link",
                                         se = TRUE))
probdec120n<- within(probdec120n, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

probdec120n

# Grafica de probabilidades 
probdec120n$SEX <- as.character(probdec120n$SEX)
chart_probability <- probdec120n %>% mutate(SEX = replace(SEX, SEX == '1','Masculino')) %>% 
  mutate(SEX = replace(SEX,SEX == '2','Femenino')) 

ggplotly(ggplot(chart_probability, aes(x = EDA, y = PredictedProb)) 
         # + ggtitle("Desempleo abierto 2020.1") 
         + geom_ribbon(aes(ymin = LL, ymax = UL, fill = SEX), alpha = 0.2) 
         + geom_line(aes(colour = SEX), size = 1) 
         + labs(title = "Desempleo abierto 2015 - 2020",
                fill = "Género",
                y="Probabilidad de desempleo",
                x="Edad")
) 

# Prueba de ajuste del modelo

with(mylogit120, null.deviance - deviance)

with(mylogit120, df.null - df.residual)

with(mylogit120, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
# Ho: Regresion lineal es mejor que regresion logistica
# P-value: 0, se recha la hipotesis nula

