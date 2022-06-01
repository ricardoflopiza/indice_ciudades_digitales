library(tidyverse)
library(rvest)
library(gsubfn)
library(mgsub)

fix_text <- function(data) {
  a = c(" ", "-", "á","é","í","ó","ú")
  b= c("_", "", "a", "e", "i", "o", "u")
  data =  mgsub(data, a,b)
  data = tolower(data)
  return(data)
}

# writexl::write_xlsx(unidad_funcional,"tablas/unidad_funcional_densidad.xlsx")
unidad_funcional_dens <- readxl::read_excel("tablas/unidad_funcional_densidad.xlsx")
names(unidad_funcional_dens) <- names(unidad_funcional_dens) %>% tolower() %>% fix_text()
unidad_funcional_dens <- map_df(unidad_funcional_dens,tolower)

unidad_funcional_dens$densidad_km2 <- unidad_funcional_dens$densidad_km2 %>% str_replace_all(",","\\.")

unidad_funcional_dens[,c("a2015","a2016","a2017","a2018","a2019","a2020","a2021","cut","superficie_km2","densidad_km2")] <- map_df(unidad_funcional_dens[,c("a2015","a2016","a2017","a2018","a2019","a2020","a2021","cut","superficie_km2","densidad_km2")],
                                                                                                                                   as.numeric)

# #### import D1 medio ambiente  ####
D1_MA_data_vars <- readxl::read_excel("tablas/D1_medioambiente_v2 - Calculosv2.xlsx",sheet = 2)
D1_MA_datos <- readxl::read_excel("tablas/D1_medioambiente_v2 - Calculosv2.xlsx",sheet = "D1- Dato Ok", skip = 5)
D1_MA_datos <- D1_MA_datos[1:101,1:21]
D1_MA_datos <- map_df(D1_MA_datos, tolower)
names(D1_MA_datos) <- names(D1_MA_datos) %>% str_replace_all(" ","_") %>% tolower()

# #### import D2_movilidad y transporte_v2  ####
D2_MT_data_vars <- readxl::read_excel("tablas/D2_movilidad y transporte_v2 - Calculosv2.xlsx",sheet = 1)
D2_MT_datos <- readxl::read_excel("tablas/D2_movilidad y transporte_v2 - Calculosv2.xlsx",sheet = "D2- Dato Ok", skip = 5)
D2_MT_datos <- D2_MT_datos[1:101,1:27]
D2_MT_datos <- map_df(D2_MT_datos, tolower)
names(D2_MT_datos) <- names(D2_MT_datos) %>% str_replace_all(" ","_") %>% tolower()

# #### import D3_planificación y desarrollo urbano  ####
D3_PD_data_vars <- readxl::read_excel("tablas/D3_planificación y desarrollo urbano_v2 - Calculosv2.xlsx",sheet = 1)
D3_PD_datos <- readxl::read_excel("tablas/D3_planificación y desarrollo urbano_v2 - Calculosv2.xlsx",sheet = "D3- Dato Ok", skip = 5)
D3_PD_datos <- D3_PD_datos[1:101,1:17]
D3_PD_datos <- map_df(D3_PD_datos, tolower)
names(D3_PD_datos) <- names(D3_PD_datos) %>% str_replace_all(" ","_") %>% tolower()

# #### import D3_planificación y desarrollo urbano  ####
D4_DE_data_vars <- readxl::read_excel("tablas/D4_desarrollo economico_v2 - Calculosv3.xlsx",sheet = 1)
D4_DE_datos <- readxl::read_excel("tablas/D4_desarrollo economico_v2 - Calculosv3.xlsx",sheet = "D4- Dato Ok", skip = 5)
D4_DE_datos <- D4_DE_datos[1:101,1:17]
D4_DE_datos <- map_df(D4_DE_datos, tolower)
names(D4_DE_datos) <- names(D4_DE_datos) %>% str_replace_all(" ","_") %>% tolower()

# #### import D3_planificación y desarrollo urbano  ####
D5_GCP_data_vars <- readxl::read_excel("tablas/D5_gobernanza y participacion ciudadana_v2 - Calculosv2.xlsx",sheet = 1)
D5_GCP_datos <- readxl::read_excel("tablas/D5_gobernanza y participacion ciudadana_v2 - Calculosv2.xlsx",sheet = "D5- Dato Ok", skip = 5)
D5_GCP_datos <- D5_GCP_datos[1:101,1:12]
D5_GCP_datos <- map_df(D5_GCP_datos, tolower)
names(D5_GCP_datos) <- names(D5_GCP_datos) %>% str_replace_all(" ","_") %>% tolower()

# #### import D3_planificación y desarrollo urbano  ####
D6_DH_data_vars <- readxl::read_excel("tablas/D6_desarrollo humano_v2 final - Calculosv2.xlsx",sheet = 1)
D6_DH_datos <- readxl::read_excel("tablas/D6_desarrollo humano_v2 final - Calculosv2.xlsx",sheet = "D6- Dato Ok", skip = 5)
D6_DH_datos <- D6_DH_datos[1:101,]
D6_DH_datos <- map_df(D6_DH_datos, tolower)
names(D6_DH_datos) <- names(D6_DH_datos) %>% str_replace_all(" ","_") %>% tolower()

### eliminamos tildes para
D1_MA_datos[,1:2] <- map_df(D1_MA_datos[,1:2], fix_text)
D2_MT_datos[,1:2] <- map_df(D2_MT_datos[,1:2], fix_text)
D3_PD_datos[,1:2] <- map_df(D3_PD_datos[,1:2], fix_text)
D4_DE_datos[,1:2] <- map_df(D4_DE_datos[,1:2], fix_text)
D5_GCP_datos[,1:2] <- map_df(D5_GCP_datos[,1:2], fix_text)
D6_DH_datos[,1:2] <- map_df(D6_DH_datos[,1:2], fix_text)

### unificamos datos de variables

# writexl::write_xlsx(data_vars,"datos_variables.xlsx")
data_vars <- readxl::read_xlsx("datos_variables.xlsx")

## unimos datos indicadores#

indicadores <- D1_MA_datos %>%
  left_join(D2_MT_datos, by = c("región","unidad_urbana_funcional","municipio")) %>%
  left_join(D3_PD_datos, by = c("región","unidad_urbana_funcional","municipio")) %>%
  left_join(D4_DE_datos, by = c("región","unidad_urbana_funcional","municipio")) %>%
  left_join(D5_GCP_datos, by = c("región","unidad_urbana_funcional","municipio")) %>%
  left_join(D6_DH_datos, by = c("región","unidad_urbana_funcional","municipio"))

### procesamos datos de indicadores
map(indicadores,table)

#indicadores2 <- indicadores
indicadores[,-c(1:3)] <- map_df(indicadores[,-c(1:3)],as.numeric)

unidad_funcional_dens[,-c(1:3)] <- map_df(unidad_funcional_dens[,-c(1:3)],as.numeric)

## unificamos indicadores con unidades funcionales y sus datos de caracterizaron
indicadores <- indicadores %>%
  left_join(unidad_funcional_dens)

pob_reg <- readxl::read_xlsx("pob_regional.xlsx")

indicadores <- indicadores %>%
  left_join(pob_reg,by = "región")

table(indicadores$región)

#### comenzamos a construir funciópn
var = "m02a"
datos = indicadores

### función para normalizar el zscore
scaling_zscore = function(var,max_val = 15){

  var1 <- sort(var,decreasing = T)

  max1 <- max(var1)

  vect <- c()

  for(i in 1:length(var1)){

    if(var1[i] == max1){
      vect[i] = max_val
    }else{
      vect[i] = (vect[i-1]-(var1[i-1]-var1[i]))
    }
  }

  df <- data.frame(var0 = var1,
                   vect = vect)

  df <- df[match(var,df$var0),]

  return(df$vect)
}

# var = "m02a"
# force_inv = "no"
# z_norm = "own"
# max_val = 15
#########################################################
#### función general para normalizar todos los datos ####
#########################################################

var = "m02a"
force_inv = "no"
z_norm = "own"
max_val = 15

### función inicial ####
normalizar = function(var,datos,minmax = FALSE,z = FALSE, z_norm = NULL,max_val = 15, force_inv = NULL){

  ### extraemos el valor si es necesario invertir la escala dependiendo del tipo de indicador
  invertir = data_vars$Inversion[data_vars$Código == var]

  ## argumento para forzar inversion
  if(!is.null(force_inv)){
    invertir = force_inv
  }

  ### extraemos el valor si es ponderar por población region o por comuna
  if(data_vars$Poblacion[data_vars$Código == var] == "regional"){
    agno <-paste0("a",data_vars$Año[data_vars$Código == var],"_reg")

  }else{
    agno <-paste0("a",data_vars$Año[data_vars$Código == var])
  }

  peso <- paste0("peso",data_vars$Año[data_vars$Código == var])

  # ponderación poblacional
  #print((datos[var] /datos[agno]*1000)[[1]])

  pond_pob <- (((datos[var] /datos[agno]*1000)[[1]])*datos[peso])[[1]]

  ### minmax = T , devuelve indicador con minmax
  if(minmax == T){

    min <- min(pond_pob, na.rm = T)
    max <- max(pond_pob, na.rm = T)

    #### hay que invertirlo?
    if(invertir == "si"){
      ret <- abs((pond_pob - min) / (max-min)-1)
    }else if(invertir == "no"){
      ret <- (pond_pob - min) / (max-min)
    }
  }

  ### z = T , devuelve indicador en zscore

  if(z == T){
    sd = sd(pond_pob,na.rm = T)
    mean = mean(pond_pob, na.rm = T)

    ret <- (pond_pob-mean)/sd
  }

  ## z_norm != null , devuelve indicador en zscore normalizado con dos opciones, por minmax o por el método propio
  if(!is.null(z_norm)){

    sd = sd(pond_pob,na.rm = T)
    mean = mean(pond_pob, na.rm = T)

    zscore <- (pond_pob-mean)/sd

    #### normalizamos z con minmax, pero no funciona bien
    if(z_norm == "minmax"){
      zscore <- (zscore - min(zscore, na.rm = T)) / (max(zscore, na.rm = T)-min(zscore, na.rm = T))

      #### hay que invertirlo?
      if(invertir == "si"){
        ret <- abs(zscore-1)
      }else if(invertir == "no"){
        ret <- zscore
     }
  }

    if(z_norm == "own"){

      # #### hay que invertirlo?
      if(invertir == "si"){
        ret <- scaling_zscore(zscore*-1,max_val = max_val)
      }else if(invertir == "no"){
        ret <- scaling_zscore(zscore,max_val = max_val)
      }
    }
  }

  if(minmax == F & z == F & is.null(z_norm)){
    ret <- pond_pob
  }

  return(round(ret,2))
}

### creamos función para ponderar por población  ####
ponderar_procesar = function(var,datos){

  ### extraemos el valor si es necesario invertir la escala dependiendo del tipo de indicador
  invertir = data_vars$Inversion[data_vars$Código == var]

  ## argumento para forzar inversion
  if(!is.null(force_inv)){
    invertir = force_inv
  }

  ### extraemos el valor si es ponderar por población region o por comuna
  if(data_vars$Poblacion[data_vars$Código == var] == "regional"){
    agno <-paste0("a",data_vars$Año[data_vars$Código == var],"_reg")

  }else{
    agno <-paste0("a",data_vars$Año[data_vars$Código == var])
  }

  peso <- paste0("peso",data_vars$Año[data_vars$Código == var])

  # ponderación poblacional
  #print((datos[var] /datos[agno]*1000)[[1]])

  pond_pob <- round((((datos[var] /datos[agno]*1000)[[1]])*datos[peso])[[1]],4)

  pond_pob
}

variables <- names(indicadores[-c(1:3)])
variables <- variables[1:100]

