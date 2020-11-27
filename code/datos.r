source(here::here("code", "pkg.r"))
#df <- read_csv(here("dts", "df.csv"))

#df %>% 
#  saveRDS(here("dts", "df.rds"))

df <- read_rds(here("dts", "df.rds"))


gf <- function(df, d = "c", var = 1){
  df %>% 
    filter(Distrito == ifelse(d == "c", "Chilca",
                              ifelse(d == "t", "El Tambo", "Huancayo"))) 
  } 
  

df1 <- df %>% 
  select(!c(zona, lugar_n, distrito)) %>% 
  mutate(gasto = reduce(
    select(., contains("gasto")), `+`
  )) %>%
  select(!contains("gasto_")) %>% 
  mutate_all(as.character) %>% 
  mutate(genero = case_when(genero == "1" ~ "Mujer",
                            TRUE ~ "Hombre")) %>% 
  pivot_longer(!c(id, Distrito)) %>% 
  mutate(variable = case_when(name == "edad" ~ "Edad",
                              name == "genero" ~ "Genero del encuestado",
                              name == "sentimiento_serguridad" ~ "Sentimiento de seguridad viviendo en su barrio",
                              name == "satisfaccion" ~ "Sentimiento de satisfaccion viviendo en su barrio",
                              name == "covid_positivo" ~ "El encustado le diagnosticaron COVID 19",
                              name == "covid_prueba" ~ "Prueba de deteccion de COVID 19",
                              name == "covid_sintomas" ~ "Tuvo algunos sintomas propios del COVID 19",
                              name == "covid_atencion_puesto_salud"~ "Acesso a atencion especializada para COVID 19", 
                              name == "covid_conocido_sintomas"~ "Estuvo en contacto con alguien sospechoso de tener COVID 19",
                              name == "covid_contacto_sintomas"~ "Estuvo en contacto con personas que tenga\nfiebre, tos o alguna dificultad para respirar",
                              name == "covid_desempleo"~ "El COVID 19 fue causante de perder su empleo temporalmente o permanentemente",
                              name == "covid_tratar"~ "Gastos para tratar el COVID 19",
                              name == "covid_prevencion" ~ "Gastos para prevenir el COVID 19",
                              name == "covid_conflictos" ~ "Conflictos en el hogar en los ultimos  meses",
                              name == "covid_internados" ~ "Numero de familiares internados por COVID 19",
                              name == "covid_muerte" ~ "Numero de familiares que murieron por COVID 19",
                              name == "covid_depresion" ~ "Sufrio de depresion en los ultimos 6 meses",
                              name == "covid_higiene" ~ "Medidas de higiene preventivas contra el COVID 19",
                              name == "servicio_agua" ~ "Acceso al servicio de agua potable por red publica en la vivienda",# porcentaje de la poblacion que tiene estos servicios basicos
                              name == "servicio_desague" ~ "Acceso al servicio de desague por red publica en la vivienda",
                              name == "servicio_telefono" ~ "Acceso al servicio de telefonia fija en la vivienda",
                              name == "servicio_internet" ~ "Acceso al servicio de internet en la vivienda",
                              name == "servicio_electricidad" ~ "Acceso al servicio de electricidad en la vivienda",
                              name == "cal_servicio_agua" ~ "Calidad del servicio de agua",
                              name == "cal_servicio_telefono" ~ "Calidad del servicio de telefonia",
                              name == "cal_servicio_internet" ~ "Calidad del servicio de internet",
                              name == "cal_servicio_electricidad" ~ "calidad del servicio de electricidad",
                              name == "cal_servicio_basura" ~ "Calidad del servicio de recoleccion de residuos solidos brindaos por la municipalidad",
                              name == "cal_servicio_seguridad" ~ "Calidad del servicio de seguridad ciudadana brindada por los serenazgos",
                              name == "cal_servicio_mant_parq" ~ "Calidad del mantenimiento de parques y jardines",
                              name == "cal_servicio_desague" ~ "Calidad del servicio de desague",
                              name == "muni_recojo_basura" ~ "La municipalidad provee el servicio de recojo de residuos solidos",
                              name == "muni_man_parques" ~ "La municipalidad hace mantenimiento de los parques y jardines",
                              name == "tiempo_comercial" ~ "Tiempo aproximado al centro comercial mas cercano",
                              name == "tiempo_cultural" ~ "Tiempo aproximado al centro cultural mas cercano",
                              name == "tiempo_escuela" ~ "Tiempo aproximado a la escuela mas cercana",
                              name == "tiempo_universidad" ~ "Tiempo aproximado a la universidad",
                              name == "tiempo_salud" ~ "Tiempo aproximado al centro de salud mas cercano",
                              name == "tiempo_aux_rap" ~ "Tiempo aproximado al puesto de auxilio rapido mas cercano",
                              name == "tiempo_via_principal" ~ "Tiempo aproximado a la via principal mas cercana",
                              name == "tiempo_iglesia" ~ "Tiempo aproximado a la iglesia mas cercana",
                              name == "vivienda_m2" ~ "Metros cuadrados de la vivienda",
                              name == "vivienda_m2_precio" ~ "Precio del metro cuadrado de la vivienda",
                              name == "vivienda_vivir_anios" ~ "Anios que vive en la vivienda actual",
                              name == "vivienda_cuartos" ~ "Numero de cuartos de la vivienda",
                              name == "vivienda_pisos" ~ "Numero de pisos de la vivienda",
                              name == "vivienda_banos" ~ "Numero de banios de la vivienda",
                              name == "valoracion_vivienda" ~ "Valor monetario de la vivienda",
                              name == "alr_calles_pavimentadas" ~ "Acceso a la cuadra paviemntadas y en buen estado",
                              name == "alr_zonas_verdes" ~ "Zonas verdes alrededor",
                              name == "alr_parques_recreativas" ~ "Paques y zonas de recreacion o deportivas",
                              name == "alr_auxilio_rap" ~ "Al rededor hay puestos de auxilio rapido",
                              name == "jefe_ocu" ~ "Ocupacion del jefe de familia",
                              name == "hogar_ingresos_cuantos" ~ "Cuantos de perciben ingresos en su familia",
                              name == "ingreso_familiar" ~ "Ingreso familiar",
                              name == "f_menor18" ~ "Composicion familiar - cantidad de menores a 18 anios",
                              name == "f_hombre" ~ "Composicion familiar - Hombres mayores a 17 anios",
                              name == "f_mujer" ~ "Composicion familiar - Mujeres mayores a 17 anioes",
                              name == "idiomas" ~ "Idiomas que habla el encuestado",
                              name == "gasto" ~ "gasto_proxy",
                              TRUE ~ paste0("falta_", name)
                              )) %>% 
  relocate(id, Distrito, name, variable, value) 
