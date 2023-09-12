# ----------------------------------------------------------------------------
#
# ECONOMÍA LABORAL - UNIVERSIDAD DEL CEMA
# SEGUNDO SEMESTRE, 2022
#
# TRABAJO PRACTICO 1
#
# -----------------------------------------------------------------------------

# PAQUETES
library(dplyr)
library(tidyverse)
library(data.table)
library(gtable)
library(ggplot2)
library(eph)
library(gt)
library(scales)


####GENERAMOS PANEL####
#Importamos la base2004 de datos que vamos a usar
base2004 <-get_microdata(year=2004, trimester=1, type="individual")
base2007 <-get_microdata(year=2007, trimester=1, type="individual")
base2011 <-get_microdata(year=2011, trimester=1, type="individual")
base2015 <-get_microdata(year=2015, trimester=1, type="individual")
base2017 <-get_microdata(year=2017, trimester=1, type="individual")
base2019 <-get_microdata(year=2019, trimester=1, type="individual")
base2022 <-get_microdata(year=2022, trimester=1, type="individual")

#Paso 1
var.ind <- c('CODUSU','NRO_HOGAR','COMPONENTE', 'ANO4','TRIMESTRE','ESTADO','REGION','CAT_OCUP','PONDERA', 'CH04', 'CH06','CH07','P21', 'NIVEL_ED')

#Paso 2  
Bases_Continua <- bind_rows(
  base2004  %>% select(var.ind),
  base2007  %>% select(var.ind),
  base2011  %>% select(var.ind),
  base2015  %>% select(var.ind), 
  base2017 %>% select(var.ind),
  base2019  %>% select(var.ind), 
  base2022  %>% select(var.ind))

#Pasos 3  y 4
Bases_Continua <-  Bases_Continua %>% 
  filter(CH06 %in% c(18:65),ESTADO !=0) %>% 
  mutate(Categoria = case_when(ESTADO == 1 ~ "Ocupados",
                               ESTADO   ==  2 ~"Desocupados"))
#Paso  5
Bases_Continua <- Bases_Continua %>% 
  mutate(Trimestre = paste(ANO4, TRIMESTRE, sep="_")) %>% 
  arrange(Trimestre) %>% 
  mutate(Id_Trimestre = match(Trimestre,unique(Trimestre)))

#Paso 6
Bases_Continua_Replica <- Bases_Continua

names(Bases_Continua_Replica)

names(Bases_Continua_Replica)[4:(length(Bases_Continua_Replica)-1)] <- 
  paste0(names(Bases_Continua_Replica)[4:(length(Bases_Continua_Replica)-1)],"_t1")

names(Bases_Continua_Replica)

#Pasos 8 y 9
Panel_Continua <- inner_join(Bases_Continua,Bases_Continua_Replica)
Panel_Continua <- Panel_Continua %>% 
  mutate(Consistencia = case_when(abs(CH06_t1-CH06) > 2 |
                                    CH04 != CH04_t1 ~ "inconsistente",
                                  TRUE ~ "consistente")) %>% 
  filter(Consistencia == "consistente")

Panel_Continua <- Panel_Continua %>% 
  mutate(rango_etario = case_when(CH06>=25 & CH06<=34  ~ "25-34",
                                  CH06>=35 & CH06<=44  ~ "35-44",
                                  CH06>=45 & CH06<=54  ~ "45-54", 
                                  CH06>=55 & CH06<=64  ~ "55-64"), 
         genero = case_when(CH04 == 1 ~ "Varón",
                            CH04 == 2 ~ "Mujer"),
         region = case_when(REGION == 01  ~ "GBA",
                            REGION == 40  ~ "Noroeste", 
                            REGION == 41  ~ "Nordeste", 
                            REGION == 42  ~ "Cuyo", 
                            REGION == 43  ~ "Pampeana", 
                            REGION == 44  ~ "Patagonia"), 
         estado_civil = case_when(CH07 == 5 & CH04 ==2 ~ "Soltera",
                                  CH07 == 1 | CH07 == 2 & CH04 ==2 ~ "Casada o unida",
                                  CH07 == 3 & CH04 ==2 ~ "Separada o divorciada",
                                  CH07 == 4 & CH04 ==2 ~ "Viuda"), 
         educacion_h = case_when(NIVEL_ED == 1 | NIVEL_ED == 2 | NIVEL_ED == 7 & CH04 == 1 ~ "Primario completo o menos",
                                 NIVEL_ED == 3 | NIVEL_ED == 4 & CH04 == 1 ~ "Secundario completo o menos",
                                 NIVEL_ED == 5 | NIVEL_ED == 6 & CH04 == 1  ~ "Terciario incompleto o más"), 
         educacion_m = case_when(NIVEL_ED == 1 | NIVEL_ED == 2 | NIVEL_ED == 7 & CH04 == 2 ~ "Primario completo o menos",
                                 NIVEL_ED == 3 | NIVEL_ED == 4 & CH04 == 2 ~ "Secundario completo o menos",
                                 NIVEL_ED == 5 | NIVEL_ED == 6 & CH04 == 2  ~ "Terciario incompleto o más"), 
         educacion = case_when(NIVEL_ED == 1 | NIVEL_ED == 2 | NIVEL_ED == 7  ~ "Primario completo o menos",
                               NIVEL_ED == 3 | NIVEL_ED == 4  ~ "Secundario completo o menos",
                               NIVEL_ED == 5 | NIVEL_ED == 6  ~ "Terciario incompleto o más"))

panel <- Panel_Continua

writexl::write_xlsx(panel, "panel_eph.xls")

####EJERCICIOS TRABAJO PRÁCTICO 1####

#Antes de arrancar vemos cuál es la tasa de Desocupación por año
desocupacion_año <- panel %>% 
  group_by(ANO4) %>% 
  summarise(Población = sum(PONDERA),
            Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = Ocupados + Desocupados,
            Tasa_Desocupacion = Desocupados/PEA)
desocupacion_año

#Primer gráfico
ggplot(desocupacion_año) +
  aes(x = ANO4, y = Tasa_Desocupacion) +
  geom_line(size = 0.5, colour = "#112446") +
  labs(
    x = "Año",
    y = "Tasa de Desocupación",
    title = "Tasa de Desocupación",
    subtitle = "Trayectoria de la tasa de Desocupación entre 2004 y 2022"
  ) +
  theme_minimal() 

###### a) Tasa de desocupación por rango etario y género #####

#Vamos a usar la función case_when 
#case_when(condicion1 ~ "Valor1",condicion2 ~ "Valor2",condicion3 ~ "Valor3")

desocupacion_edad_genero_panel <- panel %>% 
  group_by(rango_etario, ANO4, genero) %>% 
  summarise(Población = sum(PONDERA),
            Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = Ocupados + Desocupados,
            Tasa_Desocupacion =  Desocupados/PEA) %>% na.omit
desocupacion_edad_genero_panel


#Gráfico: 

ggplot(desocupacion_edad_genero_panel) +
  aes(x = ANO4, y = Tasa_Desocupacion, colour = rango_etario) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Año",
    y = "Tasa de desocupación",
    title = "Tasa de desocupación",
    subtitle = "Por rango etario y año",
    color = "Rango Etario"
  ) +
  theme_minimal()+
  facet_wrap(vars(genero))

#Tabla: 
datoss <- data.table(" "= c("25-34", "35-44", "45-54","55-64"), 
                     "2004"= percent(desocupacion_edad_genero_panel$Tasa_Desocupacion[c(1,8,15,22)]), 
                     "2007"= percent(desocupacion_edad_genero_panel$Tasa_Desocupacion[c(2,9,16,23)]), 
                     "2011"= percent(desocupacion_edad_genero_panel$Tasa_Desocupacion[c(3,10,17,24)]), 
                     "2015"= percent(desocupacion_edad_genero_panel$Tasa_Desocupacion[c(4,11,18,25)]), 
                     "2017"= percent(desocupacion_edad_genero_panel$Tasa_Desocupacion[c(5,12,19,26)]), 
                     "2019"= percent(desocupacion_edad_genero_panel$Tasa_Desocupacion[c(6,13,20,27)]), 
                     "2022"= percent(desocupacion_edad_genero_panel$Tasa_Desocupacion[c(7,14,21,28)]))

gt(datoss) %>% tab_header(
  title = md("**Tasa de Desocupación**"), 
  subtitle= md("Por rango etario y año")
) %>%
  cols_align(
    align = c("center"),
    columns = everything()) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 4
  ) %>% 
  tab_source_note(md("Datos correspondientes al 1T del 2004, 2007, 2011, 2015, 2017, 2019, 2022"))


#Ahora, por género
desocupacion_año_genero_panel <- panel %>% 
  group_by(genero, ANO4) %>% 
  summarise(Población = sum(PONDERA),
            Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = Ocupados + Desocupados,
            Tasa_Desocupacion =  Desocupados/PEA)
desocupacion_año_genero_panel

desocupacion_año_genero_panel <- desocupacion_año_genero_panel %>% na.omit() #No quiero considerar a los menores de 25 ni mayores a 65

#Gráfico
ggplot(desocupacion_año_genero_panel) +
  aes(x = ANO4, y = Tasa_Desocupacion, colour = genero) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Año",
    y = "Tasa de Desocupación",
    title = "Tasa de Desocupación",
    subtitle = "Por género",
    color = "Género"
  ) +
  theme_minimal()

#Tabla: 

#Forma 1
desocupacion_año_genero_panel %>% select(ANO4, genero, Tasa_Desocupacion) %>%gt()


#Forma 2
datos_genero <-  data.table(" "= c("25-34", "35-44", "45-54","55-64"), 
                            "Mujeres"= percent(desocupacion_año_genero_panel$Tasa_Desocupacion[1:7]), 
                            "Varones"= percent(desocupacion_año_genero_panel$Tasa_Desocupacion[8:14])) 

gt(datos_genero)%>% tab_header(
  title = md("**Tasa de Desocupación**"), 
  subtitle= md("Por rango etario y género")
) %>%
  cols_align(
    align = c("center"),
    columns = everything()) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>% 
  tab_source_note(md("Datos correspondientes al 1T del 2004, 2007, 2011, 2015, 2017, 2019, 2022"))


#Hago un gráfico para ver cómo están las cosas por género diferenciando por rango etario: 

desocupacion_edad_genero_panel <- panel %>% 
  group_by(rango_etario,genero, ANO4) %>% 
  summarise(Población = sum(PONDERA),
            Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = Ocupados + Desocupados,
            Tasa_Desocupacion =  Desocupados/PEA)
desocupacion_edad_genero_panel

desocupacion_edad_genero_panel <- desocupacion_edad_genero_panel %>% na.omit()


ggplot(desocupacion_edad_genero_panel) +
  aes(x = ANO4, y = Tasa_Desocupacion, colour = rango_etario) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Año",
    y = "Tasa de Desocupación",
    title = "Tasa de Desocupación",
    subtitle = "Por género y rango etario",
    color = "Rango Etario"
  ) +
  theme_minimal() +
  facet_wrap(vars(genero))



###### b) Participación laboral por región (y diferenciando por varones y mujeres) #####

desocupacion_region_panel <- panel %>% 
  group_by(region, ANO4) %>% 
  summarise(Población = sum(PONDERA),
            Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = Ocupados + Desocupados,
            Tasa_Desocupacion =  Desocupados/PEA)
desocupacion_region_panel

desocupacion_region_panel <- desocupacion_region_panel %>% na.omit() #No quiero considerar a los menores de 25 ni mayores a 65

#Gráfico
ggplot(desocupacion_region_panel) +
  aes(x = ANO4, y = Tasa_Desocupacion, colour = region) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Año",
    y = "Tasa de Desocupación",
    title = "Tasa de Desocupación",
    subtitle = "Por región y año",
    color = "Región"
  ) +
  theme_minimal()

#Tabla: 

#Forma 1
desocupacion_region_panel %>% select(ANO4, region, Tasa_Desocupacion) %>%gt()


#Forma 2
datos_reg <- data.table(" "= c("2004", "2007", "2011","2015", "2017", "2019", "2022"), 
                        "Cuyo"= percent(desocupacion_region_panel$Tasa_Desocupacion[1:7]), 
                        "GBA"= percent(desocupacion_region_panel$Tasa_Desocupacion[8:14]), 
                        "Nordeste"= percent(desocupacion_region_panel$Tasa_Desocupacion[15:21]), 
                        "Noroeste"= percent(desocupacion_region_panel$Tasa_Desocupacion[22:28]), 
                        "Pampeana"= percent(desocupacion_region_panel$Tasa_Desocupacion[29:35]), 
                        "Patagonia"= percent(desocupacion_region_panel$Tasa_Desocupacion[36:42]))

gt(datos_reg)%>% tab_header(
  title = md("**Tasa de Desocupación**"), 
  subtitle= md("Por región")
) %>%
  cols_align(
    align = c("center"),
    columns = everything()) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>% 
  tab_source_note(md("Datos correspondientes al 1T del 2004, 2007, 2011, 2015, 2017, 2019, 2022"))

#Puedo incorporar género

desocupacion_region_genero_panel <-panel  %>% 
  group_by(region, genero, ANO4) %>% 
  summarise(Población = sum(PONDERA),
            Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = Ocupados + Desocupados,
            Tasa_Desocupacion =  Desocupados/PEA)
desocupacion_region_genero_panel

desocupacion_region_genero_panel <- desocupacion_region_genero_panel %>% na.omit() #No quiero considerar a los menores de 25 ni mayores a 65


#Gráfico: 
ggplot(desocupacion_region_genero_panel) +
  aes(x = ANO4, y = Tasa_Desocupacion, colour = region) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Año",
    y = "Tasa de Desocupación",
    title = "Tasa de Desocupación",
    subtitle = "Por región y por género",
    color = "Región"
  ) +
  theme_minimal() +
  facet_wrap(vars(genero))

#Ahora veo por región -> see how to arrange
datos_reg_gen_f <- data.table(" "= c("2004", "2007", "2011","2015", "2017", "2019", "2022"), 
                              "Cuyo"= percent(desocupacion_region_genero_panel$Tasa_Desocupacion[1:7]), 
                              "GBA"= percent(desocupacion_region_genero_panel$Tasa_Desocupacion[15:21]), 
                              "Nordeste"= percent(desocupacion_region_genero_panel$Tasa_Desocupacion[29:35]), 
                              "Noroeste"= percent(desocupacion_region_genero_panel$Tasa_Desocupacion[43:49]), 
                              "Pampeana"= percent(desocupacion_region_genero_panel$Tasa_Desocupacion[57:63]), 
                              "Patagonia"= percent(desocupacion_region_genero_panel$Tasa_Desocupacion[71:77])) %>% gt()%>% 
  tab_header(
    title = md("**Tasa de Desocupación por región para mujeres**")
  ) %>%
  cols_align(
    align = c("center"),
    columns = everything()) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>% 
  tab_source_note(md("Datos correspondientes al primer trimestre del 2022"))


datos_reg_gen_m <- data.table(" "= c("2004", "2007", "2011","2015", "2017", "2019", "2022"), 
                              "Cuyo"= percent(desocupacion_region_genero_panel$Tasa_Desocupacion[8:14]), 
                              "GBA"= percent(desocupacion_region_genero_panel$Tasa_Desocupacion[22:28]), 
                              "Nordeste"= percent(desocupacion_region_genero_panel$Tasa_Desocupacion[36:42]), 
                              "Noroeste"= percent(desocupacion_region_genero_panel$Tasa_Desocupacion[50:56]), 
                              "Pampeana"= percent(desocupacion_region_genero_panel$Tasa_Desocupacion[64:70]), 
                              "Patagonia"= percent(desocupacion_region_genero_panel$Tasa_Desocupacion[78:84])) %>% gt()%>% 
  tab_header(
    title = md("**Tasa de Desocupación por región para Hombres**")
  ) %>%
  cols_align(
    align = c("center"),
    columns = everything()) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>% 
  tab_source_note(md("Datos correspondientes al primer trimestre del 2022"))




###### c) Tasa de participación laboral de las mujeres por estado civil ######

desocupacion_estado_civil_panel <- panel %>% 
  group_by(estado_civil, ANO4) %>% 
  summarise(Población = sum(PONDERA),
            Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = Ocupados + Desocupados,
            Tasa_Desocupacion =  Desocupados/PEA)
desocupacion_estado_civil_panel
desocupacion_estado_civil_panel <- na.omit(desocupacion_estado_civil_panel)


ggplot(desocupacion_estado_civil_panel) +
  aes(x = ANO4, y = Tasa_Desocupacion, colour = estado_civil) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Año",
    y = "Tasa de Desocupación",
    title = "Tasa de Desocupación",
    subtitle = "Por año y estado civil",
    color = "Estado Civil"
  ) +
  theme_minimal() 


###### d) Participación laboral de los varones por edad y nivel educativo ######

desocupacion_educacion_edad_hombres_panel <- panel %>% 
  group_by(educacion_h, rango_etario, ANO4) %>% 
  summarise(Población = sum(PONDERA),
            Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = Ocupados + Desocupados,
            Tasa_Desocupacion =  Desocupados/PEA)
desocupacion_educacion_edad_hombres_panel

desocupacion_educacion_edad_hombres_panel <- desocupacion_educacion_edad_hombres_panel %>% na.omit() #No quiero considerar a las mujeres
desocupacion_educacion_edad_hombres_panel

ggplot(desocupacion_educacion_edad_hombres_panel) +
  aes(x = ANO4, y = Tasa_Desocupacion, colour = educacion_h) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Año",
    y = "Tasa de Desocupación",
    title = "Tasa de Desocupación",
    subtitle = "Por rango etario y máximo nivel educativo alcanzado",
    color = "Nivel Educativo"
  ) +
  theme_minimal() +
  facet_wrap(vars(rango_etario))

###### e) Participación laboral de las mujeres por edad y nivel educativo######

desocupacion_educacion_edad_mujeres_panel <- panel %>% 
  group_by(educacion_m, rango_etario, ANO4) %>% 
  summarise(Población = sum(PONDERA),
            Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = Ocupados + Desocupados,
            Tasa_Desocupacion =  Desocupados/PEA) %>% na.omit
desocupacion_educacion_edad_mujeres_panel

ggplot(desocupacion_educacion_edad_mujeres_panel) +
  aes(x = ANO4, y = Tasa_Desocupacion, colour = educacion_m) +
  geom_line(size = 0.5) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Año",
    y = "Tasa de Desocupación",
    title = "Tasa de Desocupación",
    subtitle = "Por rango etario y nivel educativo",
    color = "Nivel Educativo"
  ) +
  theme_minimal() +
  facet_wrap(vars(rango_etario))


#Tablas interactivas
rpivotTable::rpivotTable(Desocupación_region_edad_panel, rows = "rango_etario", cols = c("ANO4", "genero"), width = "100%", height = "400px")

esquisse::esquisser()






