# ----------------------------------------------------------------------------
#
# ECONOMÍA LABORAL - UNIVERSIDAD DEL CEMA
# SEGUNDO SEMESTRE, 2022
#
# TRABAJO PRACTICO 3
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

#Importamos la base2004 de datos que vamos a usar
base2004 <-get_microdata(year=2004, trimester=1, type="individual")
base2007 <-get_microdata(year=2007, trimester=1, type="individual")
base2011 <-get_microdata(year=2011, trimester=1, type="individual")
base2015 <-get_microdata(year=2015, trimester=1, type="individual")
base2017 <-get_microdata(year=2017, trimester=1, type="individual")
base2019 <-get_microdata(year=2019, trimester=1, type="individual")
base2021 <-get_microdata(year=2021, trimester=1, type="individual")

#Paso 1
var.ind <- c('CODUSU','NRO_HOGAR','COMPONENTE', 'ANO4','TRIMESTRE','ESTADO','REGION','CAT_OCUP','PONDERA', 'CH04', 'CH06','CH07','P21', 'NIVEL_ED', 'PP07H')

#Paso 2  
Bases_Continua <- bind_rows(
  base2004  %>% select(var.ind),
  base2007  %>% select(var.ind),
  base2011  %>% select(var.ind),
  base2015  %>% select(var.ind), 
  base2017  %>% select(var.ind), 
  base2019  %>% select(var.ind), 
  base2021  %>% select(var.ind), )
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
         informalidad= case_when(PP07H == 1 ~ "Formal", 
                                 PP07H == 2 ~ "Informal"))

panel <- Panel_Continua


tabla3 <- panel %>%  
  filter(P21>0) %>%
  group_by(ANO4) %>%
  summarise(Formales        = sum(PONDERA[PP07H == 1]),
          Informales      = sum(PONDERA[PP07H == 0]),
          Total           = sum(PONDERA),
          Tasa_Informalidad = Informales/Total)

tabla44 <- panel %>%  
  filter(P21>0) %>%
  group_by(ANO4, genero) %>%
  summarise(Formales        = sum(PONDERA[PP07H == 1]),
            Informales      = sum(PONDERA[PP07H == 0]),
            Total           = sum(PONDERA),
            Tasa_Informalidad = Informales/Total)


esquisse::esquisser()


library(ggplot2)

ggplot(tabla44) +
 aes(x = ANO4, y = Tasa_Informalidad, colour = genero) +
 geom_line(size = 0.5) +
 scale_color_hue(direction = 1) +
 labs(x = "Año", y = "Tasa de informalidad", title = "Tasa de Informalidad", subtitle = "Trayectoria desde 2004 a 2021", 
 color = "Género") +
 theme_minimal()

library(ggplot2)

ggplot(tabla3) +
 aes(x = ANO4, y = Tasa_Informalidad) +
 geom_line(size = 0.85, colour = "#228B22") +
 labs(x = "Año", y = "Tasa de informalidad", title = "Tasa de Informalidad", subtitle = "Trayectoria desde 2004 a 2021") +
 theme_minimal()

