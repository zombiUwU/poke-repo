library(readr)
ruta_pkm.csv <- "C:\\Users\\Usuario\\Documents\\sebas\\cosas de R\\proyectos\\Kanto.csv"
read_csv(ruta_pkm.csv)
Kanto <- read_csv(ruta_pkm.csv,
                  col_names = c("ID",
                                "nombre",
                                "generacion",
                                "region",
                                "tipo1",
                                "tipo2",
                                "hpbase",
                                "ataquebase",
                                "defensabase",
                                "ataqueespecial base",
                                "defensaespecial base", 
                                "velocidadbase", 
                                "totalbasestats",
                                "altura(M)",
                                "peso(kg)",
                                "tasadecaptura", 
                                "eslengendario",
                                "etapaevolutiva",
                                "habilidadoculta",
                                "proporciondegenero",
                                "metododecaptura", 
                                "amistadbase", 
                                "color", 
                                "forma"))
#practica dplyr
library(dplyr)
library(magrittr)
library(tidyverse)
library(ggplot2)
is.na(Kanto$`tipo 2`)
sum(is.na(Kanto$`tipo 2`))


estadisticas <- Kanto %>%
  select(`altura(M)`, hpbase, `peso(kg)`) %>%
  filter(`altura(M)` != 0.3,hpbase == 45, `peso(kg)` != 15 ) %>%
  rename(height = `altura(M)`, vida = hpbase) %>%
  summary()

#select
select(Kanto, color, amistadbase, velocidadbase )

Kanto %>% select(`altura(M)`, nombre, forma) %>% filter(forma != 'Bipedal')
Kanto %>% select(-eslengendario, -nombre)
Kanto %>% select(starts_with("d"))
Kanto %>% select(ends_with("a"))
Kanto %>% select(contains("es"))
Kanto %>% select_if(is.numeric)
Kanto %>% select_if(is.character)

#FILTER

filter(Kanto, ID < 7)
       
resumen_de_pseudolegendarios <- Kanto %>% filter(totalbasestats == 600)             

Kanto %>% filter(eslengendario == TRUE & tipo2 == 'Flying')

evolvucion_o_color <- Kanto %>% filter(etapaevolutiva <= 2 | color == 'Green')

tipos <- Kanto$tipo2
sum(grepl('Poison', x = tipos))

Kanto %>%
  filter(grepl(pattern = 'Poison' , tipo2))

Kanto %>% filter(grepl(pattern = "45", hpbase)) %>%
  filter( ataquebase != 49)

#RENAME

rename(Kanto, 'Tipo 1' = tipo1, Vida = hpbase, ataque = ataquebase)

Kanto %>% 
  head(10) %>%
  select(tipo1, hpbase , ataquebase) %>%
  rename('Tipo 1' = tipo1, vida = hpbase, ataque = ataquebase) %>%
  filter(ataque >= 50, vida <= 65 )

#TRANSFORM

transform(Kanto, amistadbase = ifelse(amistadbase > 70 , 'amistoso', 'poco amistoso'))

Kanto %>%
  head(10) %>%
  transform(amistadbase = ifelse(amistadbase > 70 , 'amistoso', 'poco amistoso'))

Kanto %>% transform(etapaevolutiva = case_when(
  etapaevolutiva > 0 & etapaevolutiva <= 1 ~ 'inicial',
  etapaevolutiva > 1 & etapaevolutiva <= 2 ~ 'intermedio',
  TRUE ~ 'final'))

#cambiar el tipo de dato

glimpse(Kanto)

kanto <- Kanto %>% transform(proporciondegenero = as.numeric(proporciondegenero)) #funciona raro


#MUTATE 

Kanto %>% mutate(media_evolutiva = mean(etapaevolutiva)) %>% view()

#SEPARATE

prueba_de_BD <- data.frame(Kanto$proporciondegenero)


#GROUPBY

Kanto %>% group_by(proporciondegenero, nombre) %>%
  summarise(proporciones = n()) %>%
  view()

Kanto %>%
  group_by(amistadbase) %>%
  summarise(porcentaje = n()/nrow(.)*100) %>%
  arrange(desc(porcentaje)) %>%
  mutate(sumaporcentaje = sum(porcentaje)) %>%
  view()
  
  slice() #seleciona 1 o mas filas especificamente (ponlo antes del group by)
  
  