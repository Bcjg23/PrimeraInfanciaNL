setwd(DIR)

library(dplyr)
library(tidyverse)
library(survey)
library(scales)
library(ggthemes) # Themes for graphs
library(gridExtra)
library(treemapify)



#################################################################################
## Define functions
#################################################################################
pone_edos <- function(df){
  # Relaciona las claves de los estados con sus respectivos nombres en una columna adicional
  dict_edos <- read.csv("data/input/INEGI_edos_muns.csv", encoding = "UTF-8") %>%
    #colClasses=c("numeric", "character","character","numeric","numeric","character")) %>%
    select("Cve_Ent", "Nom_Ent", "Nom_Abr") %>% unique()
  colnames(dict_edos) <- tolower(colnames(dict_edos))
  
  df <-  df %>% left_join(dict_edos[,c("cve_ent","nom_ent", "nom_abr")], by = c("ent"="cve_ent") )
  
  return(df)
}

nice_porc <- function(x){
  x <- round(x*100,2)
}

#################################################################################
## Load data
#################################################################################
## POBREZA CONEVAL 2020
pobreza <- read.csv("data/input/pobreza_Coneval2020.csv",
                    encoding = "UTF-8",
                    colClasses=c("folioviv"="double", "foliohog"="integer",
                                 "numren"="integer")
                    ) %>% 
  mutate(pobreza_label=recode(pobreza,
                              `0` = "No pobre",
                              `1` = "Pobre")
         )


# POBLACION ENIGH 2020
pob <- read.csv("data/input/poblacion_enigh_2020_ns.csv",
                      encoding = "UTF-8-BOM",
                      colClasses=c("ï..folioviv"="double", "foliohog"="integer", 
                                   "residencia"="character", "numren"="integer"))

pob <- merge(pob, pobreza[,c("folioviv","foliohog","numren", "factor",
                             "ent", "ubica_geo", "rururb", "pobreza",
                             "pobreza_label", "pobreza_e","pobreza_m",
                             "ic_rezedu", "ic_asalud", "ic_segsoc", 
                             "ic_cv", "ic_sbv", "ic_ali_nc",
                             "vul_car", "vul_ing", "carencias", "carencias3",
                             "ictpc", "hli", "discap")],
             by.x = c("ï..folioviv","foliohog", "numren"),
             by.y = c("folioviv", "foliohog", "numren"))

# Indicador de personas con trabajo
pob$tiene_trabajo <- ifelse(pob$num_trabaj>=1 & pob$edad>15,1,0)
# Indicador de personas que son menores de entre 0 a 3 años
pob$hijos03 <- ifelse((!(pob$parentesco>=400 & pob$parentesco <500 |
                           pob$parentesco>=700 & pob$parentesco <800) &
                         (pob$edad>=0 & pob$edad<=3)) ,1,0)
# Indicador de hogares con menores de 0 a 3 años
menores <- pob %>% 
  group_by(ï..folioviv, foliohog) %>%
  summarise(
    num_hijos03 = sum(hijos03, na.rm = T),
    tiene_hijos03 = ifelse(num_hijos03>0,1,0),
    ninos03_en_pobreza = ifelse(sum(pobreza[edad>=0 & edad<=3])>0,1,0)
  ) %>% select(ï..folioviv, foliohog, tiene_hijos03, num_hijos03, ninos03_en_pobreza)

pob <- left_join(pob, menores, by = c("ï..folioviv", "foliohog"))

# Indicador de madres de ninos de 0 a 3 anos por hogar
madres <- pob %>%
  mutate(
    numren_madre_hijos03 = case_when(
      # Hijo del jefe de familia
      hijos03==1 & madre_hog==1 & parentesco>=300 & parentesco<400 ~ madre_id,
      # No tiene parentesco
      hijos03==1 & madre_hog==1 & parentesco==501 ~ madre_id,
      # Hermano, medio hermano, hermanastro del jefe de familia
      hijos03==1 & madre_hog==1 & parentesco>=603 & parentesco<=605 ~ madre_id,
      # Nieto del jefe de familia
      hijos03==1 & madre_hog==1 & parentesco==609 ~ madre_id,
      # Bisnieto
      hijos03==1 & madre_hog==1 & parentesco==610 ~ madre_id,
      # Tatarnieto
      hijos03==1 & madre_hog==1 & parentesco==611 ~ madre_id,
      # Sobrino
      hijos03==1 & madre_hog==1 & parentesco==613 ~ madre_id,
      # Primo
      hijos03==1 & madre_hog==1 & parentesco==614 ~ madre_id,
      # Familiar, otro parentesco
      hijos03==1 & madre_hog==1 & parentesco==623 ~ madre_id),
    
    numren_madre_hijos03 = as.numeric(numren_madre_hijos03)
  ) %>% 
  select("ï..folioviv", "foliohog", "numren_madre_hijos03") %>%
  filter(!is.na(numren_madre_hijos03))

## Mujeres que son madres de ninos de 0 a 3 anos
madres <- left_join(madres, pob, by = c("ï..folioviv"= "ï..folioviv", 
                                        "foliohog"="foliohog", 
                                        "numren_madre_hijos03"="numren"))

## HOGARES ENIGH 2020
hogar <- read.csv("data/input/concentradohogar_enigh_2020_ns.csv",
                  colClasses=c("ï..folioviv"="double", "foliohog"="integer")
)
hogar <- left_join(hogar, menores, by = c("ï..folioviv", "foliohog"))
#pob$ï..folioviv <- as.numeric(pob$ï..folioviv)
hogar <- left_join(hogar, filter(pob[,c("ï..folioviv","foliohog", 
                                        "numren", "ent",
                                        "pobreza", "hli", "discap",
                                        "tiene_trabajo", "act_pnea1")],
                                 numren==1), 
                   by = c("ï..folioviv", "foliohog"))


## POBLACION CENSO 2020 POR GRUPOS DE EDAD
pob_edad <- read.csv("data/input/poblacion_nl_grupos_edad_cpv2020.csv",
                     encoding = "UTF-8",
                     colClasses=c("Cve_mun"="character")
) %>% 
  filter(Municipio=="NL", Sexo!="Total", 
         Grupos_quinquenales_edad!="Total", 
         Grupos_quinquenales_edad!='No especificado') %>%
  mutate(Grupos_quinquenales_edad= factor(Grupos_quinquenales_edad,
                                          levels = c('00-04 años', '05-09 años', 
                                                     '10-14 años', '15-19 años',
                                                     '20-24 años', '25-29 años',
                                                     '30-34 años', '35-39 años',
                                                     '40-44 años', '45-49 años',
                                                     '50-54 años', '55-59 años',
                                                     '60-64 años', '65-69 años',
                                                     '70-74 años', '75-79 años',
                                                     '80-84 años', '85-89 años',
                                                     '90-94 años', '95-99 años',
                                                     '100 años y más')
                                          )
         )


# *****************************************************************************
# Population pyramid    
# *****************************************************************************
fig_nl <- ggplot(pob_edad, aes(x = ifelse(Sexo == "Hombres",-Poblacion, Poblacion),
                               y = Grupos_quinquenales_edad,
                               fill = Sexo)) +
  geom_col()+
  #scale_x_symmetric(labels = abs) +
  scale_x_continuous(
    breaks = seq(-300000, 300000, 50000),
    labels = scales::comma(c(seq(-300000, -50000, by = 50000) * -1, 
                             seq(0, 300000, by = 50000)))
  ) +
  scale_fill_manual(values=c("#66bfff","#ff2b00"))+
  theme_economist() +
  theme(
    plot.title = element_text(size=13, margin=margin(b=5)),
    axis.text.x = element_text(size=9, angle = 0),
    axis.text.y = element_text(size=10),
    plot.caption = element_text(size = 10, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#6D6D6D"),
    legend.position="bottom",
    legend.text = element_text(size=10)
  )+
  labs(title='Población en Nuevo Leon, por edad y sexo',
       subtitle ="",  
       x="", y="",
       fill="",
       caption = "Fuente: Elaboración propia con datos del CENSO 2020." )

ggsave(filename = paste0(DIR,"/figs/fig_pob_nl.jpg"), plot = fig_nl, 
       width = 7, height =7, units = "in", dpi=300)


# *****************************************************************************
# Population - General information
# *****************************************************************************
pob_tbl <- pob %>%
  group_by(ent) %>% 
  summarise(
    # ******************************************************************************
    # Ninos de 0 a 5 anos 
    ninos05 = sum(factor[edad>=0 & edad<=5], na.rm = TRUE),
    # Mujeres
    ninos05_mujeres = sum(factor[edad>=0 & edad<=5 & sexo==2], na.rm = TRUE),
    # Con alguna discapacidad
    ninos05_discapacidad = sum(factor[edad>=0 & edad<=5 & discap==1], na.rm = TRUE),
    # En zonas urbanas
    ninos05_urbana = sum(factor[edad>=0 & edad<=5 & rururb==0], na.rm = TRUE),
    # En pobreza
    ninos05_pobreza = sum(factor[edad>=0 & edad<=5 & pobreza==1], na.rm = TRUE ),
    # En pobreza Extrema
    ninos05_pobreza_extrema = sum(factor[edad>=0 & edad<=5 & pobreza_e==1], na.rm = TRUE ),
    # Pobreza + Urbano
    ninos05_pobreza_urb = sum(factor[edad>=0 & edad<=5 & pobreza==1 & rururb==0], na.rm = TRUE),
    # Pobreza Extrema + Urbano
    ninos05_pobreza_extrema_urb = sum(factor[edad>=0 & edad<=5 & pobreza_e==1 & rururb==0], na.rm = TRUE),
    # No Pobreza + Urbano
    ninos05_no_pobreza_urb = sum(factor[edad>=0 & edad<=5 & pobreza==0 & rururb==0], na.rm = TRUE),
    # Discapacidad + Pobreza
    ninos05_pobreza_disc = sum(factor[edad>=0 & edad<=5 & discap==1 & pobreza==1], na.rm = TRUE),
    # Discapacidad + Pobreza Extrema
    ninos05_pobreza_extrema_disc = sum(factor[edad>=0 & edad<=5 & discap==1 & pobreza_e==1], na.rm = TRUE),
    # Pobreza + Mujeres
    ninos05_pobreza_muj = sum(factor[edad>=0 & edad<=5 & pobreza==1 & sexo==2], na.rm = TRUE),
    
    # *******************************************************************************
    # Ninos de 0 a 3 anos 
    ninos03 = sum(factor[edad>=0 & edad<=3], na.rm = TRUE),
    # Mujeres
    ninos03_mujeres = sum(factor[edad>=0 & edad<=3 & sexo==2], na.rm = TRUE),
    # Con alguna discapacidad
    ninos03_discapacidad = sum(factor[edad>=0 & edad<=3 & discap==1], na.rm = TRUE),
    # En zonas urbanas
    ninos03_urbana = sum(factor[edad>=0 & edad<=3 & rururb==0], na.rm = TRUE),
    # En Pobreza
    ninos03_pobreza = sum(factor[edad>=0 & edad<=3 & pobreza==1], na.rm = TRUE ),
    # En Pobreza Extrema
    ninos03_pobreza_extrema = sum(factor[edad>=0 & edad<=3 & pobreza_e==1], na.rm = TRUE ),
    # Pobreza + Urbano
    ninos03_pobreza_urb = sum(factor[edad>=0 & edad<=3 & pobreza==1 & rururb==0], na.rm = TRUE),
    # Pobreza Extrema + Urbano
    ninos03_pobreza_extrema_urb = sum(factor[edad>=0 & edad<=3 & pobreza_e==1 & rururb==0], na.rm = TRUE),
    # No Pobreza + Urbano
    ninos03_no_pobreza_urb = sum(factor[edad>=0 & edad<=3 & pobreza==0 & rururb==0], na.rm = TRUE),
    # Discapacidad + Pobreza
    ninos03_pobreza_disc = sum(factor[edad>=0 & edad<=3 & discap==1 & pobreza==1], na.rm = TRUE),
    # Discapacidad + Pobreza Extrema
    ninos03_pobreza_extrema_disc = sum(factor[edad>=0 & edad<=3 & discap==1 & pobreza_e==1], na.rm = TRUE),
    # Pobreza + Mujeres
    ninos03_pobreza_muj = sum(factor[edad>=0 & edad<=3 & pobreza==1 & sexo==2], na.rm = TRUE),
    # Pobreza Extrema + Mujeres
    ninos03_pobreza_extrema_muj = sum(factor[edad>=0 & edad<=3 & pobreza_e==1 & sexo==2], na.rm = TRUE),
    
    # Por edad
    ninos0 = sum(factor[edad==0], na.rm = TRUE),
    ninos1 = sum(factor[edad==1], na.rm = TRUE),
    ninos2 = sum(factor[edad==2], na.rm = TRUE),
    ninos3 = sum(factor[edad==3], na.rm = TRUE),
    ninos4 = sum(factor[edad==4], na.rm = TRUE),
    ninos5 = sum(factor[edad==5], na.rm = TRUE),
    
    # Pobreza por edad y sexo
    ninos0_pobre = sum(factor[edad==0 & pobreza==1], na.rm = TRUE),
    ninos1_pobre = sum(factor[edad==1 & pobreza==1], na.rm = TRUE),
    ninos2_pobre = sum(factor[edad==2 & pobreza==1], na.rm = TRUE),
    ninos3_pobre = sum(factor[edad==3 & pobreza==1], na.rm = TRUE),
    ninos4_pobre = sum(factor[edad==4 & pobreza==1], na.rm = TRUE),
    ninos5_pobre = sum(factor[edad==5 & pobreza==1], na.rm = TRUE),
    ninos0_pobre_muj = sum(factor[edad==0 & pobreza==1 & sexo==2], na.rm = TRUE),
    ninos1_pobre_muj = sum(factor[edad==1 & pobreza==1 & sexo==2], na.rm = TRUE),
    ninos2_pobre_muj = sum(factor[edad==2 & pobreza==1 & sexo==2], na.rm = TRUE),
    ninos3_pobre_muj = sum(factor[edad==3 & pobreza==1 & sexo==2], na.rm = TRUE),
    ninos4_pobre_muj = sum(factor[edad==4 & pobreza==1 & sexo==2], na.rm = TRUE),
    ninos5_pobre_muj = sum(factor[edad==5 & pobreza==1 & sexo==2], na.rm = TRUE),
    
    # Pobreza Extrema por edad y sexo
    ninos0_pobreExt = sum(factor[edad==0 & pobreza_e==1], na.rm = TRUE),
    ninos1_pobreExt = sum(factor[edad==1 & pobreza_e==1], na.rm = TRUE),
    ninos2_pobreExt = sum(factor[edad==2 & pobreza_e==1], na.rm = TRUE),
    ninos3_pobreExt = sum(factor[edad==3 & pobreza_e==1], na.rm = TRUE),
    ninos4_pobreExt = sum(factor[edad==4 & pobreza_e==1], na.rm = TRUE),
    ninos5_pobreExt = sum(factor[edad==5 & pobreza_e==1], na.rm = TRUE),
    ninos0_pobreExt_muj = sum(factor[edad==0 & pobreza_e==1 & sexo==2], na.rm = TRUE),
    ninos1_pobreExt_muj = sum(factor[edad==1 & pobreza_e==1 & sexo==2], na.rm = TRUE),
    ninos2_pobreExt_muj = sum(factor[edad==2 & pobreza_e==1 & sexo==2], na.rm = TRUE),
    ninos3_pobreExt_muj = sum(factor[edad==3 & pobreza_e==1 & sexo==2], na.rm = TRUE),
    ninos4_pobreExt_muj = sum(factor[edad==4 & pobreza_e==1 & sexo==2], na.rm = TRUE),
    ninos5_pobreExt_muj = sum(factor[edad==5 & pobreza_e==1 & sexo==2], na.rm = TRUE),
    
    # Total de poblacion
    poblacion = sum(factor, na.rm = TRUE),
    # Poblacion en pobreza
    pobreza = sum(factor[pobreza==1], na.rm = TRUE ),
    # Poblacion en pobreza extrema
    pobreza_extrema = sum(factor[pobreza_e==1], na.rm = TRUE ),
    # Mujeres
    mujeres = sum(factor[sexo==2], na.rm = TRUE),
    # Poblacion con alguna discapacidad
    discapacidad = sum(factor[discap==1], na.rm = TRUE),
    # Poblacion que vive en zonas urbanas
    urbana = sum(factor[rururb==0], na.rm = TRUE)
  ) %>%
  mutate(
    # Porcentajes
    porc_pobre = nice_porc(pobreza/poblacion),
    porc_mujeres = nice_porc(mujeres/poblacion),
    porc_discapacidad = nice_porc(discapacidad/poblacion),
    porc_urbana = nice_porc(urbana  / poblacion),
    #
    porc_ninos03 = nice_porc(ninos03 / poblacion),
    porc_ninos03_pobre = nice_porc(ninos03_pobreza/ninos03),
    porc_ninos03_mujeres = nice_porc(ninos03_mujeres/ninos03),
    porc_ninos03_discapacidad = nice_porc(ninos03_discapacidad/ninos03),
    porc_ninos03_urbana = nice_porc(ninos03_urbana  / ninos03),
    # Diferencia entre tasas de pobreza
    diff_tasa = abs(porc_ninos03_pobre-porc_pobre),
    nl=ifelse(ent==19,1,0)
  ) %>%
  pone_edos()

pob_nl <- pob_tbl[pob_tbl$ent==19,] %>% 
  pivot_longer(cols = ninos05:diff_tasa, names_to = "var", values_to= "val" )


# *****************************************************************************
# Children ages 0 to 3 + Poverty
# *****************************************************************************
fig_ninosPobreza <- ggplot(pob_tbl[pob_tbl$ent!=0,],
                           aes(x=reorder(nom_abr, -porc_ninos03_pobre), 
                               y=porc_ninos03_pobre/100,
                               fill=factor(ifelse(nom_abr=="NL","Highlighted","Normal"))
                               )
                           ) +
  geom_bar(stat="identity") +
  scale_fill_manual(name = "area", values=c("#d9001a","#66bfff")) +
  #geom_text(aes(label=round(Porc_ninos03_pobre$n*100,1)), size =3) +
  theme_economist() +
  theme(
    plot.title = element_text(size=13, margin=margin(b=5)),
    axis.text.x = element_text(size=11, angle = 90),
    axis.text.y = element_text(size=10),
    legend.position = "none",
    plot.caption = element_text(size = 10, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#6D6D6D")
  )+
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)+
  labs(title='Niños de 0 a 3 años en situación de pobreza',
       subtitle ="(Tasa de pobreza)",  
       x="", y="",
       caption = "Fuente: Elaboración propia con datos de la ENIGH 2020." )

ggsave(filename = paste0(DIR,"/figs/fig_tasa_pobreza.jpg"), plot = fig_ninosPobreza, 
       width = 7, height =5, units = "in", dpi=300)


# *****************************************************************************
# Children ages 0 to 3 + Poverty
# Distribution by age and sex
# Percentages based on ENIGH, absolute numbers from CENSO 2020
# *****************************************************************************
ninos_pobres <- read.csv("data/input/ninos03_pobreza.csv", encoding = "UTF-8") %>%
  filter(Edad != "Niños de 0 a 3 años") %>%
  mutate(Sexo=factor(Sexo,levels = c("Mujeres", "Hombres")),
         Edad = factor(Edad, levels = c("Menos de 1 año", "1 año",
                                        "2 años ", "3 años"))
        )

fig_distribucionNinos <- ggplot(ninos_pobres, aes(fill=Sexo, y=n, x=Edad)) + 
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label=paste0("(",round(porcentaje*100,1),"%)")),
            size = 3,
            hjust = 0.5, vjust = 3, position = "stack") +
  theme_economist() +
  theme(
    plot.title = element_text(size=13, margin=margin(b=5)),
    axis.text.x = element_text(size=11, angle = 0),
    axis.text.y = element_text(size=10),
    legend.position = "bottom",
    plot.caption = element_text(size = 10, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#6D6D6D"),
    legend.title=element_text(size=9),
    legend.text = element_text(size=9),
    legend.key.size = unit(0.7, 'cm')
  )+
  scale_fill_manual(values=c("#ff2b00","#66bfff"))+
  scale_y_continuous(limits = c(0, 40000), labels = scales::comma)+
  labs(title='Distribución de los niños de 0 a 3 años en pobreza',
       subtitle ="(Por edad y sexo)",  
       x="", y="",
       fill = "",
       caption = "Fuente: Elaboración propia con datos de la ENIGH y del CENSO, 2020." )
 
ggsave(filename = paste0(DIR,"/figs/fig_distNinos.jpg"), plot = fig_distribucionNinos, 
       width = 6, height = 5, units = "in", dpi=300)


# *****************************************************************************
# Households of Children ages 0 to 3
# *****************************************************************************
ninos_en_hogares <- function(df){
  df2 <- df %>% group_by(ent) %>% 
    summarise(
      # Total de Hogares con hijos de 0 a 3
      hogares = sum(factor, na.rm = TRUE),
      # Total de Ninos de 0 a 3
      ninos03 = sum(num_hijos03*factor),
      # Tipos de hogar
      unipersonal = sum( num_hijos03[clase_hog==1]*factor[clase_hog==1],na.rm=TRUE),
      nuclear = sum( num_hijos03[clase_hog==2]*factor[clase_hog==2],na.rm=TRUE),
      ampliado = sum( num_hijos03[clase_hog==3]*factor[clase_hog==3],na.rm=TRUE),
      compuesto = sum( num_hijos03[clase_hog==4]*factor[clase_hog==4],na.rm=TRUE),
      corresidente = sum( num_hijos03[clase_hog==5]*factor[clase_hog==5],na.rm=TRUE),
      # Educacion del jefe de familia
      sin_instruccion = sum( num_hijos03[educa_jefe==1]*factor[educa_jefe==1],na.rm=TRUE),
      preescolar = sum( num_hijos03[educa_jefe==2]*factor[educa_jefe==2],na.rm=TRUE),
      primaria_incompleta = sum( num_hijos03[educa_jefe==3]*factor[educa_jefe==3],na.rm=TRUE),
      primaria = sum( num_hijos03[educa_jefe==4]*factor[educa_jefe==4],na.rm=TRUE),
      primaria = primaria_incompleta + primaria,
      
      secundaria_incompleta = sum( num_hijos03[educa_jefe==5]*factor[educa_jefe==5],na.rm=TRUE),
      secundaria = sum(num_hijos03[educa_jefe==6]*factor[educa_jefe==6],na.rm=TRUE),
      secundaria = secundaria_incompleta + secundaria,
      
      preparatoria_incompleta = sum( num_hijos03[educa_jefe==7]*factor[educa_jefe==7],na.rm=TRUE),
      preparatoria = sum( num_hijos03[educa_jefe==8]*factor[educa_jefe==8],na.rm=TRUE),
      preparatoria = preparatoria_incompleta + preparatoria,
      
      profesional_incompleta = sum( num_hijos03[educa_jefe==9]*factor[educa_jefe==9],na.rm=TRUE),
      profesional = sum( num_hijos03[educa_jefe==10]*factor[educa_jefe==10],na.rm=TRUE),
      profesional = profesional_incompleta + profesional,
      
      posgrado = sum( num_hijos03[educa_jefe==11]*factor[educa_jefe==11],na.rm=TRUE),
    )
}

dfs <- list()
# Hogares con hijos de 0 a 3 anos
dfs[[1]] <- hogar %>% filter(tiene_hijos03==1) %>% ninos_en_hogares()
# Hogares con hijos de 0 a 3 anos y en Pobreza
dfs[[2]] <- hogar %>% filter(tiene_hijos03==1 & ninos03_en_pobreza==1) %>% ninos_en_hogares()
# Hogares con hijos de 0 a 3 anos y en Pobreza y con jefatura femenina
dfs[[3]] <- hogar %>% filter(tiene_hijos03==1 & ninos03_en_pobreza==1 & sexo_jefe==2) %>% ninos_en_hogares()
# Hogares con hijos de 0 a 3 anos y en Pobreza y con jefatura masculina
dfs[[4]] <- hogar %>% filter(tiene_hijos03==1 & ninos03_en_pobreza==1 & sexo_jefe==1) %>% ninos_en_hogares()
hogares_nl <- NULL
for(df in dfs){
  aux <- df %>% filter(ent==19) 
  hogares_nl <- rbind(hogares_nl,aux)
}
hogares_nl$var <- c("Hogares", "Hogares_en_pobreza", "Hogares_en_pobreza_jefe_mujer", "Hogares_en_pobreza_jefe_hombre" )
hogares_nl <- hogares_nl %>% pivot_longer(col=!var, names_to = "variable", values_to= "total" ) %>%
  pivot_wider(names_from = "var", values_from = 'total' )


tipo_hogar <- c("compuesto", "ampliado", "nuclear")
escolaridad <- c("sin_instruccion", "primaria", "secundaria",
                 "preparatoria", "profesional")

data_figHogares <- hogares_nl %>% 
  select(variable, Hogares_en_pobreza_jefe_mujer, Hogares_en_pobreza_jefe_hombre) %>%
  rename_with(recode, Hogares_en_pobreza_jefe_mujer = 'Jefatura Femenina',
              Hogares_en_pobreza_jefe_hombre = 'Jefatura Masculina') %>%
  pivot_longer(col=!variable, names_to="Jefatura", values_to="ninos03") %>%
  filter( variable %in% tipo_hogar | variable %in% escolaridad ) %>%
  mutate(
    variable1 = ifelse(variable %in% tipo_hogar, "Tipo de hogar", "Escolaridad del Jefe"),
    variable= case_when(
      variable=="sin_instruccion" ~ "Sin instrucción",
      variable=="primaria" ~ "Primaria", 
      variable=="secundaria" ~ "Secundaria",
      variable=="preparatoria" ~ "Preparatoria", 
      variable=="profesional" ~ "Profesional",
      
      variable=="nuclear" ~ "Nuclear",
      variable=="ampliado" ~ "Ampliado",
      variable=="compuesto" ~ "Compuesto",
    ),
    variable= factor(variable,
                     levels = c("Sin instrucción", "Primaria", "Secundaria",
                                "Preparatoria", "Profesional",
                                
                                "Compuesto", "Ampliado", 
                                "Nuclear"
                     )
    )
  ) %>% select(Jefatura, variable1, variable, ninos03)


fig_tipoHogar <- data_figHogares %>% filter(variable1=="Tipo de hogar") %>%
  mutate(
    hog_f = group_by(filter(.,Jefatura=="Jefatura Femenina")) %>% summarise(n=sum(ninos03)),
    hog_m = group_by(filter(.,Jefatura=="Jefatura Masculina")) %>% summarise(n=sum(ninos03)),
    Porc_ninos03 = ifelse(Jefatura=="Jefatura Femenina", ninos03/hog_f$n, ninos03/hog_m$n),
  ) %>%
  ggplot( aes(x = variable, y = Porc_ninos03,
              fill=factor(Jefatura))
  ) +
  geom_bar(stat = "identity", alpha = 1.0) +
  coord_flip() +
  facet_grid(. ~Jefatura, 
             labeller = labeller(Jefatura=c(`Jefatura Femenina`="Female household head",
                                            `Jefatura Masculina`="Male household head")) ) + 
  geom_text(aes(label = paste0(round(Porc_ninos03*100,1),"%") ), nudge_y = 0.15,  colour = "black", size = 3) +
  scale_y_continuous(n.breaks=6, limits=c(0,1), labels = scales::percent) +
  scale_fill_manual(values=c("#ff2b00", "#66bfff")) +
  theme_economist() +
  theme(
    plot.title = element_text(size=13, margin=margin(b=5)),
    axis.text.x = element_text(size=9, angle = 0),
    axis.text.y = element_text(size=10),
    axis.title.y = element_text(face="bold", size=12, vjust = 2),
    strip.text = element_text(face="bold", size=rel(1)),
    plot.caption = element_text(size = 10, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#6D6D6D"),
    legend.position="none",
    legend.text = element_text(size=10)
  ) +
  labs(x="Tipo de Hogar", y=NULL)

fig_escol <- data_figHogares %>% filter(variable1=="Escolaridad del Jefe") %>%
  mutate(
    hog_f = group_by(filter(.,Jefatura=="Jefatura Femenina")) %>% summarise(n=sum(ninos03)),
    hog_m = group_by(filter(.,Jefatura=="Jefatura Masculina")) %>% summarise(n=sum(ninos03)),
    Porc_ninos03 = ifelse(Jefatura=="Jefatura Femenina", ninos03/hog_f$n, ninos03/hog_m$n),
  ) %>%
  ggplot( aes(x = variable, y = Porc_ninos03,
              fill=factor(Jefatura))
  ) +
  geom_bar(stat = "identity", alpha = 1.0) +
  coord_flip() +
  facet_grid(. ~Jefatura, 
             labeller = labeller(Jefatura=c(`Jefatura Femenina`="Female household head",
                                            `Jefatura Masculina`="Male household head")) ) +
  geom_text(aes(label = paste0(round(Porc_ninos03*100,1),"%") ), nudge_y = 0.15, colour = "black", size = 3) +
  scale_y_continuous(n.breaks=6, limits=c(0,1), labels = scales::percent) +
  scale_fill_manual(values=c("#ff2b00", "#66bfff")) +
  theme_economist() +
  theme(
    plot.title = element_text(size=13, margin=margin(b=5)),
    axis.text.x = element_text(size=9, angle = 0),
    axis.text.y = element_text(size=10),
    axis.title.y = element_text(face="bold", size=11, vjust = 2),
    strip.text =  element_blank(),
    plot.caption = element_text(size = 10, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#6D6D6D"),
    legend.position="none",
    legend.text = element_text(size=10)
  )+
  labs(x="Escolaridad del Jefe del hogar", y=NULL,
       caption = "Elaboración propia con datos de la ENIGH 2020.")

fig_hogares <- grid.arrange(fig_tipoHogar, fig_escol, nrow=2)

ggsave(filename = paste0(DIR,"/figs/fig_hogares.jpg"), plot = fig_hogares,
       width = 7, height = 7, units = "in")



# *****************************************************************************
# Labor force participation rate, female 
# (% of female population ages 15+)
# International comparison
# *****************************************************************************
country_fpart <- read.csv("data/input/female_labor_participation_WorldBankData.csv",
                          encoding = "UTF-8")

fig_femaleWork <- ggplot(country_fpart,
                 aes(x=reorder(Country_Name, -X2019), y=X2019/100,
                     fill=factor(Region))
) +
  geom_bar(stat="identity") + 
  theme_economist() +
  theme(
    plot.title = element_text(size=13, margin=margin(b=5)),
    axis.text.x = element_text(size=11, angle = 90),
    axis.text.y = element_text(size=10),
    legend.position = "none",
    plot.caption = element_text(size = 10, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#6D6D6D")
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("#93d2ff", "#0000d9", "#66bfff", "#0000d9", 
                             "#0000d9", "#b20015", "#d9001a", "#C8D7DE")
                    ) +
  labs(title=paste0('Participación femenina en el mercado laboral'),
       subtitle ="(Porcentaje)",  
       x='', y="",
       caption="Elaboración propia con datos del Censo 2020 para México y del Banco Mundial.")

ggsave(filename = paste0(DIR,"/figs/fig_participacion_mujer.jpg"), plot = fig_femaleWork, 
       width = 7, height =5, units = "in", dpi=300)


# *****************************************************************************
# Labor force participation rate, female 
# Mexico + State
# *****************************************************************************
# Participacion de la mujer en el mercado laboral
# Nacional 41.31
# https://www.eleconomista.com.mx/capitalhumano/Participacion-laboral-femenina-retrocedio-a-niveles-del-2005-20210303-0008.html
nac <- c(0,
         sum(pob[pob$sexo==2 & pob$tiene_trabajo==1,'factor'], na.rm = TRUE),
         sum(pob[pob$sexo==2 & pob$tiene_trabajo==0 & pob$act_pnea1==1,'factor'], na.rm = TRUE),
         sum(pob[pob$tiene_trabajo==1, 'factor'], na.rm = TRUE))

mujeres <- pob %>% 
  group_by(ent) %>%
  summarise(# Mujeres & Hombres
    mujeres = sum(factor[sexo==2], na.rm = TRUE),
    hombres = sum(factor[sexo==1], na.rm = TRUE),
    # Mujeres + Pobreza
    mujeres_pobre = sum(factor[sexo==2 & pobreza ==1], na.rm = TRUE),
    
    # Mujeres + Pobreza
    mujeres_pobre_hijos03 = sum(factor[sexo==2 & pobreza ==1], na.rm = TRUE),
    
    # Hombres + Trabaja
    a=sum(factor[tiene_trabajo==1 & sexo==1], na.rm = TRUE),
    b=sum(factor[tiene_trabajo==0 & act_pnea1==1 & sexo==1], na.rm = TRUE),
    Hombres_trabaja = a+b,
    
    # Mujeres + Trabajan   (a=trabajan, b=en busca)
    a=sum(factor[tiene_trabajo==1 & sexo==2], na.rm = TRUE),
    b=sum(factor[tiene_trabajo==0 & act_pnea1==1 & sexo==2], na.rm = TRUE),
    Mujeres_trabaja = a+b,
    
    # Mujeres + Trabajan + Pobreza
    a=sum(factor[tiene_trabajo==1 & sexo==2 & pobreza==1], na.rm = TRUE),
    b=sum(factor[tiene_trabajo==0 & act_pnea1==1 & sexo==2  & pobreza==1], na.rm = TRUE),
    Mujeres_trabaja_pobre = a+b,
    
  ) %>% pone_edos()

mujeres_nl <- mujeres %>% filter(ent==19) %>%
  pivot_longer(col=!c('ent','nom_ent','nom_abr'), names_to = "variable", values_to= "total" )



# *****************************************************************************
# Mothers of children ages 0 to 3
# *****************************************************************************
son_madres <- madres %>%
  group_by(ent) %>%
  summarise(
    # Madres de 0 a 3 anos
    madres = sum(factor),
    # madres + Jefas de familia
    madres_jefes = sum(factor[parentesco==101]),
    # madres + Menos de 25 anos
    madres_menos25 = sum(factor[edad<=25]),
    # madres + Union Libre
    madres_union = sum(factor[edo_conyug==1]),
    # madres + Casadas
    madres_casadas = sum(factor[edo_conyug==2]),
    # Madres + Pobreza
    madres_pobreza = sum(factor[pobreza==1]),
    # Madres + jefas de familia + Pobreza
    madres_jefas_pobreza = sum(factor[parentesco==101 & pobreza==1]),
    # Madres + trabajan
    madres_trab = sum(factor[tiene_trabajo==1], na.rm=TRUE),
    # Madres + Jefes de familia + trabajan
    madres_jefas_trab = sum(factor[parentesco==101 & tiene_trabajo==1], na.rm=TRUE),
    # Madres + Jefes de familia + trabajan + pobreza
    madres_jefas_trab_pobreza = sum(factor[parentesco==101 & tiene_trabajo==1 & pobreza==1], na.rm=TRUE),
    # madres + Menos de 25 anos + Pobreza
    madres_menos25_pobreza = sum(factor[edad<=25 & pobreza==1]),
    # madres + Union Libre + Pobreza
    madres_union_pobreza = sum(factor[edo_conyug==1 & pobreza==1]),
    # madres + Casadas + Pobreza
    madres_casadas_pobreza = sum(factor[edo_conyug==2 & pobreza==1]),
  ) %>%
  pone_edos()

madres_nl <- son_madres %>% filter(ent==19) %>%
  pivot_longer(col=!c('ent','nom_ent','nom_abr'), names_to = "variable", values_to= "total" )


# *****************************************************************************
# Mothers of children ages 0 to 3, Nuevo leon
# *****************************************************************************
madres_ninos03 <- madres %>% filter(ent==19) %>%
  mutate(edo_civil_label = recode(edo_conyug,
                                  `1` = "Unión libre",
                                  `2` = "Casada",
                                  `3` = "Separada",
                                  `4` = "Divorciada",
                                  `5` = "Viuda",
                                  `6` = "Soltera"),
         edo_civil_label2 = case_when(
           edo_conyug == 1 ~ "Unión libre",
           edo_conyug == 2 ~ "Casada",
           (edo_conyug == 3 | edo_conyug == 4 |
              edo_conyug == 5) ~ "Separada, divorciada o viuda",
           edo_conyug == 6 ~ "Soltera"),
         edad_label = case_when(
           edad <= 25 ~ '25 años o menos',
           edad >= 26 & edad <= 29 ~ '26-29 años',
           edad >= 30 & edad <= 34 ~ '30-34 años',
           edad >= 35 & edad <= 39 ~ '35-39 años',
           edad >= 40 ~ '40 y más'
         ),
         educacion_label = case_when(
           nivelaprob == 0 ~ 'Sin instrucción',
           nivelaprob == 1 ~ 'Preescolar',
           nivelaprob == 2 ~ 'Primaria',
           nivelaprob == 3 ~ 'Secundaria',
           nivelaprob == 4 ~ 'Preparatoria o bachillerato',
           nivelaprob == 5 ~ 'Normal',
           nivelaprob == 6 ~ 'Carrera técnica o comercial',
           nivelaprob == 7 ~ 'Profesional',
           nivelaprob == 8 ~ 'Maestría',
           nivelaprob == 9 ~ 'Doctorado')
  )

#Distribucion por pobreza y estado civil
madres1 <- madres_ninos03 %>% group_by(pobreza_label, edo_civil_label) %>%
  summarise(n= sum(factor, na.rm=T))

#Distribucion por pobreza y edad
madres2 <- madres_ninos03 %>% group_by(pobreza_label, edad) %>%
  summarise(n= sum(factor, na.rm=T))

#Distribucion por pobreza y num de hijos
madres3 <- madres_ninos03 %>% group_by(pobreza_label) %>%
  summarise(un_hijo= sum(factor[num_hijos03==1], na.rm=T),
            dos_hijos= sum(factor[num_hijos03==2], na.rm=T),
            tres_hijos= sum(factor[num_hijos03==3], na.rm=T),
  )
#Distribucion por pobreza y educacion
madres3 <- madres_ninos03 %>% group_by(pobreza_label, educacion_label) %>%
  summarise(n= sum(factor, na.rm=T))


#Distribucion por pobreza y estado civil
data_figMadres <- madres_ninos03 %>% 
  group_by(pobreza_label, edo_civil_label2) %>%
  summarise(n= sum(factor, na.rm=T)) 
data_figMadres$total <- sum(data_figMadres$n)
data_figMadres <- data_figMadres %>%
  mutate(porc_n = n/total,
         porc_n = round(porc_n*100,1))

edo_civil_eng <- c("Married","Separated, divorced or widowed", "Single", "Free union",                 
                   "Married","Separated, divorced or widowed", "Single", "Free union")
fig_madres <- ggplot(data_figMadres, aes(area = n, 
                           fill = pobreza_label,
                           label = paste0(edo_civil_eng,"\n",porc_n,"%" ),
                           subgroup = pobreza_label,
                           subgroup2 = edo_civil_label2)) +
  geom_treemap()+
  scale_fill_manual(values = c("#66bfff", "#ffb300") ) +
  geom_treemap_subgroup_border(colour = "white", size=5) +
  geom_treemap_subgroup2_border(colour = "white", size=2) +
  geom_treemap_text(
                    fontface = "italic",
                    colour = "#222323",
                    place = "bottomleft",
                    grow = F,
                    size=10,
                    reflow = T) +
  geom_treemap_subgroup_text(place = "center",
                             grow = F,
                             size=0,
                             alpha = 0.5,
                             colour = "#FAFAFA",
                             min.size = 0,
                             ) +
  theme_economist() +
  theme(
    plot.title = element_text(size=13, margin=margin(b=5)),
    axis.text.x = element_text(size=11, angle = 90),
    axis.text.y = element_text(size=10),
    legend.position = "bottom",
    legend.text = element_text(size=12),
    plot.caption = element_text(size = 10, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#6D6D6D"),
    axis.line = element_blank()
  ) +
  labs(title=paste0('Madres de niños de 0 a 3 años'),
       subtitle ="",  
       x='', y="",
       fill = "",
       caption="Elaboración propia con datos de la ENIGH 2020.")


ggsave(filename = paste0(DIR,"/figs/fig_participacion_mujer.jpg"), plot = fig_madres, 
       width = 6.5, height = 7, units = "in", dpi=300)

