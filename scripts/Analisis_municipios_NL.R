setwd(DIR)

library(dplyr)
library(stringr)

# ******************************************************************************
## Define functions & Global vars
# ******************************************************************************
pone_municipios <- function(df){
  # Relaciona las claves de los municipio con sus respectivos nombres en una columna adicional
  dict_muns <- read.csv("data/input/INEGI_edos_muns.csv", encoding = "UTF-8",
                        colClasses = c("Cve_Ent"="character",
                                       "Cve_Mun"="character",
                                       "Cve_EntMun"="character")
                        ) 
  colnames(dict_muns) <- tolower(colnames(dict_muns))
  df <-  df %>% left_join(dict_muns, by = c("ent"="cve_ent" ,"Cve_Mun"="cve_mun") )
  return(df)
}

ZMM_mun <- c("Monterrey", "Apodaca", "García",
             "Escobedo", "Guadalupe", "Juárez", 
             "San Nicolás", "San Pedro", "Santa Catarina")

ZMM_cve_mun <- c("19039", "19006", "19018",
                 "19021", "19026", "19031",
                 "19046", "19019", "19048")

# ******************************************************************************
## Population - CENSO 2020
# ******************************************************************************
pobCenso2020_xMun <- read.csv("data/input/poblacion_nl_cpv2020.csv",
                      encoding = "UTF-8",
                      colClasses=c("Cve_Ent"="character",
                                   "Cve_Mun"="character"
                                   )
                      ) %>%
  ## Population by municipality 
  group_by(Cve_Mun) %>% 
  summarise(
    poblacion_2020 = sum(Población.total[Edad.desplegada=="Total"], na.rm = T),
    mujeres_2020 = sum(Mujeres[Edad.desplegada=="Total"], na.rm = T),
    hombres_2020 = sum(Hombres[Edad.desplegada=="Total"], na.rm = T),
    
    total_ninos0_2020 = sum(Población.total[Edad.desplegada=="00 años"], na.rm = T),
    total_ninos1_2020 = sum(Población.total[Edad.desplegada=="01 años"], na.rm = T),
    total_ninos2_2020 = sum(Población.total[Edad.desplegada=="02 años"], na.rm = T),
    total_ninos3_2020 = sum(Población.total[Edad.desplegada=="03 años"], na.rm = T),
    
    total_ninos03_2020 = total_ninos0_2020 + total_ninos1_2020 + 
      total_ninos2_2020 + total_ninos3_2020,
    
    ninas0_2020 = sum(Mujeres[Edad.desplegada=="00 años"], na.rm = T),
    ninas1_2020 = sum(Mujeres[Edad.desplegada=="01 años"], na.rm = T),
    ninas2_2020 = sum(Mujeres[Edad.desplegada=="02 años"], na.rm = T),
    ninas3_2020 = sum(Mujeres[Edad.desplegada=="03 años"], na.rm = T),
    
    ninas03_2020 = ninas0_2020 + ninas1_2020 + ninas2_2020 + ninas3_2020,
  )%>% 
  mutate(
         porc_ninos03_2020 = round((total_ninos03_2020/poblacion_2020)*100,2),
  )

# ******************************************************************************
## Poverty by Municipality - CONEVAL 2015
# ******************************************************************************
pobreza2015_xMun <- read.csv("data/input/pobreza_calibrada_municipal_Coneval2015.csv",
                    encoding = "UTF-8",
                    colClasses=c("cve_mun"="character",
                                 "ent" = "character")
                    ) %>%
  filter(ent=="19") %>% 
  mutate(zmm = case_when(cve_mun %in% ZMM_cve_mun ~ 1, TRUE ~0),
         cve_mun = str_extract(cve_mun,".{3}$" ),
         porc_pobreza_2015 = round(pobreza*100,2),
         pobreza_pob_2015 = pobreza_pob,
         pobtot_2015 = pobtot,
  ) %>% 
  select(ent, cve_mun, pobtot_2015, pobreza_pob_2015, porc_pobreza_2015, zmm)

pobreza2015_xMun <- rbind(pobreza2015_xMun, 
                          c("19", "000", 
                            colSums(pobreza2015_xMun[,c("pobtot_2015",
                                                        "pobreza_pob_2015", 
                                                        "porc_pobreza_2015")],
                                    na.rm = TRUE)))


# ******************************************************************************
## Final DB
# ******************************************************************************
dataNL_mun <- left_join(pobCenso2020_xMun, pobreza2015_xMun,
                        by = c("Cve_Mun"= "cve_mun")) %>%
  pone_municipios() %>% 
  rename_with(tolower) %>%
  select(cve_entmun, ent, nom_ent, nom_abr, cve_mun, nom_mun,
         poblacion_2020, mujeres_2020, hombres_2020, 
         total_ninos0_2020, total_ninos1_2020, total_ninos2_2020,
         total_ninos3_2020, total_ninos03_2020, 
         ninas0_2020, ninas1_2020, ninas2_2020, ninas3_2020, ninas03_2020,
         pobtot_2015, pobreza_pob_2015, 
         porc_ninos03_2020, porc_pobreza_2015, zmm)

write.csv(dataNL_mun, "data/dataNL_mun.csv", row.names = F)
