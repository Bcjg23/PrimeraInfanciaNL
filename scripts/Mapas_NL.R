setwd(DIR)

library(rgdal)
library(ggplot2)
library(rgeos)
library(dplyr)
library(viridis)
library(gtable)
library(grid)
library(readr)
library(stringr)


################################################################################
# Define functions
################################################################################
pretty_map <- function(df_data, df_map, var_map){
  ## Info de la variable a mapear
  lvls <- levels(cut(df_data[, var_map], quantile(df_data[, var_map], 0:10 / 10)))
  lim_inf <- as.numeric( sub("\\((.+),.*", "\\1", lvls) )
  lim_sup <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", lvls) )
  
  pretty_breaks <- lim_sup
  
  # find the extremes
  minVal <- min(df_data[, var_map], na.rm = T)
  maxVal <- max(df_data[, var_map], na.rm = T)
  brks <- c(minVal, pretty_breaks, maxVal)
  # compute labels
  labels <- c()
  labels_interval <- c()
  # round the labels (actually, only the extremes)
  for (idx in 1:length(brks)) {
    labels <- c(labels, round(brks[idx + 1], 2))
    labels_interval <- c(labels_interval, 
                         c(paste0("(", round(brks[idx], 2), ",", 
                                  round(brks[idx + 1], 2), "]"
                                 )
                          )
                         )
  }
  labels <- labels[1:length(labels) - 1]
  labels_interval <- c(labels_interval[1:(length(labels_interval) - 3)], 
                       paste0("Más de ", round(brks[length(brks) -2], 2)),
                       labels_interval[11]
  )
  
  # define a new variable on the data set just as above
  df_map$brks <- cut(df_map[, var_map],
                     breaks = unique(brks),
                     include.lowest = TRUE,
                     labels = unique(labels)
  )
  
  return(list(df_map,levels(factor(labels[1:10])),labels_interval[1:10]))
}


################################################################################
# Load data
################################################################################
data_NL <- read.csv("data/dataNL_mun.csv",
                    colClasses = c("cve_entmun"="character",
                                   "ent"="character",
                                   "nom_ent"="character",
                                   "nom_abr"="character",
                                   "cve_mun"="character",
                                   "nom_mun"="character"
                                  )
                    ) %>% 
  rename_with(recode, ent = "cve_ent") %>%
  mutate(CLAVE = as.factor(cve_entmun))




################################################################################
## Map set up
################################################################################
mx <- readOGR(dsn = "Mapas/data", layer = "nl_municipio")
#head(mx@data, n = 10)
sapply(mx@data, class)
mx$id <- row.names(mx)
#head(mx@data, n = 10)

#Generamos data frame con datos de polígonos
mx_f <- fortify(mx, region = "id")

#Checamos estructura de los datos
#head(mx_f, n=10)
#Verificamos cuantos coinciden
mx$CVEGEO[!mx$CVEGEO %in% data_NL$CLAVE]

#Vemos estructura de datos
sapply(data_NL, class)
#Cambiamos si es necesario
data_NL$cve_ent <- as.factor(as.character(data_NL$cve_ent))
data_NL$cve_mun <- as.factor(as.character(data_NL$cve_mun ))
#Realizamos merge entre datos de población e info de mapas
mx@data <- merge(mx@data, data_NL, by.x="CVEGEO", by.y = "CLAVE")

#Left join entre datos de merge y la info de polígonos
mx_f <- left_join(mx_f, mx@data)





################################################################################
## Map: Children ages 0 to 3 (Absolute numbers)
################################################################################
df <- pretty_map(data_NL, mx_f, "total_ninos03_2020")

colr <- c('#FCD7C8','#FAC5B0','#FABAA0','#F8A584','#F7A07D',
          '#F47643','#F26026','#F15517','#D2450C','#B23A0A')

map_children <- ggplot() +
  geom_polygon(data = df[[1]], aes(long, lat, group = group, fill = brks), 
               alpha=0.9, show.legend = T) +
  geom_path(data = mx_f, aes(x = long, 
                             y = lat, 
                             group = group), 
            color = "gray", size = 0.15) +
  coord_equal() +
  scale_fill_manual(name = "Niños de 0 a 3 años", values = colr , 
                    breaks = df[[2]] , labels = df[[3]])+
  theme_minimal() +
  theme(
    plot.margin = unit(c(.5,.5,.2,.5), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    plot.caption = element_text(size = 8, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#6D6D6D"),
    axis.text.x = element_text(size=11),
    axis.text.y = element_text(size=11),
    legend.position = "right",
    legend.title=element_text(size=8),
    legend.text = element_text(size=8),
    legend.key.size = unit(0.7, 'cm')
  )

ggsave(filename = "figs/mapa_ninos.jpg", plot = map_children)

################################################################################
## Map: Children ages 0 to 3 (Percentage)
################################################################################
df <- pretty_map(data_NL, mx_f, "porc_ninos03_2020")

colr <- c('#FCD7C8','#FAC5B0','#FABAA0','#F8A584','#F7A07D',
          '#F47643','#F26026','#F15517','#D2450C','#B23A0A')

map_childrenPercent <- ggplot() +  
  geom_polygon(data = df[[1]], aes(long, lat, group = group, fill = brks), 
               alpha=0.9, show.legend = T) +
  geom_path(data = mx_f, aes(x = long, 
                             y = lat, 
                             group = group), 
            color = "gray", size = 0.15) +
  coord_equal() +
  scale_fill_manual(name = "Niños de 0 a 3 años\n(Porcentajes)", values = colr , 
                    breaks = df[[2]] , labels = df[[3]])+
  theme_minimal() +
  theme(
    plot.margin = unit(c(.5,.5,.2,.5), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    plot.caption = element_text(size = 8, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#6D6D6D"),
    axis.text.x = element_text(size=11),
    axis.text.y = element_text(size=11),
    legend.position = "right",
    legend.title=element_text(size=8),
    legend.text = element_text(size=8),
    legend.key.size = unit(0.7, 'cm')
  )+
  labs(x = "Long", 
       y = "Lat",
       caption = "Fuente: Elaboración propia con datos del CENSO 2020."
  ) 

ggsave(filename = "figs/mapa_porcNinos.jpg", plot = map_childrenPercent)



################################################################################
## Poverty rate (general population) 2015
################################################################################
df <- pretty_map(data_NL, mx_f, "porc_pobreza_2015")

colr <- c("#fde4e4", "#f79292", "#f57777", "#f14141", "#e71111",
          "#cc0f0f", "#b10d0d", "#950b0b", "#7a0909", "#3f0505")

map_gralPovertyRate <- ggplot() +  
  geom_polygon(data = df[[1]], aes(long, lat, group = group, fill = brks), 
               alpha=0.9, show.legend = T) +
  geom_path(data = mx_f, aes(x = long, 
                             y = lat, 
                             group = group), 
            color = "gray", size = 0.15) +
  coord_equal() +
  scale_fill_manual(name = "Población en pobreza\n(Porcentajes)", values = colr , 
                    breaks = df[[2]] , labels = df[[3]])+
  theme_minimal() +
  theme(
    plot.margin = unit(c(.5,.5,.2,.5), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    plot.caption = element_text(size = 8, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#6D6D6D"),
    axis.text.x = element_text(size=11),
    axis.text.y = element_text(size=11),
    legend.position = "right",
    legend.title=element_text(size=8),
    legend.text = element_text(size=8),
    legend.key.size = unit(0.7, 'cm')
  ) +
  labs(x = "Long", 
       y = "Lat",
       caption = "Fuente: Elaboración propia con datos del CENSO 2020."
  ) 

ggsave(filename = "figs/mapa_porcPobreza.jpg", plot = map_gralPovertyRate)


################################################################################
## People in poverty (general population) 2015
################################################################################
df <- pretty_map(data_NL, mx_f, "pobreza_pob_2015")

colr <- c("#fde4e4", "#f79292", "#f57777", "#f14141", "#e71111",
          "#cc0f0f", "#b10d0d", "#950b0b", "#7a0909", "#3f0505")

map_gralPoverty <- ggplot() +  
  geom_polygon(data = df[[1]], aes(long, lat, group = group, fill = brks), 
               alpha=0.9, show.legend = T) +
  geom_path(data = mx_f, aes(x = long, 
                             y = lat, 
                             group = group), 
            color = "gray", size = 0.15) +
  coord_equal() +
  scale_fill_manual(name = "Población en pobreza\n(Habitantes)", values = colr , 
                    breaks = df[[2]] , labels = df[[3]])+
  theme_minimal() +
  theme(
    plot.margin = unit(c(.5,.5,.2,.5), "cm"),
    panel.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    plot.caption = element_text(size = 10, 
                                hjust = 0.92, 
                                margin = margin(t = 0.2, 
                                                b = 0, 
                                                unit = "cm"), 
                                color = "#6D6D6D"),
    axis.text.x = element_text(size=11),
    axis.text.y = element_text(size=11),
    legend.position = "right",
    legend.title=element_text(size=8),
    legend.text = element_text(size=8),
    legend.key.size = unit(0.7, 'cm')
  ) +
  labs(x = "Long", 
       y = "Lat",
       caption = "Fuente: Elaboración propia con datos del CENSO 2020."
  ) 

ggsave(filename = "figs/mapa_Pobreza.jpg", plot = map_gralPoverty)
