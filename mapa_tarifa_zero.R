
library(maptiles)
library(terra)
library(geobr)
library(sf)
library(ggplot2)
library(dplyr)
library(here)
library(ggtext)

# Baixar mapa base do Brasil com Esri.WorldShadedRelief
brasil_ext <- ext(c(xmin = -75, xmax = -30, ymin = -35, ymax = 10))
tiles_brasil <- get_tiles(brasil_ext, 
                          provider = "Esri.WorldShadedRelief", 
                          crop = TRUE, 
                          zoom = 5,
                          forceDownload = TRUE)

# Buscar os municípios pelo código IBGE
codigos_ibge <- c(
  2211001, 2303709, 4304606, 3301900, 5212501, 3302700, 3129806, 3548807, 3522307, 4118204,
  4202008, 5107925, 5208004, 1100304, 3134202, 3504008, 3302270, 1100049, 3162500, 3522406,
  2301000, 4105508, 2304285, 3527108, 4201406, 4110706, 3555000, 3140001, 3537305, 3505906,
  3540606, 3151206, 3546801, 3301850, 3537800, 3111200, 3137205, 4314050, 3503802, 3138401,
  3556453, 3524808, 4117602, 3143104, 3301306, 3546405, 3510203, 3163706, 3511508, 3523206,
  2921005, 3303609, 3104205, 4115705, 3304805, 3109006, 3145901, 3110004, 3165537, 4122206,
  3500709, 3139003, 3305000, 3554508, 3151503, 3162922, 4314803, 4119608, 4205456, 4111506,
  3519303, 4205704, 4111258, 3305752, 3518305, 3116605, 4212809, 4109708, 3512209, 5003256,
  4202453, 3162955, 3520442, 4120804, 3547502, 3553807, 3514106, 5208905, 4104659, 5007109,
  3548005, 3100203, 3144102, 3502754, 305604, 3301405, 3553302, 4218301, 3301108, 4128500,
  3540804, 4116208, 3532405, 5201306, 4101200, 3302809, 4206009, 3301207, 3501152, 4201950,
  3528007, 4107603, 3140159, 3549953, 3519055, 3512308, 4105706, 3204302, 3522802, 3532009,
  3133709, 3520426, 3123205, 3520202, 3104106, 3300951, 3106408, 5202502, 3514304, 3162948,
  3305307, 4314209, 3130408, 3135407, 3146701, 3122702, 3158607
)

# Buscar os municípios no geobr
municipios <- read_municipality(code_muni = "all", year = 2020)

# Filtrar apenas os municípios com tarifa zero
tarifa_zero <- municipios %>% 
  filter(code_muni %in% codigos_ibge)

# encontrar os pontos onde tem tarifa_zero:

tarifa_zero_pontos <- tarifa_zero %>%
  st_centroid()


# dados para as anotacoes: 

conchas <- tarifa_zero %>% 
  filter(code_muni == "3512308")

caucaia <- tarifa_zero %>% 
  filter(code_muni == "2303709")

teresina <- tarifa_zero %>% 
  filter(code_muni == "2211001")


# Calcular os centróides
conchas_ponto <- conchas %>% 
  st_centroid()

coords_conchas <- st_coordinates(conchas_ponto)

teresina_ponto <- teresina  %>% 
  st_centroid()

coords_teresina <- st_coordinates(teresina_ponto)

caucaia_ponto <- caucaia %>% 
  st_centroid()

coords_caucaia <- st_coordinates(caucaia_ponto)


# Converter o raster para dataframe
tiles_df <- as.data.frame(tiles_brasil, xy = TRUE)

# Identificar valores baixos (oceano) no raster
valores <- values(tiles_brasil)
threshold <- quantile(valores, 0.1, na.rm = TRUE)

tiles_df_mod <- tiles_df %>%
  mutate(
    fill_type = ifelse(Esri.WorldShadedRelief_5_9_15_1 < threshold, "oceano", "terra"),
    terra_value = ifelse(fill_type == "terra", Esri.WorldShadedRelief_5_9_15_1, NA)
  )

estados <- read_state(year = 2020)



# Habemus mapa! 

tf <- ggplot() +
  geom_raster(data = tiles_df_mod %>% filter(fill_type == "oceano"), 
              aes(x = x, y = y), fill = "#C8D6E5") +
  
  geom_raster(data = tiles_df_mod %>% filter(fill_type == "terra"), 
              aes(x = x, y = y, fill = terra_value)) +
  scale_fill_gradient(low = "#C8D6E5", high = "#EBE3E1") +
  
  geom_sf(data = estados, 
          fill = "#F4EAE6", 
          color = "#cccccc", 
          linewidth = 0.3,
          alpha = 0.6) +
  
  geom_sf(data = tarifa_zero_pontos, 
          color = "#355c7d", 
          size = 1.5, 
          shape = 21, 
          alpha = 0.8) +
  geom_sf(data = tarifa_zero_pontos, 
          color = "#355c7d", 
          size = 0.5, 
          shape = 20, 
          alpha = 0.8) +
  geom_curve(
    aes(x = coords_conchas[1], xend = -47,  
        y = coords_conchas[2], yend = -27), 
    arrow = arrow(length = unit(0.11, "cm"), type = "closed"),
    color = "#333333",
    linewidth = 0.2,
    curvature = 0.3,
    angle = 90,
    alpha = 0.7
  ) +
  
  geom_label(
    aes(x = -42, y = -28, 
        label = "Conchas é a cidade mais\nantiga com tarifa zero,\nadotou em 1992"),
    family = "Ubuntu",
    size = 8.4,
    fill = "transparent",
    color = "black",
    alpha = 0.3,
    label.size = 0,
    lineheight = 0.25
  ) +
  geom_curve(
    aes(x = coords_teresina[1], xend = -42, 
        y = coords_teresina[2], yend = -2), 
    arrow = arrow(length = unit(0.11, "cm"), type = "closed"),
    color = "#333333",
    linewidth = 0.10,
    curvature = 0.3,
    angle = 90
  ) +
  
  geom_label(
    aes(x = -42, y = -0.5, 
        label = "Teresina é a única\ncapital e tem o único\nmetrô tarifa zero"),
    family = "Ubuntu",
    size = 8.4,
    fill = "transparent",
    color = "black",
    alpha = 0.3,
    label.size = 0,
    lineheight = 0.25
  ) +
  geom_curve(
    aes(x = coords_caucaia[1], xend = -38, 
        y = coords_caucaia[2], yend = -7), 
    arrow = arrow(length = unit(0.11, "cm"), type = "closed"),
    color = "#333333",
    linewidth = 0.10,
    curvature = 0.3,
    angle = 90
  ) +
  
  geom_label(
    aes(x = -34, y = -7, 
        label = "Caucaia no Ceará\né a maior cidade com\ntarifa zero no ônibus\nsão + de 400k hab."),
    family = "Ubuntu",
    size = 8.4,
    fill = "transparent",
    color = "black",
    alpha = 0.3,
    label.size = 0,
    lineheight = 0.25
  ) +
  
  coord_sf(expand = FALSE) +
  labs(
    title = "Tarifa Zero no Brasil",
    subtitle = "Atualmente existem 137 cidades com adoção da politica
    de tarifa zero universal no transporte público. 
      No mapa cada ponto é uma cidade e nele podemos ver os estados com
      <span style='font-family:\"Ubuntu\"; color: #355c7d; face:\"bold\"'>as maiores concentrações</span>",
    caption = "Fonte: Santini, Daniel, 2024"
    
  ) +
  theme_void(base_family = "Ubuntu") +
  theme(
    plot.background = element_rect(colour = "white", fill = "white"),
    panel.background = element_rect(colour = "white", fill = "white"),
    plot.title = element_text( family = "Ubuntu", size = 70, hjust = 0.5, margin = margin(b = 13), vjust = 0.1),
    plot.caption = element_text(family = "Ubuntu", size = 20, hjust = 0.96, vjust = 14),
    plot.title.position = "panel",
    plot.subtitle = element_textbox(
      width = 0.9, hjust = 0.5, lineheight = 0.3, 
      margin = margin(b = 2), size = 35, fill = "white", box.color = "white"),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0)
  )

ggsave(here("plot", "tf.png"), width = 4.4, height = 5.4, dpi = 400)

