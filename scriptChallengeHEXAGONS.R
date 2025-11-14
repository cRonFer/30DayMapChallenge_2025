# Cargar librerías necesarias
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(rnaturalearth)
library(magick)
library(cowplot)

# Cargar datos
datos <- read.csv('iberbryo1SHORT.csv', sep=';')
pIb <- ne_countries(country = c("spain", "portugal"),
                    scale = "medium", returnclass = "sf")

# Convertir a objeto sf
puntos_sf <- st_as_sf(datos, coords = c("Longitude", "Latitude"), crs = 4326)

# Crear grid hexagonal que cubra la extensión de los puntos
bbox_sf <- st_as_sfc(st_bbox(puntos_sf))
# Calcular el centro del bbox
centro <- st_centroid(bbox_sf)
coords_centro <- st_coordinates(centro)

# Calcular dimensiones del bbox
ancho <- st_bbox(puntos_sf)["xmax"] - st_bbox(puntos_sf)["xmin"] + 3
alto <- st_bbox(puntos_sf)["ymax"] - st_bbox(puntos_sf)["ymin"] 

# Calcular el radio para el hexágono (usar la dimensión más grande)
radio <- max(ancho, alto) / 2

# Función para crear un hexágono regular
crear_hexagono <- function(centro_x, centro_y, radio) {
  # Calcular los 6 vértices del hexágono (cada 60 grados)
  angulos <- seq(0, 300, by = 60)  # 0, 60, 120, 180, 240, 300 grados
  vertices <- data.frame(
    x = centro_x + radio * cos(angulos * pi / 180),
    y = centro_y + radio * sin(angulos * pi / 180)
  )
  
  # Cerrar el polígono (añadir primer punto al final)
  vertices <- rbind(vertices, vertices[1, ])
  
  # Crear polígono SF
  st_polygon(list(as.matrix(vertices))) %>%
    st_sfc(crs = 4326) %>%
    st_sf()
}

# Crear el hexágono
area_estudio <-  crear_hexagono(
  centro_x = coords_centro[1, "X"],
  centro_y = coords_centro[1, "Y"], 
  radio = radio
)


# Crear grid hexagonal
grid_hex <- st_make_grid(
  x = area_estudio,
  cellsize = 0.5, # Tamaño de celda (ajustar según necesidad)
  square = FALSE  # FALSE para hexágonos, TRUE para cuadrados
) %>%
  st_sf() %>%
  mutate(celda_id = row_number())

# 3. Cortar el grid usando el hexágono como máscara
hex_grid_hex <- st_intersection(grid_hex, area_estudio)

# Contar número de especies únicas por celda
puntos_en_celdas <- st_join(puntos_sf, hex_grid_hex, join = st_intersects)

riqueza_especies <- puntos_en_celdas %>%
  st_drop_geometry() %>%
  group_by(celda_id) %>%
  summarise(
    n_species = n_distinct(species),
    n_puntos = n()
  ) %>%
  ungroup()

# Unir la riqueza de especies al grid
grid_hex_riqueza <- hex_grid_hex %>%
  left_join(riqueza_especies, by = "celda_id") %>%
  mutate(n_species = ifelse(is.na(n_species), 0, n_species))

# Cargar imagen PNG
imagen <- image_read("Moss.png") %>%
  image_colorize(100, "black")      
imagen <- as.raster(imagen)

imagen2 <- image_read("legend.png") 
imagen2 <- as.raster(imagen2)
# Visualizar con ggplot2 usando viridis magma
mapa_principal <- ggplot() +
  geom_sf(data = grid_hex_riqueza, aes(fill = n_species), color = "white",
          size = 0.3) +
  geom_sf(data = pIb, color ='gray20', size = 0.5, fill='transparent') +
  scale_fill_viridis(option = "magma",trans = "log10", 
                     name = "Log10") +
  labs(title = "Riqueza de especies de briófitos en la península Ibérica",
       caption = 'Datos extraídos de IberBryo (Ronquillo et al 2020)') +
  theme_minimal() +
  # xlim(-10,5)+
  # ylim(30,47) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'none',
        panel.grid = element_blank()) +
  coord_sf(crs = st_crs(25830))



ggdraw() +
  draw_plot(mapa_principal) +
  draw_image(
    imagen,
    x = 0.9,    # Esquina derecha (0.85 = 85% a la derecha)
    y = 0.1,    # Esquina inferior (0.05 = 5% desde abajo)
    width = 0.3,  # Ancho de la imagen
    height = 0.3, # Alto de la imagen
    hjust = 1,   # Alineación derecha
    vjust = 0    # Alineación inferior
  )+     # Alto relativo
  draw_image(
    imagen2,
    x = 1,    # Esquina derecha (0.85 = 85% a la derecha)
    y = 0.6,
    width = 0.3,  # Ancho de la imagen
     height = 0.3,
    hjust = 1,   # Alineación derecha
    vjust = 0 )

