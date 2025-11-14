library(rnaturalearth)
library(terra)
library(ggplot2)


# Load study area
study_area_pol <- ne_countries(country = c("Australia"), scale = "large", 
                               returnclass = "sf")
study_area_pol <- vect(study_area_pol)

# Select and download raster files of variables from rnaturalearth
gaz_type <- c("roads", "urban_areas", "rivers_lake_centerlines", 
              "railroads", "populated_places")
category <- c("cultural", "physical")[c(1, 1, 2, 1, 1)]
scales <- 10
names_gaz <- c("roads", "cities", "waterbodies", 
               "railroads","populated places")
gaz <- list()
counter <- 0
for (i in seq_along(gaz_type)){
  ref <- try(suppressWarnings
             (terra::vect
               (rnaturalearth::ne_download(scale = scales,
                                           type = gaz_type[i], 
                                           category = category[i], 
                                           returnclass = "sf"))),
             silent = TRUE)
  if (inherits(ref, "try-error")){
    a <- paste("Gazetteer for", gaz_type[i], "not found at\n%s")
    warning(sprintf(a, rnaturalearth::ne_file_name(scale = scales,
                                                   type = gaz_type[i], 
                                                   full_url = TRUE)))
    warning(paste("Skipping", gaz_type[i], "from bias analysis"))
  }
  else{
    counter <- counter + 1
    gaz[[counter]] <- ref
    names(gaz)[[counter]] <- names_gaz[[counter]]
  }
}

# Crop layers
gaz_cut <- lapply(gaz, function(x) {crop(x, study_area_pol)})

# Plot the map
ggplot() +
  geom_spatvector(data = study_area_pol, aes(geometry = geometry), 
          fill = '#8B7E66', color = "#8B7E70", linewidth = 0.5) + 

  geom_spatvector(data = gaz_cut$roads, aes(geometry = geometry),
                  fill = NA, color = "grey30", linewidth = 0.5,
                    linetype = "dotted") + 
  geom_spatvector(data = gaz_cut$railroads, aes(geometry = geometry),
                  fill = NA, color = "#787838", linewidth = 0.5,
                  linetype = "longdash") +
  geom_spatvector(data = gaz_cut$waterbodies, aes(geometry = geometry), 
                  fill = NA, color = "#7A8B9B", linewidth = 0.5) + 
  geom_spatvector(data = gaz_cut$cities, aes(geometry = geometry),
                  fill = NA, color = "#FFFAF0", linewidth = 0.8) +
  geom_spatvector(data = gaz_cut$`populated places`, aes(geometry = geometry),
                  fill = NA, color = "#8B475D", size = 0.7) +
  # extent 
  xlim(112, 160) +  ylim(-45, -9) +
  # legend
  annotate("text", x = Inf, y = -Inf, label = "Populated places", 
           color = "#8B475D", size = 2.5, fontface = "bold.italic",
           family = "serif", 
           hjust = 1.05, vjust = -1.5) +
  annotate("text", x = Inf, y = -Inf, label = "Waterbodies", 
           color = "#7A8B9B", size = 2.5, fontface = "bold.italic",
           family = "serif", hjust = 1.1, vjust = -3.5) +
  annotate("text", x = Inf, y = -Inf, label = "Railroads", 
           color = "#787838", size = 2.5, fontface = "bold.italic",
           family = "serif", hjust = 1.1, vjust = -5.5) +
  annotate("text", x = Inf, y = -Inf, label = "Roads", 
           color = "grey30", size = 2.5, fontface = "bold.italic",
           family = "serif", hjust = 1.1, vjust = -7.5) +
  annotate("text", x = Inf, y = -Inf, label = "Cities", 
           color = "#FFFAF0", size = 2.5, fontface = "bold.italic",
           family = "serif", hjust = 1.05, vjust = -9.5) +
  # Labels and theme
  labs(title = 'Natural Earth map of Australia',
       caption = 'More info data source in www.naturalearthdata.com\n 
       C. Ronquillo for #30DayMapChallenge') +
  theme_minimal() +
  theme(
    plot.margin = margin(15,15,15,15),
        panel.grid.major = element_line(color = alpha("#787858", 0.2)), 
          plot.background = element_rect(fill = "#CDC8B1", color = '#CDC8B1'),
    plot.title = element_text(
      color = "grey10", size = 14,
      face = "italic", family = "serif",
      hjust = 0.5, margin = margin(b = 5)),
    plot.caption = element_text(
      color = "gray40", size = 7, face = "italic",
      hjust = 1, margin = margin(t = 10)),
    
    axis.text.x = element_text(family = "serif", color = "grey10", 
                               face = "italic", size = 7),
    axis.text.y = element_text(family = "serif", color = "grey10",
                               face = "italic", size = 7)
    )


ggsave('naturalEarthAustralia.svg', plot = last_plot(),
       dpi = 600, width = 10, height = 10, units = 'in')

