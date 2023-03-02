
## Import data (numbers of PS per country) ----

tab <- readxl::read_xlsx(here::here("outputs", 
                                    "detected_countries_pdfs.xlsx"))
tab <- as.data.frame(tab)


## Remove first and last pages ----

tab <- tab[which(tab$"page" != 1 & tab$"page" < tab$"n_pages"), ]

tab <- rev(sort(table(tab$"geographic_entity")))
tab <- as.data.frame(tab)
colnames(tab) <- c("country", "n_studies")


## Download World base map ----

get_world_basemap(path = here::here("data", "raw-data"))


## Open layer ----

world <- sf::st_read(here::here("data", "raw-data", 
                                "IPBES_Regions_Subregions2.shp"))


## Add country ISO code to data to map ----

tab <- merge(tab, geoparser::world_countries, 
             by.x = "country", by.y = "geographic_entity", 
             all.x = TRUE, all.y = FALSE)

tab <- tab[ , c("iso_alpha3", "country", "n_studies")]


## Check for mismatch before merging ----

which(!(tab$"iso_alpha3" %in% world$"ISO_3"))


## Add data to map in world map ----

world <- merge(world, tab, by.x = "ISO_3", by.y = "iso_alpha3", all = TRUE)


## Project layer ----

robin <-  "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
world <- sf::st_transform(world, robin)


# Create graticules ----

lat <- c( -90,  -60, -30, 0, 30,  60,  90)
lon <- c(-180, -120, -60, 0, 60, 120, 180)

grat <- graticule::graticule(lons = lon, lats = lat, proj = robin, 
                             xlim = range(lon), ylim = range(lat))


## Define Colors ----

classes <- data.frame(from  = c( 1, 10, 20, 30, 40,  50,  100),   # x >  from
                      to    = c(10, 20, 30, 40, 50, 100, 9999),   # x <= to
                      label = c("1", "10", "20", "30",  "40", "50", ">100"),
                      color = c("#FFFFFF", "#FFCBFE", "#FF98FD", "#FF63FC", 
                                         "#FF00FC", "#CF00C9", "#680064"))
                                         
world$"color" <- NA

for (i in 1:nrow(classes)) {
  
  pos <- which(world[ , "n_studies", drop = TRUE] >  classes[i, "from"] & 
                 world[ , "n_studies", drop = TRUE] <= classes[i, "to"])
  
  if (length(pos)) world[pos, "color"] <- classes[i, "color"]
}


## Color for NA values ----

pos <- which(is.na(world[ , "n_studies", drop = TRUE]))
if (length(pos)) world[pos, "color"] <- "#f0f0f0"
  

## Other colors ----

borders <- "#c8c8c8"
texte   <- "#666666"
  

# Graphical Device ----

png(file = here::here("figures", "map-n_studies_pdfs.png"), width = 12, 
    height = 7.5, units = "in", res = 300)


## Base map + Data + Graticules ----

par(mar = rep(1, 4), family = "serif")

sp::plot(grat, lty = 1, lwd = 0.2, col = borders)

plot(sf::st_geometry(world), col = world$"color", border = borders, 
     lwd = 0.2, add = TRUE)


## Legend ----

x_length <- 1000000
x_start  <- -1 * (x_length * (nrow(classes) / 2))

if (nrow(classes) %% 2 == 0) x_start <- x_start - (x_length / 2)

y_height <-    500000
y_middle <- -10000000


par(xpd = TRUE)

for (i in 1:nrow(classes)) {
  
  rect(xleft   = x_start + (i - 1) * x_length, 
       xright  = x_start + i * x_length,
       ybottom = y_middle - y_height, 
       ytop    = y_middle + y_height,
       col     = classes[i, "color"], border = borders)
  
  text(x      = x_start + (i - 1) * x_length, 
       y      = y_middle - y_height, 
       labels = classes[i, "label"],
       pos = 1, cex = 0.9, col = texte)
}


## Title ----

text(x = 0, y = y_middle + y_height, col = texte, font = 2, pos = 3,
     labels = "Number of primary studies (full texts)")

par(xpd = FALSE)

dev.off()
