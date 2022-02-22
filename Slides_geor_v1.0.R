## ----setup, include = FALSE----------------------------------------------------------------------------------
# opciones predeterminadas
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(
  fig.width=9, fig.height=3.5, fig.retina=3,
  out.width = "80%",
  echo = TRUE,
  message = FALSE, 
  warning = FALSE,
  fig.show = TRUE,
  hiline = TRUE,
  fig.align = "center"
  )


## ----leo_datos, echo=TRUE------------------------------------------------------------------------------------
# lectura de geoR. Instalar si no lo tengo
library(geoR)

# lectura de los datos y coordenadas
data.co=read.table("data/Madrid_LOG_co50s10h.txt", header=TRUE)
MuniMadrid <- matrix(scan("data/MuniMadrid.txt"), ncol = 2, byrow = T)
estaciones <- read.table("data/coordata.txt", header = FALSE) # lo utilizo luego

# creación del objeto as.geodata()
head(data.co)
co.50s.10h<-as.geodata(obj=data.co, coords.col = 1:2, data.col = 3)


## ----sum_co--------------------------------------------------------------------------------------------------
# descriptivos 
summary(co.50s.10h)

# análisis gráfico
plot(co.50s.10h)

# Mapa de quintiles
points(co.50s.10h, pch=21, bg=8, lwd=4, cex.max=3, col=cm.colors(12) )


## ----ms_madrid-----------------------------------------------------------------------------------------------
library(dplyr); library(sf); library(leaflet)

MuniMadrid_sf <- st_as_sf(as.data.frame(MuniMadrid), coords = c("V1","V2"),
                          crs=st_crs(25830)) %>% st_transform(4326)
ha <- st_as_sf(data.co, coords = c("x","y"), crs=st_crs(25830)) %>%  st_transform(4326)
dato= data.co$co
dato <- sprintf("<strong>Level of CO: %s</strong>", round(data.co$co,2)) %>% lapply(htmltools::HTML)
leaflet(ha) %>% addTiles() %>% addMarkers(popup = dato, clusterOptions = markerClusterOptions()) %>%
 addCircleMarkers(radius = 7, color = "red", popup = dato)


## ----co_sem_empirico-----------------------------------------------------------------------------------------
bin1.co.50s.10h <- variog(co.50s.10h, uvec = seq(800, 7000, l = 10), tolerance = pi/8)
cloud.co.50s.10h <- variog(co.50s.10h, option = "cloud")


## ------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 2))
plot(bin1.co.50s.10h, ylab = "Semivariograma", main = "", col = 1, pch = 21, bg = "darkgray", lwd = 2)
plot(cloud.co.50s.10h, xlim = c(0, 7000), col = "darkgray", main = " ", pch = 16, ylab = " ")
lines(bin1.co.50s.10h, type = "b", pch = 22, bg = 8, lwd = 2, cex = 1.2, ylab = " ")


## ----eyefit--------------------------------------------------------------------------------------------------
# cuadro interactivo
plot(bin1.co.50s.10h, ylab = "Semivariograma", main = "", col = 2, pch = 21, bg = "darkgray", lwd = 2)
#semivar_teo <- eyefit(bin1.co.50s.10h)


## ----co_ajuste_estad, message=FALSE--------------------------------------------------------------------------
ols <- variofit(bin1.co.50s.10h, ini = c(0.134, 1800), cov.model = "spherical", 
    fix.nugget = FALSE, weights = "equal")
ml <- likfit(co.50s.10h, coords = co.50s.10h$coords, data = co.50s.10h$data, 
    cov.model = "spherical", ini = c(0.134, 1800), nugget = FALSE, fix.psiA = FALSE, 
    fix.psiR = FALSE, lik.method = "ML")
wls <- variofit(bin1.co.50s.10h, ini = c(0.134, 1800), cov.model = "spherical", 
    fix.nugget = FALSE, weights = "npairs")
reml <- likfit(co.50s.10h, coords = co.50s.10h$coords, data = co.50s.10h$data, 
    cov.model = "spherical", ini = c(0.134, 1800), fix.psiA = FALSE, fix.psiR = FALSE, 
    fix.nugget = FALSE, lik.method = "RML")

# Representa ambos semivariogramas (empírico y teórico) 

plot(bin1.co.50s.10h, ylab = "Semivariogram", main = " ", 
     col = 1, pch = 21, bg = "yellow", lwd = 2, cex = 1.2)

lines(ols, lwd = 2, lty = 3); 
lines(wls, lwd = 2, lty = 1); 
lines(ml, lwd = 1, lty = 1);
lines(reml, lwd = 2, lty = 2)
legend(0.55, 0.17, legend = c("OLS", "WLS", "ML", "REML"), 
       lty = c(3, 1, 1, 2), lwd = c(2, 2, 1, 2), cex = 0.7)


## ----co_kriging----------------------------------------------------------------------------------------------
# creamos una malla de interopolación
xx <- seq(min(MuniMadrid[, 1]), max(MuniMadrid[, 1]), l = 51)
yy <- seq(min(MuniMadrid[, 2]), max(MuniMadrid[, 2]), l = 51)
  
grid_prediccion <- expand.grid(xx, yy)


# Kriging ordinario
kc.co.2s.10h <- krige.conv(co.50s.10h, 
                           coords = co.50s.10h$coords, 
                           data = co.50s.10h$data, 
                           loc = grid_prediccion, 
                           #Opición 1: especificamos todos los parámetros del semivariorama
                           krige = krige.control(cov.model = "spherical", 
                                                 cov.pars = c(0.1403, 6096.4841), 
                                                 nugget = 0)
                           # Opición 2: especificamos el semivariograma
                           #krige = krige.control(obj.m = semivar_teo) #semivariograma teórico
                           )
str(kc.co.2s.10h)


## ----co_krig_sd, cecho=FALSE, include=FALSE------------------------------------------------------------------
par(mfrow = c(1, 2), mar = c(3.5, 3.5, 1, 0), mgp = c(1.5, 0.5, 0))
image(kc.co.2s.10h, borders = MuniMadrid, main = "2D prediction map", 
      ylim = c(4461000, 4499622), col = rainbow(256))  # 
points(estaciones, col = 1, lwd = 1)
legend.krige(x.leg = c(424854, 456141), y.leg = c(4461000, 4462200), kc.co.2s.10h$predict, 
    col = rainbow(256))
image(kc.co.2s.10h, borders = MuniMadrid, val = sqrt(kc.co.2s.10h$krige.var), 
    main = "Prediction SD map", ylim = c(4461000, 4499622), col = terrain.colors(64))
points(estaciones, col = 1, lwd = 3)
legend.krige(x.leg = c(424854, 456141), y.leg = c(4461000, 4462200), sqrt(kc.co.2s.10h$krige.var), 
    col = terrain.colors(64))


## ----co_3D, echo=FALSE, include=FALSE------------------------------------------------------------------------
par(mfrow = c(1, 1), mar = c(3.5, 3.5, 1, 0), mgp = c(1.5, 0.5, 0))
persp(kc.co.2s.10h, borders = MuniMadrid, main = "3D Prediction map", theta = 0, 
    phi = 40, expand = 0.5, col = "green")


## ----co_leaflet, echo=FALSE, fig.height = 5------------------------------------------------------------------

# Paso la predicción a raster y lo proyecto a 4326


library(raster)
library(sp)
library(leaflet.providers)

# Paso la predicción a raster y lo proyecto a 4326

pred <- rasterFromXYZ(cbind(grid_prediccion, kc.co.2s.10h$predict))
crs(pred) <- st_crs(25830)$proj4string
# Projecto a lonlat (4326)
pred <- projectRaster(pred, crs = st_crs(4326)$proj4string)

# Recorto a Madrid
library(mapSpain)
Madrid_sf <- esp_get_munic_siane(munic = "^Madrid$", epsg = 4326)
pred <- mask(pred, Madrid_sf)

# preparo leaflet
pal <- colorNumeric(hcl.colors(10, "Inferno", rev = TRUE), values(pred),
  na.color = "transparent"
)

# Uso una capa con carreteras:
# https://leaflet-extras.github.io/leaflet-providers/preview/
leaflet() %>%
  # Capa fotos
  addProviderEspTiles("PNOA", group = "Terreno") %>%
  # Capa callejero
  addTiles(group = "Callejero") %>%
  # Capa carreteras
  addProviderTiles(provider = "Stamen.TonerLines", group = "Carreteras") %>%
  addRasterImage(pred,
    colors = pal,
    opacity = 0.7,
    group = "Predicción"
  ) %>%
  addPolygons(data = Madrid_sf, fill = FALSE) %>%
  addLayersControl(
    baseGroups = c("Carreteras", "Terreno", "Callejero"),
    overlayGroups = c("Predicción"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  addLegend(
    pal = pal,
    values = values(pred),
    title = "CO (ln)"
  )



## ----eval=FALSE, include=FALSE, message=FALSE, results='hide', echo=FALSE------------------------------------
## library(knitr)
## knit('Slides_geor_v1.0.Rmd', tangle=TRUE)
## source('cod_Slides_geor_v.1.0.R')

