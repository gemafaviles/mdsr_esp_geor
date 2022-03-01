## ----setup, include = FALSE----------------------------------------------------------------------------------
# opciones predeterminadas
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(
  fig.width=7, fig.height=6, 
  fig.retina=3,
 # out.width = "30%",
 # out.height="30%",
  echo = TRUE,
  message = FALSE, 
  warning = FALSE,
  fig.show = TRUE,
  hiline = TRUE,
  cache = FALSE,
  fig.align = "center"
  )



## ----cod_datos, eval=FALSE-----------------------------------------------------------------------------------
## library(geoR)
## # `mygeodata` es un objeto espacial: $coords y $data
## mygeodata <- as.geodata(mydata, coords.col = 1:2, data.col = 3)


## ----cod_eda, eval=FALSE-------------------------------------------------------------------------------------
## summary(mygeodata)
## plot(mygeodata)


## ----cod_sem_emp, eval=FALSE---------------------------------------------------------------------------------
## semivar_emp <- variog(mygeodata, max.dist = 2/3*distancia_maxima_coordenadas)
## plot(semivar_emp)


## ----cod_sem_teo, eval=FALSE---------------------------------------------------------------------------------
## semivar_teo <- eyefit(semivar_emp) # función interactiva
## semivar_teo # contiene los parámetros del semivariograma teórico
##     cov.model sigmasq  phi tausq kappa kappa2   practicalRange


## ----cod_inter, eval=FALSE-----------------------------------------------------------------------------------
## * `cov.model` = modelo de covarianza
## * `sigmasq` = varianza parcial
## * `phi` = rango o alcance
## * `tausq` = nugget
## * `practicalRange`= distancia en la que se estabiliza el semivariograma


## ----cod_krig, eval=FALSE------------------------------------------------------------------------------------
## xx <- seq(min, max, l = 51)  #min y #máx para el eje x
## yy <- seq(min, max, l = 51)  #min y #máx para el eje y
## grid_prediccion <- expand.grid(x = xx, y = yy)
## 
## # help(krige.conv)
## krig_ord <- krige.conv(mygeodata, #datos
##                        loc = grid_prediccion, #grid de predicción
##                        krige = krige.control(obj.m = semivar_teo) #semivariograma teórico
##                        )
## names(krig_ord)


## ----cod_plots, eval=FALSE-----------------------------------------------------------------------------------
## #varias funciones de mapeado
## contour(krig_ord, filled = TRUE)
## image(krig_ord, val = krig_ord$krige.var) #superficie de varianzas
## 
## # validación cruzada
## mygeodata_xv <- xvalid(mygeodata, model = semivar_teo)


## ----cod_3dplot, eval=FALSE----------------------------------------------------------------------------------
## library(plot3D)
## # install.packages('plot3D')
## persp3D(xx, yy, matrix(krig_ord$predict, nrow = length(xx)), theta=-60, phi=40)


## ----ms_madrid, echo=FALSE-----------------------------------------------------------------------------------
MuniMadrid <- matrix(scan("data/MuniMadrid.txt"), ncol = 2, byrow = T)
data.co=read.table("data/Madrid_LOG_co50s10h.txt", header=TRUE)

library(dplyr); library(sf); library(leaflet)

MuniMadrid_sf <- st_as_sf(as.data.frame(MuniMadrid), coords = c("V1","V2"),
                          crs=st_crs(25830)) %>% st_transform(4326)
ha <- st_as_sf(data.co, coords = c("x","y"), crs=st_crs(25830)) %>%  st_transform(4326)
dato= data.co$co
dato <- sprintf("<strong>Level of CO: %s</strong>", round(data.co$co,2)) %>% lapply(htmltools::HTML)
leaflet(ha) %>% addTiles() %>% addMarkers(popup = dato, clusterOptions = markerClusterOptions()) %>%
 addCircleMarkers(radius = 7, color = "red", popup = dato)


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


## ----co_krig_sd----------------------------------------------------------------------------------------------
# prediction map
contour(kc.co.2s.10h,  borders = MuniMadrid, filled=TRUE, cex=1, col=terrain.colors(20))

# sd map
contour(kc.co.2s.10h,  values = sqrt(kc.co.2s.10h$krige.var), 
        borders = MuniMadrid, filled=TRUE)




## ----co_3D, echo=FALSE, include=FALSE------------------------------------------------------------------------
par(mfrow = c(1, 1), mar = c(3.5, 3.5, 1, 0), mgp = c(1.5, 0.5, 0))
persp(kc.co.2s.10h, borders = MuniMadrid, main = "3D Prediction map", theta = 0, 
    phi = 40, expand = 0.5, col = "green")


## ----co_leaflet, echo=FALSE----------------------------------------------------------------------------------

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



## ----saca_cod_r, eval=FALSE, include=FALSE, message=FALSE, results='hide', echo=FALSE------------------------
## library(knitr)
## knit('Slides_geor_v1.0.Rmd', tangle=TRUE)
## source('Slides_geor_v1.0.R')

