Análisis geoestadístico con `geoR`
========================================================
author: Gema Fernández-Avilés (gema.faviles@uclm.es)
date: '26-02-2022'
autosize: false


Pasos de un análisis geoesdadístico con geoR
========================================================
1. Leemos los datos y las coordenadas donde se han medido y creamos un objeto de la clase as.geodata (`geoR`)

2. ¿Existe algún tipo de dependencia espacial? Análisis exploratorio de datos espaciales

3. ¿Cómo es la estructura de la dependencia espacial? Semivariograma empírico

4. ¿Es mi semivariograma válido? Ajuste del semivariograma empírico al semivariograma teórico

5. ¿Puedo hacer predicciones en sitios donde no tenga datos de la variable que analizo? Kriging. Predicción y 
Desviación típica del error de predicción

6. Evaluación y presentación de resultados. Mapping

========================================================

### 1. Leemos los datos y convertimos a clase `geodata`

```r
library(geoR)
# `mygeodata` es un objeto espacial: $coords y $data
mygeodata <- as.geodata(mydata, coords.col = 1:2, data.col = 3)
```

### 2. Análisis exploratorio espacial de datos

```r
summary(mygeodata)
plot(mygeodata)
```

### 3. Semivariograma empírico


```r
semivar_emp <- variog(mygeodata, max.dist = 2/3*distancia_maxima_coordenadas)
plot(semivar_emp)
```

*** 
### 4. [**Semivariograma teórico**]{style="color:red"}


```r
semivar_teo <- eyefit(semivar_emp) # función interactiva
semivar_teo # contiene los parámetros del semivariograma teórico
    cov.model sigmasq  phi tausq kappa kappa2   practicalRange
1 exponential    1.07 0.41  0.11  <NA>   <NA> 1.22825073946409
```



```r
* `cov.model` = modelo de covarianza 
* `sigmasq` = varianza parcial
* `phi` = rango o alcance
* `tausq` = nugget
* `practicalRange`= distancia en la que se estabiliza el semivariograma
```

***

### 5. Kriging ordinario

```r
xx <- seq(min, max, l = 51)  #min y #máx para el eje x
yy <- seq(min, max, l = 51)  #min y #máx para el eje y
grid_prediccion <- expand.grid(x = xx, y = yy) 

krig_ord <- krige.conv(mygeodata, #datos
                       loc = grid_prediccion, #grid de predicción
                       krige = krige.control(obj.m = semivar_teo) #semivariograma teórico
                       )
names(krig_ord)
```


### 6. Evaluación y presentación de resultados. Mapping

```r
#varias funciones de mapeado
contour(krig_ord, filled = TRUE)
image(krig_ord, val = krig_ord$krige.var) #superficie de varianzas
```



```r
library(plot3D)
# install.packages('plot3D')
persp3D(xx, yy, matrix(krig_ord$predict, nrow = length(xx)), theta=-60, phi=40)
```


