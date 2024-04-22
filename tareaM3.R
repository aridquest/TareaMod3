# Mon Apr 15 17:51:48 2024 ------------------------------
# Tarea Módulo 3
# Integrantes: Alejandro Cadena, Lorena Miranda Carbajal y Rodrigo Rangel



####Ejercico 1 #### 

# 1.En un ejido en Veracruz un grupo de familias ha montado un sistema de crianza de tilapias y desean
# conocer el efecto de la densidad de peces en el encierro y de la estación del año en el crecimiento de los
# individuos. Para probar el efecto de estos factores, realizan un experimento en encierros en donde colocan
# 10, 18 o 24 individuos (niveles dentro del factor densidad). Pesan 9 peces antes y después del experimento
# (marcándolos para recapturar al mismo pez) y registran el incremento en peso al cabo de dos semanas.
# Realizan estas mediciones en verano y en primavera (factor estación del año). ¿Cuál es el efecto de la época
# del año y de la densidad de peces en el encierro en el crecimiento de las tilapias? Utiliza todos los recursos
# vistos en clase para presentar los resultados de este ejercicio. NOTA: Las mediciones en cada estación vienen
# de peces distintos.

tilapias <- read.csv("TareaMod3/tilapias.csv")
library(tidyverse)
view(tilapias)

tilapias |> 
  arrange(estacion, densidad)

#otra forma más sencilla
plot(tilapias$densidad, tilapias$aumento)
plot(tilapias$estacion, tilapias$aumento)

tilapias$estacion <- as.factor(tilapias$estacion)
tilapias$densidad <- as.factor(tilapias$densidad)
aggregate(tilapias$aumento, by =list(tilapias$densidad, tilapias$estacion), FUN = mean)

plot(tilapias$densidad:tilapias$estacion, tilapias$aumento)

bartlett.test(aumento ~ interaction(densidad,estacion), data = tilapias)
# p-value = 0.1445
#Si hay homocedasticidad

anova1 <- aov(aumento ~ densidad * estacion, data = tilapias)
summary(anova1)
     

hist(residuals(anova1))
#Los residuos son normales

shapiro.test(residuals(anova1))
#p-value = 0.607
#Sí cumple con la normalidad

interaction.plot(tilapias$estacion, tilapias$densidad, tilapias$aumento, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(15,24,22), xlab="Estación",  
                 ylab="Peso (g)", main="Gráfico de interacción")


interaction.plot(tilapias$densidad, tilapias$estacion, tilapias$aumento, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(15,24,22), xlab="Estación",  
                 ylab="Peso (g)", main="Gráfico de interacción")

#Ahora prueba de Tukey para ver quienes tienes diferencias entre los grupos 
TukeyHSD(anova1)

library("agricolae")
LSD.tilapias <- LSD.test(y = anova1, trt = c("densidad","estacion"), 
                          DFerror = anova1$df.residual, MSerror = deviance(anova1)/anova1$df.residual, alpha = 0.05, group = TRUE, console = TRUE)

####Ejercicio 2 ####

# 2.Analiza los mismos datos del ejercicio anterior pero probando el efecto de la estación y de la densidad por
# separado. ¿Qué diferencia encuentras en la interpretación de los resultados? Dado el diseño experimental
# y la pregunta central, ¿qué tipo de análisis es el más conveniente y por qué? No te olvides de revisar los
# supuestos.

#######DENSIDAD#######

bartlett.test(aumento ~ densidad, data = tilapias)
#p-value = 0.03617
#No cumple supuesto de homocedasticidad, vamos a convertor los valores con log10

bartlett.test(log10(aumento) ~ densidad, data = tilapias)
#p-value = 0.5134
#Hay homocedasticidad

anova2 <- aov(aumento ~ densidad, data = tilapias)
summary(anova2)
shapiro.test(residuals(anova2))
#p-value = 0.0004259
#NO cumple con la normalidad de residuos
#Como no cumple normalidad aplicaremos Kruskal Wallis en vez de anova

kw.tilapias <- kruskal.test(aumento ~ densidad, data = tilapias)                   
#p-value = 0.0002621
#Se rechaza H0 al menos una de las densidades es diferente

#Post hoc Tukey
kwAllPairsNemenyiTest(x = tilapias$aumento, g = tilapias$densidad, dist="Tukey")
#Son significativas las diferencias en la densidad entre 10 y 24

####ESTACION####

bartlett.test(aumento ~ estacion, data = tilapias)
# p-value = 0.002773

bartlett.test(log10(aumento) ~ estacion, data = tilapias)
#p-value = 0.4545
#Hay homocedasticidad

anova3 <- aov(log10(aumento) ~ estacion, data = tilapias)
summary(anova3)
 

shapiro.test(residuals(anova3))
#p-value = 0.09754
#Hay normalidad de los residuos

hist(residuals(anova3))
#Los residuos son normales

plot(anova3)
#El QQ plot nos confirma que los residuos se distribuyen de manera normal

LSD.tilapias.estacion <- LSD.test(y = anova3, trt = "estacion", 
                         DFerror = anova3$df.residual, MSerror = deviance(anova3)/anova3$df.residual, alpha = 0.05, group = TRUE, console = TRUE)
#hay diferencias significativas entre las estaciones

#¿Qué diferencia encuentras en la interpretación de los resultados?
#NO HAY DIFERENCIAS ENTRE LOS RESULTADOS DEL MODELO CONJUNTO Y POR SEPARADO, DE AMBAS FORMAS SE ENCUENTRAN EFECTO SIGNIFICATIVO DE LAS ESTACIÓNES Y LAS LAS DENSIDADES SOBRE EL PESO DE LAS TILAPIAS

#Dado el diseño experimentaL y la pregunta central, ¿qué tipo de análisis es el más conveniente y por qué?
#ES MÁS CONVENIENTE USAR EL MODELO DE DOS VÍAS YA QUE EL MODELO EVALÚA EL EFECTO DE LAS DOS VARIABLES Y SU INTERACCIÓN DE MANERA CONJUNTA
#EN CAMBIO SI SE HACE POR SEPARADO SE LLEGA A LA MISMA CONCLUSIÓN PERO SI SE HACEN POR SEPARADO SE DEBE INTERPRETAR Y COMPARAR CADA MODELO SIENDO MUCHO MÁS COLPEJO QUE SIMPLEMENTE INTERPRETAR UN SOLO MODELO CON LAS DOS VARIABLES

#####Ejercicio 3########

# 3.Descarga los datos toluca2.csv de la página del curso. Estos datos provienen de un estudio del maíz en
# un área periurbana. Aunque hay más variables en la base, para este ejercicio queremos construir un modelo
# que permita predecir la producción de 2009 (variable produccion_2009) a partir de las hectáreas sembradas
# (ha_maiz). ¿Qué tan bueno es este modelo para realizar predicciones? Utiliza todos los recursos vistos en
# clase para contestar. NOTA. Elimina el dato con producción de 2009 igual a cero, pues el entrevistador ha
# reportado que se trata de un error de captura.

toluca2 <- read.csv("TareaMod3/toluca2.csv")
toluca2
view(toluca2)

datos.ml <- toluca2 |> 
  dplyr::select(produccion_2009, ha_maiz)
glimpse(datos.ml)

view(datos.ml)
#H0: La producción del 2009 no se relaciona con las hectáreas de maíz (explicado por azar)
#Ha: La producción del 2009 se relaciona con las hectareas de maíz (el modelo lo explica)
#Para observar la homocedasticidad de los datos
plot(datos.ml$ha_maiz, datos.ml$produccion_2009)
#Sí tenían forma de cono por lo que se transformaron
plot(log10(datos.ml$ha_maiz), log10(datos.ml$produccion_2009))

datos.ml$ha_maiz <- log10(datos.ml$ha_maiz)     
datos.ml$produccion_2009 <- log10(datos.ml$produccion_2009)

#Medir correlación
cor.test(datos.ml$ha_maiz, datos.ml$produccion_2009,
         alternative = "two.sided",
         method = "pearson")
#Sí HAY CORRELACIÓN r=0.79

#Ajustamos un modelo de regresión lineal
lm.toluca2 <- lm(datos.ml$produccion_2009 ~ datos.ml$ha_maiz)
summary(lm.toluca2)
# Se rechazan todas las Ho.

plot(lm.toluca2)

#Gráfica con etiquetas
plot(datos.ml$produccion_2009 ~ datos.ml$ha_maiz, xlab="Hectáreas de maíz", ylab="Producción 2009", cex.axis=1.2, cex.lab=1.1)

library("calibrate")
library("MASS")
textxy(datos.ml$ha_maiz, datos.ml$produccion_2009, row.names(datos.ml))
# no salen las etiquetas

#Distancia de Cook
library(car)
cooks.distance(lm.toluca2)
plot(lm.toluca2, which = 4)
#12, 17 y 114 son datos influyentes.

#Independencia de los errores
durbinWatsonTest(lm.toluca2)
#Los errores no están correlacionados 

#Ajustamos recta de regresión
abline(lm.toluca2)

#¿Qué tan bueno es este modelo para realizar predicciones?
#Es un buen modelo porque explica el 63% de la variación de los datos. 


####Ejercicio 4 ####

# 4. Utilizando de nuevo los datos del estudio de maiz periurbano, realiza una comparación entre la producción
# del 2009 entre las diferentes comunidades. Sabemos que la producción está asociada con el área sembrada,
# por lo que quisiéramos tomar esto en cuenta en nuestro análisis al comparar las comunidades, i.e. queremos
# incorporar la covariable hectáreas sembradas de maíz (ha_maiz). ¿Hay diferencias en producción entre los
# sitios tomando en cuenta las hectáreas sembradas de maíz? Aunque no se detecte una diferencia significativa
# entre comunidades, ¿cuál sería la ecuación de regresión para cada comunidad (la finalidad es ejercitar el uso
# de las variables dummy)? Genera la gráfica correspondiente con tres rectas, una para cada comunidad a
# partir de las ecuaciones que generaste para cada comunidad


toluca2$comunidad <- as.factor(toluca2$comunidad)

datos.comunidad <- toluca2 |> 
  select(produccion_2009, ha_maiz, comunidad)
glimpse(datos.comunidad)
 
summary(datos.comunidad)

plot(log10(produccion_2009) ~ comunidad * log10(ha_maiz), data = datos.comunidad, col = comunidad)
#ajustar ANCOVA
modelo.ancova <- aov(log10(produccion_2009) ~ comunidad + log10(ha_maiz), data = datos.comunidad)
Anova(modelo.ancova, type = "III")

#Prueba de normalidad 
shapiro.test(resid(aov(log10(produccion_2009) ~ comunidad + log10(ha_maiz), data = datos.comunidad)))
# p-value = 0.1934. Hay normalidad de los residuos

#Prueba de homogeneidad de varianzas
bartlett.test(log10(produccion_2009) ~ comunidad, data = datos.comunidad)
#p-value = 0.6089. Hay homogeneidad de varianzas

#Prueba de colinealidad
Anova(aov(log10(produccion_2009) ~ comunidad * log10(ha_maiz), data = datos.comunidad), type = 3)
#p-value = 0.2600. No hay interacción entre la variable independiente categórica y la covariable

#ecuación de regresión para cada comunidad
coef.comunidad <- coef(modelo.ancova)
view(coef.comunidad)

#comunidad = intercepto + pendiente(variable independiente)

Chapultepec <- paste("Chapultepec= ", round(coef.comunidad[1], 2), 
                              "+ (", round(coef.comunidad[4], 5), "* Ha_maiz)")
Paredon <- paste("Paredón= ", round(coef.comunidad[1],2) + round(coef.comunidad[2], 3), 
                          "+ (", round(coef.comunidad[4], 5), "* Ha_maiz)")
SanFran <- paste("San Francisco= ", round(coef.comunidad[1],2) + round(coef.comunidad[3], 4), 
                               "+ (", round(coef.comunidad[4], 5), "* Ha_maiz)")

plot(log10(produccion_2009) ~ comunidad * log10(ha_maiz), data = datos.comunidad, col = comunidad,
                xlab = "Hectáreas de maíz (Log10)",
                ylab = "Producción del 2009 (Log10)", main = "Producción de maíz por comunidad")
legend("bottomright", legend = unique(datos.comunidad$comunidad), col = unique(datos.comunidad$comunidad), pch = 1)

#recta Chapultepec
abline(a = 3.29, b = 0.67672, col = "black")

#recta Paredón
abline(a = 3.337, b = 0.67672, col = "red")

#recta San Francisco
abline(a =  3.323, b = 0.67672, col = "darkgreen")

#¿Hay diferencias en producción entre los sitios tomando en cuenta las hectáreas sembradas de maíz?
#No se observan diferencias en la producción entre los sitios 


####Ejercicio 5 ####

# 5.Entender qué aspectos del ambiente y de las actividades humanas afectan la abundancia de organismos es
# un aspecto muy importante para la conservación de áreas naturales. En 1987 se colectaron datos de aves en
# 56 parches de vegetación natural en Australia. En este estudio se registró la abundancia de aves (abundancia)
# y algunas variables que serán utilizadas como predictoras de esta abundancia, incluyendo el área del parche
# medido (area), el tiempo en años en que dicho parche ha quedado aislado del resto de la vegetación natural
# (anos.aislam), la distancia al parche de vegetación más cercano (dist), la distancia al parche más grande
# de vegetación en el área (dist.parche.grande), la cantidad de ganado presente en el parche (ganado, medido
#                                                                                             de 1 a 5 donde 1 es poco ganado y 5 es abundante ganado), y la altitud. Se tienen en total seis variables
# predictoras. OJO: Revisa que exista una relación lineal entre la variable de respuesta y las predictoras. Es
# posible que algunas requieran una transformación. Realiza lo siguiente:

# 5.1.Ajusta un modelo que permita predecir la abundancia de aves a partir de todas las seis variables predictoras. Interpreta los resultados (ajuste, pendientes, etc.)

datos.aves <- read.csv("TareaMod3/AbundanciaAves.csv")
glimpse(datos.aves)


plot(log10(datos.aves$area), log10(datos.aves$abund))
plot(datos.aves$anos.aislam, datos.aves$abund)
plot(log10(datos.aves$dist), log10(datos.aves$abund))
plot(log10(datos.aves$dist.parche.grande), log10(datos.aves$abund))
plot(datos.aves$ganado, datos.aves$abund)
plot(datos.aves$altitud, datos.aves$abund)

cor.test(log10(datos.aves$area), log10(datos.aves$abund)) #0.6763199
cor.test(datos.aves$anos.aislam, datos.aves$abund)#0.5033577 
cor.test(log10(datos.aves$dist), log10(datos.aves$abund))#0.111979
cor.test(log10(datos.aves$dist.parche.grande), log10(datos.aves$abund))#0.0928905
cor.test(datos.aves$ganado, datos.aves$abund)#-0.6825114 
cor.test(datos.aves$altitud, datos.aves$abund)#0.3858362 

mod.aditivo <- lm(abund ~ area + anos.aislam + dist + dist.parche.grande + ganado + altitud, data = datos.aves)
summary(mod.aditivo)

mod.multi <- (lm(abund ~ area + anos.aislam * dist * dist.parche.grande * ganado * altitud, data = datos.aves))
summary(mod.multi) #p-value: 0.07972. Los datos se explican por el azar.

#comparamos los dos modelos
#Ho: el modelo más simple (aditivo) es adecuado para explicar los datos
#Ha: el modelo más complejo (multiplicativo) es adecuado para explicar los datos
anova(mod.aditivo, mod.multi)
#p-value: 0.8763. Se comprueba que el modelo aditivo es mejor para explicar los datos.

plot(mod.aditivo)

#importancia relativa de las variables
library(relaimpo)
calc.relimp(mod.aditivo)  
#El ganado y los años de aislamiento de los parches son las variables más importantes que explican el modelo

#5.2.Utiliza un procedimiento de eliminación backward comenzando con el modelo saturado.
mod.saturado <- lm(abund ~ area + anos.aislam + dist + dist.parche.grande + ganado + altitud, data = datos.aves)
summary(mod.saturado)
drop1(mod.saturado, test = "F")
newmod <- update(mod.saturado, . ~ . - area)
summary(newmod)
drop1(newmod, test = "F")
newmod1 <- update(newmod, . ~ . - dist)
summary(newmod1)
drop1(newmod1, test = "F")
newmod2 <- update(newmod1, . ~ . - anos.aislam)
summary(newmod2)
drop1(newmod2, test = "F")
newmod3 <- update(newmod2, . ~ . - dist.parche.grande)
summary(newmod3)
drop1(newmod3, test = "F")
newmod4 <- update(newmod3, . ~ . - altitud)
summary(newmod4)
# La cantidad de ganado predice la abundancia de las aves.

#5.3.Utiliza un procedimiento de selección forward comenzando con un modelo que solamente contenga al intercepto
mod.sencillo <- lm(datos.aves$abund ~ 1, data = datos.aves)
summary(mod.sencillo)
add1(mod.sencillo, test="F",scope = ~ area + anos.aislam + dist + dist.parche.grande + ganado +altitud)
newfit.aves <- update(mod.sencillo, . ~ . + ganado)
summary(newfit.aves)
add1(newfit.aves, test="F",scope = ~ area + anos.aislam + dist + dist.parche.grande + ganado +altitud)
#La cantidad de ganado predice la abundancia de las aves.

# #5.4.Utiliza el procedimiento de selección de modelos basado en el AIC para seleccionar un grupo de modelos
# que predigan la abundancia a partir de un subconjunto de las variables predictoras.
# Utiliza solamente el siguiente conjunto de cuatro variables predictoras en lugar de las seis de los modelos
# # de los incisos previos: área, ganado, distancia al parche más grande y altitud.
# ¿Cuál sería el conjunto que propondrías como buenos modelos? ¿Cuál es la importancia
# relativa de las diferentes variables tomando en cuenta su aparición en los diferentes modelos candidatos?
datos.seleccion <- datos.aves |> 
  dplyr::select(abund, area, ganado, dist.parche.grande, altitud)
glimpse(datos.seleccion)
view(datos.seleccion)



datos.seleccion$ganado <- as.factor(datos.seleccion$ganado)

str(datos.seleccion)

#Exploración de modelos
library(MASS)
m1 <- glm.nb(abund ~ 1, data = datos.seleccion)
m2 <- glm.nb(abund ~ area, data = datos.seleccion)
m3 <- glm.nb(abund ~ ganado, data = datos.seleccion)
m4 <- glm.nb(abund ~ dist.parche.grande, data = datos.seleccion)
m5 <- glm.nb(abund ~ altitud, data = datos.seleccion)
m6 <- glm.nb(abund ~ area + ganado, data = datos.seleccion)
m7 <- glm.nb(abund ~ area + dist.parche.grande, data = datos.seleccion)
m8 <- glm.nb(abund ~ area + altitud, data = datos.seleccion)
m9 <- glm.nb(abund ~ dist.parche.grande + ganado, data = datos.seleccion)
m10 <- glm.nb(abund ~ dist.parche.grande + altitud, data = datos.seleccion)
m11 <- glm.nb(abund ~ ganado + altitud, data = datos.seleccion)
m12 <- glm.nb(abund ~ area + ganado + dist.parche.grande, data = datos.seleccion)
m13 <- glm.nb(abund ~ area + ganado + altitud, data = datos.seleccion)
m14 <- glm.nb(abund ~ area + dist.parche.grande + altitud, data = datos.seleccion)
m15 <- glm.nb(abund ~ dist.parche.grande + ganado + altitud, data = datos.seleccion)
m16 <- glm.nb(abund ~ area + ganado + dist.parche.grande + altitud, data = datos.seleccion)

#Comparamos los valores AIC 
aics <- AIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16)
names(aics)
#Calculamos la diferencia en AIC entre el mejor modelo y cada uno de los otros modelos.

aics$AIC.Diff <- aics$AIC - min(aics$AIC)

#Convertimos la diferencia en AIC a un peso W
aics$AIC.Wt <- (exp(-0.5*aics$AIC.Diff))/(sum(exp(-0.5*aics$AIC.Diff)))
options(scipen=5)## para expresar sin notación científica
aics
#el modelo 3 tiene un 27.36% de probabilidades de ser el modelo más apropiado.
#el modelo 11 tiene un 23.58% de probabilidades de ser el modelo más apropiado.

ganado.weight <- sum(aics[c("m3", "m6", "m9", "m11", "m12", "m13", "m15", "m16"),]$AIC.Wt)
altitud.weight <- sum(aics[c("m5", "m8", "m10", "m11", "m13", "m14", "m15", "m16"),]$AIC.Wt)
area.weight <- sum(aics[c("m2", "m6", "m7", "m8", "m12", "m13", "m14", "m16"),]$AIC.Wt)
dpg.weight <- sum(aics[c("m4", "m7", "m9", "m10", "m12", "m14", "m15", "m16"),]$AIC.Wt)

#La variable del ganado seguramente aparecerá en el mejor modelo (99%), 
#mientras que la variable de la altitud tiene un 48.6% de probabilidades de estar presente.
#se concluye que la altitud es una variable con alta probabilidad de afectar la abundancia de aves.



# 5.5.Compara los resultados obtenidos con los diferentes métodos de selección de modelos. ¿A qué conclusión
# llegamos que conteste la pregunta principal de investigación?

#modelo lineal
datos.aves <- read.csv("TareaMod3/AbundanciaAves.csv")
summary(mod.aditivo)
library(relaimpo)
calc.relimp(mod.aditivo) 

#eliminación backward
summary(newmod3)

#selección forward
summary(newfit.aves)

#selección de modelos basado en el AIC
aics$AIC.Wt <- (exp(-0.5*aics$AIC.Diff))/(sum(exp(-0.5*aics$AIC.Diff)))
options(scipen=5)## para expresar sin notación científica
aics

ganado.weight <- sum(aics[c("m3", "m6", "m9", "m11", "m12", "m13", "m15", "m16"),]$AIC.Wt)
altitud.weight <- sum(aics[c("m5", "m8", "m10", "m11", "m13", "m14", "m15", "m16"),]$AIC.Wt)