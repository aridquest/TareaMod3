# Mon Apr 15 17:51:48 2024 ------------------------------
# Tarea Módulo 3
# Integrantes: Alejandro Cadena, Lorena Miranda Carbajal y Rodrigo Rangel



####Ejercico 1 #### 

tilapias <- read.csv("data/tilapias.csv")
library(tidyverse)
view(tilapias)

tilapias |> 
  arrange(estacion, densidad)

#Visualización de los datos
plot(tilapias$densidad, tilapias$aumento)
plot(tilapias$estacion, tilapias$aumento)

tilapias$estacion <- as.factor(tilapias$estacion)
tilapias$densidad <- as.factor(tilapias$densidad)
aggregate(tilapias$aumento, by =list(tilapias$densidad, tilapias$estacion), FUN = mean)

plot(tilapias$densidad:tilapias$estacion, tilapias$aumento,
     xlab = "Densidad de peces por estación",
     ylab = "Aumento de crecimiento de los peces (g)",
     main = "Aumento vs. Densidad para tilapias por estación",
     cex.lab = 0.6,
     cex.axis = 0.5,
     cex.main = 0.8,
     las = 2)

#Prueba de homocedasticidad 
bartlett.test(aumento ~ interaction(densidad,estacion), data = tilapias)

anova1 <- aov(aumento ~ densidad * estacion, data = tilapias)
summary(anova1)

#Normalidad de los residuos
hist(residuals(anova1))

shapiro.test(residuals(anova1))

#Gráficas de interacción
interaction.plot(tilapias$estacion, tilapias$densidad, tilapias$aumento, 
                 type="b", 
                 col=c(1:3),
                 leg.bty="o", 
                 leg.bg="beige",
                 lty = 1,
                 cex=0.8,
                 trace.label = "Densidad",
                 lwd=2, 
                 pch=c(15,24,22), 
                 xlab="Estación",  
                 ylab="Aumento (g)",
                 cex.lab= 0.9,
                 cex.axis=0.6,
                 cex.main=0.9,
                 las=1,
                 main="Gráfico de interacción: Densidad de los peces")

interaction.plot(tilapias$densidad, tilapias$estacion, tilapias$aumento, 
                 type="b", 
                 col=c(1:3), 
                 leg.bty="o", 
                 leg.bg="beige",
                 lty = 1,
                 cex=0.8,
                 trace.label = "Estación",
                 lwd=2, 
                 pch=c(15,24,22), 
                 xlab="Densidad",  
                 ylab="Aumento (g)",
                 cex.lab= 0.9,
                 cex.axis=0.6,
                 cex.main=0.9,
                 las=1,
                 main="Gráfico de interacción: Estación del año")

#Ahora prueba de Tukey para ver qué grupos son diferentes. 
TukeyHSD(anova1)

library("agricolae")
LSD.tilapias <- LSD.test(y = anova1, trt = c("densidad","estacion"), 
                          DFerror = anova1$df.residual, MSerror = deviance(anova1)/anova1$df.residual, alpha = 0.05, group = TRUE, console = TRUE)

####Ejercicio 2 ####

# Probando el efecto de la estación y de la densidad por separado. 

#######DENSIDAD#######

#Prueba de homocedasticidad
bartlett.test(aumento ~ densidad, data = tilapias)

#Prueba de homocedasticidad con variables transformadas (Log10).
bartlett.test(log10(aumento) ~ densidad, data = tilapias)

anova2 <- aov(log10(aumento) ~ densidad, data = tilapias)
summary(anova2)

#Normalidad de los residuos
shapiro.test(residuals(anova2))

#Kruskal-Wallis debido a la no normalidad de los residuos.
kw.tilapias <- kruskal.test(aumento ~ densidad, data = tilapias)                   

#Post hoc Nemenyi
kwAllPairsNemenyiTest(x = tilapias$aumento, g = tilapias$densidad, dist="Tukey")

####ESTACION####

#Prueba de homocedasticidad
bartlett.test(aumento ~ estacion, data = tilapias)

#Transformamos las variable de aumento.
bartlett.test(log10(aumento) ~ estacion, data = tilapias)

anova3 <- aov(log10(aumento) ~ estacion, data = tilapias)
summary(anova3)
 

shapiro.test(residuals(anova3))

#Normalidad de los residuos
hist(residuals(anova3))

plot(anova3)

#Comprobamos si hay diferencias entre las estaciones.
LSD.tilapias.estacion <- LSD.test(y = anova3, trt = "estacion", 
                         DFerror = anova3$df.residual, MSerror = deviance(anova3)/anova3$df.residual, alpha = 0.05, group = TRUE, console = TRUE)

#####Ejercicio 3########

toluca2 <- read.csv("data/toluca_tarea.csv")
toluca2
view(toluca2)

datos.ml <- toluca2 |> 
  dplyr::select(produccion_2009, ha_maiz)
glimpse(datos.ml)
view(datos.ml)

#Visualización de los datos.
plot(datos.ml$ha_maiz, datos.ml$produccion_2009,
     xlab = "Hectáreas de maíz",
     ylab = "producción del 2009",
     main = "Producción del maíz por hectárea",
     cex.lab = 0.9,
     cex.axis = 0.6,
     cex.main = 0.9,
     las =1)

#Tranformación de los datos (Log10).
plot(log10(datos.ml$ha_maiz), log10(datos.ml$produccion_2009),
     xlab = "Hectáreas de maíz",
     ylab = "producción del 2009",
     main = "Producción del maíz por hectárea",
     cex.lab = 0.9,
     cex.axis = 0.6,
     cex.main = 0.9,
     las =1)

datos.ml$ha_maiz <- log10(datos.ml$ha_maiz)     
datos.ml$produccion_2009 <- log10(datos.ml$produccion_2009)

#Medimos la correlación entre variables.
cor.test(datos.ml$ha_maiz, datos.ml$produccion_2009,
         alternative = "two.sided",
         method = "pearson")

#Ajustamos un modelo de regresión lineal
lm.toluca2 <- lm(datos.ml$produccion_2009 ~ datos.ml$ha_maiz)
summary(lm.toluca2)

plot(lm.toluca2)
plot(lm.toluca2$residuals)

#Distancia de Cook
library(car)
cooks.distance(lm.toluca2)
plot(lm.toluca2, which = 4)

#Independencia de los residuos
durbinWatsonTest(lm.toluca2) 

#Ajustamos recta de regresión
plot(datos.ml$ha_maiz, datos.ml$produccion_2009,
     xlab = "Hectáreas de maíz",
     ylab = "producción del 2009",
     main = "Producción del maíz por hectárea",
     cex.lab = 0.9,
     cex.axis = 0.6,
     cex.main = 0.9,
     las =1)
abline(lm.toluca2)

####Ejercicio 4 ####

#Comparación entre la producción del 2009 entre las diferentes comunidades.

toluca2$comunidad <- as.factor(toluca2$comunidad)

datos.comunidad <- toluca2 |> 
  dplyr::select(produccion_2009, ha_maiz, comunidad)
glimpse(datos.comunidad)
summary(datos.comunidad)

plot(log10(produccion_2009) ~ comunidad * log10(ha_maiz), data = datos.comunidad, col = comunidad)

#ajustar ANCOVA
modelo.ancova <- aov(log10(produccion_2009) ~ comunidad + log10(ha_maiz), data = datos.comunidad)
Anova(modelo.ancova, type = "III")

#Prueba de normalidad 
shapiro.test(resid(aov(log10(produccion_2009) ~ comunidad + log10(ha_maiz), data = datos.comunidad)))

#Prueba de homogeneidad de varianzas
bartlett.test(log10(produccion_2009) ~ comunidad, data = datos.comunidad)

#Prueba de interacción entre variables
Anova(aov(log10(produccion_2009) ~ comunidad * log10(ha_maiz), data = datos.comunidad), type = 3)

#No colinealidad
vif(modelo.ancova)

#Ecuación de regresión para cada comunidad
coef.comunidad <- coef(modelo.ancova)
view(coef.comunidad)

#comunidad = intercepto + pendiente(variable independiente)

Chapultepec <- paste("Chapultepec= ", round(coef.comunidad[1], 2), 
                              "+ (", round(coef.comunidad[4], 5), "* Ha_maiz)")
Paredon <- paste("Paredón= ", round(coef.comunidad[1],2) + round(coef.comunidad[2], 3), 
                          "+ (", round(coef.comunidad[4], 5), "* Ha_maiz)")
SanFran <- paste("San Francisco= ", round(coef.comunidad[1],2) + round(coef.comunidad[3], 4), 
                               "+ (", round(coef.comunidad[4], 5), "* Ha_maiz)")

#Llamamos el gráfico y ajustamos las rectas de cada comunidad.
plot(log10(produccion_2009) ~ comunidad * log10(ha_maiz), 
     data = datos.comunidad, 
     col = comunidad,
     xlab = "Hectáreas de maíz (Log10)",
     ylab = "Producción del 2009 (Log10)", 
     main = "Producción de maíz por comunidad",
     cex.lab = 0.9,
     cex.axis = 0.6,
     cex.main = 0.9,
     las =1)
        
legend("bottomright", legend = unique(datos.comunidad$comunidad), col = unique(datos.comunidad$comunidad), pch = 1, cex = 0.6)

#recta Chapultepec
abline(a = 3.29, b = 0.67672, col = "black")

#recta Paredón
abline(a = 3.337, b = 0.67672, col = "red")

#recta San Francisco
abline(a =  3.323, b = 0.67672, col = "darkgreen")

####Ejercicio 5 ####

# 5.1.Ajusta un modelo que permita predecir la abundancia de aves a partir de todas las seis variables predictoras. Interpreta los resultados (ajuste, pendientes, etc.)

datos.aves <- read.csv("data/AbundanciaAves.csv")
glimpse(datos.aves)

#Visualización y transformación de las variables.
plot(log10(datos.aves$area), log10(datos.aves$abund))
plot(datos.aves$anos.aislam, datos.aves$abund)
plot(log10(datos.aves$dist), log10(datos.aves$abund))
plot(log10(datos.aves$dist.parche.grande), log10(datos.aves$abund))
plot(datos.aves$ganado, datos.aves$abund)
plot(datos.aves$altitud, datos.aves$abund)

#Prueba de correlación entre las variables predictoras y la abundancia.
cor.test(log10(datos.aves$area), log10(datos.aves$abund)) #0.6763199
cor.test(datos.aves$anos.aislam, datos.aves$abund)#0.5033577 
cor.test(log10(datos.aves$dist), log10(datos.aves$abund))#0.111979
cor.test(log10(datos.aves$dist.parche.grande), log10(datos.aves$abund))#0.0928905
cor.test(datos.aves$ganado, datos.aves$abund)#-0.6825114 
cor.test(datos.aves$altitud, datos.aves$abund)#0.3858362 

#Elaboración de modelos.
mod.aditivo <- lm(abund ~ log10(area) + anos.aislam + log10(dist) + log10(dist.parche.grande) + ganado + altitud, data = datos.aves)
summary(mod.aditivo) #p-value: 8.443e-11

mod.multi <- (lm(abund ~ log10(area) + anos.aislam * log10(dist) * log10(dist.parche.grande) * ganado * altitud, data = datos.aves))
summary(mod.multi) #p-value: 0.00007826

#Comparamos los dos modelos
#Ho: el modelo más simple (aditivo) es adecuado para explicar los datos
#Ha: el modelo más complejo (multiplicativo) es adecuado para explicar los datos
anova(mod.aditivo, mod.multi)
plot(mod.aditivo)

#Importancia relativa de las variables
library(relaimpo)
calc.relimp(mod.aditivo)  

#5.2.Utiliza un procedimiento de eliminación backward comenzando con el modelo saturado.

mod.saturado <- lm(abund ~ log10(area) + anos.aislam + log10(dist) + log10(dist.parche.grande) + ganado + altitud, data = datos.aves)
summary(mod.saturado)
drop1(mod.saturado, test = "F")
newmod <- update(mod.saturado, . ~ . - log10(dist.parche.grande))
summary(newmod)
drop1(newmod, test = "F")
newmod1 <- update(newmod, . ~ . - log10(dist))
summary(newmod1)
drop1(newmod1, test = "F")
newmod2 <- update(newmod1, . ~ . - altitud)
summary(newmod2)
drop1(newmod2, test = "F")
newmod3 <- update(newmod2, . ~ . - anos.aislam)
summary(newmod3)

#5.3.Utiliza un procedimiento de selección forward comenzando con un modelo que solamente contenga al intercepto

mod.sencillo <- lm(datos.aves$abund ~ 1, data = datos.aves)
summary(mod.sencillo)
add1(mod.sencillo, test="F",scope = ~ log10(area) + anos.aislam + log10(dist) + log10(dist.parche.grande) + ganado +altitud)
newfit.aves <- update(mod.sencillo, . ~ . + log10(area))
summary(newfit.aves)
add1(newfit.aves, test="F",scope = ~ log10(area) + anos.aislam + log10(dist) + log10(dist.parche.grande) + ganado +altitud)
newfit.aves1 <- update(mod.sencillo, . ~ . + log10(area) + ganado)
summary(newfit.aves1)
add1(newfit.aves1, test="F",scope = ~ log10(area) + anos.aislam + log10(dist) + log10(dist.parche.grande) + ganado +altitud)

#5.4.Utiliza el procedimiento de selección de modelos basado en el AIC para seleccionar un grupo de modelos
# que predigan la abundancia a partir de un subconjunto de las variables predictoras.
 
datos.seleccion <- datos.aves |> 
  dplyr::select(abund, area, ganado, dist.parche.grande, altitud)
glimpse(datos.seleccion)
view(datos.seleccion)

datos.seleccion$ganado <- as.factor(datos.seleccion$ganado)
str(datos.seleccion)

#Exploración de modelos
library(MASS)
m1 <- glm.nb(abund ~ 1, data = datos.seleccion)
m2 <- glm.nb(abund ~ log10(area), data = datos.seleccion)
m3 <- glm.nb(abund ~ ganado, data = datos.seleccion)
m4 <- glm.nb(abund ~ log10(dist.parche.grande), data = datos.seleccion)
m5 <- glm.nb(abund ~ altitud, data = datos.seleccion)
m6 <- glm.nb(abund ~ log10(area) + ganado, data = datos.seleccion)
m7 <- glm.nb(abund ~ log10(area) + log10(dist.parche.grande), data = datos.seleccion)
m8 <- glm.nb(abund ~ log10(area) + altitud, data = datos.seleccion)
m9 <- glm.nb(abund ~ log10(dist.parche.grande) + ganado, data = datos.seleccion)
m10 <- glm.nb(abund ~ log10(dist.parche.grande) + altitud, data = datos.seleccion)
m11 <- glm.nb(abund ~ ganado + altitud, data = datos.seleccion)
m12 <- glm.nb(abund ~ log10(area) + ganado + log10(dist.parche.grande), data = datos.seleccion)
m13 <- glm.nb(abund ~ log10(area) + ganado + altitud, data = datos.seleccion)
m14 <- glm.nb(abund ~ log10(area) + log10(dist.parche.grande) + altitud, data = datos.seleccion)
m15 <- glm.nb(abund ~ log10(dist.parche.grande) + ganado + altitud, data = datos.seleccion)
m16 <- glm.nb(abund ~ log10(area) + ganado + log10(dist.parche.grande) + altitud, data = datos.seleccion)

#Comparamos los valores AIC 
aics <- AIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16)

#Calculamos la diferencia en AIC entre el mejor modelo y cada uno de los otros modelos.
aics$AIC.Diff <- aics$AIC - min(aics$AIC)

#Convertimos la diferencia en AIC a un peso W
aics$AIC.Wt <- (exp(-0.5*aics$AIC.Diff))/(sum(exp(-0.5*aics$AIC.Diff)))
options(scipen=5)## para expresar sin notación científica
aics

#Importancia relativa de las variables y su probabilidad de aparecer en el mejor modelo.
ganado.weight <- sum(aics[c("m3", "m6", "m9", "m11", "m12", "m13", "m15", "m16"),]$AIC.Wt)
altitud.weight <- sum(aics[c("m5", "m8", "m10", "m11", "m13", "m14", "m15", "m16"),]$AIC.Wt)
area.weight <- sum(aics[c("m2", "m6", "m7", "m8", "m12", "m13", "m14", "m16"),]$AIC.Wt)
dpg.weight <- sum(aics[c("m4", "m7", "m9", "m10", "m12", "m14", "m15", "m16"),]$AIC.Wt)

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
summary(newfit.aves1)

#selección de modelos basado en el AIC
aics$AIC.Wt <- (exp(-0.5*aics$AIC.Diff))/(sum(exp(-0.5*aics$AIC.Diff)))
options(scipen=5)## para expresar sin notación científica
aics

