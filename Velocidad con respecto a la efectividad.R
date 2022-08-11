#1-VARIABLES

# Variable Independiente X: velocidad
velocidad <- c(93.3, 96, 96.1, 90.8, 86.4, 95.3, 89.8, 96.5, 91, 88.4, 91.6, 92, 97.3, 93.3, 86.5, 93.1, 89.8, 91.6, 88.4, 88.6, 88.6, 94.5, 93.1, 93.4, 97.5, 89, 94.3, 95.2, 97.4, 95.1, 93.1, 94.9, 86.7, 92.6, 96.5, 89.8, 94.2, 89.4, 95.7, 88.4, 91.6, 93.2, 96.2, 92.5, 91.6, 89.7 , 92.5, 90.5, 93.2, 91.7, 97.14, 88.3, 91.6, 93.2, 91.9, 93, 93.2)


# Variable dependiente Y: efectividad
efectividad <- c(3.32, 3.08, 1.74, 2.53, 4.76, 2.29, 3.45, 2.10, 3.25, 3.23, 3.63, 2.89, 2.36, 4.14, 6.25, 4.45, 3.35, 2.83, 1.87, 6.44, 5.64, 2.10, 1.93, 3.24, 2.8, 3.75, 3.71, 4.32, 3.78, 4.01, 5.1, 3.18, 4.66, 3.53, 3.28, 4.33, 5.06, 3.21, 2.5, 4.89, 3.34, 2.61, 1.14, 3.41, 2.25, 3.69, 2.27, 3.53, 3.08, 1.25, 1.66, 4.21, 4.63, 2.25, 4.02, 3.56, 4.29)

# ESTADISTICOS DESCRIPTIVOS

# DESCRIPTIVOS DE VELOCIDAD

mean (velocidad)
sd(velocidad)
IQR(velocidad)
# summary calcula: la media, el valor minimo , el valor maximo, 1er y tercer cuartil,y la mediana (RESPECTIVAMENTE)
summary(velocidad)

# DESCRIPTIVOS DE EFECTIVIDAD

mean (efectividad)
sd(efectividad)
IQR(efectividad)
summary(efectividad)

# 2- MODELO DE REGRESION
MdR <- lm(efectividad~velocidad)

# 3- DIAGRAMA DE DISPERSION
plot(velocidad, efectividad, pch=21, bg="red", xlab = "Velocidad del Lanzamiento", ylab = "Efectividad del Lanzador", main = "Diagrama de Dispersion")

# 4- RECTA  DE REGRESION
abline(MdR, col="green")

# 5- DATOS DEL MODELO DE REGRESION
summary(MdR)

#Estimacion de los coeficientes de regresion Estimada

# calculo de b
sum(efectividad*velocidad)
mean(velocidad)
mean(efectividad)
n <- 57 
sum(velocidad^2)
n*(mean(velocidad))^2

(sum(efectividad*velocidad)-n*mean(velocidad)*mean(efectividad))/(sum(velocidad^2)-n*(mean(velocidad))^2)

b <- (sum(efectividad*velocidad)-n*mean(velocidad)*mean(efectividad))/(sum(velocidad^2)-n*(mean(velocidad))^2)
 
# Calculo de A

mean(efectividad)-(b*mean(velocidad))

a <-mean(efectividad)-(b*mean(velocidad))

# Cuando la variable Velocidad Toma el Valor de la Media se estima su efectividad


a+(b*mean(velocidad))

# Cuando la variable Independiente toma el valor de la velocidad m?s alta de mi muetsra, el lanzador es mas efectivo que cuando la toma en valor de la velocidad m?s baja
a+(b*(97.5))
a+(b*(86.4))

# Inferencia acerca de los coeficientes de regresion a y b.

  #Coeficiente a
n <- 57
alpha <- 0.05
a <- 21.02059
# Desviacion tipica de a
Dsa <- 4.24714

#Contraste de Hipotesis para a
# Definir variables
#X:  La velocidad del lanzador
#y: La efectividad del lanzador
# Contraste de significacion
# H0: A igual a 0
# HI: A es distinto de 0
# NIVEL DE SIGNIFICACION
alpha

# Estadistico de contraste
a/Dsa
Estadistico <- a/Dsa

# Region Critica

print(c(-qnorm(alpha/2, lower.tail = FALSE), qnorm(alpha/2, lower.tail = FALSE)))


# Regla de decicion
#Se rechaza ho si y solo si
(Estadistico)<(qnorm(alpha/2))
(Estadistico)>(qnorm(1-alpha/2))

# Regla de decicion
# Con un nivel de significacion de o.o5 existen evidencias suficientes para rechazar ho se concluye que a es distinto de 0 

#Coeficiente b

n <- 57
alpha <- 0.05
b <- -0.19065
Dsb <- 0.04595
#Contraste de Hipotesis para a
# Definir variables

# X: La velocidad del lanzador
# Y: La efectividad del lanzador

# Contraste de significacion

# H0: A IGUAL QUE 0
# HI: A DISTINTO DE 0

# NIVEL DE SIGNIFICACION
alpha

# Estadistico de contraste

b/Dsb
Estadistico <- b/Dsb

# Region Critica

print(c(-qnorm(alpha/2, lower.tail = FALSE), qnorm(alpha/2, lower.tail = FALSE)))


# Regla de decicion 
#Se rechaza ho si y solo si
(Estadistico)<(qnorm(alpha/2))
(Estadistico)>(qnorm(1-alpha/2))

# Regla de decicion
# Con un nivel de significacion de o.o5 existen evidencias suficientes para rechazar ho por el cual se concluye que b es distinto de cero 

# Inferencia acerca de los coeficientes de regresion a y b.

# CMR: Cuadrados medios de refresion
CMR <- 17757
# CME: Cuadrados medios de los residuos
CME <- 1.0314
n <- 57
k <-2

alpha <- 0.05
Estadistico <- CMR/CME
CMR/CME

#Contraste de Hipotesis para a
# Definir variables
#X:  La velocidad del lanzador
#y: La efectividad del lanzador
# Contraste de significacion
# H0: a igual b y b es igual que 0
# HI: A es distinto b y b es distinto de 0
# NIVEL DE SIGNIFICACION
alpha

# Estadistico de contraste

Estadistico

# Region Critica
# Desde CERO HASTA qf(alpha/2,2,55) 
qf(alpha/2,2,55) 
# Desde qf(1-alpha/2,2,55) hasta +infinito
qf(1-alpha/2,2,55)


# Regla de decicion
#Se rechaza ho si y solo si
(Estadistico)<(qf(alpha/2,2,55))
(Estadistico)>(qnorm(1-alpha,2,55))

# Regla de decicion
# Con un nivel de significacion de o.o5 se rechaza H0:a:b:0 y se concluye que con el 95% de confianza que la Velocidad del lanzador tiene poder significativo en este modelo, es decir, La Velocidad del lanzador influye de forma significativa en su efectividad 


# Analisis de Correlacion

#covarianza
cov(velocidad,efectividad)

# Correlacion
cor(velocidad,efectividad)
cor.test(velocidad,efectividad)
-0.4882712^2
# Tabla Anova
anova(MdR)


# Inferencia acerca de los coeficientes de regresion a y b.
# CMR: Cuadrados medios de refresion
CMR <- 17757
# CME: Cuadrados medios de los residuos
CME <- 1.0314
n <- 57
k <-2

alpha <- 0.05
Estadistico <- CMR/CME
CMR/CME

#Contraste de Hipotesis para a
# Definir variables
#X:  La velocidad del lanzador
#y: La efectividad del lanzador
# Contraste de significacion
# H0: a igual b y b es igual que 0
# HI: A es distinto b y b es distinto de 0
# NIVEL DE SIGNIFICACION
alpha

# Estadistico de contraste

Estadistico

# Region Critica
# Desde CERO HASTA qf(alpha/2,2,55) 
qf(alpha/2,2,55) 
# Desde qf(1-alpha/2,2,55) hasta +infinito
qf(1-alpha/2,2,55)


# Regla de decicion
#Se rechaza ho si y solo si
(Estadistico)<(qf(alpha/2,2,55))
(Estadistico)>(qnorm(1-alpha,2,55))

# Regla de decicion
# Con un nivel de significacion de o.o5 se rechaza H0:a:b:0 y se concluye que con el 95% de confianza que la Velocidad del lanzador tiene poder significativo en este modelo, es decir, La Velocidad del lanzador influye de forma significativa en su efectividad 

