load("HIPER200.RData")
attach(HIPER200)

names(HIPER200)
str(HIPER200)
summary(HIPER200)



# ---- CUALITATIVA vs CUALITATIVA (Atributos / Factores) ----

# Tabla de contingencia de frecuencias absolutas conjuntas
tabla_conjunta <- table(act_fisi, es_hiper)
print(tabla_conjunta)

# Frecuencias marginales
margin.table(tabla_conjunta, 1)  # Marginal por filas (act_fisi)
margin.table(tabla_conjunta, 2)  # Marginal por columnas (es_hiper)
addmargins(tabla_conjunta)       # Tabla completa con totales marginales

# Frecuencias relativas / Porcentajes condicionados
# margin = 1 -> Condicionada por filas (% dentro de cada nivel de la variable en fila)
prop.table(tabla_conjunta, margin = 1) * 100

# margin = 2 -> Condicionada por columnas (% dentro de cada nivel de la variable en columna)
prop.table(tabla_conjunta, margin = 2) * 100

# Filtrado complejo por múltiples condiciones + Porcentaje condicionado
# Ejemplo: % en cada nivel de cl_peso para personas con actividad física moderada que no toman café
sub_datos <- HIPER200[act_fisi == "moderada" & cafe == "no toma", ]
prop.table(table(sub_datos$cl_peso)) * 100

# Test de Independencia Chi-cuadrado
chisq.test(tabla_conjunta)

# Representaciones gráficas
barplot(tabla_conjunta, legend.text = TRUE, main = "Gráfico de Barras Agrupadas", col = rainbow(3))
mosaicplot(tabla_conjunta, main = "Gráfico de Mosaico", color = TRUE)

# ---- CUALITATIVA vs CUANTITATIVA (Medidas Condicionadas / Comparación de Grupos) ----

# Media de una variable continua según categorías de un factor
tapply(TAsist1, es_hiper, mean, na.rm = TRUE)

# Desviación típica según categorías de un factor
tapply(TAsist0, act_fisi, sd, na.rm = TRUE)

# Resumen estadístico completo por subgrupos
tapply(peso, genero, summary)

# Gráfico comparativo de cajas (Boxplot)
boxplot(TAsist0 ~ es_hiper, main = "TAS inicial según estado de Hipertensión", col = "lightblue")

# Test no paramétrico para comparar diferencias entre grupos (Kruskal-Wallis)
kruskal.test(TAsist0 ~ act_fisi)

# CUANTITATIVA vs CUANTITATIVA (Asociación Lineal y Correlación)

# Diagrama de dispersión / Nube de puntos
plot(edad, peso, main = "Nube de Puntos: Edad vs Peso", xlab = "Edad", ylab = "Peso", pch = 19, col = "blue")

# Covarianza muestral (S_xy)
cov(edad, peso, use = "complete.obs")

# Coeficiente de correlación lineal de Pearson (r)
cor(edad, peso, use = "complete.obs")

# Test de significación de la correlación de Pearson
cor.test(edad, peso)

# Matriz de correlaciones numéricas entre múltiples variables
vars_num <- HIPER200[, c("edad", "peso", "talla", "TAsist0", "TAdias0", "TAsist1", "TAdias1")]
cor(vars_num, use = "complete.obs")

# ---- MODELOS DE REGRESIÓN LINEAL Y NO LINEAL (lm) ----

# Regresión Lineal Simple: Y = a + b*X (ej. TAdias1 en función de TAdias0)
mod_lin <- lm(TAdias1 ~ TAdias0)
summary(mod_lin)

# Extracción de parámetros clave del modelo
coef(mod_lin)                           # Intersección (a) y Pendiente b_{y/x}
a <- coef(mod_lin)[1]                   # Término independiente (a / Intercept)
b_yx <- coef(mod_lin)["TAdias0"]        # Pendiente de Y sobre X (b_{y/x})
r2 <- summary(mod_lin)$r.squared        # Coeficiente de determinación (R^2)

# Dibujar la recta de regresión sobre el gráfico de dispersión
plot(TAdias0, TAdias1, main = "Regresión Lineal Simple", xlab = "TAD Inicial", ylab = "TAD Final")
abline(mod_lin, col = "red", lwd = 2)

# Estimación / Predicción para un nuevo valor de X (ej. TAdias0 = 85)
predict(mod_lin, newdata = data.frame(TAdias0 = 85))


# Regresión Polinómica / Cuadrática: Y = a + b*X + c*X^2
mod_quad <- lm(peso ~ edad + I(edad^2))
summary(mod_quad)

# Lectura de coeficientes específicos
b_edad <- summary(mod_quad)$coefficients["edad", "Estimate"]
c_edad2 <- summary(mod_quad)$coefficients["I(edad^2)", "Estimate"]

# Regresión con Transformación Logarítmica: Y = a + b*ln(X)
mod_log <- lm(TAdias1 ~ log(TAdias0))
summary(mod_log)
r2_log <- summary(mod_log)$r.squared

# Regresión por Subgrupos (filtrando con el argumento `subset`)
# Modelo de regresión lineal para hombres
mod_masc <- lm(TAdias1 ~ TAdias0, subset = (genero == "masculino"))
summary(mod_masc)

# Modelo de regresión lineal para mujeres
mod_fem <- lm(TAdias1 ~ TAdias0, subset = (genero == "femenino"))
summary(mod_fem)

# Comparación de pendientes b entre subgrupos
b_masc <- summary(mod_masc)$coefficients["TAdias0", "Estimate"]
b_fem <- summary(mod_fem)$coefficients["TAdias0", "Estimate"]

# ---- DIAGNÓSTICO DEL MODELO Y ANÁLISIS DE RESIDUOS ----

# Residuos del modelo (e_i = y_i - y_predicho)
residuos <- residuals(mod_lin)

# Test de normalidad de los residuos (Shapiro-Wilk)
shapiro.test(residuos)

# Gráficos habituales de diagnóstico de regresión
par(mfrow = c(2, 2))
plot(mod_lin)
par(mfrow = c(1, 1))




# ---- VARIANZA RESIDUAL MUESTRAL (S_e^2) Y PROPIEDADES DE LOS RESIDUOS ----
mod_ejemplo <- lm(TAdias1 ~ TAdias0)
residuos_ej <- residuals(mod_ejemplo)

# Varianza residual muestral (S_e^2 = promedio de residuos al cuadrado)
var_residual <- mean(residuos_ej^2)
print(var_residual)

# Comprobación de propiedades de los residuos en R:
mean(residuos_ej)                      # La media de los residuos es exactamente 0
cor(residuos_ej, TAdias0)              # La correlación entre residuos y X es 0


# ---- REGRESIÓN EXPONENCIAL Y OTRAS TRANSFORMACIONES NO LINEALES ----
# Modelo Exponencial: Y = A * e^(B*X)  -->  log(Y) = ln(A) + B*X
mod_exp <- lm(log(TAsist1) ~ edad)
summary(mod_exp)

# Predicción en escala original para modelo exponencial:
pred_log <- predict(mod_exp, newdata = data.frame(edad = 50))
pred_original <- exp(pred_log)         # Se aplica exp() para deshacer la transformación

# Modelo Log-Log: log(Y) ~ log(X)
mod_loglog <- lm(log(TAsist1) ~ log(TAsist0))
summary(mod_loglog)


# ---- EXTRACCIÓN DIRECTA DE P-VALORES ($p.value) PARA CUESTIONARIOS ----
# Para obtener directamente la cifra exacta del p-valor:
pval_chi     <- chisq.test(table(cl_tabac, es_hiper))$p.value
pval_kruskal <- kruskal.test(edad ~ estudios)$p.value
pval_shapiro <- shapiro.test(residuals(mod_ejemplo))$p.value


# ---- REGRESIÓN DE X SOBRE Y Y COMPROBACIÓN DE R^2 = b_{y/x} * b_{x/y} ----
mod_y_x <- lm(TAsist1 ~ TAsist0)      # Regresión de Y sobre X
mod_x_y <- lm(TAsist0 ~ TAsist1)      # Regresión de X sobre Y

b_yx <- coef(mod_y_x)["TAsist0"]
b_xy <- coef(mod_x_y)["TAsist1"]

# El producto de ambas pendientes da exactamente el R^2:
r2_producto <- b_yx * b_xy
r2_directo  <- summary(mod_y_x)$r.squared
