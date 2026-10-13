####################################################################################################
##   =========================   SALARIO MEDIO Y IPV ANUAL NACIONAL   =========================   ##
####################################################################################################

wage <- read.csv("F:/TFG Econometría/wage.csv", sep=";", header=TRUE)
IPV <- read.csv("F:/TFG Econometría/IPV.csv", sep=";", header=TRUE)

wage <- wage[nrow(wage):1, ]
rownames(wage) <- NULL

IPV <- IPV[nrow(IPV):1, ]
rownames(IPV) <- NULL

wage[] <- lapply(wage, function(x) {
  x <- as.character(x)    # CONVIERTE A CARACTER
  x <- gsub(",", ".", x)  # PASAR DE "," A "."
  as.numeric(x)})           # CONVIERTE A NUMERICO
IPV[ , 2:ncol(IPV)] <- lapply(IPV[ , 2:ncol(IPV)], function(x) {
  x <- as.character(x)
  x <- gsub(",", ".", x)
  as.numeric(x)})
head(wage)
head(IPV)

#LIBRERIAS
library(tseries)
library(forecast)
library(writexl)   # COMPROBAR EL DIRECTORIO, SI NO ES E:, DEBERA SER F:
library(vars)

##########################################################
### --ESTADISTICOS BASICOS DE LOS TOTALES NACIONALES-- ###
##########################################################

summary_wage <- summary(wage)

write_xlsx(cbind(Estadistico = rownames(summary_wage),
                 as.data.frame.matrix(summary_wage)),
           "F:/TFG Econometría/R/Resultados R/summary_wage.xlsx")

summary_IPV <- summary(IPV)

write_xlsx(cbind(Estadistico = rownames(summary_IPV),
                 as.data.frame.matrix(summary_IPV)),
           "F:/TFG Econometría/R/Resultados R/summary_IPV.xlsx")

##########################################################
### --GRAFICOS DESCRIPTIVOS DE LAS SERIES WAGE E IPV-- ###
##########################################################

años <- 2008:2023

matplot(años,
        wage[, c("TOT.Todas",
                 "TOT.Menos.de.25.años",
                 "TOT.De.25.a.34.años",
                 "TOT.De.35.a.44.años",
                 "TOT.De.45.a.54.años",
                 "TOT.55.y.más.años")],
        type = "l", lwd = 2, lty = 1,
        col = 1:6,
        xlab = "Año", ylab = "Euros")

legend("topleft",
       legend = c("Todas", "<25", "25-34", "35-44", "45-54", "55+"),
       col = 1:6, lwd = 2)


tiempo <- seq(from = 2008, to = 2023.75, by = 0.25)

matplot(tiempo,
        IPV[, c("TOT.General",
                      "TOT.Vivienda.nueva",
                      "TOT.Vivienda.segunda.mano")],
        type = "l", lwd = 2, lty = 1,
        col = 1:3,
        xlab = "Año", ylab = "Valor")

legend("topleft",
       legend = c("General", "Vivienda nueva", "Segunda mano"),
       col = 1:3, lwd = 2)

######################################
### --GRAFICO TASAS DE VARIACION-- ###
######################################

años <- 2008:2023

wage_sel <- wage[, c("TOT.Todas",
                     "TOT.Menos.de.25.años",
                     "TOT.De.25.a.34.años",
                     "TOT.De.35.a.44.años",
                     "TOT.De.45.a.54.años",
                     "TOT.55.y.más.años")]

wage_sel <- as.data.frame(lapply(wage_sel, function(x) as.numeric(as.character(x))))

wage_tv <- 100 * diff(log(as.matrix(wage_sel)))

años_tv <- años[-1]

matplot(años_tv,
        wage_tv,
        type = "l", lwd = 2, lty = 1,
        col = 1:6,
        xlab = "Año",
        ylab = "% variación")
abline(h = 0, lty = 2, lwd = 1)

legend("topleft",
       legend = c("Todas", "<25", "25-34", "35-44", "45-54", "55+"),
       col = 1:6, lwd = 2)



tiempo <- seq(from = 2008, to = 2023.75, by = 0.25)

ipv_sel <- IPV[, c("TOT.General",
                   "TOT.Vivienda.nueva",
                   "TOT.Vivienda.segunda.mano")]

ipv_sel <- as.data.frame(lapply(ipv_sel, function(x) as.numeric(as.character(x))))

ipv_tv <- 100 * diff(log(as.matrix(ipv_sel)))

tiempo_tv <- tiempo[-1]

matplot(tiempo_tv,
        ipv_tv,
        type = "l", lwd = 2, lty = 1,
        col = 1:3,
        xlab = "Año",
        ylab = "% variación")

abline(h = 0, lty = 2, lwd = 1)

legend("topleft",
       legend = c("General", "Vivienda nueva", "Segunda mano"),
       col = 1:3, lwd = 2)

#####################################################################################################
#REGRESIONES AUTORREGRESIVAS AR(1) DE LAS SERIES TOT (TOTAL NACIONAL) PARA LOS DATASET "WAGE" Y "IPV"
#####################################################################################################

  ###        TOTALES NACIONALES        ###
###    --/---SERIE WAGE---/--    ###

years1<-nrow(wage)

######
yt <- wage$TOT.Menos.de.25.años
yt_1 <- c(NA, yt[-length(yt)])
ar1yt <- lm(yt ~ yt_1, na.action = na.exclude)  # EXCLUYO LOS "NA", LUEGO HARE INTRAPOLACION
summary(ar1yt) # POCO EXPLICATIVA = R^2 BAJO ~ 60%
plot.ts(yt, xlab = "EVOLUCIÓN AÑOS", ylab = "Salario Medio",main = "TOTAL Menos de 25 años")
lines(fitted(ar1yt), col="green")  # COMPROBAR VISUALMENTE LA EXPLICABILIDAD
######

######
yt1 <- wage$TOT.De.25.a.34.años
yt1_1 <- c(NA, yt1[-length(yt1)])
ar1yt1 <- lm(yt1 ~ yt1_1, na.action = na.exclude)
summary(ar1yt1)
plot.ts(yt1, xlab = "EVOLUCIÓN AÑOS", ylab = "Salario Medio",main = "TOTAL de 25 a 34 años")
lines(fitted(ar1yt1), col="green")
######

######
yt2 <- wage$TOT.De.35.a.44.años
yt2_1 <- c(NA, yt2[-length(yt2)])
ar1yt2 <- lm(yt2 ~ yt2_1, na.action = na.exclude)
summary(ar1yt2)
plot.ts(yt2, xlab = "EVOLUCIÓN AÑOS", ylab = "Salario Medio",main = "TOTAL de 35 a 44 años")
lines(fitted(ar1yt2), col="green")
######

######
yt3 <- wage$TOT.De.45.a.54.años
yt3_1 <- c(NA, yt3[-length(yt3)])
ar1yt3 <- lm(yt3 ~ yt3_1, na.action = na.exclude)
summary(ar1yt3)
plot.ts(yt3, xlab = "EVOLUCIÓN AÑOS", ylab = "Salario Medio",main = "TOTAL de 45 a 54 años")
lines(fitted(ar1yt3), col="green")
######

######
yt4 <- wage$TOT.55.y.más.años
yt4_1 <- c(NA, yt4[-length(yt4)])
ar1yt4 <- lm(yt4 ~ yt4_1, na.action = na.exclude)
summary(ar1yt4) # MAS EXPLICACION EN FRANJAS DE EDAD MAYORES, VA AUMENTANTO SU VALOR R^2
plot.ts(yt4, xlab = "EVOLUCIÓN AÑOS", ylab = "Salario Medio",main = "TOTAL Más de 55 años")
lines(fitted(ar1yt4), col="green")  # COMPROBAR VISUALMENTE LA EXPLICABILIDAD
######

######
yt <- wage$TOT.Menos.de.25.años
yt_1 <- c(NA, yt[-length(yt)])
yt_2 <- c(NA, NA, yt[-c(length(yt), length(yt)-1)])
ar2yt <- lm(yt ~ yt_1 + yt_2, na.action = na.exclude)
summary(ar2yt)  # R^2 MAS BAJO QUE EN EL CASO DEL AR(1) -> PROBAR OTRO METODO (COMO ARIMA)
plot.ts(yt, xlab = "EVOLUCIÓN AÑOS", ylab = "Salario Medio",main = "TOTAL Menos de 25 años")
lines(fitted(ar2yt), col="blue")


###    --/---SERIE IPV---/--    ###

######

######
xt <- IPV$TOT.General
xt_1 <- c(NA, xt[-length(xt)])
ar1xt <- lm(xt ~ xt_1, na.action = na.exclude)
summary(ar1xt)
plot.ts(xt, xlab = "EVOLUCIÓN AÑOS", ylab = "Índice Precios",main = "General")
lines(fitted(ar1xt), col="red")
######

######
xt1 <- IPV$TOT.Vivienda.nueva
xt1_1 <- c(NA, xt1[-length(xt1)])
ar1xt1 <- lm(xt1 ~ xt1_1, na.action = na.exclude)
summary(ar1xt1)
plot.ts(xt1, xlab = "EVOLUCIÓN AÑOS", ylab = "Índice Precios",main = "Primera Vivienda")
lines(fitted(ar1xt1), col="red")
######

######
xt2 <- IPV$TOT.Vivienda.segunda.mano
xt2_1 <- c(NA, xt2[-length(xt2)])
ar1xt2 <- lm(xt2 ~ xt2_1, na.action = na.exclude)
summary(ar1xt2)       
plot.ts(xt2, xlab = "EVOLUCIÓN AÑOS", ylab = "Índice Precios",main = "Segunda Vivienda")
lines(fitted(ar1xt2), col="red")
# LOS 3 CASOS SON CONSISTENTES, MAS DE UN 70% ES EXPLICADA LA VAR.DEP. POR SUS RETARDOS
######

################################################################################################
### --CASO CANTABRIA (VIVIENDA GENERAL Y SALARIOS DE JOVENES DE CANTABRIA (MENORES DE 25))-- ###
################################################################################################

cant_wage_menos25 <- wage$CANT.Menos.de.25.años
cant_wage_menos25_1 <- c(NA, cant_wage_menos25[-length(cant_wage_menos25)])
ar1cantmenos25 <- lm(cant_wage_menos25 ~ cant_wage_menos25_1, na.action = na.exclude)

summary(ar1cantmenos25)  # R^2 MUY BAJO, LOS RETARDOS DE LOS SALARIOS DE LOS JOVENES NO LOS EXPLICAN BIEN
plot.ts(cant_wage_menos25, xlab = "EVOLUCIÓN AÑOS", ylab = "Salario Medio",main = "CANTABRIA Menos de 25 años")
lines(fitted(ar1cantmenos25), col="red")  # POCO EXPLICATIVO Y FALTAN MUCHOS DATOS

cant_wage_menos25 <- wage$CANT.Menos.de.25.años
cant_wage_menos25_1 <- c(NA, cant_wage_menos25[-length(cant_wage_menos25)])
cant_wage_menos25_2 <- c(NA, NA, cant_wage_menos25[-c(length(cant_wage_menos25), length(cant_wage_menos25)-1)])
ar2cantmenos25 <- lm(cant_wage_menos25 ~ cant_wage_menos25_1 + cant_wage_menos25_2, na.action = na.exclude)
summary(ar2cantmenos25)  # R^2 MAYOR QUE EN EL CASO DEL AR(1)

plot.ts(cant_wage_menos25, xlab = "EVOLUCIÓN AÑOS", ylab = "Salario Medio",main = "CANTABRIA Menos de 25 años")
lines(fitted(ar2cantmenos25), col="orange")  # VEMOS QUE GANAMOS EXPLICABILIDAD TANTO ESTADISTICA COMO GRAFICAMENTE

cant_IPV <- IPV$CANT.General
cant_IPV_1 <- c(NA, cant_IPV[-length(cant_IPV)])
ar1cantIPV <- lm(cant_IPV ~ cant_IPV_1, na.action = na.exclude)

summary(ar1cantIPV)  # ESTE CASO SI TIENE UN R^2 ALTO (76.7%)
plot.ts(cant_IPV, xlab = "EVOLUCIÓN AÑOS", ylab = "IPV",main = "CANTABRIA Índice de Precios de la Vivienda")
lines(fitted(ar1cantIPV), col="red")  # MAS EXPLICATIVO QUE EL CASO ANTERIOR DE CANTABRIA

wage_cols <- c("TOT todas", "TOT.Menos.de.25.años", "TOT.De.25.a.34.años",
               "TOT.De.35.a.44.años", "TOT.De.45.a.54.años",
               "TOT.55.y.más.años")
IPV_cols <- c("TOT.General", "TOT.Vivienda.nueva", "TOT.Vivienda.segunda.mano")
ar1_results_wage <- list()   # GUARDAR RESULTADOS
ar1_results_IPV  <- list()

#---------------------------------------------------------------------------------------------------------------------------------------------------

######################
####  /  BUCLE  / ####
######################

#######################
####  / AR (1)  / #####
#######################

###    --/---SERIE WAGE---/--    ###
# DATAFRAME
resultados_wage <- data.frame(
  Serie = character(),
  Retardo = numeric(),
  Retardo_SE = numeric(),
  Retardo_p_value = numeric(),
  Intercepto = numeric(),
  Intercepto_SE = numeric(),
  Intercepto_p_value = numeric(),
  stringsAsFactors = FALSE)

# SELECCIONAR COLUMNAS NUMERICAS
cols <- setdiff(names(wage)[sapply(wage, is.numeric)], "Periodo")

# BUCLE AR(1) PARA TODAS LAS COLUMNAS
for(col in cols){
  
  serie <- wage[[col]]
  serie_lag1 <- c(NA, serie[-length(serie)])
  
# MODELO AR(1) CON INTERCEPTO
  modelo <- lm(serie ~ serie_lag1, na.action = na.exclude)
  sumario <- summary(modelo)
  
# --- RETARDO ---
  lag_coef <- sumario$coefficients["serie_lag1", 1]
  lag_se   <- sumario$coefficients["serie_lag1", 2]
  lag_p    <- sumario$coefficients["serie_lag1", 4]
  
# --- INTERCEPTO ---
  intercepto      <- sumario$coefficients["(Intercept)", 1]
  intercepto_se   <- sumario$coefficients["(Intercept)", 2]
  intercepto_p    <- sumario$coefficients["(Intercept)", 4]
  
# GUARDAR RESULTADOS
  resultados_wage <- rbind(resultados_wage, data.frame(
    Serie = col,
    Retardo = lag_coef,
    Retardo_SE = lag_se,
    Retardo_p_value = lag_p,
    Intercepto = intercepto,
    Intercepto_SE = intercepto_se,
    Intercepto_p_value = intercepto_p))}

# VER RESULTADOS
print(resultados_wage)

# EXPORTAR A EXCEL
write_xlsx(resultados_wage, "E:/TFG Econometría/R/Resultados R/resultados_WAGE.xlsx")


###    --/---SERIE IPV---/--    ###

# DATAFRAME PARA RESULTADOS
resultados_IPV <- data.frame(
  Serie = character(),
  Retardo = numeric(),
  Retardo_SE = numeric(),
  Retardo_p_value = numeric(),
  Intercepto = numeric(),
  Intercepto_SE = numeric(),
  Intercepto_p_value = numeric(),
  stringsAsFactors = FALSE)

# COLUMNAS NUMERICAS
cols_IPV <- names(IPV)[sapply(IPV, is.numeric)]

# BUCLE AR(1) CON INTERCEPTO
for(col in cols_IPV){
  
  serie <- IPV[[col]]
  serie_lag1 <- c(NA, serie[-length(serie)])
  
  modelo <- lm(serie ~ serie_lag1, na.action = na.exclude)
  sumario <- summary(modelo)
# --- RETARDO ---
  lag_coef <- sumario$coefficients["serie_lag1", 1]
  lag_se   <- sumario$coefficients["serie_lag1", 2]
  lag_p    <- sumario$coefficients["serie_lag1", 4]
  
# --- INTERCEPTO ---
  intercepto      <- sumario$coefficients["(Intercept)", 1]
  intercepto_se   <- sumario$coefficients["(Intercept)", 2]
  intercepto_p    <- sumario$coefficients["(Intercept)", 4]
  
# GUARDAR RESULTADOS
  resultados_IPV <- rbind(resultados_IPV, data.frame(
    Serie = col,
    Retardo = lag_coef,
    Retardo_SE = lag_se,
    Retardo_p_value = lag_p,
    Intercepto = intercepto,
    Intercepto_SE = intercepto_se,
    Intercepto_p_value = intercepto_p))}

# VER RESULTADOS
print(resultados_IPV)

# EXPORTAR A EXCEL
write_xlsx(resultados_IPV, "E:/TFG Econometría/R/Resultados R/resultados_IPV.xlsx")

##############################
####  /  DICKEY-FULLER  / ####
##############################

###    --/---SERIE WAGE---/--    ###

resultados_adf_wage <- data.frame(
  Serie = character(),
  ADF_stat = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE)

cols <- setdiff(names(wage)[sapply(wage, is.numeric)], "Periodo")

for(col in cols){
  
  serie <- wage[[col]]
  
# OMITIR NA
serie <- na.omit(serie)
  
# DICKEY-FULLER
adf <- adf.test(serie)
  
# RESULTADOS
resultados_adf_wage <- rbind(resultados_adf_wage, data.frame(
   Serie = col,
   ADF_stat = adf$statistic,
   p_value = adf$p.value))}

print(resultados_adf_wage)

# EXPORTAR A EXCEL
write_xlsx(resultados_adf_wage, "E:/TFG Econometría/R/Resultados R/resultados_DickeyFuller_WAGE.xlsx")  
# SI NO DEJA, CAMBIAR F: POR E:

###    --/---SERIE IPV---/--    ###

# DATAFRAME
resultados_adf_ipv <- data.frame(
  Serie = character(),
  ADF_stat = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE)

cols_ipv <- setdiff(names(IPV)[sapply(IPV, is.numeric)], "Trimestre")

# BUCLE PARA ADF
for(col in cols_ipv){
  
serie <- IPV[[col]]
  
# OMITIR NA
serie <- na.omit(serie)
  
# DICKEY-FULLER
adf <- tseries::adf.test(serie)
  
# RESULTADOS
resultados_adf_ipv <- rbind(resultados_adf_ipv, data.frame(
  Serie = col,
  ADF_stat = adf$statistic,
  p_value = adf$p.value))}

print(resultados_adf_ipv)

# GUARDAR EN EXCEL
write_xlsx(resultados_adf_ipv, "E:/TFG Econometría/R/Resultados R/resultados_DickeyFuller_IPV.xlsx")

###################################
####  /  BUCLE ARIMA(1,1,0)  / ####
###################################

###    --/---SERIE WAGE---/--    ###

resultados_wage_arima <- data.frame(
  Serie = character(),
  Retardo = numeric(),
  Retardo_SE = numeric(),
  Retardo_p_value = numeric(),
  Intercepto = numeric(),
  stringsAsFactors = FALSE)

cols <- setdiff(names(wage)[sapply(wage, is.numeric)], "Periodo")

for(col in cols){
  
  serie <- na.omit(wage[[col]])
  
  fit <- tryCatch(
    arima(serie, order = c(1,1,0), include.mean = TRUE, method = "ML"),
    error = function(e) NULL)
  
  if(!is.null(fit)){
    
# AR(1)
    lag_coef <- fit$coef["ar1"]
    
    se_ar1 <- sqrt(diag(fit$var.coef))["ar1"]
    
    z_val <- lag_coef / se_ar1
    lag_p <- 2 * (1 - pnorm(abs(z_val)))
    
# INTERCEPTO
    intercepto <- if("intercept" %in% names(fit$coef)) fit$coef["intercept"] else NA
    
# RESULTADOS
    resultados_wage_arima <- rbind(resultados_wage_arima, data.frame(
      Serie = col,
      Retardo = lag_coef,
      Retardo_SE = se_ar1,
      Retardo_p_value = lag_p,
      Intercepto = intercepto))} else {
    
    resultados_wage_arima <- rbind(resultados_wage_arima, data.frame(
      Serie = col,
      Retardo = NA,
      Retardo_SE = NA,
      Retardo_p_value = NA,
      Intercepto = NA))}}

print(resultados_wage_arima)

# EXPORTAR A EXCEL
write_xlsx(resultados_wage_arima, "E:/TFG Econometría/R/Resultados R/resultados_WAGE_ARIMA.xlsx")

###    --/---SERIE IPV---/--    ###

# DATAFRAME PARA RESULTADOS
resultados_IPV_arima <- data.frame(
  Serie = character(),
  Retardo = numeric(),
  Retardo_SE = numeric(),
  Retardo_p_value = numeric(),
  Intercepto = numeric(),
  stringsAsFactors = FALSE)

# COLUMNAS NUMERICAS
cols_IPV <- names(IPV)[sapply(IPV, is.numeric)]

# BUCLE ARIMA(1,1,0) CON DRIFT
for(col in cols_IPV){
  
  serie <- na.omit(IPV[[col]])  # QUITAR NA
  
# AJUSTAR ARIMA(1,1,0)
  fit <- tryCatch(
    arima(serie, order = c(1,1,0), include.mean = TRUE, method = "ML"),
    error = function(e) NULL)
  
  if(!is.null(fit)){
# --- RETARDO ---
    lag_coef <- fit$coef["ar1"]
    
# ERROR ESTANDAR Y P-VALOR
    se_ar1 <- sqrt(diag(fit$var.coef))["ar1"]
    z_val  <- lag_coef / se_ar1
    lag_p  <- 2 * (1 - pnorm(abs(z_val)))
    
# --- INTERCEPTO (drift) ---
    intercepto <- fit$coef["intercept"]
    intercepto <- if("intercept" %in% names(fit$coef)) fit$coef["intercept"] else 0
    
# GUARDAR RESULTADOS
    resultados_IPV_arima <- rbind(resultados_IPV_arima, data.frame(
      Serie = col,
      Retardo = lag_coef,
      Retardo_SE = se_ar1,
      Retardo_p_value = lag_p,
      Intercepto = intercepto))} else {
        
    resultados_IPV_arima <- rbind(resultados_IPV_arima, data.frame(
      Serie = col,
      Retardo = NA,
      Retardo_SE = NA,
      Retardo_p_value = NA,
      Intercepto = NA))}}

# VER RESULTADOS
print(resultados_IPV_arima)

# EXPORTAR A EXCEL
write_xlsx(resultados_IPV_arima, "E:/TFG Econometría/R/Resultados R/resultados_IPV_ARIMA.xlsx")

###################################
####  /  BUCLE ARIMA(2,1,0)  / ####
###################################

###    --/---WAGE---/--    ###

resultados_wage_arima <- data.frame(
  Serie = character(),
  AR1 = numeric(),
  AR1_SE = numeric(),
  AR1_p_value = numeric(),
  AR2 = numeric(),
  AR2_SE = numeric(),
  AR2_p_value = numeric(),
  Intercepto = numeric(),
  Intercepto_SE = numeric(),
  stringsAsFactors = FALSE)

cols <- setdiff(names(wage)[sapply(wage, is.numeric)], "Periodo")

for(col in cols){
  serie <- na.omit(wage[[col]])
  
  fit <- tryCatch(
    arima(serie, order = c(2,1,0), include.mean = TRUE, method = "ML"),
    error = function(e) NULL)
  
  if(!is.null(fit)){
    
    coefs <- fit$coef
    se <- sqrt(diag(fit$var.coef))
    
    ar_coefs <- coefs[grep("^ar", names(coefs))]
    ar_se <- se[grep("^ar", names(coefs))]
    
    z_val <- ar_coefs / ar_se
    p_val <- 2 * (1 - pnorm(abs(z_val)))
    
    intercepto <- if("intercept" %in% names(coefs)) coefs["intercept"] else NA
    intercepto_se <- if("intercept" %in% names(coefs)) se["intercept"] else NA
    
    resultados_wage_arima <- rbind(
      resultados_wage_arima,
      data.frame(
        Serie = col,
        AR1 = ar_coefs[1],
        AR1_SE = ar_se[1],
        AR1_p_value = p_val[1],
        AR2 = if(length(ar_coefs) > 1) ar_coefs[2] else NA,
        AR2_SE = if(length(ar_se) > 1) ar_se[2] else NA,
        AR2_p_value = if(length(p_val) > 1) p_val[2] else NA,
        Intercepto = intercepto,
        Intercepto_SE = intercepto_se))} else {
    
    resultados_wage_arima <- rbind(
      resultados_wage_arima,
      data.frame(
        Serie = col,
        AR1 = NA,
        AR1_SE = NA,
        AR1_p_value = NA,
        AR2 = NA,
        AR2_SE = NA,
        AR2_p_value = NA,
        Intercepto = NA,
        Intercepto_SE = NA))}}

print(resultados_wage_arima)

write_xlsx(resultados_wage_arima, "E:/TFG Econometría/R/Resultados R/resultados_WAGE_ARIMA_2_1_0.xlsx")

###    --/---SERIE IPV---/--    ###

resultados_IPV_arima <- data.frame(
  Serie = character(),
  AR1 = numeric(),
  AR1_SE = numeric(),
  AR1_p_value = numeric(),
  AR2 = numeric(),
  AR2_SE = numeric(),
  AR2_p_value = numeric(),
  Intercepto = numeric(),
  Intercepto_SE = numeric(),
  stringsAsFactors = FALSE)

cols_IPV <- names(IPV)[sapply(IPV, is.numeric)]

for(col in cols_IPV){
  serie <- na.omit(IPV[[col]])
  
  fit <- tryCatch(
    arima(serie, order = c(2,1,0), include.mean = TRUE, method = "ML"),
    error = function(e) NULL)
  
  if(!is.null(fit)){
    
    coefs <- fit$coef
    se <- sqrt(diag(fit$var.coef))
    
    ar_coefs <- coefs[grep("^ar", names(coefs))]
    ar_se <- se[grep("^ar", names(coefs))]
    
    z_val <- ar_coefs / ar_se
    p_val <- 2 * (1 - pnorm(abs(z_val)))
    
    intercepto <- if("intercept" %in% names(coefs)) coefs["intercept"] else NA
    intercepto_se <- if("intercept" %in% names(coefs)) se["intercept"] else NA
    
    resultados_IPV_arima <- rbind(
      resultados_IPV_arima,
      data.frame(
        Serie = col,
        AR1 = ar_coefs[1],
        AR1_SE = ar_se[1],
        AR1_p_value = p_val[1],
        AR2 = if(length(ar_coefs) > 1) ar_coefs[2] else NA,
        AR2_SE = if(length(ar_se) > 1) ar_se[2] else NA,
        AR2_p_value = if(length(p_val) > 1) p_val[2] else NA,
        Intercepto = intercepto,
        Intercepto_SE = intercepto_se))} else {
    
    resultados_IPV_arima <- rbind(
      resultados_IPV_arima,
      data.frame(
        Serie = col,
        AR1 = NA,
        AR1_SE = NA,
        AR1_p_value = NA,
        AR2 = NA,
        AR2_SE = NA,
        AR2_p_value = NA,
        Intercepto = NA,
        Intercepto_SE = NA))}}

print(resultados_IPV_arima)

write_xlsx(resultados_IPV_arima, "E:/TFG Econometría/R/Resultados R/resultados_IPV_ARIMA_2_1_0.xlsx")

###################################
####  /  BUCLE ARIMA(1,1,1)  / ####
###################################

resultados <- list()

###    --/---SERIE WAGE---/--    ###

# LIMPIEZA DE DATOS
wage_clean <- wage

for(col in names(wage_clean)){
  if(col != "Periodo"){
    wage_clean[[col]] <- as.numeric(gsub(",", ".", gsub("\\.", "", wage_clean[[col]])))}}

# COLUMNAS NUMERICAS
cols <- setdiff(names(wage_clean)[sapply(wage_clean, is.numeric)], "Periodo")

for(col in cols){
  
  serie <- wage_clean[[col]]
  serie <- na.omit(as.numeric(serie))
  
  if(length(serie) < 10 || var(serie) == 0){
    next}
  
  modelo_arima <- tryCatch(
    arima(serie, order = c(1,1,1), method = "ML"),
    error = function(e) NULL)
  
  if(is.null(modelo_arima)) next
  
  coefs <- modelo_arima$coef
  se <- sqrt(diag(modelo_arima$var.coef))
  
  df <- data.frame(
    Serie = col,
    Coeficiente = names(coefs),
    Estimado = as.numeric(coefs),
    SE = as.numeric(se),
    p_value = 2 * (1 - pnorm(abs(coefs / se))))
  
  resultados[[col]] <- df}

resultado_final <- do.call(rbind, resultados)

print(resultado_final)

write_xlsx(resultado_final, "E:/TFG Econometría/R/Resultados R/resultados_WAGE_ARIMA_1_1_1.xlsx")

###    --/---SERIE IPV---/--    ###

resultados <- list()

cols <- setdiff(names(IPV)[sapply(IPV, is.numeric)], "Periodo")

for(col in cols){
  
  serie <- IPV[[col]]
  serie <- na.omit(as.numeric(serie))
  
  if(length(serie) < 10 || var(serie) == 0){
    next}
  
  modelo_arima <- tryCatch(
    arima(serie, order = c(1,1,1), method = "ML"),
    error = function(e) NULL)
  
  if(is.null(modelo_arima)) next
  
  coefs <- modelo_arima$coef
  se <- sqrt(diag(modelo_arima$var.coef))
  
  df <- data.frame(
    Serie = col,
    Coeficiente = names(coefs),
    Estimado = as.numeric(coefs),
    SE = as.numeric(se),
    p_value = 2 * (1 - pnorm(abs(coefs / se))))
  
  resultados[[length(resultados) + 1]] <- df}

resultado_final <- do.call(rbind, resultados)

print(resultado_final)

write_xlsx(resultado_final, "E:/TFG Econometría/R/Resultados R/resultados_IPV_ARIMA_1_1_1.xlsx")

################################
##  CRITERIOS DE INFORMACION  ##
################################

#################################
##  AIC Y BIC DE TOT.NACIONAL  ##
#################################

# AR(1)
rbind(
  c(AIC = AIC(lm(wage$TOT.Todas ~ c(NA, wage$TOT.Todas[-length(wage$TOT.Todas)]), na.action=na.exclude)),
    BIC = BIC(lm(wage$TOT.Todas ~ c(NA, wage$TOT.Todas[-length(wage$TOT.Todas)]), na.action=na.exclude))),
  
  c(AIC = AIC(lm(wage$TOT.Menos.de.25.años ~ c(NA, wage$TOT.Menos.de.25.años[-length(wage$TOT.Menos.de.25.años)]), na.action=na.exclude)),
    BIC = BIC(lm(wage$TOT.Menos.de.25.años ~ c(NA, wage$TOT.Menos.de.25.años[-length(wage$TOT.Menos.de.25.años)]), na.action=na.exclude))),
  
  c(AIC = AIC(lm(wage$TOT.De.25.a.34.años ~ c(NA, wage$TOT.De.25.a.34.años[-length(wage$TOT.De.25.a.34.años)]), na.action=na.exclude)),
    BIC = BIC(lm(wage$TOT.De.25.a.34.años ~ c(NA, wage$TOT.De.25.a.34.años[-length(wage$TOT.De.25.a.34.años)]), na.action=na.exclude))),
  
  c(AIC = AIC(lm(wage$TOT.De.35.a.44.años ~ c(NA, wage$TOT.De.35.a.44.años[-length(wage$TOT.De.35.a.44.años)]), na.action=na.exclude)),
    BIC = BIC(lm(wage$TOT.De.35.a.44.años ~ c(NA, wage$TOT.De.35.a.44.años[-length(wage$TOT.De.35.a.44.años)]), na.action=na.exclude))),
  
  c(AIC = AIC(lm(wage$TOT.De.45.a.54.años ~ c(NA, wage$TOT.De.45.a.54.años[-length(wage$TOT.De.45.a.54.años)]), na.action=na.exclude)),
    BIC = BIC(lm(wage$TOT.De.45.a.54.años ~ c(NA, wage$TOT.De.45.a.54.años[-length(wage$TOT.De.45.a.54.años)]), na.action=na.exclude))),
  
  c(AIC = AIC(lm(wage$TOT.55.y.más.años ~ c(NA, wage$TOT.55.y.más.años[-length(wage$TOT.55.y.más.años)]), na.action=na.exclude)),
    BIC = BIC(lm(wage$TOT.55.y.más.años ~ c(NA, wage$TOT.55.y.más.años[-length(wage$TOT.55.y.más.años)]), na.action=na.exclude))))

rbind(
  c(AIC = AIC(lm(IPV$TOT.General ~ c(NA, IPV$TOT.General[-length(IPV$TOT.General)]), na.action=na.exclude)),
    BIC = BIC(lm(IPV$TOT.General ~ c(NA, IPV$TOT.General[-length(IPV$TOT.General)]), na.action=na.exclude))),
  
  c(AIC = AIC(lm(IPV$TOT.Vivienda.nueva ~ c(NA, IPV$TOT.Vivienda.nueva[-length(IPV$TOT.Vivienda.nueva)]), na.action=na.exclude)),
    BIC = BIC(lm(IPV$TOT.Vivienda.nueva ~ c(NA, IPV$TOT.Vivienda.nueva[-length(IPV$TOT.Vivienda.nueva)]), na.action=na.exclude))),
  
  c(AIC = AIC(lm(IPV$TOT.Vivienda.segunda.mano ~ c(NA, IPV$TOT.Vivienda.segunda.mano[-length(IPV$TOT.Vivienda.segunda.mano)]), na.action=na.exclude)),
    BIC = BIC(lm(IPV$TOT.Vivienda.segunda.mano ~ c(NA, IPV$TOT.Vivienda.segunda.mano[-length(IPV$TOT.Vivienda.segunda.mano)]), na.action=na.exclude))))

# ARIMA(1,1,0)
rbind(
  c(AIC = AIC(arima(na.omit(wage$TOT.Todas), order = c(1,1,0), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.Todas), order = c(1,1,0), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(wage$TOT.Menos.de.25.años), order = c(1,1,0), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.Menos.de.25.años), order = c(1,1,0), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(wage$TOT.De.25.a.34.años), order = c(1,1,0), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.De.25.a.34.años), order = c(1,1,0), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(wage$TOT.De.35.a.44.años), order = c(1,1,0), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.De.35.a.44.años), order = c(1,1,0), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(wage$TOT.De.45.a.54.años), order = c(1,1,0), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.De.45.a.54.años), order = c(1,1,0), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(wage$TOT.55.y.más.años), order = c(1,1,0), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.55.y.más.años), order = c(1,1,0), method="ML"))))

rbind(
  c(AIC = AIC(arima(na.omit(IPV$TOT.General), order = c(1,1,0), method="ML")),
    BIC = BIC(arima(na.omit(IPV$TOT.General), order = c(1,1,0), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(IPV$TOT.Vivienda.nueva), order = c(1,1,0), method="ML")),
    BIC = BIC(arima(na.omit(IPV$TOT.Vivienda.nueva), order = c(1,1,0), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(IPV$TOT.Vivienda.segunda.mano), order = c(1,1,0), method="ML")),
    BIC = BIC(arima(na.omit(IPV$TOT.Vivienda.segunda.mano), order = c(1,1,0), method="ML"))))

# ARIMA(2,1,0)
rbind(
  c(AIC = AIC(arima(na.omit(wage$TOT.Todas), order = c(2,1,0), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.Todas), order = c(2,1,0), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(wage$TOT.Menos.de.25.años), order = c(2,1,0), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.Menos.de.25.años), order = c(2,1,0), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(wage$TOT.De.25.a.34.años), order = c(2,1,0), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.De.25.a.34.años), order = c(2,1,0), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(wage$TOT.De.35.a.44.años), order = c(2,1,0), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.De.35.a.44.años), order = c(2,1,0), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(wage$TOT.De.45.a.54.años), order = c(2,1,0), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.De.45.a.54.años), order = c(2,1,0), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(wage$TOT.55.y.más.años), order = c(2,1,0), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.55.y.más.años), order = c(2,1,0), method="ML"))))

rbind(
  c(AIC = AIC(arima(na.omit(IPV$TOT.General), order = c(2,1,0), method="ML")),
    BIC = BIC(arima(na.omit(IPV$TOT.General), order = c(2,1,0), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(IPV$TOT.Vivienda.nueva), order = c(2,1,0), method="ML")),
    BIC = BIC(arima(na.omit(IPV$TOT.Vivienda.nueva), order = c(2,1,0), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(IPV$TOT.Vivienda.segunda.mano), order = c(2,1,0), method="ML")),
    BIC = BIC(arima(na.omit(IPV$TOT.Vivienda.segunda.mano), order = c(2,1,0), method="ML"))))

# ARIMA(1,1,1)
rbind(
  c(AIC = AIC(arima(na.omit(wage$TOT.Todas), order = c(1,1,1), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.Todas), order = c(1,1,1), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(wage$TOT.Menos.de.25.años), order = c(1,1,1), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.Menos.de.25.años), order = c(1,1,1), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(wage$TOT.De.25.a.34.años), order = c(1,1,1), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.De.25.a.34.años), order = c(1,1,1), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(wage$TOT.De.35.a.44.años), order = c(1,1,1), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.De.35.a.44.años), order = c(1,1,1), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(wage$TOT.De.45.a.54.años), order = c(1,1,1), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.De.45.a.54.años), order = c(1,1,1), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(wage$TOT.55.y.más.años), order = c(1,1,1), method="ML")),
    BIC = BIC(arima(na.omit(wage$TOT.55.y.más.años), order = c(1,1,1), method="ML"))))

rbind(
  c(AIC = AIC(arima(na.omit(IPV$TOT.General), order = c(1,1,1), method="ML")),
    BIC = BIC(arima(na.omit(IPV$TOT.General), order = c(1,1,1), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(IPV$TOT.Vivienda.nueva), order = c(1,1,1), method="ML")),
    BIC = BIC(arima(na.omit(IPV$TOT.Vivienda.nueva), order = c(1,1,1), method="ML"))),
  
  c(AIC = AIC(arima(na.omit(IPV$TOT.Vivienda.segunda.mano), order = c(1,1,1), method="ML")),
    BIC = BIC(arima(na.omit(IPV$TOT.Vivienda.segunda.mano), order = c(1,1,1), method="ML"))))


#########################
##    OUT-OF-SAMPLE    ##
#########################

# AR(1)

###    --/---SERIE WAGE---/--    ###

series_names <- c(
  "TOT.Todas",
  "TOT.Menos.de.25.años",
  "TOT.De.25.a.34.años",
  "TOT.De.35.a.44.años",
  "TOT.De.45.a.54.años",
  "TOT.55.y.más.años")

# LISTA PARA RESULTADOS
resultados_oosmse <- list()

for (serie in series_names) {
  
  y <- wage[[serie]]
  T <- length(y)
  t_star <- floor(T / 2)
  
  errores <- c()
  
  for (t in t_star:(T - 1)) {
    
    y_train <- y[1:t]
    
    y_lag <- y_train[-length(y_train)]
    y_dep <- y_train[-1]
    
    modelo <- lm(y_dep ~ y_lag)
    
    coef <- coefficients(modelo)
    pred <- coef[1] + coef[2] * y[t]
    
    y_real <- y[t + 1]

    errores <- c(errores, (y_real - pred)^2)}

  oosmse <- mean(errores)
  
  resultados_oosmse[[serie]] <- oosmse}

tabla_oosmse1 <- data.frame(
  Serie = names(resultados_oosmse),
  OOSMSE = unlist(resultados_oosmse))

print(tabla_oosmse1)

###    --/---SERIE IPV---/--    ###

series_names_ipv <- c(
  "TOT.General",
  "TOT.Vivienda.nueva",
  "TOT.Vivienda.segunda.mano")

resultados_oosmse_ipv <- list()

for (serie in series_names_ipv) {
  
  y <- IPV_anual[[serie]]
  T <- length(y)
  t_star <- floor(T / 2)
  
  errores <- c()
  
  for (t in t_star:(T - 1)) {
    
    y_train <- y[1:t]

    y_lag <- y_train[-length(y_train)]      
    y_dep <- y_train[-1]                   
    

    modelo <- lm(y_dep ~ y_lag)
    
    y_last <- y[t]
    
    coef <- coefficients(modelo)
    y_pred <- coef[1] + coef[2] * y_last
    

    error <- (y[t + 1] - y_pred)^2
    
    errores <- c(errores, error)}
  

  oosmse <- mean(errores)
  
  resultados_oosmse_ipv[[serie]] <- oosmse}

tabla_oosmse_ipv1 <- data.frame(
  Serie = names(resultados_oosmse_ipv),
  OOSMSE = unlist(resultados_oosmse_ipv))

print(tabla_oosmse_ipv1)

# ARIMA(1,1,0)

###    --/---SERIE WAGE---/--    ###

series_names <- c(
  "TOT.Todas",
  "TOT.Menos.de.25.años",
  "TOT.De.25.a.34.años",
  "TOT.De.35.a.44.años",
  "TOT.De.45.a.54.años",
  "TOT.55.y.más.años")

resultados_oosmse <- list()

for (serie in series_names) {
  
  y <- wage[[serie]]
  T <- length(y)
  t_star <- floor(T / 2)
  
  errores <- c()
  
  for (t in t_star:(T - 1)) {
    
    y_train <- y[1:t]
    
    modelo <- arima(y_train, order = c(1,1,0), method = "ML")
    
    pred <- predict(modelo, n.ahead = 1)$pred[1]
    
    y_real <- y[t + 1]
    errores <- c(errores, (y_real - pred)^2)}
  
  resultados_oosmse[[serie]] <- mean(errores)}

tabla_oosmse2 <- data.frame(
  Serie = names(resultados_oosmse),
  OOSMSE = unlist(resultados_oosmse))

print(tabla_oosmse2)

###    --/---SERIE IPV---/--    ###

series_names_ipv <- c(
  "TOT.General",
  "TOT.Vivienda.nueva",
  "TOT.Vivienda.segunda.mano")

resultados_oosmse_ipv <- list()

for (serie in series_names_ipv) {
  
  y <- IPV_anual[[serie]]
  T <- length(y)
  t_star <- floor(T / 2)
  
  errores <- c()
  
  for (t in t_star:(T - 1)) {
    
    y_train <- y[1:t]

    modelo <- arima(y_train, order = c(1,1,0), method = "ML")
    
    pred <- predict(modelo, n.ahead = 1)$pred[1]
    
    y_real <- y[t + 1]
    errores <- c(errores, (y_real - pred)^2)}
  
  oosmse <- mean(errores)
  
  resultados_oosmse_ipv[[serie]] <- oosmse}

tabla_oosmse_ipv2 <- data.frame(
  Serie = names(resultados_oosmse_ipv),
  OOSMSE = unlist(resultados_oosmse_ipv))

print(tabla_oosmse_ipv2)

# ARIMA(2,1,0)

###    --/---SERIE WAGE---/--    ###

series_names <- c(
  "TOT.Todas",
  "TOT.Menos.de.25.años",
  "TOT.De.25.a.34.años",
  "TOT.De.35.a.44.años",
  "TOT.De.45.a.54.años",
  "TOT.55.y.más.años")

resultados_oosmse <- list()

for (serie in series_names) {
  
  y <- wage[[serie]]
  T <- length(y)
  t_star <- floor(T / 2)
  
  errores <- c()
  
  for (t in t_star:(T - 1)) {
    
    y_train <- y[1:t]
    
    # ARIMA(2,1,0)
    modelo <- arima(y_train, order = c(2,1,0), method = "ML")
    
    pred <- predict(modelo, n.ahead = 1)$pred[1]
    
    y_real <- y[t + 1]
    errores <- c(errores, (y_real - pred)^2)}
  
  resultados_oosmse[[serie]] <- mean(errores)}

tabla_oosmse3 <- data.frame(
  Serie = names(resultados_oosmse),
  OOSMSE = unlist(resultados_oosmse))

print(tabla_oosmse3)

###    --/---SERIE IPV---/--    ###

series_names_ipv <- c(
  "TOT.General",
  "TOT.Vivienda.nueva",
  "TOT.Vivienda.segunda.mano")

resultados_oosmse_ipv <- list()

for (serie in series_names_ipv) {
  
  y <- IPV_anual[[serie]]
  T <- length(y)
  t_star <- floor(T / 2)
  
  errores <- c()
  
  for (t in t_star:(T - 1)) {
    
    y_train <- y[1:t]
    
    # ARIMA(2,1,0)
    modelo <- arima(y_train, order = c(2,1,0), method = "ML")
    
    pred <- predict(modelo, n.ahead = 1)$pred[1]
    
    y_real <- y[t + 1]
    errores <- c(errores, (y_real - pred)^2)}
  
  resultados_oosmse_ipv[[serie]] <- mean(errores)}

tabla_oosmse_ipv3 <- data.frame(
  Serie = names(resultados_oosmse_ipv),
  OOSMSE = unlist(resultados_oosmse_ipv))

print(tabla_oosmse_ipv3)

# ARIMA(1,1,1)

###    --/---SERIE WAGE---/--    ###

series_names <- c(
  "TOT.Todas",
  "TOT.Menos.de.25.años",
  "TOT.De.25.a.34.años",
  "TOT.De.35.a.44.años",
  "TOT.De.45.a.54.años",
  "TOT.55.y.más.años")

resultados_oosmse <- list()

for (serie in series_names) {
  
  y <- wage[[serie]]
  y <- na.omit(y)
  
  T <- length(y)
  t_star <- 12
  
  errores <- c()
  
  for (t in t_star:(T - 1)) {
    
    y_train <- y[1:t]
    
    modelo <- try(
      arima(y_train, order = c(1,1,1), method = "ML"),
      silent = TRUE)
    
    if (inherits(modelo, "try-error")) next
    
    pred <- predict(modelo, n.ahead = 1)$pred[1]
    
    y_real <- y[t + 1]
    
    errores <- c(errores, (y_real - pred)^2)}
  
  resultados_oosmse[[serie]] <- mean(errores, na.rm = TRUE)}

tabla_oosmse4 <- data.frame(
  Serie = names(resultados_oosmse),
  OOSMSE = unlist(resultados_oosmse))

print(tabla_oosmse4)

###    --/---SERIE IPV---/--    ###

series_names_ipv <- c(
  "TOT.General",
  "TOT.Vivienda.nueva",
  "TOT.Vivienda.segunda.mano")

resultados_oosmse_ipv <- list()

for (serie in series_names_ipv) {
  
  y <- IPV_anual[[serie]]
  T <- length(y)
  t_star <- floor(T / 2)
  
  errores <- c()
  
  for (t in t_star:(T - 1)) {
    
    y_train <- y[1:t]
    
    modelo <- arima(y_train, order = c(1,1,1), method = "ML")
    
    pred <- predict(modelo, n.ahead = 1)$pred[1]
    
    y_real <- y[t + 1]
    errores <- c(errores, (y_real - pred)^2)}
  
  resultados_oosmse_ipv[[serie]] <- mean(errores)}

tabla_oosmse_ipv4 <- data.frame(
  Serie = names(resultados_oosmse_ipv),
  OOSMSE = unlist(resultados_oosmse_ipv))

print(tabla_oosmse_ipv4)

# ------ RESULTADOS ------

# 1. ASIGNAR MODELO

tabla_oosmse1$modelo <- "AR(1)"
tabla_oosmse2$modelo <- "ARIMA(1,1,0)"
tabla_oosmse3$modelo <- "ARIMA(2,1,0)"
tabla_oosmse4$modelo <- "ARIMA(1,1,1)"

tabla_oosmse_ipv1$modelo <- "AR(1)"
tabla_oosmse_ipv2$modelo <- "ARIMA(1,1,0)"
tabla_oosmse_ipv3$modelo <- "ARIMA(2,1,0)"
tabla_oosmse_ipv4$modelo <- "ARIMA(1,1,1)"

# 2. UNIR TODAS LAS TABLAS

tabla_final_oosmse <- rbind(
  tabla_oosmse1,
  tabla_oosmse2,
  tabla_oosmse3,
  tabla_oosmse4,
  tabla_oosmse_ipv1,
  tabla_oosmse_ipv2,
  tabla_oosmse_ipv3,
  tabla_oosmse_ipv4)

# 3. QUEDARSE SOLO CON LO IMPORTANTE

tabla_final_oosmse <- tabla_final_oosmse[, c("Serie", "modelo", "OOSMSE")]

# 4. ORDENAR BIEN

tabla_final_oosmse <- tabla_final_oosmse[order(
  tabla_final_oosmse$Serie,
  tabla_final_oosmse$modelo), ]

# 5. MOSTRAR RESULTADO

print(tabla_final_oosmse)
write_xlsx(tabla_final_oosmse, "E:/TFG Econometría/R/Resultados R/tabla_OOSMSE.xlsx")

##############################
####  /  COINTEGRACION  / ####
##############################

# CONVERSION A SERIES ANUALES
IPV$Año <- substr(IPV$Periodo, 1, 4)
IPV_anual <- aggregate(. ~ Año, data = IPV[ , !(names(IPV) %in% "Periodo")], FUN = mean)  #NUEVO DATASET ANUAL
IPV$Año <- NULL

years2<-nrow(IPV_anual)  # COMPROBAMOS QUE COINCIDEN EL NUMERO DE PERIODOS ANUALES (16)

# TOTAL NACIONAL TODAS LAS EDADES (WAGE) ~ TOTAL GENERAL (IPV)

TOT.General <- IPV_anual$TOT.General
TOT.Todas <- wage$TOT.Todas

adf.test(TOT.General)
adf.test(TOT.Todas)

coint.1 <- lm(TOT.General ~ TOT.Todas)
summary(coint.1)

TOThat1 <- residuals(coint.1)

adf.test(TOThat1)

# TOTAL NACIONAL MENORES DE 25 (WAGE) ~ TOTAL GENERAL (IPV)

TOT.General <- IPV_anual$TOT.General
TOT.Menos.25 <- wage$TOT.Menos.de.25.años

adf.test(TOT.General)
adf.test(TOT.Menos.25)

coint.2 <- lm(TOT.General ~ TOT.Menos.25)
summary(coint.2)

TOThat2 <- residuals(coint.2)

adf.test(TOThat2)

# TOTAL NACIONAL DE 25 a 34 (WAGE) ~ TOTAL GENERAL (IPV)

TOT.General <- IPV_anual$TOT.General
TOT.25.34 <- wage$TOT.De.25.a.34.años

adf.test(TOT.General)
adf.test(TOT.25.34)

coint.3 <- lm(TOT.General ~ TOT.25.34)
summary(coint.3)

TOThat3 <- residuals(coint.3)

adf.test(TOThat3)

# TOTAL NACIONAL DE 35 a 44 (WAGE) ~ TOTAL GENERAL (IPV)

TOT.General <- IPV_anual$TOT.General
TOT.35.44 <- wage$TOT.De.35.a.44.años

adf.test(TOT.General)
adf.test(TOT.35.44)

coint.4 <- lm(TOT.General ~ TOT.35.44)
summary(coint.4)

TOThat4 <- residuals(coint.4)

adf.test(TOThat4)

# TOTAL NACIONAL DE 45 a 54 (WAGE) ~ TOTAL GENERAL (IPV)

TOT.General <- IPV_anual$TOT.General
TOT.45.54 <- wage$TOT.De.45.a.54.años

adf.test(TOT.General)
adf.test(TOT.45.54)

coint.5 <- lm(TOT.General ~ TOT.45.54)
summary(coint.5)

TOThat5 <- residuals(coint.5)

adf.test(TOThat5)

# TOTAL NACIONAL DE MAYORES DE 55 (WAGE) ~ TOTAL GENERAL (IPV)

TOT.General <- IPV_anual$TOT.General
TOT.Mayores.55 <- wage$TOT.55.y.más.años

adf.test(TOT.General)
adf.test(TOT.Mayores.55)

coint.6 <- lm(TOT.General ~ TOT.Mayores.55)
summary(coint.6)

TOThat6 <- residuals(coint.6)

adf.test(TOThat6)

# TOTAL NACIONAL TODAS LAS EDADES (WAGE) ~ PRIMERA VIVIENDA (IPV)

TOT.Vivienda.nueva <- IPV_anual$TOT.Vivienda.nueva
TOT.Todas <- wage$TOT.Todas

adf.test(TOT.Vivienda.nueva)
adf.test(TOT.Todas)

coint.7 <- lm(TOT.Vivienda.nueva ~ TOT.Todas)
summary(coint.7)

TOThat7 <- residuals(coint.7)

adf.test(TOThat7)

# TOTAL NACIONAL MENORES DE 25 (WAGE) ~ PRIMERA VIVIENDA (IPV)

TOT.Vivienda.nueva <- IPV_anual$TOT.Vivienda.nueva
TOT.Menos.25 <- wage$TOT.Menos.de.25.años

adf.test(TOT.Vivienda.nueva)
adf.test(TOT.Menos.25)

coint.8 <- lm(TOT.Vivienda.nueva ~ TOT.Menos.25)
summary(coint.8)

TOThat8 <- residuals(coint.8)

adf.test(TOThat8)

# TOTAL NACIONAL DE 25 a 34 (WAGE) ~ PRIMERA VIVIENDA (IPV)

TOT.Vivienda.nueva <- IPV_anual$TOT.Vivienda.nueva
TOT.25.34 <- wage$TOT.De.25.a.34.años

adf.test(TOT.Vivienda.nueva)
adf.test(TOT.25.34)

coint.9 <- lm(TOT.Vivienda.nueva ~ TOT.25.34)
summary(coint.9)

TOThat9 <- residuals(coint.9)

adf.test(TOThat9)

# TOTAL NACIONAL DE 35 a 44 (WAGE) ~ PRIMERA VIVIENDA (IPV)

TOT.Vivienda.nueva <- IPV_anual$TOT.Vivienda.nueva
TOT.35.44 <- wage$TOT.De.35.a.44.años

adf.test(TOT.Vivienda.nueva)
adf.test(TOT.35.44)

coint.10 <- lm(TOT.Vivienda.nueva ~ TOT.35.44)
summary(coint.10)

TOThat10 <- residuals(coint.10)

adf.test(TOThat10)

# TOTAL NACIONAL DE 45 a 54 (WAGE) ~ PRIMERA VIVIENDA (IPV)

TOT.Vivienda.nueva <- IPV_anual$TOT.Vivienda.nueva
TOT.45.54 <- wage$TOT.De.45.a.54.años

adf.test(TOT.Vivienda.nueva)
adf.test(TOT.45.54)

coint.11 <- lm(TOT.Vivienda.nueva ~ TOT.45.54)
summary(coint.11)

TOThat11 <- residuals(coint.11)

adf.test(TOThat11)

# TOTAL NACIONAL DE MAYORES DE 55 (WAGE) ~ PRIMERA VIVIENDA (IPV)

TOT.Vivienda.nueva <- IPV_anual$TOT.Vivienda.nueva
TOT.Mayores.55 <- wage$TOT.55.y.más.años

adf.test(TOT.Vivienda.nueva)
adf.test(TOT.Mayores.55)

coint.12 <- lm(TOT.Vivienda.nueva ~ TOT.Mayores.55)
summary(coint.12)

TOThat12 <- residuals(coint.12)

adf.test(TOThat12)

# TOTAL NACIONAL TODAS LAS EDADES (WAGE) ~ VIVIENDA SEGUNDA MANO (IPV)

TOT.Vivienda.segunda.mano <- IPV_anual$TOT.Vivienda.segunda.mano
TOT.Todas <- wage$TOT.Todas

adf.test(TOT.Vivienda.segunda.mano)
adf.test(TOT.Todas)

coint.13 <- lm(TOT.Vivienda.segunda.mano ~ TOT.Todas)
summary(coint.13)

TOThat13 <- residuals(coint.13)

adf.test(TOThat13)

# TOTAL NACIONAL MENORES DE 25 (WAGE) ~ VIVIENDA SEGUNDA MANO (IPV)

TOT.Vivienda.segunda.mano <- IPV_anual$TOT.Vivienda.segunda.mano
TOT.Menos.25 <- wage$TOT.Menos.de.25.años

adf.test(TOT.Vivienda.segunda.mano)
adf.test(TOT.Menos.25)

coint.14 <- lm(TOT.Vivienda.segunda.mano ~ TOT.Menos.25)
summary(coint.14)

TOThat14 <- residuals(coint.14)

adf.test(TOThat14)

# TOTAL NACIONAL DE 25 a 34 (WAGE) ~ VIVIENDA SEGUNDA MANO (IPV)

TOT.Vivienda.segunda.mano <- IPV_anual$TOT.Vivienda.segunda.mano
TOT.25.34 <- wage$TOT.De.25.a.34.años

adf.test(TOT.Vivienda.segunda.mano)
adf.test(TOT.25.34)

coint.15 <- lm(TOT.Vivienda.segunda.mano ~ TOT.25.34)
summary(coint.15)

TOThat15 <- residuals(coint.15)

adf.test(TOThat15)

# TOTAL NACIONAL DE 35 a 44 (WAGE) ~ VIVIENDA SEGUNDA MANO (IPV)

TOT.Vivienda.segunda.mano <- IPV_anual$TOT.Vivienda.segunda.mano
TOT.35.44 <- wage$TOT.De.35.a.44.años

adf.test(TOT.Vivienda.segunda.mano)
adf.test(TOT.35.44)

coint.16 <- lm(TOT.Vivienda.segunda.mano ~ TOT.35.44)
summary(coint.16)

TOThat16 <- residuals(coint.16)

adf.test(TOThat16)

# TOTAL NACIONAL DE 45 a 54 (WAGE) ~ VIVIENDA SEGUNDA MANO (IPV)

TOT.Vivienda.segunda.mano <- IPV_anual$TOT.Vivienda.segunda.mano
TOT.45.54 <- wage$TOT.De.45.a.54.años

adf.test(TOT.Vivienda.segunda.mano)
adf.test(TOT.45.54)

coint.17 <- lm(TOT.Vivienda.segunda.mano ~ TOT.45.54)
summary(coint.17)

TOThat17 <- residuals(coint.17)

adf.test(TOThat17)

# TOTAL NACIONAL DE MAYORES DE 55 (WAGE) ~ VIVIENDA SEGUNDA MANO (IPV)

TOT.Vivienda.segunda.mano <- IPV_anual$TOT.Vivienda.segunda.mano
TOT.Mayores.55 <- wage$TOT.55.y.más.años

adf.test(TOT.Vivienda.segunda.mano)
adf.test(TOT.Mayores.55)

coint.18 <- lm(TOT.Vivienda.segunda.mano ~ TOT.Mayores.55)
summary(coint.18)

TOThat18 <- residuals(coint.18)

adf.test(TOThat18)

# PRINT DE RESULTADOS

wage_vars <- c(
  "TOT.Todas",
  "TOT.Menos.de.25.años",
  "TOT.De.25.a.34.años",
  "TOT.De.35.a.44.años",
  "TOT.De.45.a.54.años",
  "TOT.55.y.más.años")

ip_types <- c(
  "TOT.General",
  "TOT.Vivienda.nueva",
  "TOT.Vivienda.segunda.mano")

resultados_coint <- data.frame(
  IPV = character(),
  WAGE = character(),
  ADF_stat = numeric(),
  p_value = numeric(),
  cointegrado_5pct = logical(),
  stringsAsFactors = FALSE)

for (ip in ip_types) {
  
  y_ip <- IPV_anual[[ip]]
  
  for (w in wage_vars) {
    
    y_w <- wage[[w]]
    
    model <- lm(y_ip ~ y_w)
    
    resid <- residuals(model)

    adf <- adf.test(resid)

    coint <- adf$p.value < 0.05
    
    resultados_coint <- rbind(
      resultados_coint,
      data.frame(
        IPV = ip,
        WAGE = w,
        ADF_stat = adf$statistic,
        p_value = adf$p.value,
        cointegrado_5pct = coint))}}

resultados_coint <- resultados_coint[order(resultados_coint$p_value), ]

resultados_coint

write_xlsx(resultados_coint, "E:/TFG Econometría/R/Resultados R/Resultados_COINT.xlsx")

######################
####   VAR (1)   #####
######################

d_tg <- cbind(
  wage$TOT.Todas,
  IPV_anual$TOT.General)

colnames(d_tg) <- c("wage_total", "ipv_general")

# CRITERIOS DE INFO
c_tg <- VARselect(
  d_tg,
  lag.max = 2,
  type = "const")

c_tg

# VAR(1)
v_tg <- VAR(
  d_tg,
  p = 1,
  type = "const")

summary(v_tg)


#25

d_m25g <- cbind(
  wage$TOT.Menos.de.25.años,
  IPV_anual$TOT.General)

colnames(d_m25g) <- c("wage_m25", "ipv_general")

# CRITERIOS DE INFO
c_m25g <- VARselect(
  d_m25g,
  lag.max = 2,
  type = "const")

c_m25g

# VAR(1)
v_m25g <- VAR(
  d_m25g,
  p = 1,
  type = "const")

summary(v_m25g)


d_2534g <- cbind(
  wage$TOT.De.25.a.34.años,
  IPV_anual$TOT.General)

colnames(d_2534g) <- c("wage_2534", "ipv_general")

# CRITERIOS DE INFO
c_2534g <- VARselect(
  d_2534g,
  lag.max = 2,
  type = "const")

c_2534g

# VAR(1)
v_2534g <- VAR(
  d_2534g,
  p = 1,
  type = "const")

summary(v_2534g)


d_3544g <- cbind(
  wage$TOT.De.35.a.44.años,
  IPV_anual$TOT.General)

colnames(d_3544g) <- c("wage_3544", "ipv_general")

# CRITERIOS DE INFO
c_3544g <- VARselect(
  d_3544g,
  lag.max = 2,
  type = "const")

c_3544g

# VAR(1)
v_3544g <- VAR(
  d_3544g,
  p = 1,
  type = "const")

summary(v_3544g)


d_4554g <- cbind(
  wage$TOT.De.45.a.54.años,
  IPV_anual$TOT.General)

colnames(d_4554g) <- c("wage_4554", "ipv_general")

# CRITERIOS DE INFO
c_4554g <- VARselect(
  d_4554g,
  lag.max = 2,
  type = "const")

c_4554g

# VAR(1)
v_4554g <- VAR(
  d_4554g,
  p = 1,
  type = "const")

summary(v_4554g)


d_55masg <- cbind(
  wage$TOT.55.y.más.años,
  IPV_anual$TOT.General)

colnames(d_55masg) <- c("wage_55mas", "ipv_general")

# CRITERIOS DE INFO
c_55masg <- VARselect(
  d_55masg,
  lag.max = 2,
  type = "const")

c_55masg

# VAR(1)
v_55masg <- VAR(
  d_55masg,
  p = 1,
  type = "const")

summary(v_55masg)


d_55mas_nueva <- cbind(
  wage$TOT.55.y.más.años,
  IPV_anual$TOT.Vivienda.nueva)

colnames(d_55mas_nueva) <- c("wage_55mas", "ipv_nueva")

# CRITERIOS DE INFO
c_55mas_nueva <- VARselect(
  d_55mas_nueva,
  lag.max = 2,
  type = "const")

c_55mas_nueva

# VAR(1)
v_55mas_nueva <- VAR(
  d_55mas_nueva,
  p = 1,
  type = "const")

summary(v_55mas_nueva)


d_4554_nueva <- cbind(
  wage$TOT.De.45.a.54.años,
  IPV_anual$TOT.Vivienda.nueva)

colnames(d_4554_nueva) <- c("wage_4554", "ipv_nueva")

# CRITERIOS DE INFO
c_4554_nueva <- VARselect(
  d_4554_nueva,
  lag.max = 2,
  type = "const")

c_4554_nueva

# VAR(1)
v_4554_nueva <- VAR(
  d_4554_nueva,
  p = 1,
  type = "const")

summary(v_4554_nueva)


d_3544_nueva <- cbind(
  wage$TOT.De.35.a.44.años,
  IPV_anual$TOT.Vivienda.nueva)

colnames(d_3544_nueva) <- c("wage_3544", "ipv_nueva")

# CRITERIOS DE INFO
c_3544_nueva <- VARselect(
  d_3544_nueva,
  lag.max = 2,
  type = "const")

c_3544_nueva

# VAR(1)
v_3544_nueva <- VAR(
  d_3544_nueva,
  p = 1,
  type = "const")

summary(v_3544_nueva)


d_2534_nueva <- cbind(
  wage$TOT.De.25.a.34.años,
  IPV_anual$TOT.Vivienda.nueva)

colnames(d_2534_nueva) <- c("wage_2534", "ipv_nueva")

# CRITERIOS DE INFO
c_2534_nueva <- VARselect(
  d_2534_nueva,
  lag.max = 2,
  type = "const")

c_2534_nueva

# VAR(1)
v_2534_nueva <- VAR(
  d_2534_nueva,
  p = 1,
  type = "const")

summary(v_2534_nueva)


d_menor25_nueva <- cbind(
  wage$TOT.Menos.de.25.años,
  IPV_anual$TOT.Vivienda.nueva)

colnames(d_menor25_nueva) <- c("wage_menor25", "ipv_nueva")

# CRITERIOS DE INFO
c_menor25_nueva <- VARselect(
  d_menor25_nueva,
  lag.max = 2,
  type = "const")

c_menor25_nueva

# VAR(1)
v_menor25_nueva <- VAR(
  d_menor25_nueva,
  p = 1,
  type = "const")

summary(v_menor25_nueva)


d_total_nueva <- cbind(
  wage$TOT.Todas,
  IPV_anual$TOT.Vivienda.nueva)

colnames(d_total_nueva) <- c("wage_total", "ipv_nueva")

# CRITERIOS DE INFO
c_total_nueva <- VARselect(
  d_total_nueva,
  lag.max = 2,
  type = "const")

c_total_nueva

# VAR(1)
v_total_nueva <- VAR(
  d_total_nueva,
  p = 1,
  type = "const")

summary(v_total_nueva)


d_total_segunda <- cbind(
  wage$TOT.Todas,
  IPV_anual$TOT.Vivienda.segunda.mano)

colnames(d_total_segunda) <- c("wage_total", "ipv_segunda")

# CRITERIOS DE INFO
c_total_segunda <- VARselect(
  d_total_segunda,
  lag.max = 2,
  type = "const")

c_total_segunda

# VAR(1)
v_total_segunda <- VAR(
  d_total_segunda,
  p = 1,
  type = "const")

summary(v_total_segunda)


d_menor25_segunda <- cbind(
  wage$TOT.Menos.de.25.años,
  IPV_anual$TOT.Vivienda.segunda.mano)

colnames(d_menor25_segunda) <- c("wage_menor25", "ipv_segunda")

# CRITERIOS DE INFO
c_menor25_segunda <- VARselect(
  d_menor25_segunda,
  lag.max = 2,
  type = "const")

c_menor25_segunda

# VAR(1)
v_menor25_segunda <- VAR(
  d_menor25_segunda,
  p = 1,
  type = "const")

summary(v_menor25_segunda)


d_2534_segunda <- cbind(
  wage$TOT.De.25.a.34.años,
  IPV_anual$TOT.Vivienda.segunda.mano)

colnames(d_2534_segunda) <- c("wage_2534", "ipv_segunda")

# CRITERIOS DE INFO
c_2534_segunda <- VARselect(
  d_2534_segunda,
  lag.max = 2,
  type = "const")

c_2534_segunda

# VAR(1)
v_2534_segunda <- VAR(
  d_2534_segunda,
  p = 1,
  type = "const")

summary(v_2534_segunda)


d_3544_segunda <- cbind(
  wage$TOT.De.35.a.44.años,
  IPV_anual$TOT.Vivienda.segunda.mano)

colnames(d_3544_segunda) <- c("wage_3544", "ipv_segunda")

# CRITERIOS DE INFO
c_3544_segunda <- VARselect(
  d_3544_segunda,
  lag.max = 2,
  type = "const")

c_3544_segunda

# VAR(1)
v_3544_segunda <- VAR(
  d_3544_segunda,
  p = 1,
  type = "const")

summary(v_3544_segunda)


d_4554_segunda <- cbind(
  wage$TOT.De.45.a.54.años,
  IPV_anual$TOT.Vivienda.segunda.mano)

colnames(d_4554_segunda) <- c("wage_4554", "ipv_segunda")

# CRITERIOS DE INFO
c_4554_segunda <- VARselect(
  d_4554_segunda,
  lag.max = 2,
  type = "const")

c_4554_segunda

# VAR(1)
v_4554_segunda <- VAR(
  d_4554_segunda,
  p = 1,
  type = "const")

summary(v_4554_segunda)


d_55mas_segunda <- cbind(
  wage$TOT.55.y.más.años,
  IPV_anual$TOT.Vivienda.segunda.mano)

colnames(d_55mas_segunda) <- c("wage_55mas", "ipv_segunda")

# CRITERIOS DE INFO
c_55mas_segunda <- VARselect(
  d_55mas_segunda,
  lag.max = 2,
  type = "const")

c_55mas_segunda

# VAR(1)
v_55mas_segunda <- VAR(
  d_55mas_segunda,
  p = 1,
  type = "const")

summary(v_55mas_segunda)


colnames(d_tg) <- c("wage_total", "ipv_general")

#  OOSMSE
# -----------------------------
# PARAMETROS
# -----------------------------

T <- nrow(d_tg)

t_star <- floor(T / 2)

errores_wage <- c()
errores_ipv  <- c()

# -----------------------------
# ROLLING OOS FORECAST
# -----------------------------

for (t in t_star:(T - 1)) {

  train <- d_tg[1:t, ]

  modelo_var <- VAR(
    train,
    p = 1,
    type = "const")

  pred <- predict(
    modelo_var,
    n.ahead = 1)

  pred_wage <- pred$fcst$wage_total[1,1]
  pred_ipv  <- pred$fcst$ipv_general[1,1]

  real_wage <- d_tg[t + 1, "wage_total"]
  real_ipv  <- d_tg[t + 1, "ipv_general"]

  error_wage <- (real_wage - pred_wage)^2
  error_ipv  <- (real_ipv - pred_ipv)^2
  
  errores_wage <- c(errores_wage, error_wage)
  errores_ipv  <- c(errores_ipv, error_ipv)}

# -----------------------------
# OOS MSE
# -----------------------------

oosmse_wage <- mean(errores_wage)

oosmse_ipv <- mean(errores_ipv)

oosmse_global <- mean(c(oosmse_wage, oosmse_ipv))

oosmse_wage
oosmse_ipv
oosmse_global

##############################################
####   VAR (1) CON PRIMERAS DIFERENCIAS  #####
##############################################

# =========================
# TOTAL / IPV GENERAL
# =========================

d_tg <- na.omit(cbind(
  diff(wage$TOT.Todas),
  diff(IPV_anual$TOT.General)))

colnames(d_tg) <- c("d_wage_total", "d_ipv_general")

c_tg <- VARselect(d_tg, lag.max = 2, type = "const")
c_tg

v_tg <- VAR(d_tg, p = 1, type = "const")
summary(v_tg)

###########################
#### IMPULSO/RESPUESTA ####
###########################

irf_ipv <- irf(
  v_tg,
  impulse = "d_ipv_general",
  response = "d_wage_total",
  n.ahead = 3,
  boot = TRUE,
  ci = 0.95)

plot(irf_ipv)

irf_wage <- irf(
  v_tg,
  impulse = "d_wage_total",
  response = "d_ipv_general",
  n.ahead = 12,
  boot = TRUE,
  ci = 0.95)

plot(irf_wage)



#  OOSMSE
# =========================
# TOTAL / IPV GENERAL
# =========================

d_tg <- na.omit(cbind(
  diff(wage$TOT.Todas),
  diff(IPV_anual$TOT.General)))

colnames(d_tg) <- c("d_wage_total", "d_ipv_general")

# -----------------------------
# PARAMETROS
# -----------------------------

T <- nrow(d_tg)

t_star <- floor(T / 2)

errores_wage <- c()
errores_ipv  <- c()

# -----------------------------
# ROLLING OOS FORECAST
# -----------------------------

for (t in t_star:(T - 1)) {
  
  # TRAIN
  train <- d_tg[1:t, ]
  
  # VAR(1)
  modelo_var <- VAR(
    train,
    p = 1,
    type = "const")
  
  pred <- predict(
    modelo_var,
    n.ahead = 1)

  pred_wage <- pred$fcst$d_wage_total[1,1]
  pred_ipv  <- pred$fcst$d_ipv_general[1,1]

  real_wage <- d_tg[t + 1, "d_wage_total"]
  real_ipv  <- d_tg[t + 1, "d_ipv_general"]

  error_wage <- (real_wage - pred_wage)^2
  error_ipv  <- (real_ipv - pred_ipv)^2
  
  errores_wage <- c(errores_wage, error_wage)
  errores_ipv  <- c(errores_ipv, error_ipv)}

# -----------------------------
# OOS MSE
# -----------------------------

oosmse_wage <- mean(errores_wage)

oosmse_ipv <- mean(errores_ipv)

oosmse_global <- mean(c(oosmse_wage, oosmse_ipv))

oosmse_wage
oosmse_ipv
oosmse_global

# =========================
# MENOR 25 / GENERAL
# =========================

d_m25g <- na.omit(cbind(
  diff(wage$TOT.Menos.de.25.años),
  diff(IPV_anual$TOT.General)))

colnames(d_m25g) <- c("d_wage_m25", "d_ipv_general")

c_m25g <- VARselect(d_m25g, lag.max = 2, type = "const")
c_m25g

v_m25g <- VAR(d_m25g, p = 1, type = "const")
summary(v_m25g)


# =========================
# 25-34 / GENERAL
# =========================

d_2534g <- na.omit(cbind(
  diff(wage$TOT.De.25.a.34.años),
  diff(IPV_anual$TOT.General)))

colnames(d_2534g) <- c("d_wage_2534", "d_ipv_general")

c_2534g <- VARselect(d_2534g, lag.max = 2, type = "const")
c_2534g

v_2534g <- VAR(d_2534g, p = 1, type = "const")
summary(v_2534g)


# =========================
# 35-44 / GENERAL
# =========================

d_3544g <- na.omit(cbind(
  diff(wage$TOT.De.35.a.44.años),
  diff(IPV_anual$TOT.General)))

colnames(d_3544g) <- c("d_wage_3544", "d_ipv_general")

c_3544g <- VARselect(d_3544g, lag.max = 2, type = "const")
c_3544g

v_3544g <- VAR(d_3544g, p = 1, type = "const")
summary(v_3544g)


# =========================
# 45-54 / GENERAL
# =========================

d_4554g <- na.omit(cbind(
  diff(wage$TOT.De.45.a.54.años),
  diff(IPV_anual$TOT.General)))

colnames(d_4554g) <- c("d_wage_4554", "d_ipv_general")

c_4554g <- VARselect(d_4554g, lag.max = 2, type = "const")
c_4554g

v_4554g <- VAR(d_4554g, p = 1, type = "const")
summary(v_4554g)


# =========================
# 55+ / GENERAL
# =========================

d_55masg <- na.omit(cbind(
  diff(wage$TOT.55.y.más.años),
  diff(IPV_anual$TOT.General)))

colnames(d_55masg) <- c("d_wage_55mas", "d_ipv_general")

c_55masg <- VARselect(d_55masg, lag.max = 2, type = "const")
c_55masg

v_55masg <- VAR(d_55masg, p = 1, type = "const")
summary(v_55masg)


# =========================
# 55+ / VIVIENDA NUEVA
# =========================

d_55mas_nueva <- na.omit(cbind(
  diff(wage$TOT.55.y.más.años),
  diff(IPV_anual$TOT.Vivienda.nueva)))

colnames(d_55mas_nueva) <- c("d_wage_55mas", "d_ipv_nueva")

c_55mas_nueva <- VARselect(d_55mas_nueva, lag.max = 2, type = "const")
c_55mas_nueva

v_55mas_nueva <- VAR(d_55mas_nueva, p = 1, type = "const")
summary(v_55mas_nueva)


# =========================
# 45-54 / NUEVA
# =========================

d_4554_nueva <- na.omit(cbind(
  diff(wage$TOT.De.45.a.54.años),
  diff(IPV_anual$TOT.Vivienda.nueva)))

colnames(d_4554_nueva) <- c("d_wage_4554", "d_ipv_nueva")

c_4554_nueva <- VARselect(d_4554_nueva, lag.max = 2, type = "const")
c_4554_nueva

v_4554_nueva <- VAR(d_4554_nueva, p = 1, type = "const")
summary(v_4554_nueva)


# =========================
# 35-44 / NUEVA
# =========================

d_3544_nueva <- na.omit(cbind(
  diff(wage$TOT.De.35.a.44.años),
  diff(IPV_anual$TOT.Vivienda.nueva)))

colnames(d_3544_nueva) <- c("d_wage_3544", "d_ipv_nueva")

c_3544_nueva <- VARselect(d_3544_nueva, lag.max = 2, type = "const")
c_3544_nueva

v_3544_nueva <- VAR(d_3544_nueva, p = 1, type = "const")
summary(v_3544_nueva)


# =========================
# 25-34 / NUEVA
# =========================

d_2534_nueva <- na.omit(cbind(
  diff(wage$TOT.De.25.a.34.años),
  diff(IPV_anual$TOT.Vivienda.nueva)))

colnames(d_2534_nueva) <- c("d_wage_2534", "d_ipv_nueva")

c_2534_nueva <- VARselect(d_2534_nueva, lag.max = 2, type = "const")
c_2534_nueva

v_2534_nueva <- VAR(d_2534_nueva, p = 1, type = "const")
summary(v_2534_nueva)


# =========================
# <25 / NUEVA
# =========================

d_menor25_nueva <- na.omit(cbind(
  diff(wage$TOT.Menos.de.25.años),
  diff(IPV_anual$TOT.Vivienda.nueva)))

colnames(d_menor25_nueva) <- c("d_wage_m25", "d_ipv_nueva")

c_menor25_nueva <- VARselect(d_menor25_nueva, lag.max = 2, type = "const")
c_menor25_nueva

v_menor25_nueva <- VAR(d_menor25_nueva, p = 1, type = "const")
summary(v_menor25_nueva)


# =========================
# TOTAL / NUEVA
# =========================

d_total_nueva <- na.omit(cbind(
  diff(wage$TOT.Todas),
  diff(IPV_anual$TOT.Vivienda.nueva)))

colnames(d_total_nueva) <- c("d_wage_total", "d_ipv_nueva")

c_total_nueva <- VARselect(d_total_nueva, lag.max = 2, type = "const")
c_total_nueva

v_total_nueva <- VAR(d_total_nueva, p = 1, type = "const")
summary(v_total_nueva)


# =========================
# TOTAL / SEGUNDA MANO
# =========================

d_total_segunda <- na.omit(cbind(
  diff(wage$TOT.Todas),
  diff(IPV_anual$TOT.Vivienda.segunda.mano)))

colnames(d_total_segunda) <- c("d_wage_total", "d_ipv_segunda")

c_total_segunda <- VARselect(d_total_segunda, lag.max = 2, type = "const")
c_total_segunda

v_total_segunda <- VAR(d_total_segunda, p = 1, type = "const")
summary(v_total_segunda)


# =========================
# <25 / SEGUNDA MANO
# =========================

d_menor25_segunda <- na.omit(cbind(
  diff(wage$TOT.Menos.de.25.años),
  diff(IPV_anual$TOT.Vivienda.segunda.mano)))

colnames(d_menor25_segunda) <- c("d_wage_m25", "d_ipv_segunda")

c_menor25_segunda <- VARselect(d_menor25_segunda, lag.max = 2, type = "const")
c_menor25_segunda

v_menor25_segunda <- VAR(d_menor25_segunda, p = 1, type = "const")
summary(v_menor25_segunda)


# =========================
# 25-34 / SEGUNDA MANO
# =========================

d_2534_segunda <- na.omit(cbind(
  diff(wage$TOT.De.25.a.34.años),
  diff(IPV_anual$TOT.Vivienda.segunda.mano)))

colnames(d_2534_segunda) <- c("d_wage_2534", "d_ipv_segunda")

c_2534_segunda <- VARselect(d_2534_segunda, lag.max = 2, type = "const")
c_2534_segunda

v_2534_segunda <- VAR(d_2534_segunda, p = 1, type = "const")
summary(v_2534_segunda)


# =========================
# 35-44 / SEGUNDA MANO
# =========================

d_3544_segunda <- na.omit(cbind(
  diff(wage$TOT.De.35.a.44.años),
  diff(IPV_anual$TOT.Vivienda.segunda.mano)))

colnames(d_3544_segunda) <- c("d_wage_3544", "d_ipv_segunda")

c_3544_segunda <- VARselect(d_3544_segunda, lag.max = 2, type = "const")
c_3544_segunda

v_3544_segunda <- VAR(d_3544_segunda, p = 1, type = "const")
summary(v_3544_segunda)


# =========================
# 45-54 / SEGUNDA MANO
# =========================

d_4554_segunda <- na.omit(cbind(
  diff(wage$TOT.De.45.a.54.años),
  diff(IPV_anual$TOT.Vivienda.segunda.mano)))

colnames(d_4554_segunda) <- c("d_wage_4554", "d_ipv_segunda")

c_4554_segunda <- VARselect(d_4554_segunda, lag.max = 2, type = "const")
c_4554_segunda

v_4554_segunda <- VAR(d_4554_segunda, p = 1, type = "const")
summary(v_4554_segunda)


# =========================
# 55+ / SEGUNDA MANO
# =========================

d_55mas_segunda <- na.omit(cbind(
  diff(wage$TOT.55.y.más.años),
  diff(IPV_anual$TOT.Vivienda.segunda.mano)))

colnames(d_55mas_segunda) <- c("d_wage_55mas", "d_ipv_segunda")

c_55mas_segunda <- VARselect(d_55mas_segunda, lag.max = 2, type = "const")
c_55mas_segunda

v_55mas_segunda <- VAR(d_55mas_segunda, p = 1, type = "const")
summary(v_55mas_segunda)


# 1. FUNCIÓN OOSMSE (DIFERENCIAS)

get_oosmse_diff <- function(data){
  
  data <- na.omit(data)
  
  n <- nrow(data)
  train_size <- floor(0.8 * n)
  
  train <- data[1:train_size, ]
  test  <- data[(train_size+1):n, ]
  
  model <- VAR(train, p = 1, type = "const")
  fc <- predict(model, n.ahead = nrow(test))
  
  f1 <- fc$fcst[[1]][,1]
  f2 <- fc$fcst[[2]][,1]
  
  r1 <- test[,1]
  r2 <- test[,2]
  
  oosmse_1 <- mean((r1 - f1)^2)
  oosmse_2 <- mean((r2 - f2)^2)
  
  oosmse_tot <- mean(c((r1 - f1)^2,
                       (r2 - f2)^2))
  
  return(c(oosmse_1, oosmse_2, oosmse_tot))}

# 2. LISTA 18 MODELOS (DIFERENCIAS)

lista_diff <- list(
  d_tg,
  d_m25g,
  d_2534g,
  d_3544g,
  d_4554g,
  d_55masg,
  
  d_55mas_nueva,
  d_4554_nueva,
  d_3544_nueva,
  d_2534_nueva,
  d_menor25_nueva,
  d_total_nueva,
  
  d_total_segunda,
  d_menor25_segunda,
  d_2534_segunda,
  d_3544_segunda,
  d_4554_segunda,
  d_55mas_segunda)

# 3. NOMBRES MODELOS

nombres <- c(
  "total_general",
  "menor25_general",
  "2534_general",
  "3544_general",
  "4554_general",
  "55mas_general",
  
  "55mas_nueva",
  "4554_nueva",
  "3544_nueva",
  "2534_nueva",
  "menor25_nueva",
  "total_nueva",
  
  "total_segunda",
  "menor25_segunda",
  "2534_segunda",
  "3544_segunda",
  "4554_segunda",
  "55mas_segunda")

# 4. CALCULO OOSMSE (SOLO DIFERENCIAS)

oosmse_diff_matrix <- t(sapply(lista_diff, get_oosmse_diff))

colnames(oosmse_diff_matrix) <- c(
  "OOSMSE_wage_diff",
  "OOSMSE_IPV_diff",
  "OOSMSE_total_diff")

rownames(oosmse_diff_matrix) <- nombres


# DATOS
d_tg <- cbind(
  wage$TOT.Todas,
  IPV_anual$TOT.General)

colnames(d_tg) <- c("wage_total", "ipv_general")

# -----------------------------
# DIVISION 50% TRAIN - 50% TEST
# -----------------------------

n <- nrow(d_tg)

mitad <- floor(n / 2)

train <- d_tg[1:mitad, ]
test  <- d_tg[(mitad + 1):n, ]

# -----------------------------
# ESTIMAR VAR(1)
# -----------------------------

v_tg <- VAR(
  train,
  p = 1,
  type = "const")

summary(v_tg)

# -----------------------------
# PREDICCION OOS
# -----------------------------

h <- nrow(test)

pred <- predict(
  v_tg,
  n.ahead = h)

pred_wage <- pred$fcst$wage_total[,1]
pred_ipv  <- pred$fcst$ipv_general[,1]

# -----------------------------
# OOS MSE
# -----------------------------

mse_wage <- mean(
  (test[, "wage_total"] - pred_wage)^2)

mse_ipv <- mean(
  (test[, "ipv_general"] - pred_ipv)^2)

# MSE GLOBAL
mse_global <- mean(c(mse_wage, mse_ipv))

mse_wage
mse_ipv
mse_global

# 5. RESULTADO FINAL

oosmse_diff_matrix

######################
####   VAR (2)   #####
######################

d_total_general <- cbind(
  wage$TOT.Todas,
  IPV_anual$TOT.General)

colnames(d_total_general) <- c("wage_total", "ipv_general")

# CRITERIOS DE INFO
c_total_general <- VARselect(
  d_total_general,
  lag.max = 2,
  type = "const")

c_total_general

# VAR(2)
v_total_general <- VAR(
  d_total_general,
  p = 2,
  type = "const")

summary(v_total_general)

# =========================
# OOSMSE VAR(2)
# Rolling Forecast
# =========================

d_total_general <- na.omit(cbind(
  wage$TOT.Todas,
  IPV_anual$TOT.General))

colnames(d_total_general) <- c(
  "wage_total",
  "ipv_general")

# -----------------------------
# PARAMETROS
# -----------------------------

T <- nrow(d_total_general)

t_star <- floor(T / 2)

errores_wage <- c()

errores_ipv <- c()

# -----------------------------
# ROLLING OOS FORECAST
# -----------------------------

for (t in t_star:(T - 1)) {
  
  # TRAIN
  train <- d_total_general[1:t, ]
  
  # VAR(2)
  modelo_var <- VAR(
    train,
    p = 2,
    type = "const")
  
  # PREDICCION
  pred <- predict(
    modelo_var,
    n.ahead = 1)
  
  pred_wage <- pred$fcst$wage_total[1,1]
  
  pred_ipv <- pred$fcst$ipv_general[1,1]
  
  # VALORES REALES
  real_wage <- d_total_general[
    t + 1,
    "wage_total"]
  
  real_ipv <- d_total_general[
    t + 1,
    "ipv_general"]
  
  # ERRORES CUADRATICOS
  error_wage <- (
    real_wage - pred_wage)^2
  
  error_ipv <- (
    real_ipv - pred_ipv)^2
  
  errores_wage <- c(
    errores_wage,
    error_wage)
  
  errores_ipv <- c(
    errores_ipv,
    error_ipv)}

# -----------------------------
# OOSMSE
# -----------------------------

oosmse_wage <- mean(
  errores_wage)

oosmse_ipv <- mean(
  errores_ipv)

oosmse_global <- mean(
  c(oosmse_wage,
    oosmse_ipv))

oosmse_wage

oosmse_ipv

oosmse_global

###########################
####   PREDICCIONES   #####
###########################

###    --/---SERIES WAGE---/--    ###

wage_ts <- wage$TOT.Todas

h <- 3  # 2024, 2025, 2026

ar1      <- ar(wage_ts, method = "ols", aic = FALSE, order.max = 1)
arima110 <- Arima(wage_ts, order = c(1,1,0), method = "ML")
arima210 <- Arima(wage_ts, order = c(2,1,0), method = "ML")
arima111 <- Arima(wage_ts, order = c(1,1,1), method = "ML")

pred_ar1 <- forecast(ar1, h = h)$mean
pred_110 <- forecast(arima110, h = h)$mean
pred_210 <- forecast(arima210, h = h)$mean
pred_111 <- forecast(arima111, h = h)$mean

years <- 2008:2023
future_years <- 2024:2026

ylim_range <- range(c(wage_ts, pred_ar1, pred_110, pred_210, pred_111))

plot(years, wage_ts,
     type = "l",
     xlim = c(2008, 2026),
     xaxt = "n",
     ylim = ylim_range,
     xlab = "Año",
     ylab = "Euros")

lines(future_years, pred_ar1, col = "blue",   lty = 2, lwd = 2)
lines(future_years, pred_110, col = "red",    lty = 3, lwd = 2)
lines(future_years, pred_210, col = "green",  lty = 4, lwd = 2)
lines(future_years, pred_111, col = "purple", lty = 5, lwd = 2)

axis(1, at = c(2010, 2015, 2020, 2025))

abline(v = 2023, lty = 2, col = "grey")

legend("topleft",
       legend = c("AR(1)", "ARIMA(1,1,0)", "ARIMA(2,1,0)", "ARIMA(1,1,1)"),
       col = c("blue", "red", "green", "purple"),
       lty = c(2,3,4,5),
       lwd = 2)

###

wage_ts <- wage$TOT.Menos.de.25.años

h <- 3  # 2024, 2025, 2026

ar1      <- ar(wage_ts, method = "ols", aic = FALSE, order.max = 1)
arima110 <- Arima(wage_ts, order = c(1,1,0), method = "ML")
arima210 <- Arima(wage_ts, order = c(2,1,0), method = "ML")
arima111 <- Arima(wage_ts, order = c(1,1,1), method = "ML")

pred_ar1 <- forecast(ar1, h = h)$mean
pred_110 <- forecast(arima110, h = h)$mean
pred_210 <- forecast(arima210, h = h)$mean
pred_111 <- forecast(arima111, h = h)$mean

years <- 2008:2023
future_years <- 2024:2026

ylim_range <- range(c(wage_ts, pred_ar1, pred_110, pred_210, pred_111))

plot(years, wage_ts,
     type = "l",
     xlim = c(2008, 2026),
     xaxt = "n",
     ylim = ylim_range,
     xlab = "Año",
     ylab = "Euros")

lines(future_years, pred_ar1, col = "blue",   lty = 2, lwd = 2)
lines(future_years, pred_110, col = "red",    lty = 3, lwd = 2)
lines(future_years, pred_210, col = "green",  lty = 4, lwd = 2)
lines(future_years, pred_111, col = "purple", lty = 5, lwd = 2)

axis(1, at = c(2010, 2015, 2020, 2025))

abline(v = 2023, lty = 2, col = "grey")

legend("topleft",
       legend = c("AR(1)", "ARIMA(1,1,0)", "ARIMA(2,1,0)", "ARIMA(1,1,1)"),
       col = c("blue", "red", "green", "purple"),
       lty = c(2,3,4,5),
       lwd = 2)

###

wage_ts <- wage$TOT.De.25.a.34.años

h <- 3  # 2024, 2025, 2026

ar1      <- ar(wage_ts, method = "ols", aic = FALSE, order.max = 1)
arima110 <- Arima(wage_ts, order = c(1,1,0), method = "ML")
arima210 <- Arima(wage_ts, order = c(2,1,0), method = "ML")
arima111 <- Arima(wage_ts, order = c(1,1,1), method = "ML")

pred_ar1 <- forecast(ar1, h = h)$mean
pred_110 <- forecast(arima110, h = h)$mean
pred_210 <- forecast(arima210, h = h)$mean
pred_111 <- forecast(arima111, h = h)$mean

years <- 2008:2023
future_years <- 2024:2026

ylim_range <- range(c(wage_ts, pred_ar1, pred_110, pred_210, pred_111))

plot(years, wage_ts,
     type = "l",
     xlim = c(2008, 2026),
     xaxt = "n",
     ylim = ylim_range,
     xlab = "Año",
     ylab = "Euros")

lines(future_years, pred_ar1, col = "blue",   lty = 2, lwd = 2)
lines(future_years, pred_110, col = "red",    lty = 3, lwd = 2)
lines(future_years, pred_210, col = "green",  lty = 4, lwd = 2)
lines(future_years, pred_111, col = "purple", lty = 5, lwd = 2)

axis(1, at = c(2010, 2015, 2020, 2025))

abline(v = 2023, lty = 2, col = "grey")

legend("topleft",
       legend = c("AR(1)", "ARIMA(1,1,0)", "ARIMA(2,1,0)", "ARIMA(1,1,1)"),
       col = c("blue", "red", "green", "purple"),
       lty = c(2,3,4,5),
       lwd = 2)

###

wage_ts <- wage$TOT.De.35.a.44.años

h <- 3  # 2024, 2025, 2026

ar1      <- ar(wage_ts, method = "ols", aic = FALSE, order.max = 1)
arima110 <- arima(wage_ts, order = c(1,1,0), method = "ML")
arima210 <- arima(wage_ts, order = c(2,1,0), method = "ML")
arima111 <- arima(wage_ts, order = c(1,1,1), method = "ML")

pred_ar1 <- forecast(ar1, h = h)$mean
pred_110 <- forecast(arima110, h = h)$mean
pred_210 <- forecast(arima210, h = h)$mean
pred_111 <- forecast(arima111, h = h)$mean

years <- 2008:2023
future_years <- 2024:2026

ylim_range <- range(c(wage_ts, pred_ar1, pred_110, pred_210, pred_111))

plot(years, wage_ts,
     type = "l",
     xlim = c(2008, 2026),
     xaxt = "n",
     ylim = ylim_range,
     xlab = "Año",
     ylab = "Euros")

lines(future_years, pred_ar1, col = "blue",   lty = 2, lwd = 2)
lines(future_years, pred_110, col = "red",    lty = 3, lwd = 2)
lines(future_years, pred_210, col = "green",  lty = 4, lwd = 2)
lines(future_years, pred_111, col = "purple", lty = 5, lwd = 2)

axis(1, at = c(2010, 2015, 2020, 2025))

abline(v = 2023, lty = 2, col = "grey")

legend("topleft",
       legend = c("AR(1)", "ARIMA(1,1,0)", "ARIMA(2,1,0)", "ARIMA(1,1,1)"),
       col = c("blue", "red", "green", "purple"),
       lty = c(2,3,4,5),
       lwd = 2)

###

wage_ts <- wage$TOT.De.45.a.54.años

h <- 3  # 2024, 2025, 2026

ar1      <- ar(wage_ts, method = "ols", aic = FALSE, order.max = 1)
arima110 <- arima(wage_ts, order = c(1,1,0), method = "ML")
arima210 <- arima(wage_ts, order = c(2,1,0), method = "ML")
arima111 <- arima(wage_ts, order = c(1,1,1), method = "ML")

pred_ar1 <- forecast(ar1, h = h)$mean
pred_110 <- forecast(arima110, h = h)$mean
pred_210 <- forecast(arima210, h = h)$mean
pred_111 <- forecast(arima111, h = h)$mean

years <- 2008:2023
future_years <- 2024:2026

ylim_range <- range(c(wage_ts, pred_ar1, pred_110, pred_210, pred_111))

plot(years, wage_ts,
     type = "l",
     xlim = c(2008, 2026),
     xaxt = "n",
     ylim = ylim_range,
     xlab = "Año",
     ylab = "Euros")

lines(future_years, pred_ar1, col = "blue",   lty = 2, lwd = 2)
lines(future_years, pred_110, col = "red",    lty = 3, lwd = 2)
lines(future_years, pred_210, col = "green",  lty = 4, lwd = 2)
lines(future_years, pred_111, col = "purple", lty = 5, lwd = 2)

axis(1, at = c(2010, 2015, 2020, 2025))

abline(v = 2023, lty = 2, col = "grey")

legend("topleft",
       legend = c("AR(1)", "ARIMA(1,1,0)", "ARIMA(2,1,0)", "ARIMA(1,1,1)"),
       col = c("blue", "red", "green", "purple"),
       lty = c(2,3,4,5),
       lwd = 2)

###

wage_ts <- wage$TOT.55.y.más.años

h <- 3  # 2024, 2025, 2026

ar1      <- ar(wage_ts, method = "ols", aic = FALSE, order.max = 1)
arima110 <- arima(wage_ts, order = c(1,1,0), method = "ML")
arima210 <- arima(wage_ts, order = c(2,1,0), method = "ML")
arima111 <- arima(wage_ts, order = c(1,1,1), method = "ML")

pred_ar1 <- forecast(ar1, h = h)$mean
pred_110 <- forecast(arima110, h = h)$mean
pred_210 <- forecast(arima210, h = h)$mean
pred_111 <- forecast(arima111, h = h)$mean

years <- 2008:2023
future_years <- 2024:2026

ylim_range <- range(c(wage_ts, pred_ar1, pred_110, pred_210, pred_111))

plot(years, wage_ts,
     type = "l",
     xlim = c(2008, 2026),
     xaxt = "n",
     ylim = ylim_range,
     xlab = "Año",
     ylab = "Euros")

lines(future_years, pred_ar1, col = "blue",   lty = 2, lwd = 2)
lines(future_years, pred_110, col = "red",    lty = 3, lwd = 2)
lines(future_years, pred_210, col = "green",  lty = 4, lwd = 2)
lines(future_years, pred_111, col = "purple", lty = 5, lwd = 2)

axis(1, at = c(2010, 2015, 2020, 2025))

abline(v = 2023, lty = 2, col = "grey")

legend("topleft",
       legend = c("AR(1)", "ARIMA(1,1,0)", "ARIMA(2,1,0)", "ARIMA(1,1,1)"),
       col = c("blue", "red", "green", "purple"),
       lty = c(2,3,4,5),
       lwd = 2)

###    --/---SERIES IPV---/--    ###

ipv_ts <- ts(IPV$TOT.General,
             start = c(2008, 1),
             frequency = 4)

h <- 12

ar1 <- ar(ipv_ts, method = "ols", aic = FALSE, order.max = 1)

fc_ar1 <- forecast(ar1, h = h)

arima110 <- Arima(ipv_ts, order = c(1,1,0), method = "ML")
arima210 <- Arima(ipv_ts, order = c(2,1,0), method = "ML")
arima111 <- Arima(ipv_ts, order = c(1,1,1), method = "ML")

fc_110 <- forecast(arima110, h = h)
fc_210 <- forecast(arima210, h = h)
fc_111 <- forecast(arima111, h = h)

ylim_range <- range(c(ipv_ts,
                      fc_ar1$mean,
                      fc_110$mean,
                      fc_210$mean,
                      fc_111$mean))

plot(ipv_ts,
     type = "l",
     xlim = c(2008, 2026),
     xaxt = "n",
     ylim = ylim_range,
     xlab = "Año",
     ylab = "Valor")

lines(fc_ar1$mean, col = "blue",   lty = 2, lwd = 2)
lines(fc_110$mean, col = "red",    lty = 3, lwd = 2)
lines(fc_210$mean, col = "green",  lty = 4, lwd = 2)
lines(fc_111$mean, col = "purple", lty = 5, lwd = 2)

axis(1, at = c(2010, 2015, 2020, 2025))

abline(v = 2023, lty = 2, col = "grey")

legend("topleft",
       legend = c("AR(1)", "ARIMA(1,1,0)", "ARIMA(2,1,0)", "ARIMA(1,1,1)"),
       col = c("blue", "red", "green", "purple"),
       lty = c(2,3,4,5),
       lwd = 2)

###

ipv_ts <- ts(IPV$TOT.Vivienda.nueva,
             start = c(2008, 1),
             frequency = 4)

h <- 12

ar1 <- ar(ipv_ts, method = "ols", aic = FALSE, order.max = 1)

fc_ar1 <- forecast(ar1, h = h)

arima110 <- Arima(ipv_ts, order = c(1,1,0), method = "ML")
arima210 <- Arima(ipv_ts, order = c(2,1,0), method = "ML")
arima111 <- Arima(ipv_ts, order = c(1,1,1), method = "ML")

fc_110 <- forecast(arima110, h = h)
fc_210 <- forecast(arima210, h = h)
fc_111 <- forecast(arima111, h = h)

ylim_range <- range(c(ipv_ts,
                      fc_ar1$mean,
                      fc_110$mean,
                      fc_210$mean,
                      fc_111$mean))

plot(ipv_ts,
     type = "l",
     xlim = c(2008, 2026),
     xaxt = "n",
     ylim = ylim_range,
     xlab = "Año",
     ylab = "Valor")

lines(fc_ar1$mean, col = "blue",   lty = 2, lwd = 2)
lines(fc_110$mean, col = "red",    lty = 3, lwd = 2)
lines(fc_210$mean, col = "green",  lty = 4, lwd = 2)
lines(fc_111$mean, col = "purple", lty = 5, lwd = 2)

axis(1, at = c(2010, 2015, 2020, 2025))

abline(v = 2023, lty = 2, col = "grey")

legend("topleft",
       legend = c("AR(1)", "ARIMA(1,1,0)", "ARIMA(2,1,0)", "ARIMA(1,1,1)"),
       col = c("blue", "red", "green", "purple"),
       lty = c(2,3,4,5),
       lwd = 2)

###

ipv_ts <- ts(IPV$TOT.Vivienda.segunda.mano,
             start = c(2008, 1),
             frequency = 4)

h <- 12

ar1 <- ar(ipv_ts, method = "ols", aic = FALSE, order.max = 1)

fc_ar1 <- forecast(ar1, h = h)

arima110 <- Arima(ipv_ts, order = c(1,1,0), method = "ML")
arima210 <- Arima(ipv_ts, order = c(2,1,0), method = "ML")
arima111 <- Arima(ipv_ts, order = c(1,1,1), method = "ML")

fc_110 <- forecast(arima110, h = h)
fc_210 <- forecast(arima210, h = h)
fc_111 <- forecast(arima111, h = h)

ylim_range <- range(c(ipv_ts,
                      fc_ar1$mean,
                      fc_110$mean,
                      fc_210$mean,
                      fc_111$mean))

plot(ipv_ts,
     type = "l",
     xlim = c(2008, 2026),
     xaxt = "n",
     ylim = ylim_range,
     xlab = "Año",
     ylab = "Valor")

lines(fc_ar1$mean, col = "blue",   lty = 2, lwd = 2)
lines(fc_110$mean, col = "red",    lty = 3, lwd = 2)
lines(fc_210$mean, col = "green",  lty = 4, lwd = 2)
lines(fc_111$mean, col = "purple", lty = 5, lwd = 2)

axis(1, at = c(2010, 2015, 2020, 2025))

abline(v = 2023, lty = 2, col = "grey")

legend("topleft",
       legend = c("AR(1)", "ARIMA(1,1,0)", "ARIMA(2,1,0)", "ARIMA(1,1,1)"),
       col = c("blue", "red", "green", "purple"),
       lty = c(2,3,4,5),
       lwd = 2)

####################################################
####   VAR(1) WAGE EN NIVELES E IPV INTEGRADO   ####
####################################################

# =========================
# TOTAL / IPV GENERAL
# =========================

tg <- na.omit(cbind(
  wage$TOT.Todas,
  diff(IPV_anual$TOT.General)))

colnames(tg) <- c("wage_total", "d_ipv_general")

c_tg <- VARselect(tg, lag.max = 2, type = "const")
c_tg

v_tg <- VAR(tg, p = 1, type = "const")
summary(v_tg)

# =========================
# OOS MSE
# =========================

tg <- na.omit(cbind(
  wage$TOT.Todas,
  IPV_anual$TOT.General))

colnames(tg) <- c("wage_total", "ipv_general")

# -----------------------------
# PARAMETROS
# -----------------------------

T <- nrow(tg)

t_star <- floor(T / 2)

errores_wage <- c()
errores_ipv  <- c()

# -----------------------------
# ROLLING OOS FORECAST
# -----------------------------

for (t in t_star:(T - 1)) {
  
  train <- tg[1:t, ]
  
  modelo_var <- VAR(
    train,
    p = 1,
    type = "const")
  
  pred <- predict(
    modelo_var,
    n.ahead = 1)
  
  pred_wage <- pred$fcst$wage_total[1,1]
  pred_ipv  <- pred$fcst$ipv_general[1,1]
  
  real_wage <- tg[t + 1, "wage_total"]
  real_ipv  <- tg[t + 1, "ipv_general"]
  
  error_wage <- (real_wage - pred_wage)^2
  error_ipv  <- (real_ipv - pred_ipv)^2
  
  errores_wage <- c(errores_wage, error_wage)
  errores_ipv  <- c(errores_ipv, error_ipv)}

# -----------------------------
# OOS MSE
# -----------------------------

oosmse_wage <- mean(errores_wage)

oosmse_ipv <- mean(errores_ipv)

oosmse_global <- mean(c(oosmse_wage, oosmse_ipv))

oosmse_wage
oosmse_ipv
oosmse_global

#############################################
#### IMPULSO-RESPUESTA VAR(1) WAGE E IPV ####
#############################################

irf_tg <- irf(
  v_tg,
  n.ahead = 10,
  boot = TRUE,
  ci = 0.95)

plot(irf_tg)

irf_tg$irf$d_ipv_general[, "wage_total"]

irf_tg$irf$wage_total[, "d_ipv_general"]


##############################################
#### PREDECIR QUITANDO ULTIMO PERIODO IPV ####
##############################################


ipv_ts_trim <- window(ipv_ts, end = c(2023, 3))

h <- 16

ar1 <- ar(ipv_ts_trim, method = "ols", aic = FALSE, order.max = 1)
fc_ar1 <- forecast(ar1, h = h)

arima110 <- Arima(ipv_ts_trim, order = c(1,1,0), method = "ML")
arima210 <- Arima(ipv_ts_trim, order = c(2,1,0), method = "ML")
arima111 <- Arima(ipv_ts_trim, order = c(1,1,1), method = "ML")

fc_110 <- forecast(arima110, h = h)
fc_210 <- forecast(arima210, h = h)
fc_111 <- forecast(arima111, h = h)

ylim_range <- range(c(ipv_ts_trim, fc_ar1$mean, fc_110$mean, fc_210$mean, fc_111$mean)) 
plot(ipv_ts_trim, type = "l", xlim = c(2008, 2026), xaxt = "n", ylim = ylim_range, xlab = "Año", ylab = "Valor") 
lines(fc_ar1$mean, col = "blue", lty = 2, lwd = 2) 
lines(fc_110$mean, col = "red", lty = 3, lwd = 2) 
lines(fc_210$mean, col = "green", lty = 4, lwd = 2) 
lines(fc_111$mean, col = "purple", lty = 5, lwd = 2) 
axis(1, at = c(2010, 2015, 2020, 2025)) 
abline(v = 2023, lty = 2, col = "grey") 
legend("topleft", 
legend = c("AR(1)", "ARIMA(1,1,0)", "ARIMA(2,1,0)", "ARIMA(1,1,1)"), 
col = c("blue", "red", "green", "purple"), lty = c(2,3,4,5), lwd = 2)


# REESTIMAR MODELOS

#AR(1)
y <- as.numeric(ipv_ts_trim)
y_lag <- c(NA, y[-length(y)])

datos <- na.omit(data.frame(y = y, y_lag = y_lag))

ar1_lm <- lm(y ~ y_lag, data = datos)

summary(ar1_lm)


#ARIMA(1,1,0)
coef_110 <- arima110_trim$coef
se_110 <- sqrt(diag(arima110_trim$var.coef))

t_110 <- coef_110 / se_110
p_110 <- 2 * (1 - pnorm(abs(t_110)))

data.frame(
  Modelo = "ARIMA(1,1,0)",
  Coeficiente = coef_110,
  SE = se_110,
  t_value = t_110,
  p_value = p_110)


#ARIMA(2,1,0)
coef_210 <- arima210_trim$coef
se_210 <- sqrt(diag(arima210_trim$var.coef))

t_210 <- coef_210 / se_210
p_210 <- 2 * (1 - pnorm(abs(t_210)))

data.frame(
  Modelo = "ARIMA(2,1,0)",
  Coeficiente = coef_210,
  SE = se_210,
  t_value = t_210,
  p_value = p_210)


#ARIMA(1,1,1)
coef_111 <- arima111_trim$coef
se_111 <- sqrt(diag(arima111_trim$var.coef))

t_111 <- coef_111 / se_111
p_111 <- 2 * (1 - pnorm(abs(t_111)))

data.frame(
  Modelo = "ARIMA(1,1,1)",
  Coeficiente = coef_111,
  SE = se_111,
  t_value = t_111,
  p_value = p_111)


#CRITERIOS INFO Y OOSMSE
n <- length(na.omit(ipv_ts_trim))

sigma2 <- ar1_trim$var.pred
k <- 2

AIC_ar1 <- n * log(sigma2) + 2 * k
BIC_ar1 <- n * log(sigma2) + log(n) * k

data.frame(
  Modelo = c("AR(1)", "ARIMA(1,1,0)", "ARIMA(2,1,0)", "ARIMA(1,1,1)"),
  AIC = c(
    AIC_ar1,
    AIC(arima110_trim),
    AIC(arima210_trim),
    AIC(arima111_trim)),
  BIC = c(
    BIC_ar1,
    BIC(arima110_trim),
    BIC(arima210_trim),
    BIC(arima111_trim)))

# OOSMSE

h <- 16

n <- length(ipv_ts_trim)

train <- window(ipv_ts_trim, end = time(ipv_ts_trim)[n - h])
test  <- window(ipv_ts_trim, start = time(ipv_ts_trim)[n - h + 1])


ar1_tr <- ar(train, method = "ols", aic = FALSE, order.max = 1)

arima110_tr <- Arima(train, order = c(1,1,0), method = "ML")
arima210_tr <- Arima(train, order = c(2,1,0), method = "ML")
arima111_tr <- Arima(train, order = c(1,1,1), method = "ML")


fc_ar1 <- forecast(ar1_tr, h = h)
fc_110 <- forecast(arima110_tr, h = h)
fc_210 <- forecast(arima210_tr, h = h)
fc_111 <- forecast(arima111_tr, h = h)


mse_ar1 <- mean((test - fc_ar1$mean)^2)
mse_110 <- mean((test - fc_110$mean)^2)
mse_210 <- mean((test - fc_210$mean)^2)
mse_111 <- mean((test - fc_111$mean)^2)


oosmse_table <- data.frame(
  Modelo = c("AR(1)", "ARIMA(1,1,0)", "ARIMA(2,1,0)", "ARIMA(1,1,1)"),
  OOSMSE = c(mse_ar1, mse_110, mse_210, mse_111))

print(oosmse_table)

# GRAFICO DE PREDICCION DE IPV GENERAL COMPARANDOLO CON SENSIBILIDAD, ESCOGIENDO EL MEJOR MODELO ARIMA(1,1,0)

ipv_ts <- ts(IPV$TOT.General,
             start = c(2008,1),
             frequency = 4)

ipv_ts_trim <- window(ipv_ts, end = c(2023,3))

h <- 12

arima110 <- Arima(ipv_ts, order = c(1,1,0), method = "ML")
fc_110 <- forecast(arima110, h = h)

arima110S <- Arima(ipv_ts_trim, order = c(1,1,0), method = "ML")
fc_110S <- forecast(arima110S, h = h)

ylim_range <- range(c(ipv_ts,
                      fc_110$mean,
                      fc_110S$mean))

plot(ipv_ts,
     type = "l",
     xlim = c(2008, 2026),
     xaxt = "n",
     ylim = ylim_range,
     xlab = "Año",
     ylab = "Valor")

lines(fc_110$mean,
      col = "orange",
      lty = 2,
      lwd = 2)

lines(fc_110S$mean,
      col = "blue",
      lty = 2,
      lwd = 2)

axis(1, at = c(2010, 2015, 2020, 2025))
abline(v = 2023, lty = 2, col = "grey")

# WAGE

wage_ts <- ts(wage$TOT.Todas,
              start = 2008,
              frequency = 1)

wage_ts_trim <- window(wage_ts, end = 2023)

h <- 3

arima110 <- Arima(wage_ts,
                  order = c(1,1,0),
                  method = "ML")

fc_110 <- forecast(arima110, h = h)

arima110S <- Arima(wage_ts_trim,
                   order = c(1,1,0),
                   method = "ML")

fc_110S <- forecast(arima110S, h = h)

ylim_range <- range(c(wage_ts,
                      fc_110$mean,
                      fc_110S$mean))

plot(wage_ts,
     type = "l",
     xlim = c(2008, 2026),
     xaxt = "n",
     ylim = ylim_range,
     xlab = "Año",
     ylab = "Valor")

lines(fc_110$mean,
      col = "orange",
      lty = 2,
      lwd = 2)

lines(fc_110S$mean,
      col = "blue",
      lty = 2,
      lwd = 2)

axis(1, at = seq(2008, 2026, by = 2))

abline(v = 2023, lty = 2, col = "grey")

legend("topleft",
       legend = c("Serie observada",
                  "ARIMA(1,1,0) muestra completa",
                  "ARIMA(1,1,0) hasta 2023"),
       col = c("black", "orange", "blue"),
       lty = c(1,2,2),
       lwd = c(1,2,2),
       bty = "n")

#################################
##  BOOTSTRAP DE LOS RESIDUOS  ##
#################################

####### WAGE - AR(1)

library(forecast)

# PASO 1 - ESTIMAR B0 Y B1
y <- wage$TOT.Todas

Y <- y[-1]
X <- y[-length(y)]

modelo <- lm(Y ~ X)

alpha_hat <- coef(modelo)[1]
phi_hat   <- coef(modelo)[2]

# PASO 2 - RESUDUOS

u_hat <- residuals(modelo)

B <- 1000
phi_boot <- numeric(B)

# PASO 3 - BUCLE Y REGRESION INCORPORANDO LOS RESIDUOS

for(r in 1:B){
  
  # PASO 3.1
  
  X_r <- sample(X,
                size = length(X),
                replace = TRUE)
  
  # PASO 3.2
  
  u_r <- sample(u_hat,
                size = length(u_hat),
                replace = TRUE)
  
  # PASO 3.3
  
  Y_r <- alpha_hat + phi_hat*X_r + u_r
  
  # PASO 3.4
  
  modelo_r <- lm(Y_r ~ X_r)
  
  phi_boot[r] <- coef(modelo_r)[2]}

# PASO 4 - CALCULO VARIANZA

var(phi_boot)
sd(phi_boot)
coef(modelo)

hist(phi_boot,
     breaks = 30,
     main = "Bootstrap del parámetro AR(1)",
     xlab = expression(hat(phi)))
abline(v = phi_hat, lwd = 2)

####### IPV - AR(1)

# PASO 1 - ESTIMAR B0 Y B1
y <- IPV$TOT.General

Y <- y[-1]
X <- y[-length(y)]

modelo <- lm(Y ~ X)

alpha_hat <- coef(modelo)[1]
phi_hat   <- coef(modelo)[2]

# PASO 2 - RESIDUOS

u_hat <- residuals(modelo)

B <- 1000
phi_boot <- numeric(B)

# PASO 3 - BUCLE Y REGRESION INCORPORANDO LOS RESIDUOS

for(r in 1:B){
  
  # PASO 3.1
  X_r <- sample(
    X,
    size = length(X),
    replace = TRUE)
  
  # PASO 3.2
  u_r <- sample(
    u_hat,
    size = length(u_hat),
    replace = TRUE)
  
  # PASO 3.3
  Y_r <- alpha_hat + phi_hat * X_r + u_r
  
  # PASO 3.4
  modelo_r <- lm(Y_r ~ X_r)
  
  phi_boot[r] <- coef(modelo_r)[2]}

# PASO 4 - RESULTADOS

var(phi_boot)
sd(phi_boot)
coef(modelo)

quantile(phi_boot, c(0.025, 0.975))

hist(
  phi_boot,
  breaks = 30,
  main = "Bootstrap del parámetro AR(1) - IPV Total General",
  xlab = expression(hat(phi)))

abline(v = phi_hat, lwd = 2)

####### WAGE - ARIMA(1,1,0)

# PASO 1 - ESTIMAR ARIMA(1,1,0)

y <- wage$TOT.Todas

modelo <- Arima(y,
                order = c(1,1,0),
                method = "ML")

phi_hat <- coef(modelo)["ar1"]

# PASO 2 - RESIDUOS

u_hat <- residuals(modelo)

B <- 1000
phi_boot <- numeric(B)

n <- length(y)

# PASO 3 - BOOTSTRAP

for(r in 1:B){
  
  u_r <- sample(u_hat,
                size = length(u_hat),
                replace = TRUE)
  
  y_r <- simulate(
    modelo,
    nsim = n,
    bootstrap = TRUE,
    innov = u_r)
  
  modelo_r <- tryCatch(
    Arima(y_r,
          order = c(1,1,0),
          method = "ML"),
    error = function(e) NULL)
  
  if(!is.null(modelo_r)){
    phi_boot[r] <- coef(modelo_r)["ar1"]} else {
    phi_boot[r] <- NA}}

phi_boot <- na.omit(phi_boot)

# PASO 4 - RESULTADOS

var(phi_boot)
sd(phi_boot)

coef(modelo)

quantile(phi_boot, c(0.025, 0.975))

hist(phi_boot,
     breaks = 30,
     main = "Bootstrap del parámetro AR(1) - wage TOT.Todas (ARIMA 1,1,0)",
     xlab = expression(hat(phi)))

abline(v = phi_hat,
       lwd = 2)

####### IPV - ARIMA(1,1,0)

# PASO 1 - ESTIMAR ARIMA(1,1,0)

y <- IPV$TOT.General

modelo <- Arima(y,
                order = c(1,1,0),
                method = "ML")

phi_hat <- coef(modelo)["ar1"]

# PASO 2 - RESIDUOS

u_hat <- residuals(modelo)

B <- 1000
phi_boot <- numeric(B)

n <- length(y)

# PASO 3 - BOOTSTRAP

for(r in 1:B){
  u_r <- sample(u_hat,
                size = length(u_hat),
                replace = TRUE)

  y_r <- simulate(
    modelo,
    nsim = n,
    bootstrap = TRUE,
    innov = u_r)
  
  modelo_r <- tryCatch(
    Arima(y_r,
          order = c(1,1,0),
          method = "ML"),
    error = function(e) NULL)
  
  if(!is.null(modelo_r)){
    phi_boot[r] <- coef(modelo_r)["ar1"]} else {
    phi_boot[r] <- NA}}

phi_boot <- na.omit(phi_boot)

# PASO 4 - RESULTADOS

var(phi_boot)
sd(phi_boot)

coef(modelo)

quantile(phi_boot, c(0.025, 0.975))

hist(phi_boot,
     breaks = 30,
     main = "Bootstrap del parámetro AR(1) - ARIMA(1,1,0)",
     xlab = expression(hat(phi)))

abline(v = phi_hat,
       lwd = 2)

####### WAGE - ARIMA(2,1,0)

# PASO 1 - ESTIMAR MODELO
y <- wage$TOT.Todas

modelo <- Arima(y,
                order = c(2,1,0),
                method = "ML")

coefs <- coef(modelo)
phi1_hat <- coefs["ar1"]
phi2_hat <- coefs["ar2"]

# RESIDUOS
u_hat <- residuals(modelo)

B <- 1000
phi1_boot <- numeric(B)
phi2_boot <- numeric(B)

n <- length(y)

# PASO 2 - BOOTSTRAP
for(r in 1:B){
  
  u_r <- sample(u_hat,
                size = length(u_hat),
                replace = TRUE)
  
  y_r <- simulate(modelo,
                  nsim = n,
                  bootstrap = TRUE,
                  innov = u_r)
  
  modelo_r <- tryCatch(
    Arima(y_r,
          order = c(2,1,0),
          method = "ML"),
    error = function(e) NULL)
  
  if(!is.null(modelo_r)){
    cf <- coef(modelo_r)
    
    phi1_boot[r] <- ifelse("ar1" %in% names(cf), cf["ar1"], NA)
    phi2_boot[r] <- ifelse("ar2" %in% names(cf), cf["ar2"], NA)} else {
    phi1_boot[r] <- NA
    phi2_boot[r] <- NA}}

phi1_boot <- na.omit(phi1_boot)
phi2_boot <- na.omit(phi2_boot)

# PASO 3 - RESULTADOS

cat("AR1:\n")
print(c(
  Estimacion = phi1_hat,
  SD_boot = sd(phi1_boot),
  IC_2.5 = quantile(phi1_boot, 0.025),
  IC_97.5 = quantile(phi1_boot, 0.975)))

cat("\nAR2:\n")
print(c(
  Estimacion = phi2_hat,
  SD_boot = sd(phi2_boot),
  IC_2.5 = quantile(phi2_boot, 0.025),
  IC_97.5 = quantile(phi2_boot, 0.975)))

# HISTOGRAMAS

hist(phi1_boot,
     breaks = 30,
     main = "Bootstrap AR1 - wage TOT.Todas",
     xlab = expression(hat(phi[1])))

abline(v = phi1_hat, lwd = 2)

hist(phi2_boot,
     breaks = 30,
     main = "Bootstrap AR2 - wage TOT.Todas",
     xlab = expression(hat(phi[2])))

abline(v = phi2_hat, lwd = 2)

####### IPV - ARIMA(2,1,0)

# PASO 1 - ESTIMAR MODELO
y <- IPV$TOT.General

modelo <- Arima(y,
                order = c(2,1,0),
                method = "ML")

coefs <- coef(modelo)
phi1_hat <- coefs["ar1"]
phi2_hat <- coefs["ar2"]

# RESIDUOS
u_hat <- residuals(modelo)

B <- 1000
phi1_boot <- numeric(B)
phi2_boot <- numeric(B)

n <- length(y)

# PASO 2 - BOOTSTRAP
for(r in 1:B){

  u_r <- sample(u_hat,
                size = length(u_hat),
                replace = TRUE)

  y_r <- simulate(modelo,
                  nsim = n,
                  bootstrap = TRUE,
                  innov = u_r)

  modelo_r <- tryCatch(
    Arima(y_r,
          order = c(2,1,0),
          method = "ML"),
    error = function(e) NULL)
  
  if(!is.null(modelo_r)){
    cf <- coef(modelo_r)
    
    phi1_boot[r] <- ifelse("ar1" %in% names(cf), cf["ar1"], NA)
    phi2_boot[r] <- ifelse("ar2" %in% names(cf), cf["ar2"], NA)} else {
    phi1_boot[r] <- NA
    phi2_boot[r] <- NA}}

phi1_boot <- na.omit(phi1_boot)
phi2_boot <- na.omit(phi2_boot)

# PASO 3 - RESULTADOS
cat("AR1:\n")
print(c(
  Estimacion = phi1_hat,
  SD_boot = sd(phi1_boot),
  IC_2.5 = quantile(phi1_boot, 0.025),
  IC_97.5 = quantile(phi1_boot, 0.975)))

cat("\nAR2:\n")
print(c(
  Estimacion = phi2_hat,
  SD_boot = sd(phi2_boot),
  IC_2.5 = quantile(phi2_boot, 0.025),
  IC_97.5 = quantile(phi2_boot, 0.975)))

# HISTOGRAMAS
hist(phi1_boot,
     breaks = 30,
     main = "Bootstrap AR1 - ARIMA(2,1,0)",
     xlab = expression(hat(phi[1])))

abline(v = phi1_hat, lwd = 2)

hist(phi2_boot,
     breaks = 30,
     main = "Bootstrap AR2 - ARIMA(2,1,0)",
     xlab = expression(hat(phi[2])))

abline(v = phi2_hat, lwd = 2)

##############################################
####### WAGE - ARIMA(1,1,1) ##################
##############################################

y <- wage$TOT.Todas

modelo <- Arima(y,
                order = c(1,1,1),
                method = "ML")

phi_hat   <- coef(modelo)["ar1"]
theta_hat <- coef(modelo)["ma1"]

B <- 1000
phi_boot <- numeric(B)
theta_boot <- numeric(B)

n <- length(y)

set.seed(123)

for(r in 1:B){
  
  y_r <- simulate(modelo, nsim = n)
  
  modelo_r <- tryCatch(
    Arima(y_r,
          order = c(1,1,1),
          method = "ML"),
    error = function(e) NULL,
    warning = function(w) NULL)
  
  if(!is.null(modelo_r) && !is.null(coef(modelo_r))){
    
    cf <- coef(modelo_r)
    
    phi_boot[r]   <- ifelse("ar1" %in% names(cf), cf["ar1"], NA)
    theta_boot[r] <- ifelse("ma1" %in% names(cf), cf["ma1"], NA)
    } else {
    phi_boot[r] <- NA
    theta_boot[r] <- NA}}

phi_boot <- na.omit(phi_boot)
theta_boot <- na.omit(theta_boot)

cat("WAGE AR1:\n")
print(c(
  Est = phi_hat,
  SD = sd(phi_boot),
  IC_2.5 = quantile(phi_boot, 0.025),
  IC_97.5 = quantile(phi_boot, 0.975)))

cat("\nWAGE MA1:\n")
print(c(
  Est = theta_hat,
  SD = sd(theta_boot),
  IC_2.5 = quantile(theta_boot, 0.025),
  IC_97.5 = quantile(theta_boot, 0.975)))

hist(phi_boot, breaks = 30,
     main = "WAGE AR1 bootstrap",
     xlab = "AR1")
abline(v = phi_hat, lwd = 2)

hist(theta_boot, breaks = 30,
     main = "WAGE MA1 bootstrap",
     xlab = "MA1")
abline(v = theta_hat, lwd = 2)

##############################################
####### IPV - ARIMA(1,1,1) ###################
##############################################

y <- IPV$TOT.General

modelo <- Arima(y,
                order = c(1,1,1),
                method = "ML")

phi_hat   <- coef(modelo)["ar1"]
theta_hat <- coef(modelo)["ma1"]

B <- 1000
phi_boot <- numeric(B)
theta_boot <- numeric(B)

n <- length(y)

set.seed(123)

for(r in 1:B){
  
  y_r <- simulate(modelo, nsim = n)
  
  modelo_r <- tryCatch(
    Arima(y_r,
          order = c(1,1,1),
          method = "ML"),
    error = function(e) NULL,
    warning = function(w) NULL)
  
  if(!is.null(modelo_r)){
    
    cf <- coef(modelo_r)
    
    phi_boot[r]   <- ifelse("ar1" %in% names(cf), cf["ar1"], NA)
    theta_boot[r] <- ifelse("ma1" %in% names(cf), cf["ma1"], NA)} else {
    
    phi_boot[r] <- NA
    theta_boot[r] <- NA}}


phi_boot <- phi_boot[is.finite(phi_boot)]
theta_boot <- theta_boot[is.finite(theta_boot)]

# =========================
# RESULTADOS
# =========================

cat("IPV AR1:\n")
print(c(
  Est = phi_hat,
  SD = sd(phi_boot),
  IC_2.5 = quantile(phi_boot, 0.025),
  IC_97.5 = quantile(phi_boot, 0.975)))

cat("\nIPV MA1:\n")
print(c(
  Est = theta_hat,
  SD = sd(theta_boot),
  IC_2.5 = quantile(theta_boot, 0.025),
  IC_97.5 = quantile(theta_boot, 0.975)))

# =========================
# HISTOGRAMAS
# =========================

hist(phi_boot,
     breaks = 30,
     main = "IPV AR1 bootstrap (corregido)",
     xlab = expression(hat(phi[1])))

abline(v = phi_hat, lwd = 2)

hist(theta_boot,
     breaks = 30,
     main = "IPV MA1 bootstrap (corregido)",
     xlab = expression(hat(theta[1])))

abline(v = theta_hat, lwd = 2)

####### VAR 1 WAGE E IPV INTEGRADOS

library(vars)

# ======
# DATOS
# =====
d_tg <- na.omit(cbind(
  diff(wage$TOT.Todas),
  diff(IPV_anual$TOT.General)))

colnames(d_tg) <- c("d_wage_total", "d_ipv_general")

# =========================
# ESTIMACIÓN VAR(1)
# =========================

v_tg <- VAR(d_tg, p = 1, type = "const")
summary(v_tg)

phi_hat <- summary(v_tg)$varresult$d_ipv_general$coefficients[
  "d_ipv_general.l1", "Estimate"]

# ========
# RESIDUOS
# ========

u_hat <- residuals(v_tg)

# =========
# BOOTSTRAP
# =========

B <- 1000
phi_boot <- numeric(B)
n <- nrow(d_tg)

fitted_vals <- fitted(v_tg)

for(r in 1:B){

  u_r <- u_hat[sample(1:nrow(u_hat),
                      size = nrow(u_hat),
                      replace = TRUE), ]

  y_r <- fitted_vals + u_r
  
  d_tg_r <- as.data.frame(y_r)
  colnames(d_tg_r) <- c("d_wage_total", "d_ipv_general")

  v_r <- tryCatch(
    VAR(d_tg_r, p = 1, type = "const"),
    error = function(e) NULL)
  
  if(!is.null(v_r)){
    phi_boot[r] <- summary(v_r)$varresult$d_ipv_general$coefficients[
      "d_ipv_general.l1", "Estimate"]} else {
    phi_boot[r] <- NA}}

phi_boot <- na.omit(phi_boot)

# =========================
# RESULTADOS
# =========================

var(phi_boot)
sd(phi_boot)

coef(v_tg)

quantile(phi_boot, c(0.025, 0.975))

hist(phi_boot,
     breaks = 30,
     main = "Bootstrap VAR(1) - d_ipv_general.l1",
     xlab = expression(hat(phi)))

abline(v = phi_hat, lwd = 2)

####################################
### -- PREDICCIONES PARA WAGE -- ###
####################################

# SERIE
y <- wage$TOT.Todas

# MODELO ORIGINAL
modelo <- Arima(y,
                order = c(1,1,0),
                method = "ML")

# PREDICCION DEL MODELO ORIGINAL
fc <- forecast(modelo,
               h = 3)

pred <- as.numeric(fc$mean)

# RESIDUOS
u_hat <- residuals(modelo)

B <- 1000
n <- length(y)

# MATRIZ PARA GUARDAR PREDICCIONES BOOTSTRAP
pred_boot <- matrix(NA,
                    nrow = B,
                    ncol = 3)

#############
# BOOTSTRAP
#############

for(r in 1:B){
  
  u_r <- sample(u_hat,
                size = length(u_hat),
                replace = TRUE)
  
  y_r <- simulate(modelo,
                  nsim = n,
                  bootstrap = TRUE,
                  innov = u_r)
  
  modelo_r <- tryCatch(
    Arima(y_r,
          order = c(1,1,0),
          method = "ML"),
    error = function(e) NULL)
  
  if(!is.null(modelo_r)){
    
    fc_r <- forecast(modelo_r,
                     h = 3)
    
    pred_boot[r, ] <- as.numeric(fc_r$mean)}}

# ELIMINAR NA
pred_boot <- pred_boot[complete.cases(pred_boot), ]

######################
# BANDAS BOOTSTRAP 95%
######################

LI95 <- apply(pred_boot,
              2,
              quantile,
              probs = 0.025)

LS95 <- apply(pred_boot,
              2,
              quantile,
              probs = 0.975)

#########
# TABLA #
#########

library(openxlsx)

sd_boot <- apply(pred_boot, 2, sd)

tabla_pred <- data.frame(
  Horizonte = 1:length(pred),
  Valor_Predicho = pred,
  SD = sd_boot,
  LI95 = LI95,
  LS95 = LS95)

tabla_pred

write.xlsx(
  tabla_pred,
  file = "tabla_predicciones_wage_bootstrap.xlsx",
  rowNames = FALSE)

###########
# GRAFICO #
###########

y <- ts(y,
        start = 2008,
        frequency = 1)

modelo <- Arima(y,
                order = c(1,1,0),
                method = "ML")

fc <- forecast(modelo,
               h = 3)

plot(fc,
     xlab = "Año",
     ylab = "Euros")

plot(fc,
     xlab = "Año",
     ylab = "Euros",
     main = "Predicción IPV General")

lines(fc$mean,
      lwd = 2)

points(fc$mean,
       pch = 16,
       cex = 1.1)

###########################################
### -- PREDICCIONES PARA IPV GENERAL -- ###
###########################################

# SERIE
y <- IPV$TOT.General

# SERIE TEMPORAL (TRIMESTRAL)
y <- ts(y,
        start = c(2008,1),
        frequency = 4)

#########################
# MODELO ARIMA
#########################

modelo <- Arima(y,
                order = c(1,1,0),
                method = "ML")


h <- 12 

fc <- forecast(modelo, h = h)
pred <- as.numeric(fc$mean)

#########################
# RESIDUOS
#########################

u_hat <- residuals(modelo)

#########################
# BOOTSTRAP
#########################

B <- 1000
n <- length(y)

pred_boot <- matrix(NA,
                    nrow = B,
                    ncol = h)

for(r in 1:B){
  
  u_r <- sample(u_hat,
                size = length(u_hat),
                replace = TRUE)
  
  y_r <- simulate(modelo,
                  nsim = n,
                  bootstrap = TRUE,
                  innov = u_r)
  
  modelo_r <- tryCatch(
    Arima(y_r,
          order = c(1,1,0),
          method = "ML"),
    error = function(e) NULL)
  
  if(!is.null(modelo_r)){
    
    fc_r <- forecast(modelo_r, h = h)
    pred_boot[r, ] <- as.numeric(fc_r$mean)}}

#########################
# LIMPIEZA BOOTSTRAP
#########################

pred_boot <- pred_boot[complete.cases(pred_boot), ]

#########################
# BANDAS 95%
#########################

LI95 <- apply(pred_boot, 2, quantile, probs = 0.025)
LS95 <- apply(pred_boot, 2, quantile, probs = 0.975)

#########################
# FECHAS (TRIMESTRAL)
#########################

tiempo <- seq(2024, by = 0.25, length.out = h)

#########
# TABLA #
#########

sd_boot <- apply(pred_boot, 2, sd)

tabla_pred <- data.frame(
  Trimestre = tiempo,
  Valor_Predicho = pred,
  SD = sd_boot)

sd_boot <- apply(pred_boot, 2, sd)

tabla_pred <- data.frame(
  Trimestre = tiempo,
  Valor_Predicho = pred,
  SD = sd_boot,
  LI95 = LI95,
  LS95 = LS95)

tabla_pred

install.packages("openxlsx")
library(openxlsx)

write.xlsx(
  tabla_pred,
  file = "tabla_predicciones_bootstrap.xlsx",
  rowNames = FALSE)

##########
# GRAFICO
##########

plot(fc,
     xlab = "Año",
     ylab = "Índice")

fc$level

######################################################




