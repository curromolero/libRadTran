# Test ficheros Mie
library(RNetCDF) # Packete con funciones netCDF
library(lubridate) # Mejores funciones para fechas

  fechaMedida <- as.POSIXlt(strptime(c("28/06/2013 16:58"), "%d/%m/%Y %H:%M", tz = "UTC"))

# Datos AERONET
  # Carga las funciones requeridas
  source("R/leerMedidaAERONET.R")
  source("R/organizarDatosAERONET.R")
  tipoFichero <- 'DUBOVIKfile'
  nivelDatosAERONET <- 15 # nivel expresado x10 para evitar problemas con el punto: level 1.0 & 1.5 & 2.0 = level 10 & 15 & 20
  datosAERONET_fechaMasCercana <- leerMedidaAERONET(tipoFichero, nivelDatosAERONET, fechaMedida)
  datosAERONET <- organizarDatosAERONET(datosAERONET_fechaMasCercana)

# Datos en el fichero Mie
  source('C:/R/libRadTran/R/leerFicheroMie_libRadTran.R')
  dirDatos <- file.path('Y:', 'PROACLIM_ForzamientoRadiativo', 'Modelos', 'libRadTran', 'libRadtran-2.0.1', 'examples')
  ficheroMie <- paste(paste0(paste('Output', 'Mie', format(fechaMedida, '%Y%m%d_%H%M'), sep = '_'), 'mie'), 'CDF', sep = '.')
  datosMie <- leerFicheroMie_libRadTran(file.path(dirDatos, ficheroMie))
  # wavelen reff ntheta theta phase nmom pmom ext ssa gg refre refim rho
  
# Representar valores
  par(mfrow = c(2, 2))  # 2 rows and 2 columns
  plot(datosMie[, 1], datosMie[,9], main="SSA", xlab=expression(paste("wavelength (", mu, "m)")), ylab="SSA")
  points(datosAERONET[[9]]$wvl/1000, datosAERONET[[9]]$SSA, col = "red")
  
  plot(datosMie[, 1], datosMie[,10], main="gg", xlab=expression(paste("wavelength (", mu, "m)")), ylab="gg", ylim=c(0.5, 1))
  points(datosAERONET[[10]]$wvl/1000, datosAERONET[[10]]$ASYM_T, col = "red")
  
  plot(datosMie[, 1], datosMie[,11], main="Ref. index", xlab=expression(paste("wavelength (", mu, "m)")), ylab="REFRE")
  points(datosAERONET[[4]]$wvl/1000, datosAERONET[[4]]$REFR, col = "red")
  
  plot(datosMie[, 1], datosMie[,12], main="Ref. index", xlab=expression(paste("wavelength (", mu, "m)")), ylab="REFIM")
  points(datosAERONET[[4]]$wvl/1000, datosAERONET[[4]]$REFI, col = "red")