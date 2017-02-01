leerMedidaAERONET <- function(tipoFichero, nivelDatosAERONET, fechaMedida) {
  # Lee los datos AERONET guardados en ficheros netCDF para la fecha M?S CERCANA a la de medida seleccionada
  library(RNetCDF) # Packete con funciones netCDF
  library(lubridate) # Mejores funciones para fechas
  dirFiles <- file.path('//cendat2', 'lidar', 'CIMEL AEMET', 'AOD_V2',
                        format(fechaMedida, '%Y'), 'InversionsV2', paste0('Level', toString(nivelDatosAERONET)),
                        fsep = .Platform$file.sep)
  nombreFichero <- paste0('CIMEL_AEMET_', tipoFichero, '_', format(fechaMedida, '%y'), '0101_', format(fechaMedida, '%y'),
                          '1231_Madrid_Version', '2', '_Level', toString(nivelDatosAERONET), '.nc')
  dirFichero <- file.path(dirFiles, nombreFichero, fsep = .Platform$file.sep)
  nc <- open.nc(dirFichero, write = FALSE)
  details <- file.inq.nc(nc)
  fechasDatosAERONET_netCDF <- var.get.nc(nc, 1) # La variable 1 es la fecha y hora en el fichero netCDF
  fechasDatosAERONET <- as.POSIXlt((fechasDatosAERONET_netCDF - 719529)*86400, origin = "1970-01-01", tz = "UTC")
  # Averiguar si se midi? ese d?a
  medidasDiaSeleccionado <- which(as.numeric(floor_date(fechasDatosAERONET, "day")) == as.numeric(floor_date(fechaMedida, "day")))
  if (is.null(medidasDiaSeleccionado)) {
    datosAERONET <- NULL
  } else {
    datosAERONET <- list(nombresVariables = '', valores = 0)
    # Seleccionar dia y hora de la medida m?s cercana a la fecha seleccionada
    indiceMedidaMasCercana <-  which(abs(fechasDatosAERONET-fechaMedida) == min(abs(fechasDatosAERONET-fechaMedida)))
    for (numVar in 0:(details$nvars - 1)) {
      detailsVariable <- var.inq.nc(nc, numVar)
      valorEnFecha <- var.get.nc(nc, numVar, start = indiceMedidaMasCercana, count = length(indiceMedidaMasCercana))  
      datosAERONET$nombresVariables[numVar] <- detailsVariable$name
      datosAERONET$valores [numVar] <- valorEnFecha
    }
  }
  for (numAtt in 0:(details$ngatts - 1)) {
    attDetails <- att.inq.nc(nc, "NC_GLOBAL", numAtt)
    attValue <- att.get.nc(nc, "NC_GLOBAL", numAtt)
    attr(datosAERONET, attDetails$name) <- attValue
  }
  close.nc(nc)
  return(datosAERONET)
}