# Lee los datos de los ficheros de salida generados por libRadTran y los organiza en un tabla
dirDatos <- file.path('Y:', 'PROACLIM_ForzamientoRadiativo', 'Modelos', 'libRadTran', 'libRadtran-2.0.1', 'data', 'AERONET')
directorios <- list.dirs(path = dirDatos, full.names = FALSE, recursive = TRUE)

resultados <- matrix(data = NA, nrow = 100, ncol = 21)
colnames(resultados) <- c('fecha', 'edir.BOA.SinAerosoles', 'edn.BOA.SinAerosoles', 'ediryedn.BOA.SinAerosoles', 'eup.BOA.SinAerosoles',
                          'edir.TOA.SinAerosoles', 'edn.TOA.SinAerosoles', 'ediryedn.TOA.SinAerosoles', 'eup.TOA.SinAerosoles',
                          'edir.BOA.ConAerosoles', 'edn.BOA.ConAerosoles', 'ediryedn.BOA.ConAerosoles', 'eup.BOA.ConAerosoles',
                          'edir.TOA.ConAerosoles', 'edn.TOA.ConAerosoles', 'ediryedn.TOA.ConAerosoles', 'eup.TOA.ConAerosoles',
                          'RF.BOA', 'RF.TOA', 'RF.BOA.AERONET', 'RF.TOA.AERONET')
# Salidas libRadTran:
# Output (default: lambda, edir, edn, eup, uavgdir, uavgdn, uavgup
# namely direct, diffuse downward, diffuse upward solar irradiance and actinic fluxes at surface)
# Total downward irradiance eglo = edir + edn. Total mean intensity = uavgdir + uavgdn + uavgup
# Seleccionado: lambda, edir, edn, eup
for (queDirectorio in 1:length(directorios)) {
  ficherosOutput <- list.files(path = file.path(dirDatos, directorios[queDirectorio]), pattern = 'Output*.*', all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  if (length(ficherosOutput) > 0) {
    for (quefichero in 1:length(ficherosOutput)) {
      resultados[queDirectorio, 'fecha'] <- as.numeric(as.POSIXlt(strptime(substr(basename(ficherosOutput[quefichero]), 12, 24), "%Y%m%d_%H%M", tz = "UTC"))) 
      datos <- read.table(file.path(dirDatos, directorios[queDirectorio], ficherosOutput[quefichero]))
      if (grepl('BOA', basename(ficherosOutput[quefichero]))) {
        if (grepl('SIN_AEROSOLES', basename(ficherosOutput[quefichero]))) {
          resultados[queDirectorio, 'edir.BOA.SinAerosoles'] <- datos$V2/1000 # Units factor = 1000 cuando se usa NewGuey2003.dat
          resultados[queDirectorio, 'edn.BOA.SinAerosoles'] <- datos$V3/1000
          resultados[queDirectorio, 'ediryedn.BOA.SinAerosoles'] <- (datos$V2 + datos$V3)/1000
          resultados[queDirectorio, 'eup.BOA.SinAerosoles'] <- datos$V4/1000
        } else if (grepl('CON_AEROSOLES', basename(ficherosOutput[quefichero]))) {
          resultados[queDirectorio, 'edir.BOA.ConAerosoles'] <- datos$V2/1000 # Units factor = 1000 cuando se usa NewGuey2003.dat
          resultados[queDirectorio, 'edn.BOA.ConAerosoles'] <- datos$V3/1000
          resultados[queDirectorio, 'ediryedn.BOA.ConAerosoles'] <- (datos$V2 + datos$V3)/1000
          resultados[queDirectorio, 'eup.BOA.ConAerosoles'] <- datos$V4/1000
        } else {
          message(paste('fichero', basename(ficherosOutput[quefichero]), 'no se procesó', sep = ' '),"\r",appendLF=FALSE)
        }
      } else if (grepl('TOA', basename(ficherosOutput[quefichero]))) {
        if (grepl('SIN_AEROSOLES', basename(ficherosOutput[quefichero]))) {
          resultados[queDirectorio, 'edir.TOA.SinAerosoles'] <- datos$V2/1000 # Units factor = 1000 cuando se usa NewGuey2003.dat
          resultados[queDirectorio, 'edn.TOA.SinAerosoles'] <- datos$V3/1000
          resultados[queDirectorio, 'ediryedn.TOA.SinAerosoles'] <- (datos$V2 + datos$V3)/1000
          resultados[queDirectorio, 'eup.TOA.SinAerosoles'] <- datos$V4/1000
        } else if (grepl('CON_AEROSOLES', basename(ficherosOutput[quefichero]))) {
          resultados[queDirectorio, 'edir.TOA.ConAerosoles'] <- datos$V2/1000 # Units factor = 1000 cuando se usa NewGuey2003.dat
          resultados[queDirectorio, 'edn.TOA.ConAerosoles'] <- datos$V3/1000
          resultados[queDirectorio, 'ediryedn.TOA.ConAerosoles'] <- (datos$V2 + datos$V3)/1000
          resultados[queDirectorio, 'eup.TOA.ConAerosoles'] <- datos$V4/1000
        } else {
          message(paste('fichero', basename(ficherosOutput[quefichero]), 'no se procesó', sep = ' '),"\r",appendLF=FALSE)
        }
      } else {
        message(paste('fichero', basename(ficherosOutput[quefichero]), 'no se procesó', sep = ' '),"\r",appendLF=FALSE)
      }
      resultados[queDirectorio, 'RF.TOA'] <- (resultados[queDirectorio, 'ediryedn.TOA.ConAerosoles'] - resultados[queDirectorio, 'eup.TOA.ConAerosoles']) -(resultados[queDirectorio, 'ediryedn.TOA.SinAerosoles'] - resultados[queDirectorio, 'eup.TOA.SinAerosoles'])
      resultados[queDirectorio, 'RF.BOA'] <- (resultados[queDirectorio, 'ediryedn.BOA.ConAerosoles'] - resultados[queDirectorio, 'eup.BOA.ConAerosoles']) -(resultados[queDirectorio, 'ediryedn.BOA.SinAerosoles'] - resultados[queDirectorio, 'eup.BOA.SinAerosoles'])
      resultados[queDirectorio, 'RF.TOA.AERONET'] <- resultados[queDirectorio, 'eup.TOA.SinAerosoles'] - resultados[queDirectorio, 'eup.TOA.ConAerosoles']
      resultados[queDirectorio, 'RF.BOA.AERONET'] <- resultados[queDirectorio, 'ediryedn.BOA.ConAerosoles'] - resultados[queDirectorio, 'ediryedn.BOA.SinAerosoles']
    }
  }
}
write.csv(resultados, file = file.path(dirDatos, "ForzamientoRadiativo_aerosolesStandard.csv"))