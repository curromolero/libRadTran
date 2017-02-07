# Bucle para generar los ficheros, organizados en directorios, para los cálculos con libRadTran

library(RNetCDF) # Packete con funciones netCDF
library(lubridate) # Mejores funciones para fechas

# Cargar las funciones requeridas
dirFiles <- file.path('//cendat', 'u4627', 'Mis Documentos', 'R', 'Proyectos', 'EstudioAERONETvsIntrusiones', fsep = .Platform$file.sep)
source(file.path(dirFiles, 'combinarFicherosAERONET.R', fsep = .Platform$file.sep))
source(file.path(dirFiles, 'abrirFicheroAERONET.R', fsep = .Platform$file.sep))

# Abre los sucesivos ficheros anuales y junta todos los datos de un determinado nivel y version
# nivel expresado x10 para evitar problemas con el punto: level 1.0 & 1.5 & 2.0 = level 10 & 15 & 20
# Define la version, nivel y a?os del estudio para analizar los datos
typeFile = 'DUBOVIKfile'
# typeFile = 'PhaseFunctions'
nivelDatosAERONET <- 15 # nivel expresado x10 para evitar problemas con el punto: level 1.0 & 1.5 & 2.0 = level 10 & 15 & 20
versionDatosAERONET <- 2
agnosEstudio <- c(2012:2015)
# Abre los sucesivos ficheros anuales y junta todos los datos de un determinado nivel y version
datosAERONET <- combinarFicherosAERONET(tipoFichero = typeFile, levelAERONET = nivelDatosAERONET, versionAERONET = versionDatosAERONET,
                                        agnos = agnosEstudio)
fechasDatosAERONET <- as.POSIXlt((datosAERONET$Time.hh.mm.ss. - 719529)*86400, origin = "1970-01-01", tz = "UTC")

# Grabar dias relevantes para Arantxa: 17/08/2012, 11/09/2012, 05/11/2012, 13/05/2013 y 28/06/2013
diasRelevantesArantxa <- as.POSIXlt(strptime(c("17/08/2012", "11/09/2012", "5/11/2012", "13/05/2013", "28/06/2013"), "%d/%m/%Y", tz = "UTC"))
for (i in 1:length(diasRelevantesArantxa)) {
  actualIndicesDiasRelevantes <- which(as.numeric(floor_date(fechasDatosAERONET, "day")) == as.numeric(diasRelevantesArantxa[i]))
  if (i == 1) {
    indicesDiasRelevantes <- actualIndicesDiasRelevantes
  } else {
    indicesDiasRelevantes <- c(indicesDiasRelevantes, actualIndicesDiasRelevantes)
  }
}

for (i in 19:length(indicesDiasRelevantes)) {
  forzamientoRadiativo_libRadTran_AERONETInput(fechasDatosAERONET[indicesDiasRelevantes[i]])  
}
