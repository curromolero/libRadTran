# Comparar las salidas Mie de los diferentes días
# Ejecuta el programa UVSPEC desde dentro de Cygwin (Linux simulator) para todos los ficheros de datos de entrada generados por
# la funcion forzamientoRadiativo_libRadTran_AERONETinput.R y graba el resultado en ficheros output
dirDatos <- file.path('Y:', 'PROACLIM_ForzamientoRadiativo', 'Modelos', 'libRadTran', 'libRadtran-2.0.1', 'data', 'AERONET')
directorios <- list.dirs(path = dirDatos, full.names = FALSE, recursive = TRUE)

for (queDirectorio in 1:length(directorios)) {
  ficherosMie <- list.files(path = file.path(dirDatos, directorios[queDirectorio]), pattern = 'Mie*.*', all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  if (length(ficherosMie) > 0) {
    for (quefichero in 1:length(ficherosMie)) {
      ficheroOutput <- paste0('MieOutputFile_', substr(ficherosMie[quefichero], 5, nchar(ficherosMie[quefichero])))
      textoParaPegarEnCygwin <- paste('(../bin/mie < ', file.path('../data/AERONET', directorios[queDirectorio], ficherosMie[quefichero]),
                                      ' > ', file.path('../data/AERONET', directorios[queDirectorio], ficheroOutput), ') >&', 'verbose.txt', sep = ' ')
      system(textoParaPegarEnCygwin)      
    }
  }
}