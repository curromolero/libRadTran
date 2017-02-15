generarFicheroPerfil <- function(ficheroINP_Mie, fechaMedida, dirMedida_absoluto, dirMedida) {
  # Genera un fichero de perfil vertical para usar con aerosol_file explicit
  ficheroINPUT_Perfil <- paste('Perfil', format(fechaMedida, '%Y%m%d_%H%M'), sep = '_')
  dirINPUT_Perfil <- file.path(dirMedida_absoluto, ficheroINPUT_Perfil, fsep = .Platform$file.sep)
  
  fecha_generado <- paste('# Generado por el script forzamientoRadiativo_libRadTran.R, el', format(Sys.time(), '%d/%m/%y, a las %H:%M'), sep = ' ')
  file_handle <- file(dirINPUT_Perfil, open="wb")
  cat('# Pruebas realizadas para ver el funcionamiento de libRadTran con R', fecha_generado,
      sep = '\n', file = file_handle, append = FALSE)
  
  capas_atmosfera <- c(0.000, 2.000)
  
  capa_atmosfera <-  paste(formatC(capas_atmosfera[2], format = 'f', digits = 3),  file.path('../examples', 'NULL.LAYER'), sep = ' ')
  cat(capa_atmosfera, file = file_handle, sep = '\n', append = TRUE)
  
  capa_atmosfera <-  paste( formatC(capas_atmosfera[1], format = 'f', digits = 3),  file.path('../data/AERONET', dirMedida, ficheroINP_Mie), sep = ' ')
  cat(capa_atmosfera, file = file_handle, sep = '\n', append = TRUE)
  return(ficheroINPUT_Perfil)
}
  