generarFicheroINP_Mie <- function(mie_input_details, dirMedida) {
  # genera un fichero para INPUT en el paquete Mie de LibRadTran y va grabando, linea a linea, la informacion de mie_input_details
  library(RNetCDF) # Packete con funciones netCDF
  ficheroINPUT_Mie <- paste(paste('Mie', format(mie_input_details$fechaMedida, '%Y%m%d_%H%M'), sep = '_'), 'dat', sep = '.')
  ficheroINPUT_Completo <- file.path(dirMedida, ficheroINPUT_Mie, fsep = .Platform$file.sep)
  fecha_generado <- paste('# Generado por el script generarFicheroINP_Mie.R, el', format(Sys.time(), '%d/%m/%y, a las %H:%M'), sep = ' ')
  cat('# Pruebas realizadas para ver el funcionamiento de libRadTran con R', fecha_generado,
      sep = '\n', file = ficheroINPUT_Completo, append = FALSE)
  
  # Codigo Mie
  codigoMie <- paste('mie_program', 'MIEV0', sep = ' ') # Opciones: Wiscombe = MIEV0, Bohren & Huffman = BH  
  cat("", "# Select Mie code by Wiscombe", codigoMie, file = ficheroINPUT_Completo, sep = "\n", append = TRUE)
  
  # Specify refractive Index
  refrIndex <- paste('refrac', 'file', mie_input_details$refrIndex_file, sep = ' ')
  cat("", "# Specify refractive Index", refrIndex, file = ficheroINPUT_Completo, sep = "\n", append = TRUE)
  
  # Specify size distribution in Number
  numDist <- paste('size_distribution_file', mie_input_details$numDist_file, sep = ' ')
  cat("", "# Specify size distribution in Number", numDist, file = ficheroINPUT_Completo,sep = "\n", append = TRUE)
  
  cat("", paste("output_user", mie_input_details$output_user, sep = " "), file = ficheroINPUT_Completo, sep = "\n", append = TRUE) 
  
  # Fichero de salida
  nombreFicheroSalida <- paste0("Output_", unlist(strsplit(ficheroINPUT_Mie,  "[.]"))[1])
  cat("", paste("basename",  nombreFicheroSalida, sep = " "), file = ficheroINPUT_Completo, sep = "\n", append = TRUE)
  
  return(ficheroINPUT_Mie)
}


