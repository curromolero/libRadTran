generarFicheroAOD <- function(valoresAOD_AERONET, fechaMedida, dirMedida) {
  # Interpola los espesores ópticos a todo el rango espectral 200 - 4000 nm
  # Define el nuevo rango espectral para inter/extrapolar
  AOD_file <- data.frame(wvl = seq(200, 4000, by = 1))
  # Ajusta logaritmicamente e inter/extrapola
  logAOD <- log(valoresAOD_AERONET)
  lin.AOD <- lm(AOD~wvl, data = valoresAOD_AERONET)
  AOD_file$AOD <- predict(lin.AOD, newdata = AOD_file)
  # Comprobacion de las interpolaciones
  # plot(AOD_file$wvl, AOD_file$REFR)
  # points(valoresREF_AERONET$wvl, valoresREF_AERONET$REFR, col = 'red')
  # plot(AOD_file$wvl, AOD_file$REFI)
  # points(valoresREF_AERONET$wvl, valoresREF_AERONET$REFI, col = 'red')
  # Graba el fichero con los indices de refracci?n en todo el rango espectral
  ficheroINPUT_REF <- paste(paste('AOD', format(fechaMedida, '%Y%m%d_%H%M'), sep = '_'), 'dat', sep = '.')
  dirINPUT_REF <- file.path(dirMedida, ficheroINPUT_REF, fsep = .Platform$file.sep)
  write.table(AOD_file, file = dirINPUT_REF, append = FALSE, quote = TRUE, sep = " ", eol = "\n", na = "NA",
              dec = ".", row.names = FALSE, col.names = FALSE, qmethod = c("escape", "double"), fileEncoding = "")
  return(ficheroINPUT_REF)
}
  