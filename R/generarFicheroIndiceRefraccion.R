generarFicheroIndiceRefraccion <- function(valoresREF_AERONET, fechaMedida, dirMedida) {
  # Interpola los indices de refraccion real e imaginario a todo el rango espectral 200 - 4000 nm
  # Define el nuevo rango espectral para inter/extrapolar
  indicesRef_file <- data.frame(wvl = seq(200, 4000, by = 1))
  # Ajusta linealmente e inter/extrapola
  lin.REFR <- lm(REFR~wvl, data = valoresREF_AERONET)
  indicesRef_file$REFR <- predict(lin.REFR, newdata = indicesRef_file)
  lin.REFI <- lm(REFI~wvl, data = valoresREF_AERONET)
  indicesRef_file$REFI <- predict(lin.REFI, newdata = indicesRef_file)
  # Comprobacion de las interpolaciones
  # plot(indicesRef_file$wvl, indicesRef_file$REFR)
  # points(valoresREF_AERONET$wvl, valoresREF_AERONET$REFR, col = 'red')
  # plot(indicesRef_file$wvl, indicesRef_file$REFI)
  # points(valoresREF_AERONET$wvl, valoresREF_AERONET$REFI, col = 'red')
  # Graba el fichero con los indices de refracci?n en todo el rango espectral
  ficheroINPUT_REF <- paste(paste('indicesRef', format(fechaMedida, '%Y%m%d_%H%M'), sep = '_'), 'dat', sep = '.')
  dirINPUT_REF <- file.path(dirMedida, ficheroINPUT_REF, fsep = .Platform$file.sep)
  write.table(indicesRef_file, file = dirINPUT_REF, append = FALSE, quote = TRUE, sep = " ", eol = "\n", na = "NA",
              dec = ".", row.names = FALSE, col.names = FALSE, qmethod = c("escape", "double"), fileEncoding = "")
  return(ficheroINPUT_REF)
}
  