generarFicheroAOD <- function(valoresAOD_AERONET, fechaMedida, dirMedida) {
  # Interpola los espesores ópticos a todo el rango espectral 200 - 4000 nm
  # Define el nuevo rango espectral para inter/extrapolar
  AOD_file <- data.frame(wvl = seq(200, 4000, by = 1))
  logAOD_df <- data.frame(valoresAOD_AERONET$wvl[which(!is.na(valoresAOD_AERONET$AOT))], log(valoresAOD_AERONET$AOT[which(!is.na(valoresAOD_AERONET$AOT))]))
  colnames(logAOD_df) <- c("wvl","logAOD")
  # Ajusta logaritmicamente e inter/extrapola
  lin.logAOD <- lm(logAOD~wvl, data = logAOD_df)
  logAOD_interpolated <- predict(lin.logAOD, newdata = AOD_file)
  AOD_file$AOD <- exp(logAOD_interpolated)
  # Comprobacion de las interpolaciones
  # plot(AOD_file$wvl, AOD_file$AOD)
  # points(valoresAOD_AERONET$wvl, valoresAOD_AERONET$AOT, col = 'red')
  # Graba el fichero con los AOD en todo el rango espectral
  ficheroINPUT_AOD <- paste(paste('AOD', format(fechaMedida, '%Y%m%d_%H%M'), sep = '_'), 'dat', sep = '.')
  dirINPUT_AOD <- file.path(dirMedida, ficheroINPUT_AOD, fsep = .Platform$file.sep)
  write.table(AOD_file, file = dirINPUT_AOD, append = FALSE, quote = TRUE, sep = " ", eol = "\n", na = "NA",
              dec = ".", row.names = FALSE, col.names = FALSE, qmethod = c("escape", "double"), fileEncoding = "")
  return(ficheroINPUT_AOD)
}
  