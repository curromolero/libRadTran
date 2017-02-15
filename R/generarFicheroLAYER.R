generarFicheroLAYER <- function(datosAERONET, nmom, fechaMedida, dirMedida) {
  # Genera un fichero representativo de la capa (LAYER) siguiendo el 
  # ejemplo AERO_*.LAYER en el rango espectral 200 -4000 nm, pero solo
  # extendiendo los valores extremos
  data_LAYER <- data.frame(wvl = c(200, 442, 677, 868, 1020, 4000))
  # AOT
  logAOD_df <- data.frame(datosAERONET[[6]]$wvl[which(!is.na(datosAERONET[[6]]$AOT))], log(datosAERONET[[6]]$AOT[which(!is.na(datosAERONET[[6]]$AOT))]))
  colnames(logAOD_df) <- c("wvl","logAOD")
  # Ajusta logaritmicamente e inter/extrapola
  lin.logAOD <- lm(logAOD~wvl, data = logAOD_df)
  logAOD_interpolated <- predict(lin.logAOD, newdata = data_LAYER)
  data_LAYER$AOD <- exp(logAOD_interpolated) # Dividido entre 2 porque es extinción [km^-1], y se asume una capa de 2 km
  # Comprobacion de las interpolaciones
  # plot(data_LAYER$wvl, data_LAYER$AOD)
  # points(datosAERONET[[6]]$wvl, datosAERONET[[6]]$AOT, col = 'red')
  
  # SSA
  data_LAYER$SSA <- c(datosAERONET[[9]]$SSA[1], datosAERONET[[9]]$SSA[1], datosAERONET[[9]]$SSA[2], datosAERONET[[9]]$SSA[3], datosAERONET[[9]]$SSA[4],
                      datosAERONET[[9]]$SSA[4])
  # Henyey-Greenstein phase function moments
  moments <- matrix(data = NA, nrow = length(data_LAYER$wvl), ncol = nmom + 1)
  moments[1, ] <- calculateHenyeyGreensteinMoments(datosAERONET[[10]]$ASYM_T[1], nmom)
  for (queLambda in (1:(length(data_LAYER$wvl)-2))) {
    moments[queLambda + 1, ] <- calculateHenyeyGreensteinMoments(datosAERONET[[10]]$ASYM_T[queLambda], nmom) # nmom momentos HG seleccionados
  }
  moments[length(data_LAYER$wvl), ] <- calculateHenyeyGreensteinMoments(datosAERONET[[10]]$ASYM_T[length(datosAERONET[[10]]$ASYM_T)], nmom)
  data_LAYER$PFMoments <- moments
  
  # Graba el fichero con los valores de data_LAYER en todo el rango espectral
  ficheroINPUT <- paste(paste('AERO', format(fechaMedida, '%Y%m%d_%H%M'), sep = '_'), 'LAYER', sep = '.')
  dirINPUT <- file.path(dirMedida, ficheroINPUT, fsep = .Platform$file.sep)
  write.table(data_LAYER, file = dirINPUT, append = FALSE, quote = TRUE, sep = " ", eol = "\n", na = "NA",
              dec = ".", row.names = FALSE, col.names = FALSE, qmethod = c("escape", "double"), fileEncoding = "")
  return(ficheroINPUT)
}
  