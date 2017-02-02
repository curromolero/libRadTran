generarFicheroDistribucionTamagnos <- function(distTamagnosVolumen, fechaMedida, dirMedida) {
  # Graba un fichero con la distribucion numerica calculada a partir de la de volumen proporcionada por AERONET
  # Carga la funcion de ajuste requerida
  source("R/fitAERONETDistribution_bimodal.R")
  # Extrae la informacion de la estructura en lista
  distActual <- data.frame(distTamagnosVolumen$tamagnos, distTamagnosVolumen$dV)
  colnames(distActual) <- c('radius', 'dV/dlnr')
  # Ajusta los modos de la distribucion en volumen y comprueba el resultado
  fitResults <- fitAERONETDistribution_bimodal(distActual)
  nuevoEjeX <- seq(-10, 3, by = 0.01)
  modo1 <- fitResults$C1 * exp(-(nuevoEjeX-fitResults$mean1)**2/(2 * fitResults$sigma1**2))
  modo2 <- fitResults$C2 * exp(-(nuevoEjeX-fitResults$mean2)**2/(2 * fitResults$sigma2**2))
  # plot(log(distActual$radius), distActual$`dV/dlnr`, "b", xlab = "log particle radius, log(r)", ylab = "dV/dlogr")
  # lines(nuevoEjeX, modo1, col = "red")
  # lines(nuevoEjeX, modo2, col = "blue")
  
  # Convierte la distribucion AERONET en volumen dV/dlnr en numero dN(r) / d lnr
  NumberDist <- distActual$`dV/dlnr` / ((4/3)*pi*distActual$radius**3)
  # Convierte los modos ajustados en volumen a dN(r) / d lnr
  NumberDistMode1 <- modo1 / ((4/3)*pi*exp(nuevoEjeX)**3)
  NumberDistMode2 <- modo2 / ((4/3)*pi*exp(nuevoEjeX)**3)
  # Comprueba el resultado final
  # plot(log(distActual$radius), log(NumberDist), "b", xlim=c(-6,2.5), ylim=c(-20, 5), xlab = "log particle radius, log(r)", ylab = "dN/dlogr (log scale)")
  # lines(nuevoEjeX, log(NumberDistMode1), col = "black")
  # lines(nuevoEjeX, log(NumberDistMode2), col = "blue")
  # lines(nuevoEjeX, log(NumberDistMode1 + NumberDistMode2), col = "red")
  # Suma los dos modos y prepara el data.frame para guardar el fichero
  NumberDist_file <- data.frame(exp(nuevoEjeX), NumberDistMode1 + NumberDistMode2)
  # Graba el fichero con nombre 
  ficheroINPUT_Dist <- paste(paste('distTamagnos', format(fechaMedida, '%Y%m%d_%H%M'), sep = '_'), 'dat', sep = '.')
  dirINPUT_Dist <- file.path(dirMedida, ficheroINPUT_Dist, fsep = .Platform$file.sep)
  write.table(NumberDist_file, file = dirINPUT_Dist, append = FALSE, quote = TRUE, sep = " ", eol = "\n", na = "NA",
              dec = ".", row.names = FALSE, col.names = FALSE, qmethod = c("escape", "double"), fileEncoding = "")
  return(ficheroINPUT_Dist)
}