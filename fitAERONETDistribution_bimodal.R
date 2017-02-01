fitAERONETDistribution_bimodal <- function(distAERONET, ...) {
  # Devuelve un data.frame con los resultados
  results_df <- data.frame(matrix(data = NA, nrow = 1, ncol = 10))
  row.names(results_df) <- 'BiModal'
  colnames(results_df) <- c(paste0(sort(rep(c('C', 'mean', 'sigma'), 2)), c(1:2)), 'residuals', 'R-squared', 'AIC', 'SW-test')
  # Ajusta distribuciones lognormal a las distribuciones de AERONET
  ejeX <- log(distAERONET[, 1])
  # Try two modes
  midPoint <- floor(nrow(distAERONET)/2)
  radFineMode <- ejeX[which(distAERONET[, 2] == max(distAERONET[1:midPoint, 2]))]
  radCoarseMode <- ejeX[which(distAERONET[, 2] == max(distAERONET[midPoint:nrow(distAERONET), 2]))]
  fitBiModal <- NULL
  try(fitBiModal <- nls(distAERONET[, 2] ~ (C1 * exp(-(ejeX-mean1)**2/(2 * sigma1**2)) + C2 * exp(-(ejeX-mean2)**2/(2 * sigma2**2))),
                                        data=distAERONET, start=list(C1=max(distAERONET[1:midPoint, 2]), mean1=radFineMode, sigma1=0.2,
                                        C2=max(distAERONET[midPoint:nrow(distAERONET), 2]), mean2=radCoarseMode, sigma2=0.2),
                                        algorithm="port", lower=list(C1=0.0, mean1=log(0.05), sigma1=0.0, C2=0.0, mean2=log(0.4), sigma2=0.0)))
  if(!is.null(fitBiModal)) {
    # plot(ejeX, distAERONET[, 2])
    # nuevoEjeX <- seq(min(ejeX), max(ejeX), by = 0.1)
    # lines(nuevoEjeX, predict(fitBiModal, list(ejeX = nuevoEjeX)), lty=2, col = "red")
    # curve(coef(fitBiModal)[1] * exp(-(x-coef(fitBiModal)[2])**2/(2 * coef(fitBiModal)[3]**2)), from = min(nuevoEjeX), to = max(nuevoEjeX), add = TRUE, col = "black")
    # curve(coef(fitBiModal)[4] * exp(-(x-coef(fitBiModal)[5])**2/(2 * coef(fitBiModal)[6]**2)), from = min(nuevoEjeX), to = max(nuevoEjeX), add = TRUE, col = "blue")
    results_df['BiModal', 'C1'] <- coef(fitBiModal)[1]
    results_df['BiModal', 'mean1'] <- coef(fitBiModal)[2]
    results_df['BiModal', 'sigma1'] <- coef(fitBiModal)[3]
    results_df['BiModal', 'C2'] <- coef(fitBiModal)[4]
    results_df['BiModal', 'mean2'] <- coef(fitBiModal)[5]
    results_df['BiModal', 'sigma2'] <- coef(fitBiModal)[6]
    results_df['BiModal', 'residuals'] <- sum(residuals(fitBiModal)^2)
    results_df['BiModal', 'R-squared'] <- 1 - (sum(residuals(fitBiModal)^2)/(sum((distAERONET[, 2] - mean(distAERONET[, 2]))^2)))
    results_df['BiModal', 'AIC'] <- AIC(fitBiModal)
    results_df['BiModal', 'SW-test'] <- shapiro.test(residuals(fitBiModal))
  }
  return(results_df)
}