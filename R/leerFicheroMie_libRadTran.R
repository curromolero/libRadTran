leerFicheroMie_libRadTran <- function(dirFicheroMie) {
  # Lee los datos del fichero Mie generados por el modulo Mie de libRadTran
  library(RNetCDF) # Packete con funciones netCDF
  nc <- open.nc(dirFicheroMie, write = FALSE)
  details <- file.inq.nc(nc)
  valor <- var.get.nc(nc, 0)
  datosMie <- matrix(data = 0, nrow = length(valor), ncol = details$nvars, byrow = FALSE, dimnames = NULL)
  for (numVar in 0:(details$nvars - 1)) {
    detailsVariable <- var.inq.nc(nc, numVar)
    if (numVar == 0) {
      nombresColumnas <- detailsVariable$name
    } else {
      nombresColumnas <- c(nombresColumnas, detailsVariable$name)
    }
    if (detailsVariable$name %in% c("wavelen", "ext", "ssa", "gg", "refre", "refim", "rho")) {
      datosMie[, numVar + 1] <- var.get.nc(nc, numVar)
    }
  }
  colnames(datosMie) <- nombresColumnas
  for (numAtt in 0:(details$ngatts - 1)) {
    attDetails <- att.inq.nc(nc, "NC_GLOBAL", numAtt)
    attValue <- att.get.nc(nc, "NC_GLOBAL", numAtt)
    attr(datosMie, attDetails$name) <- attValue
  }
  close.nc(nc)
  return(datosMie)
}