forzamientoRadiativo_libRadTran_AERONETInput <- function(fechaMedida) {
  
  # Comparacion de flujos AERONET vs libRadTran
  library(ggplot2) # Packete con funciones graficas
  library(knitr) # Genera un documento pdf con los resultados
  library(rmarkdown) # Genera un documento html con los resultados
  
  # Carga las funciones requeridas
  source("R/leerMedidaAERONET.R")
  source("R/organizarDatosAERONET.R")
  source("R/generarFicheroDistribucionTamagnos.R")
  source("R/generarFicheroIndiceRefraccion.R")
  source("R/generarFicheroAOD.R")
  source("R/generarFicheroINP_Mie.R")
  source("R/generarFicheroINP_libRadTran.R")
  
  # Medida seleccionada
  # fechaMedida <- as.POSIXlt(strptime(c("11/09/2012 07:14"), "%d/%m/%Y %H:%M", tz = "UTC"))
  
  # Crea un directorio para poner los ficheros generados, o utiliza el ya existente, si existe
  dirFiles <- file.path('//cendat2', 'lidar', 'PROACLIM_ForzamientoRadiativo', 'Modelos', 'libRadTran', 'libRadtran-2.0.1', 'data', 'AERONET', fsep = .Platform$file.sep)
  dirMedida <- file.path(dirFiles, paste('AERONET', format(fechaMedida, '%Y%m%d_%H%M'), sep = '_'))
  ifelse(!dir.exists(dirMedida), dir.create(dirMedida), FALSE)
  
  # Lectura de datos AERONET, generacion de ficheros refractive_indices & size_distribution
  tipoFichero <- 'DUBOVIKfile'
  nivelDatosAERONET <- 15 # nivel expresado x10 para evitar problemas con el punto: level 1.0 & 1.5 & 2.0 = level 10 & 15 & 20
  datosAERONET_fechaMasCercana <- leerMedidaAERONET(tipoFichero, nivelDatosAERONET, fechaMedida)
  datosAERONET <- organizarDatosAERONET(datosAERONET_fechaMasCercana)
  
  # Copia los ficheros de albedo, que se generan con Matlab para el caso de MODIS y no pueden generarse sobre la marcha
  dirAlbedo <- file.path('//cendat2', 'lidar', 'Satelites', 'MODIS', 'Datos', fsep = .Platform$file.sep)
  list.of.files <- list.files(dirAlbedo, paste0('*', format(fechaMedida, '%Y%m%d_%H%M'), '*'))
  file.copy(file.path(dirAlbedo, list.of.files), dirMedida)
  ficherosGenerados <- list(list.of.files)
  
  # Lista con la informacion de la comparacion
  uvspec_input_details <- list (fechaMedida = datosAERONET[[1]]$fechaMedidaAERONET,
                                data_files_path = dirMedida,
                                atmosphere = 'afglmw.dat',
                                mol_abs_param = NULL, # 'kato2' o 'reptran fine' 
                                solar_source = 'NewGuey2003.dat',  # 'atlas_plus_modtran',
                                ozone_column = NULL,
                                altitude = datosAERONET[[2]]$`Altitude(BOA)(km)`,
                                TOA = FALSE,
                                day_of_year = floor(datosAERONET[[1]]$Julian_Day),
                                albedo = list.of.files[1], # 1: albedo AERONET, 2: albedo MODIS
                                sza = datosAERONET[[1]]$average_solar_zenith_angle_for_flux_calculation,
                                solver = 'disort disort_intcor moments',
                                wavelengthRange = c(200.5, 4000),
                                Slit = NULL, 
                                Splines = NULL,
                                output_process = 'integrate',
                                output_user = 'lambda edir edn eup',
                                quiet = TRUE,
                                aerosols = NULL,
                                aerosol_angstrom = NULL,
                                refrIndex = NULL,
                                numDist_file = NULL)
  
  nombreFicheroDIST <- generarFicheroDistribucionTamagnos(datosAERONET[[3]], uvspec_input_details$fechaMedida, dirMedida)
  ficherosGenerados[[length(ficherosGenerados)+1]] <- nombreFicheroDIST
  nombreFicheroREFR <- generarFicheroIndiceRefraccion(datosAERONET[[4]], uvspec_input_details$fechaMedida, dirMedida)
  ficherosGenerados[[length(ficherosGenerados)+1]] <- nombreFicheroREFR
  nombreFicheroAOD <- generarFicheroAOD(datosAERONET[[6]], uvspec_input_details$fechaMedida, dirMedida)
  ficherosGenerados[[length(ficherosGenerados)+1]] <- nombreFicheroAOD
  
  # Llamada al modulo Mie para generar fichero de aerosoles con los ficheros refractive_indices & size_distribution con datos AERONET
  # Lista con la informacion de la comparacion
  mie_input_details <- list (fechaMedida = uvspec_input_details$fechaMedida,
                             codigoMie = 'MIEV0',
                             refrIndex_file = nombreFicheroREFR,
                             numDist_file = nombreFicheroDIST,
                             output_user = 'netcdf')
  ficheroINP_Mie <- generarFicheroINP_Mie(mie_input_details, dirMedida)
  # (textoParaPegarEnCygwin <- paste('(../bin/mie < ', ficheroINP_Mie, ') >&', 'verbose.txt', sep = ' '))
  # Llamada al modulo uvspec para calcular la transferencia radiativa con y sin aerosoles
  
  # Sin aerosoles
  ficheroINP_SinAerosoles <- generarFicheroINP_libRadTran(uvspec_input_details, dirMedida) # Si no se pone fichero Sin Aerosoles, lo genera
  OutFile_SinAerosoles <- paste(paste('Output', format(uvspec_input_details$fechaMedida, '%Y%m%d_%H%M'), 'SinAerosoles', sep = '_'), 'dat', sep = '.')
  (textoParaPegarEnCygwin <- paste('(../bin/uvspec < ', file.path(dirMedida, ficheroINP_SinAerosoles), ' > ', file.path(dirMedida, OutFile_SinAerosoles), ') >&', 'verbose.txt', sep = ' '))
  # Llama a libRadTran desde R usando Cygwin. Aun no funciona, pegar en Cygwin para obtener el fichero de salida
  # textoLlamada <- paste('cmd.exe', '/c', 'c:\\cygwin64\\bin\\env', '/cygdrive/c/cygwin64/home/u4627/ejecutarSH_SinAerosoles.sh', sep = ' ')
  # system(textoLlamada, intern = TRUE)
  
  
  # Con aerosoles
  uvspec_input_details$aerosols <- paste(paste0("Output_", unlist(strsplit(ficheroINP_Mie,  "[.]"))[1], 'mie'), 'CDF', sep = '.')
  # uvspec_input_details$refrIndex <- c(datosAERONET[[4]]$REFR[1], datosAERONET[[4]]$REFI[1]) # REFR & REFI para 442 nm
  # uvspec_input_details$numDist_file <- nombreFicheroDIST
  uvspec_input_details$aerosol_angstrom <- c(datosAERONET[[1]]$`alpha440-870`, datosAERONET[[1]]$`tau440(measured)`*(440/1000)^datosAERONET[[1]]$`alpha440-870`)
  # libRadTran define Tau = Beta*lambda^-alfa, pero Angstrom es Tau(lambda)/Tau(Lambda0) = (lambda/Lambda0)^-alfa, luego Beta = Tau(Lambda0)*Lambda0^alfa y se calcula para 440nm
  # 0.44 micras en realidad, porque aerosol_angstrom lo toma en micras (p. 75 del manual)
  
  ficheroINP_ConAerosoles <- generarFicheroINP_libRadTran(uvspec_input_details, dirMedida) # Si se pone fichero Sin Aerosoles, genera uno equivalente con aerosoles
  OutFile_ConAerosoles <- paste(paste('Output', format(uvspec_input_details$fechaMedida, '%Y%m%d_%H%M'), 'ConAerosoles', sep = '_'), 'dat', sep = '.')
  (textoParaPegarEnCygwin <- paste('(../bin/uvspec < ', file.path(dirMedida, ficheroINP_ConAerosoles), ' > ', file.path(dirMedida, OutFile_ConAerosoles), ') >&', 'verbose.txt', sep = ' '))
  
  # Guarda los resultados de los ajustes para estudiarlos luego
  nombreFichero <- paste0('Dia_', format(uvspec_input_details$fechaMedida, '%Y%m%d_%H%M'), 'h.html')
  render(file.path(getwd(), 'R', 'informeFicherosInputLibRadTran.Rmd', fsep = .Platform$file.sep), output_file = nombreFichero, output_dir = dirMedida)
  
  
  # Comparacion de flujos AERONET vs libRadTran
  
  # Procesar ficheros de salida generados
  # salidalibRadTran_Sin_Aerosoles <- read.table(file.path(dirFiles, OutFile_SinAerosoles, fsep = .Platform$file.sep))
  # salidalibRadTran_Con_Aerosoles <- read.table(file.path(dirFiles, OutFile_ConAerosoles, fsep = .Platform$file.sep))
  # 
  # diffConYSinAerosoles <- salidalibRadTran_Con_Aerosoles - salidalibRadTran_Sin_Aerosoles
  
  # Datos de entrada generales
  # Output (default: lambda, edir, edn, eup, uavgdir, uavgdn, uavgup
  # namely direct, diffuse downward, diffuse upward solar irradiance and actinic fluxes at surface)
  # Total downward irradiance eglo = edir + edn. Total mean intensity = uavgdir + uavgdn + uavgup
  # type_output <- paste('output_process', 'sum', sep = ' ') # Opciones: 
  
  # datosAERONET[[2]]$`DownwardFlux(BOA)`-datosAERONET[[2]]$`UpwardFlux(BOA)`
  # 
  # datosAERONET[[2]]$`DownwardFlux(TOA)`-datosAERONET[[2]]$`UpwardFlux(TOA)`
  # 
  # 
  # datosAERONET[[2]]$`DownwardFlux442-T`
  # 
  # 
  # 
  # 
  # 
  # # libRadTran output vs wavelength
  # tidy_salidalibRadTran <- melt(salidalibRadTran_Con_Aerosoles, id = 1)
  # ggplot(tidy_salidalibRadTran, aes(x = tidy_salidalibRadTran$V1)) +
  #   geom_point(aes(y = tidy_salidalibRadTran$value, colour = factor(tidy_salidalibRadTran$variable))) +
  #   geom_line(aes(y = tidy_salidalibRadTran$value, colour = factor(tidy_salidalibRadTran$variable))) + 
  #   theme(legend.title=element_blank()) + 
  #   scale_y_continuous(name = "salida libRadTran") +
  #   ggtitle("salida libRadTran") +
  #   theme(plot.title = element_text(size = 12, face = "bold", margin = margin(10, 0, 10, 0))) +
  #   scale_x_continuous(name = "wavelength (nm)")
  # 
  # tidy_salidalibRadTran <- melt(salidalibRadTran_Sin_Aerosoles, id = 1)
  # ggplot(tidy_salidalibRadTran, aes(x = tidy_salidalibRadTran$V1)) +
  #   geom_point(aes(y = tidy_salidalibRadTran$value, colour = factor(tidy_salidalibRadTran$variable))) +
  #   geom_line(aes(y = tidy_salidalibRadTran$value, colour = factor(tidy_salidalibRadTran$variable))) + 
  #   theme(legend.title=element_blank()) + 
  #   scale_y_continuous(name = "salida libRadTran") +
  #   ggtitle("salida libRadTran") +
  #   theme(plot.title = element_text(size = 12, face = "bold", margin = margin(10, 0, 10, 0))) +
  #   scale_x_continuous(name = "wavelength (nm)")
  # 
  # combined_salidalibRadTran <- data.frame(salidalibRadTran_Sin_Aerosoles$V1, salidalibRadTran_Sin_Aerosoles$V3, salidalibRadTran_Con_Aerosoles$V3)
  # tidy_salidalibRadTran <- melt(combined_salidalibRadTran, id = 1)
  # ggplot(tidy_salidalibRadTran, aes(x = tidy_salidalibRadTran$salidalibRadTran_Sin_Aerosoles.V1)) +
  #   geom_point(aes(y = tidy_salidalibRadTran$value, colour = factor(tidy_salidalibRadTran$variable))) +
  #   geom_line(aes(y = tidy_salidalibRadTran$value, colour = factor(tidy_salidalibRadTran$variable))) + 
  #   theme(legend.title=element_blank()) + 
  #   scale_y_continuous(name = "salida libRadTran") +
  #   ggtitle("salida libRadTran") +
  #   theme(plot.title = element_text(size = 12, face = "bold", margin = margin(10, 0, 10, 0))) +
  #   scale_x_continuous(name = "wavelength (nm)")

  return(ficherosGenerados)  
}
