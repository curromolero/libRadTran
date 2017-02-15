generarFicheros_inputlibRadTran_informacionAERONET <- function(fechaMedida) {
# Función que genera los ficheros de entrada para libRadTran a partir de la información AERONET
  library(knitr) # Genera un documento pdf con los resultados
  library(rmarkdown) # Genera un documento html con los resultados
  
  # Carga las funciones requeridas
  source("R/leerMedidaAERONET.R")
  source("R/organizarDatosAERONET.R")
  source("R/generarFicheroDistribucionTamagnos.R")
  source("R/generarFicheroIndiceRefraccion.R")
  source("R/generarFicheroAOD.R")
  source("R/generarFicheroPerfil.R")
  source("R/generarFicheroINP_Mie.R")
  source("R/generarFicheroINP_libRadTran.R")
  
  # Medida para probar la función, comentar para usar desde otro script que la proporcione
  # fechaMedida <- as.POSIXlt(strptime(c("17/08/2012 06:51"), "%d/%m/%Y %H:%M", tz = "UTC"))
  
  # Crea un directorio para poner los ficheros generados, o utiliza el ya existente, si existe
  dirFiles <- file.path('//cendat2', 'lidar', 'PROACLIM_ForzamientoRadiativo', 'Modelos', 'libRadTran', 'libRadtran-2.0.1', 'data', 'AERONET', fsep = .Platform$file.sep)
  dirMedida <- paste('AERONET', format(fechaMedida, '%Y%m%d_%H%M'), sep = '_')
  dirMedida_absoluto <- file.path(dirFiles, dirMedida)
  ifelse(!dir.exists(dirMedida_absoluto), dir.create(dirMedida_absoluto), FALSE)
  
  # Lectura de datos AERONET, generacion de ficheros refractive_indices & size_distribution
  tipoFichero <- 'DUBOVIKfile'
  nivelDatosAERONET <- 15 # nivel expresado x10 para evitar problemas con el punto: level 1.0 & 1.5 & 2.0 = level 10 & 15 & 20
  datosAERONET_fechaMasCercana <- leerMedidaAERONET(tipoFichero, nivelDatosAERONET, fechaMedida)
  datosAERONET <- organizarDatosAERONET(datosAERONET_fechaMasCercana)
  
  # Copia los ficheros de albedo, que se generan con Matlab para el caso de MODIS y no pueden generarse sobre la marcha
  dirAlbedo <- file.path('//cendat2', 'lidar', 'Satelites', 'MODIS', 'Datos', fsep = .Platform$file.sep)
  list.of.files <- list.files(dirAlbedo, paste0('*', format(fechaMedida, '%Y%m%d_%H%M'), '*'))
  file.copy(file.path(dirAlbedo, list.of.files), dirMedida_absoluto)
  ficherosGenerados <- list(list.of.files)
  
  # Lista con la informacion de la comparacion
  uvspec_input_details <- list (fechaMedida = datosAERONET[[1]]$fechaMedidaAERONET,
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
                                aerosol_perfil = NULL,
                                refrIndex = NULL,
                                numDist_file = NULL)
  
  # nombreFicheroDIST <- generarFicheroDistribucionTamagnos(datosAERONET[[3]], uvspec_input_details$fechaMedida, dirMedida_absoluto)
  # ficherosGenerados[[length(ficherosGenerados)+1]] <- nombreFicheroDIST
  # nombreFicheroREFR <- generarFicheroIndiceRefraccion(datosAERONET[[4]], uvspec_input_details$fechaMedida, dirMedida_absoluto)
  # ficherosGenerados[[length(ficherosGenerados)+1]] <- nombreFicheroREFR
  # nombreFicheroAOD <- generarFicheroAOD(datosAERONET[[6]], uvspec_input_details$fechaMedida, dirMedida_absoluto)
  # ficherosGenerados[[length(ficherosGenerados)+1]] <- nombreFicheroAOD
  
  nombreFicheroLAYER <- generarFicheroLAYER(datosAERONET, nmom = 20, uvspec_input_details$fechaMedida, dirMedida_absoluto)
  # Numero de momentos de Henyey-Greenstein que se calculan: nmom = 10
  ficherosGenerados[[length(ficherosGenerados)+1]] <- nombreFicheroLAYER
  
    # Llamada al modulo Mie para generar fichero de aerosoles con los ficheros refractive_indices & size_distribution con datos AERONET
  # Lista con la informacion de la comparacion
  # mie_input_details <- list (fechaMedida = uvspec_input_details$fechaMedida,
  #                            codigoMie = 'MIEV0',
  #                            refrIndex_file = nombreFicheroREFR,
  #                            numDist_file = nombreFicheroDIST,
  #                            output_user = 'netcdf')
  # ficheroINP_Mie <- generarFicheroINP_Mie(mie_input_details, dirMedida_absoluto)
  # (textoParaPegarEnCygwin <- paste('(../bin/mie < ', ficheroINP_Mie, ') >&', 'verbose.txt', sep = ' '))
  # Llamada al modulo uvspec para calcular la transferencia radiativa con y sin aerosoles
  
  nombreFicheroPerfil <- generarFicheroPerfil(nombreFicheroLAYER, uvspec_input_details$fechaMedida, dirMedida_absoluto, dirMedida)
  ficherosGenerados[[length(ficherosGenerados)+1]] <- nombreFicheroPerfil
  
  # Sin aerosoles
  ficheroINP_SinAerosoles <- generarFicheroINP_libRadTran(uvspec_input_details, 'BOA', dirMedida_absoluto, dirMedida)
  ficheroINP_SinAerosoles <- generarFicheroINP_libRadTran(uvspec_input_details, 'TOA', dirMedida_absoluto, dirMedida)
  
  # Con aerosoles
  uvspec_input_details$aerosols <- nombreFicheroPerfil
  # uvspec_input_details$refrIndex <- c(datosAERONET[[4]]$REFR[1], datosAERONET[[4]]$REFI[1]) # REFR & REFI para 442 nm
  # uvspec_input_details$numDist_file <- nombreFicheroDIST
  # uvspec_input_details$aerosol_angstrom <- c(datosAERONET[[1]]$`alpha440-870`, datosAERONET[[1]]$`tau440(measured)`*(440/1000)^datosAERONET[[1]]$`alpha440-870`)
  # libRadTran define Tau = Beta*lambda^-alfa, pero Angstrom es Tau(lambda)/Tau(Lambda0) = (lambda/Lambda0)^-alfa, luego Beta = Tau(Lambda0)*Lambda0^alfa y se calcula para 440nm
  # 0.44 micras en realidad, porque aerosol_angstrom lo toma en micras (p. 75 del manual)
  
  uvspec_input_details$aerosol_perfil <- nombreFicheroPerfil
  ficheroINP_ConAerosoles <- generarFicheroINP_libRadTran(uvspec_input_details, 'BOA', dirMedida_absoluto, dirMedida) # Si se pone fichero Sin Aerosoles, genera uno equivalente con aerosoles
  ficheroINP_ConAerosoles <- generarFicheroINP_libRadTran(uvspec_input_details, 'TOA', dirMedida_absoluto, dirMedida)
 
  # Guarda los resultados de los ajustes para estudiarlos luego
  # nombreFichero <- paste0('Dia_', format(uvspec_input_details$fechaMedida, '%Y%m%d_%H%M'), 'h.html')
  # render(file.path(getwd(), 'R', 'informeFicherosInputLibRadTran.Rmd', fsep = .Platform$file.sep), output_file = nombreFichero, output_dir = dirMedida_absoluto)
  # 
  return(ficherosGenerados)  
}
