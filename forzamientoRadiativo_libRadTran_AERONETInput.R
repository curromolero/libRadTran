# Comparacion de flujos AERONET vs libRadTran
library(reshape2) # Organiza los datos para representarlos con ggplot2
library(ggplot2) # Packete con funciones gr?ficas
# Carga las funciones requeridas
dirFuncion <- file.path("//cendat2", "lidar", "PROACLIM_ForzamientoRadiativo", fsep = .Platform$file.sep)
source(file.path(dirFuncion, "leerMedidaAERONET.R", fsep = .Platform$file.sep))
source(file.path(dirFuncion, "organizarDatosAERONET.R", fsep = .Platform$file.sep))
source(file.path(dirFuncion, "generarFicheroDistribucionTamagnos.R", fsep = .Platform$file.sep))
source(file.path(dirFuncion, "generarFicheroIndiceRefraccion.R", fsep = .Platform$file.sep))
source(file.path(dirFuncion, "generarFicheroINP_Mie.R", fsep = .Platform$file.sep))
source(file.path(dirFuncion, "generarFicheroINP_libRadTran.R", fsep = .Platform$file.sep))

# Directorio con los ficheros que se generar/usan
dirFiles <- file.path('//cendat2', 'lidar', 'PROACLIM_ForzamientoRadiativo', 'Modelos', 'libRadTran', 'libRadtran-2.0.1', 'examples', fsep = .Platform$file.sep)

# Medida seleccionada
fechaMedida <- as.POSIXlt(strptime(c("28/06/2013 16:45"), "%d/%m/%Y %H:%M", tz = "UTC"))
# Lectura de datos AERONET, generacion de ficheros refractive_indices & size_distribution
tipoFichero <- 'DUBOVIKfile'
nivelDatosAERONET <- 15 # nivel expresado x10 para evitar problemas con el punto: level 1.0 & 1.5 & 2.0 = level 10 & 15 & 20
datosAERONET_fechaMasCercana <- leerMedidaAERONET(tipoFichero, nivelDatosAERONET, fechaMedida)
datosAERONET <- organizarDatosAERONET(datosAERONET_fechaMasCercana)


# Lista con la informaci?n de la comparacion
uvspec_input_details <- list (fechaMedida = datosAERONET[[1]]$fechaMedidaAERONET,
                              atmosphere = 'afglus.dat',
                              mol_abs_param = NULL, # 'kato2' o 'reptran fine' 
                              solar_source = 'NewGuey2003.dat',  # 'atlas_plus_modtran',
                              ozone_column = NULL,
                              altitude = datosAERONET[[2]]$`Altitude(BOA)(km)`,
                              day_of_year = floor(datosAERONET[[1]]$Julian_Day),
                              albedo = NULL, 
                              sza = datosAERONET[[1]]$average_solar_zenith_angle_for_flux_calculation,
                              solver = 'disort disort_intcor moments',
                              wavelengthRange = c(199.5, 4000),
                              Slit = NULL, 
                              Splines = NULL,
                              output_process = 'integrate',
                              output_user = 'lambda eglo sza albedo',
                              quiet = TRUE,
                              aerosols = NULL,
                              refrIndex = NULL,
                              numDist_file = NULL)

nombreFicheroDIST <- generarFicheroDistribucionTamagnos(datosAERONET[[3]], uvspec_input_details$fechaMedida)
nombreFicheroREFR <- generarFicheroIndiceRefraccion(datosAERONET[[4]], uvspec_input_details$fechaMedida)
nombreFicheroAlbedo <- generarFicheroAlbedo(datosAERONET[[5]], uvspec_input_details$fechaMedida)
# Llamada al m?dulo Mie para generar fichero de aerosoles con los ficheros refractive_indices & size_distribution con datos AERONET
# Lista con la informaci?n de la comparacion
mie_input_details <- list (fechaMedida = uvspec_input_details$fechaMedida,
                           codigoMie = 'MIEV0',
                           refrIndex_file = nombreFicheroREFR,
                           numDist_file = nombreFicheroDIST,
                           output_user = 'netcdf')
ficheroINP_Mie <- generarFicheroINP_Mie(mie_input_details)
# (textoParaPegarEnCygwin <- paste('(../bin/mie < ', ficheroINP_Mie, ') >&', 'verbose.txt', sep = ' '))
# Llamada al modulo uvspec para calcular la transferencia radiativa con y sin aerosoles

# Sin aerosoles
ficheroINP_SinAerosoles <- generarFicheroINP_libRadTran(uvspec_input_details) # Si no se pone fichero Sin Aerosoles, lo genera
OutFile_SinAerosoles <- paste(paste('Output', format(uvspec_input_details$fechaMedida, '%Y%m%d_%H%M'), 'SinAerosoles', sep = '_'), 'dat', sep = '.')
(textoParaPegarEnCygwin <- paste('(../bin/uvspec < ', ficheroINP_SinAerosoles, ' > ', OutFile_SinAerosoles, ') >&', 'verbose.txt', sep = ' '))
# Llama a libRadTran desde R usando Cygwin. Aun no funciona, pegar en Cygwin para obtener el fichero de salida
# textoLlamada <- paste('cmd.exe', '/c', 'c:\\cygwin64\\bin\\env', '/cygdrive/c/cygwin64/home/u4627/ejecutarSH_SinAerosoles.sh', sep = ' ')
# system(textoLlamada, intern = TRUE)


# Con aerosoles
uvspec_input_details$aerosols <- paste(paste0("Output_", unlist(strsplit(ficheroINP_Mie,  "[.]"))[1], 'mie'), 'CDF', sep = '.')
uvspec_input_details$refrIndex <- c(datosAERONET[[4]]$REFR[1], datosAERONET[[4]]$REFI[1]) # REFR & REFI para 442 nm
uvspec_input_details$numDist_file <- nombreFicheroDIST
ficheroINP_ConAerosoles <- generarFicheroINP_libRadTran(uvspec_input_details) # Si se pone fichero Sin Aerosoles, genera uno equivalente con aerosoles
OutFile_ConAerosoles <- paste(paste('Output', format(uvspec_input_details$fechaMedida, '%Y%m%d_%H%M'), 'ConAerosoles', sep = '_'), 'dat', sep = '.')
(textoParaPegarEnCygwin <- paste('(../bin/uvspec < ', ficheroINP_ConAerosoles, ' > ', OutFile_ConAerosoles, ') >&', 'verbose.txt', sep = ' '))

# Comparaci?n de flujos AERONET vs libRadTran

# Procesar ficheros de salida generados
salidalibRadTran_Sin_Aerosoles <- read.table(file.path(dirFiles, OutFile_SinAerosoles, fsep = .Platform$file.sep))
salidalibRadTran_Con_Aerosoles <- read.table(file.path(dirFiles, OutFile_ConAerosoles, fsep = .Platform$file.sep))

diffConYSinAerosoles <- salidalibRadTran_Con_Aerosoles - salidalibRadTran_Sin_Aerosoles

# Datos de entrada generales
# Output (default: lambda, edir, edn, eup, uavgdir, uavgdn, uavgup
# namely direct, diffuse downward, diffuse upward solar irradiance and actinic fluxes at surface)
# Total downward irradiance eglo = edir + edn. Total mean intensity = uavgdir + uavgdn + uavgup
# type_output <- paste('output_process', 'sum', sep = ' ') # Opciones: 

datosAERONET[[2]]$`DownwardFlux(BOA)`-datosAERONET[[2]]$`UpwardFlux(BOA)`

datosAERONET[[2]]$`DownwardFlux(TOA)`-datosAERONET[[2]]$`UpwardFlux(TOA)`


datosAERONET[[2]]$`DownwardFlux442-T`





# libRadTran output vs wavelength
tidy_salidalibRadTran <- melt(salidalibRadTran_Con_Aerosoles, id = 1)
ggplot(tidy_salidalibRadTran, aes(x = tidy_salidalibRadTran$V1)) +
  geom_point(aes(y = tidy_salidalibRadTran$value, colour = factor(tidy_salidalibRadTran$variable))) +
  geom_line(aes(y = tidy_salidalibRadTran$value, colour = factor(tidy_salidalibRadTran$variable))) + 
  theme(legend.title=element_blank()) + 
  scale_y_continuous(name = "salida libRadTran") +
  ggtitle("salida libRadTran") +
  theme(plot.title = element_text(size = 12, face = "bold", margin = margin(10, 0, 10, 0))) +
  scale_x_continuous(name = "wavelength (nm)")

tidy_salidalibRadTran <- melt(salidalibRadTran_Sin_Aerosoles, id = 1)
ggplot(tidy_salidalibRadTran, aes(x = tidy_salidalibRadTran$V1)) +
  geom_point(aes(y = tidy_salidalibRadTran$value, colour = factor(tidy_salidalibRadTran$variable))) +
  geom_line(aes(y = tidy_salidalibRadTran$value, colour = factor(tidy_salidalibRadTran$variable))) + 
  theme(legend.title=element_blank()) + 
  scale_y_continuous(name = "salida libRadTran") +
  ggtitle("salida libRadTran") +
  theme(plot.title = element_text(size = 12, face = "bold", margin = margin(10, 0, 10, 0))) +
  scale_x_continuous(name = "wavelength (nm)")

combined_salidalibRadTran <- data.frame(salidalibRadTran_Sin_Aerosoles$V1, salidalibRadTran_Sin_Aerosoles$V3, salidalibRadTran_Con_Aerosoles$V3)
tidy_salidalibRadTran <- melt(combined_salidalibRadTran, id = 1)
ggplot(tidy_salidalibRadTran, aes(x = tidy_salidalibRadTran$salidalibRadTran_Sin_Aerosoles.V1)) +
  geom_point(aes(y = tidy_salidalibRadTran$value, colour = factor(tidy_salidalibRadTran$variable))) +
  geom_line(aes(y = tidy_salidalibRadTran$value, colour = factor(tidy_salidalibRadTran$variable))) + 
  theme(legend.title=element_blank()) + 
  scale_y_continuous(name = "salida libRadTran") +
  ggtitle("salida libRadTran") +
  theme(plot.title = element_text(size = 12, face = "bold", margin = margin(10, 0, 10, 0))) +
  scale_x_continuous(name = "wavelength (nm)")