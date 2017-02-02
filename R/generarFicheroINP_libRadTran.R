generarFicheroINP_libRadTran <- function(uvspec_input_details, dirMedida) {
  # genera un fichero, con detalle _Sin/Con Aerosoles, para input ewn uvspec
  # y va grabando, linea a linea, la informacion de uvspec_input_details
  if (is.character(uvspec_input_details$aerosols)) {
    ficheroINPUT <- paste('InputFile', format(uvspec_input_details$fechaMedida, '%Y%m%d_%H%M'), 'CON_AEROSOLES.dat', sep = '_')
  } else {
    ficheroINPUT <- paste('InputFile', format(uvspec_input_details$fechaMedida, '%Y%m%d_%H%M'), 'SIN_AEROSOLES.dat', sep = '_')
  }
  ficheroINPUT_Completo <- file.path(dirMedida, ficheroINPUT, fsep = .Platform$file.sep)
  fecha_generado <- paste('# Generado por el script forzamientoRadiativo_libRadTran.R, el', format(Sys.time(), '%d/%m/%y, a las %H:%M'), sep = ' ')
  cat('# Pruebas realizadas para ver el funcionamiento de libRadTran con R', fecha_generado,
      sep = '\n', file = ficheroINPUT_Completo, append = FALSE)
  
  # Atmosphere (Temperature, pressure, air density, concentrations of O3, H2O, CO2 & NO2 profiles)
  if (is.character(uvspec_input_details$atmosphere)) {
    molecular_atmosphere <-  paste('atmosphere_file', paste0('../data/atmmod/', uvspec_input_details$atmosphere), sep = ' ')
    # Options: midlatitude summer, midlatitude winter, subarctic summer, subarctic winter, tropical & US-standard (afglus.dat)
    cat('', '# Location of atmospheric profile file', molecular_atmosphere, file = ficheroINPUT_Completo, sep = '\n', append = TRUE)
  }
  
  # molecular absorption parameters
  if (is.character(uvspec_input_details$mol_abs_param)) {
    mol_atm_param_txt <-  paste('mol_abs_param', uvspec_input_details$mol_abs_param, sep = ' ')
    cat('', '# Parametros de absorci?n, para ir de 200 a 4000 nm', mol_atm_param_txt, file = ficheroINPUT_Completo, sep = '\n', append = TRUE)
  }
  
  # Solar source
  if (is.character(uvspec_input_details$solar_source)) {
    solar_source <- paste('source', 'solar', paste0('../data/solar_flux/', uvspec_input_details$solar_source),  sep = ' ') # Opciones: solar, thermal. Not both, if required, run each
    # Geometry (SZA, phi0 (Azimuth), day_of_year, latitude, longitude, time, altitude)
    cat('', '# Location of the extraterrestrial spectrum', solar_source, file = ficheroINPUT_Completo, sep = '\n', append = TRUE)
  }
  
  # Ozone Column
  if (is.character(uvspec_input_details$ozone_column)) {
    ozone_column <-  paste('mol_modify', 'O3', '300.', 'DU', sep = ' ')
    cat('', '# Set ozone column', ozone_column, file = ficheroINPUT_Completo, sep = '\n', append = TRUE)
  }
  
  # Day of year (Julian Day in AERONET)
  if (!is.null(uvspec_input_details$day_of_year)) {
    day_of_year <- paste('day_of_year', uvspec_input_details$day_of_year, sep = ' ')
    cat('', '# Correct for Earth-Sun distance', day_of_year, file = ficheroINPUT_Completo, sep = '\n', append = TRUE)
  }
  
  # Altitude (Km)
  if (!is.null(uvspec_input_details$altitude)) {
    altitude <- paste('altitude', uvspec_input_details$altitude, sep = ' ')
    cat('', '# Altitude of station', altitude, file = ficheroINPUT_Completo, sep = '\n', append = TRUE)
  }
  
  # Albedo
  if (is.character(uvspec_input_details$albedo)) {
    albedo_txt <- paste('albedo', uvspec_input_details$albedo, sep = ' ')
    cat('', '# Surface Albedo', albedo_txt, file = ficheroINPUT_Completo, sep = '\n', append = TRUE)
  }
  
  # SZA
  if (!is.null(uvspec_input_details$sza)) {
    sza_txt <- paste('sza', uvspec_input_details$sza, sep = ' ')
    cat('', '# Solar Zenith Angle', sza_txt, file = ficheroINPUT_Completo, sep = '\n', append = TRUE)
  }
  
  # Solver
  if (is.character(uvspec_input_details$solver)) {
    solver <- paste('rte_solver', uvspec_input_details$solver, sep = ' ')  # Options: sdisort, spsdisort, twostr, polradtran, mystic, sslidar       
    cat('', '# Solver selected', solver, file = ficheroINPUT_Completo, sep = '\n', append = TRUE)
  }
  
  # Spectral range
  if (is.numeric(uvspec_input_details$wavelengthRange)) {
    wvl_init <- uvspec_input_details$wavelengthRange[1] # nm
    wvl_end <- uvspec_input_details$wavelengthRange[2] # nm
    wvl_Range <- paste('wavelength', wvl_init, wvl_end, sep = ' ') 
    cat('', wvl_Range, file = ficheroINPUT_Completo, sep = '\n', append = TRUE)
  }
  
  # Slit
  if (is.character(uvspec_input_details$Slit)) {
    slit <- paste('slit_function_file', '../examples/TRI_SLIT.DAT',  sep = ' ')
    cat('', slit, file = ficheroINPUT_Completo, sep = '\n', append = TRUE) 
  }
  
  # Splines
  if (is.character(uvspec_input_details$Splines)) {
    spline_init <- 300 # nm
    spline_end <- 340 # nm
    spline_Range <- paste('spline', spline_init, spline_end, '1', sep = ' ') 
    cat('', spline_Range, file = ficheroINPUT_Completo, sep = '\n', append = TRUE)
  }
  
  # Aerosoles
  if (is.character(uvspec_input_details$aerosols)) {
    cat('', '# Inicializa a aerosoles por defecto:', 'aerosol_default', file = ficheroINPUT_Completo, sep = '\n', append = TRUE)  
    # aerosols_file <- paste('aerosol_files', paste0('../examples/', uvspec_input_details$aerosols),  sep = ' ')
    # cat('', '# El fichero de aerosoles es:', aerosols_file, file = ficheroINPUT_Completo, sep = '\n', append = TRUE) 
  }
  
  if (length(uvspec_input_details$refrIndex) == 2) {
    # Specify refractive Index
    refrIndex <- paste('aerosol_refrac_index', toString(uvspec_input_details$refrIndex[1]), toString(uvspec_input_details$refrIndex[2]), sep = ' ')
    cat("", "# Specify refractive Index", refrIndex, file = ficheroINPUT_Completo, sep = "\n", append = TRUE)
  }
  
  if (is.character(uvspec_input_details$numDist_file)) {
    # Specify size distribution in Number
    numDist <- paste('aerosol_sizedist_file', paste0('../examples/', mie_input_details$numDist_file), sep = ' ')
    cat("", "# Specify size distribution in Number", numDist, file = ficheroINPUT_Completo,sep = "\n", append = TRUE)
  }
  
  # Type of output: integrate, sum
  if (is.character(uvspec_input_details$output_process)) {
    cat('', paste('output_process', uvspec_input_details$output_process, sep = ' '), file = ficheroINPUT_Completo, sep = '\n', append = TRUE)
  }
  
  # Variables selected to output
  if (is.character(uvspec_input_details$output_user)) {
    cat('', paste('output_user', uvspec_input_details$output_user, sep = ' '), file = ficheroINPUT_Completo, sep = '\n', append = TRUE)
  }
  
  # Quiet or Verbose
  if (uvspec_input_details$quiet) {
    cat('', 'quiet', file = ficheroINPUT_Completo, sep = '\n', append = TRUE)
  } else {
    cat('', 'verbose', file = ficheroINPUT_Completo, sep = '\n', append = TRUE)
  }
  return(ficheroINPUT)
}


