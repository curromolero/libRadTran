organizarDatosAERONET <- function(datosAERONET_fechaMasCercana) {
  # Organiza los datos de AERONET, separando la distribucion de tama?os, el indice de refraccion y los valores de tranferencia radiativa
  
  # Indices de refraccion
  valoresREFR <- list(wvl = c(0, 0, 0 ,0), REFR = c(0, 0, 0 ,0), REFI = c(0, 0, 0 ,0))
  nombresREFR <- grep("REFR", datosAERONET_fechaMasCercana$nombresVariables, value=TRUE)
  nombresREFI <- grep("REFI", datosAERONET_fechaMasCercana$nombresVariables, value=TRUE)
  for (i in 1:length(nombresREFR)) {
    wavelength <- as.numeric(gsub("\\D", "", nombresREFR[i]))
    indiceREFR <- which(datosAERONET_fechaMasCercana$nombresVariables == nombresREFR[i])
    indiceREFI <- which(datosAERONET_fechaMasCercana$nombresVariables == nombresREFI[i])
    valoresREFR$wvl[i] <- wavelength
    valoresREFR$REFR[i] <- datosAERONET_fechaMasCercana$valores[indiceREFR]
    valoresREFR$REFI[i] <- datosAERONET_fechaMasCercana$valores[indiceREFI]
  }
  
  # Distribuciones de tama?o
  nombresNumericos <- which(!is.na(as.numeric(datosAERONET_fechaMasCercana$nombresVariables)))
  valoresDist <- list(tamagnos = c(1:length(nombresNumericos)), dV = c(1:length(nombresNumericos)))
  valoresDist$tamagnos <- as.numeric(datosAERONET_fechaMasCercana$nombresVariables[nombresNumericos])/1E6
  valoresDist$dV <- datosAERONET_fechaMasCercana$valores[nombresNumericos]
  parametrosAdicionales <- c('VolCon-T', 'EffRad-T', 'VolMedianRad-T', 'StdDev-T', 'VolCon-F', 'EffRad-F',
                            'VolMedianRad-F', 'StdDev-F', 'VolCon-C', 'EffRad-C', 'VolMedianRad-C', 'StdDev-C')
  for (numAtt in 1:length(parametrosAdicionales)) {
    indiceAtt <- which(datosAERONET_fechaMasCercana$nombresVariables == parametrosAdicionales[numAtt])
    attr(valoresDist, parametrosAdicionales[numAtt]) <- datosAERONET_fechaMasCercana$valores[indiceAtt]
  }
  # Comprobacion de la distribucion
  # plot(valoresDist$tamagnos, valoresDist$dV, log = 'x')
  # abline(v = attributes(valoresDist)[7])
  # abline(v = attributes(valoresDist)[11])
  
  # Valores de transferencia radiativa
  datosTR <- list('Altitude(BOA)(km)' = '',
                  'Altitude(TOA)(km)' = '',
                  'DownwardFlux(BOA)' = '',
                  'DownwardFlux(TOA)' = '',	
                  'UpwardFlux(BOA)' = '',
                  'UpwardFlux(TOA)' = '',
                  'RadiativeForcing(BOA)' = '',
                  'RadiativeForcing(TOA)' = '',
                  'ForcingEfficiency(BOA)' = '',
                  'ForcingEfficiency(TOA)' = '',
                  'DownwardFlux442-T' = '',
                  'DownwardFlux677-T' = '',
                  'DownwardFlux868-T' = '',
                  'DownwardFlux1020-T' = '',
                  'UpwardFlux442-T' = '',
                  'UpwardFlux677-T' = '',
                  'UpwardFlux868-T' = '',
                  'UpwardFlux1020-T' = '',
                  'DiffuseFlux442-T' = '',
                  'DiffuseFlux677-T' = '',
                  'DiffuseFlux868-T' = '',
                  'DiffuseFlux1020-T' = '')
  for (numTR in 1:length(datosTR)) {
    indiceAtt <- which(datosAERONET_fechaMasCercana$nombresVariables == names(datosTR[numTR]))
    datosTR[numTR] <- datosAERONET_fechaMasCercana$valores[indiceAtt]
  }
  
  # Albedo
  valoresAlbedo <- list(wvl = c(rep(0, times = 4)), albedo = c(rep(0, times = 4)))
  nombresAlbedo <- grep("albedo", datosAERONET_fechaMasCercana$nombresVariables, value=TRUE)
  for (i in 1:length(nombresAlbedo)) {
    wavelength <- as.numeric(gsub("\\D", "", nombresAlbedo[i]))
    indiceAlbedo <- which(datosAERONET_fechaMasCercana$nombresVariables == nombresAlbedo[i])
    valoresAlbedo$wvl[i] <- wavelength
    valoresAlbedo$albedo[i] <- datosAERONET_fechaMasCercana$valores[indiceAlbedo]
  }
  
  # AOTs
  valoresAOT <- list(wvl = c(rep(0, times = 16)), AOT = c(rep(0, times = 16)))
  valoresAOTExt <- list(wvl = c(rep(0, times = 4)), AOTExt_T = c(rep(0, times = 4)), AOTExt_F = c(rep(0, times = 4)), AOTExt_C = c(rep(0, times = 4)))
  valoresAOTAbs <- list(wvl = c(rep(0, times = 4)), AOTAbs = c(rep(0, times = 4)))
  nombresAOT <- grep("AOT", datosAERONET_fechaMasCercana$nombresVariables, value=TRUE)
  for (i in 1:length(nombresAOT)) {
    if (!grepl("AngstromParam", nombresAOT[i])) { # AngstromParam excluded
      if (grepl("Ext", nombresAOT[i])) { # Extinction AOT
        if (grepl("-T", nombresAOT[i])) { # Extinction AOT Total
          wvl <- as.numeric(gsub("\\D", "", nombresAOT[i]))
          indexWvl <- which(c(442, 677, 868, 1020) %in% wvl)
          valoresAOTExt$wvl[indexWvl] <- wvl
          valoresAOTExt$AOTExt_T[indexWvl] <- datosAERONET_fechaMasCercana$valores[which(datosAERONET_fechaMasCercana$nombresVariables == nombresAOT[i])]
        } else if (grepl("-F", nombresAOT[i])) { # Extinction AOT Fine
          wvl <- as.numeric(gsub("\\D", "", nombresAOT[i]))
          indexWvl <- which(c(442, 677, 868, 1020) %in% wvl)
          valoresAOTExt$wvl[indexWvl] <- wvl
          valoresAOTExt$AOTExt_F[indexWvl] <- datosAERONET_fechaMasCercana$valores[which(datosAERONET_fechaMasCercana$nombresVariables == nombresAOT[i])]
        } else if (grepl("-C", nombresAOT[i])) { # Extinction AOT Coarse
          wvl <- as.numeric(gsub("\\D", "", nombresAOT[i]))
          indexWvl <- which(c(442, 677, 868, 1020) %in% wvl)
          valoresAOTExt$wvl[indexWvl] <- wvl
          valoresAOTExt$AOTExt_C[indexWvl] <- datosAERONET_fechaMasCercana$valores[which(datosAERONET_fechaMasCercana$nombresVariables == nombresAOT[i])]
        }
      } else if (grepl("Abs", nombresAOT[i])) { # Absorption AOT
        wvl <- as.numeric(gsub("\\D", "", nombresAOT[i]))
        indexWvl <- which(c(442, 677, 868, 1020) %in% wvl)
        valoresAOTAbs$wvl[indexWvl] <- wvl
        valoresAOTAbs$AOTAbs[indexWvl] <- datosAERONET_fechaMasCercana$valores[which(datosAERONET_fechaMasCercana$nombresVariables == nombresAOT[i])]
      } else {
        wvl <- as.numeric(gsub("\\D", "", nombresAOT[i]))
        indexWvl <- which(c(1640, 1020, 870, 675, 667, 555, 551, 532, 531, 500, 490, 443, 440, 412, 380, 340) %in% wvl)
        valoresAOT$wvl[indexWvl] <- wvl
        valoresAOT$AOT[indexWvl] <- datosAERONET_fechaMasCercana$valores[which(datosAERONET_fechaMasCercana$nombresVariables == nombresAOT[i])]
      }
    }
  }
  
  # SSA
  valoresSSA <- list(wvl = c(rep(0, times = 4)), SSA = c(rep(0, times = 4)))
  nombresSSA <- grep("SSA", datosAERONET_fechaMasCercana$nombresVariables, value=TRUE)
  for (i in 1:length(nombresSSA)) {
    wavelength <- as.numeric(gsub("\\D", "", nombresSSA[i]))
    indiceSSA <- which(datosAERONET_fechaMasCercana$nombresVariables == nombresSSA[i])
    valoresSSA$wvl[i] <- wavelength
    valoresSSA$SSA[i] <- datosAERONET_fechaMasCercana$valores[indiceSSA]
  }
  
  # ASYM
  valoresASYM <- list(wvl = c(rep(0, times = 4)), ASYM_T = c(rep(0, times = 4)), ASYM_F = c(rep(0, times = 4)), ASYM_C = c(rep(0, times = 4)))
  nombresASYM <- grep("ASYM", datosAERONET_fechaMasCercana$nombresVariables, value=TRUE)
  for (i in 1:length(nombresASYM)) {
    wvl <- as.numeric(gsub("\\D", "", nombresASYM[i]))
    indexWvl <- which(c(442, 677, 868, 1020) %in% wvl)
    valoresASYM$wvl[indexWvl] <- wvl
    if (grepl("-T", nombresASYM[i])) { # ASYM Total
      valoresASYM$ASYM_T[indexWvl] <- datosAERONET_fechaMasCercana$valores[which(datosAERONET_fechaMasCercana$nombresVariables == nombresASYM[i])]
    } else if (grepl("-F", nombresASYM[i])) { # ASYM Fine
      valoresASYM$ASYM_F[indexWvl] <- datosAERONET_fechaMasCercana$valores[which(datosAERONET_fechaMasCercana$nombresVariables == nombresASYM[i])]
    } else if (grepl("-C", nombresASYM[i])) { # ASYM Coarse
      valoresASYM$ASYM_C[indexWvl] <- datosAERONET_fechaMasCercana$valores[which(datosAERONET_fechaMasCercana$nombresVariables == nombresASYM[i])]
    }
  }

  # Detalles de la medida
  datosMedida <- list('fechaMedidaAERONET' = '',
                  'Julian_Day' = '',
                  'average_solar_zenith_angle_for_flux_calculation' = '',
                  'solar_zenith_angle_for_1020nm_scan' = '',
                  'Water(cm)' = '', 
                  '870-440AngstromParam(AOTExt)-Total' = '',
                  '870-440AngstromParam(AOTAbsp)' = '',
                  'sky_error' = '',
                  'sun_error' = '',
                  'alpha440-870' = '',
                  'tau440(measured)' = '',
                  'sphericity' = '',
                  'if_level2_AOD' = '')
  datosMedida$fechaMedidaAERONET = as.POSIXlt((datosAERONET_fechaMasCercana$valores[1] - 719529)*86400, origin = "1970-01-01", tz = "UTC")
  for (numMedida in 2:length(datosMedida)) {
    indiceMedida <- which(datosAERONET_fechaMasCercana$nombresVariables == names(datosMedida[numMedida]))
    datosMedida[numMedida] <- datosAERONET_fechaMasCercana$valores[indiceMedida]
  }
  
  listaDedatos <- list(datosMedida, datosTR, valoresDist, valoresREFR, valoresAlbedo, valoresAOT, valoresAOTExt, valoresAOTAbs, valoresSSA, valoresASYM)
  return(listaDedatos)
}
