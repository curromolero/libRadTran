# Calculate HenyeyGreenstein moments
calculateHenyeyGreensteinMoments <- function(g, nmom){
  library(pracma)
  # Input g: Asymmetry parameter, first order moment
  # input nmom: number of moments to calculate
  
  # Output moments: Legendre moments for Henyey-Greenstein phase function
  
  mu <- seq(from = -1, to = 1, by = 0.01) # Resolution 0.01 in cos(Theta), accurate enough?
  P_HG <- function(mu) {(1-g^2)/((1+g^2-2*g*mu)^(3/2))} # Henyey-Greenstein phase function
  moment <- matrix(data = NA, nrow = nmom, ncol = 1)
  moment[1] <- 1
  for (i in 1:nmom) {
    integrand <- function(mu) {P_HG(mu)*legendre(i, mu)[1, ]}
    resultadoIntegrado <- integrate(integrand, lower = -1, upper = 1, stop.on.error = FALSE)
    moment[i+1] <- (1/2)*resultadoIntegrado$value
  }
  return(moment)  
}