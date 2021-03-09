#' Convert degrees to radians
#'
#' @param x A numeric vector of degrees
#'
#' @return A numeric vector of radians
#'
#' @examples
#' radians(55.55555)
#' @export
radians <- function(x) {
  x*pi/180
}

#' Convert radians to degrees
#'
#' @param x A numeric vector of radians
#'
#' @return A numeric vector of degrees
#'
#' @examples
#' degrees(0.9696273)
#' @export
degrees <- function(x) {
  x*180/pi
}

# Ellipsoid parameters for different datums (m): semi-major axis, a, and
# semi-minor axis, b.
datum_ellipsoid = list(
  # Airy 1830 ellipsoid
  osgb36=list(a=6.377563396e6,
              b=6.356256909e6),
  # WGS84 ellipsoid parameters
  wgs84=list(a=6.378137e6,
             b=6356752.314245))

datum_ellipsoid <- list(
  wgs84 = list(a=6378137, b=6356752.314245, f=1/298.257223563),
  Airy1830 = list(a=6377563.396, b=6356256.909, f=1/299.3249646),
  osgb36  = list(a=6377563.396, b=6356256.909, f=1/299.3249646),
  AiryModified = list(a=6377340.189, b=6356034.448, f=1/299.3249646),
  Bessel1841 = list(a=6377397.155, b=6356078.962818, f=1/299.1528128),
  Clarke1866 = list(a=6378206.4, b=6356583.8, f=1/294.978698214),
  Clarke1880IGN = list(a=6378249.2, b=6356515.0, f=1/293.466021294),
  GRS80 = list(a=6378137, b=6356752.314140, f=1/298.257222101),
  Intl1924 = list(a=6378388, b=6356911.946, f=1/297), ## aka Hayford
  WGS72 = list(a=6378135, b=6356750.5, f=1/298.26))

fM <- function(phi, a, b) {
  # Transverse Mercator projection parameters: Map coordinates of true origin,
  # (E0, N0), scale factor on central meridian, F0, true origin (phi0, lambda0).
  N0 <- -100000
  E0 <- 400000
  F0 <- 0.9996012717
  phi0 <- radians(49)
  lambda0 <- radians(-2)

  n <- (a-b)/(a+b)
  n2 <- n^2
  n3 <- n * n2
  #dphi, sphi = phi - phi0, phi + phi0
  dphi <- phi - phi0
  sphi <- phi + phi0
  M <- b * F0 * (
    (1 + n + 5/4 * (n2+n3)) * dphi -
      (3*n + 3*n2 + 21/8 * n3) * sin(dphi) * cos(sphi) +
      (15/8 * (n2 + n3)) * sin(2*dphi) * cos(2*sphi) -
      (35/24 * n3 * sin(3*dphi) * cos(3*sphi))
  )
  return(M)
}


get_prms <- function(phi, a, F0, e2) {
  ## Calculate and return the parameters rho, nu, and eta2

  rho <- a * F0 * (1-e2) * (1-e2*sin(phi)^2)^-1.5
  nu <- a * F0 / sqrt(1-e2*sin(phi)^2)
  eta2 <- nu/rho - 1
  return(c(rho, nu, eta2))
}


#' Convert from British Grid to latitude & longitude
#'
#' @param E The East part of the grid coordinates
#' @param N The North part of the grid coordinates
#' @param datum A character string naming the datum to convert from
#'
#' @details only datum = "osgb36" currently supported
#'
#' @return A numeric vector of two: the latitude and longitude
#'
#' @examples
#' os_to_ll(337730, 679013)
#' @export
os_to_ll <- function(E, N, datum='osgb36') {
  # Transverse Mercator projection parameters: Map coordinates of true origin,
  # (E0, N0), scale factor on central meridian, F0, true origin (phi0, lambda0).
  N0 <- -100000
  E0 <- 400000
  F0 <- 0.9996012717
  phi0 <- radians(49)
  lambda0 <- radians(-2)

  ## Convert from OS grid reference (E, N) to latitude and longitude.
  ## Latitude, phi, and longitude, lambda, are returned in degrees.

  a <- datum_ellipsoid[[datum]][['a']]
  b <- datum_ellipsoid[[datum]][['b']]
  f <- datum_ellipsoid[[datum]][['f']]
  e2 <- (a^2 - b^2)/a^2
  e2 <- 2*f - f*f
  M <- 0
  phip <- phi0

  while (abs(N-N0-M) >= 1e-5) {
    phip <- (N - N0 - M)/(a*F0) + phip
    M <- fM(phip, a, b)
  }

  rho_nu_eta2 = get_prms(phip, a, F0, e2)
  rho <- rho_nu_eta2[1]
  nu <- rho_nu_eta2[2]
  eta2 <- rho_nu_eta2[3]

  tan_phip = tan(phip)
  tan_phip2 = tan_phip^2
  nu3 <- nu^3
  nu5 <- nu^5
  sec_phip = 1/cos(phip)

  c1 <- tan_phip / 2 / rho / nu
  c2 <- tan_phip / 24 / rho / nu3 * (5 + 3*tan_phip2 + eta2 * (1 - 9*tan_phip2))
  c3 <- tan_phip / 720 / rho / nu5 * (61 + tan_phip2*(90 + 45 * tan_phip2))
  d1 <- sec_phip / nu
  d2 <- sec_phip / 6 / nu3 * (nu/rho + 2*tan_phip2)
  d3 <- sec_phip / 120 / nu5 * (5 + tan_phip2*(28 + 24*tan_phip2))
  d4 <- sec_phip / 5040 / nu^7 *  (61 + tan_phip2*(662 + tan_phip2*
                                                     (1320 + tan_phip2*720)))
  EmE0 <- E - E0
  EmE02 <- EmE0^2
  phi <- phip + EmE0^2 * (-c1 + EmE02*(c2 - c3*EmE02))
  lam <- lambda0 + EmE0 * (d1 + EmE02*(-d2 + EmE02*(d3 - d4*EmE02)))
  return(c(degrees(phi), degrees(lam)))
}


#' Convert from latitude & longitude to British Grid
#'
#' @param phi The latitude as decimal degrees
#' @param lam The longitude as decimal degrees
#' @param datum A character string naming the datum to convert to
#'
#' @details only datum = "osgb36" currently supported
#'
#' @return A numeric vector of two: the East and North of the grid coordinates
#'
#' @examples
#' ll_to_os()
#' @export
ll_to_os <- function(phi, lam, datum='osgb36') {
  # Transverse Mercator projection parameters: Map coordinates of true origin,
  # (E0, N0), scale factor on central meridian, F0, true origin (phi0, lambda0).
  N0 <- -100000
  E0 <- 400000
  F0 <- 0.9996012717
  phi0 <- radians(49)
  lambda0 <- radians(-2)

  ## Convert from latitude and longitude to OS grid reference (E, N).
  ## Latitude, phi, and longitude, lambda, are to be provided in degrees.

  phi <- radians(phi)
  lam <- radians(lam)

  a <- datum_ellipsoid[[datum]][['a']]
  b <- datum_ellipsoid[[datum]][['b']]
  f <- datum_ellipsoid[[datum]][['f']]
  e2 <- (a^2 - b^2)/a^2
  e2 <- 2*f - f*f
  rho_nu_eta2 = get_prms(phi, a, F0, e2)
  rho <- rho_nu_eta2[1]
  nu <- rho_nu_eta2[2]
  eta2 <- rho_nu_eta2[3]

  M <- fM(phi, a, b)

  sin_phi <- sin(phi)
  cos_phi <- cos(phi)
  cos_phi2 <- cos_phi^2
  cos_phi3 <- cos_phi2 * cos_phi
  cos_phi5 <- cos_phi3 * cos_phi2
  tan_phi2 <- tan(phi)^2
  tan_phi4 <- tan_phi2 * tan_phi2

  a1 = M + N0
  a2 = nu/2 * sin_phi * cos_phi
  a3 = nu/24 * sin_phi * cos_phi3 * (5 - tan_phi2 + 9*eta2)
  a4 = nu/720 * sin_phi * cos_phi5 * (61 - 58*tan_phi2 + tan_phi4)
  b1 = nu * cos_phi
  b2 = nu/6 * cos_phi3 * (nu/rho - tan_phi2)
  b3 = nu/120 * cos_phi5 * (5 - 18*tan_phi2 + tan_phi4 + eta2*(14 -
                                                                 58*tan_phi2))
  lml0 = lam - lambda0
  lml02 = lml0^2
  N = a1 + lml02 * (a2 + lml02*(a3 + a4*lml02))
  E = E0 + lml0 * (b1 + lml02*(b2 + b3*lml02))
  return(c(E, N))
}
