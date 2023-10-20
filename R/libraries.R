('Loading required R packages...')
pacman::p_load(
  sf,
  expp,
  spdep,
  rlang,
  tidyverse,
  dplyr
)


# .onLoad = function (libname, pkgname) {
#  datafile = sf::st_read(system.file("extdata", "stands_data.shp", package="ForSysXR"))
#  assign('stands_data', datafile, envir = topenv())#.GlobalEnv)#envir = topenv())
# }


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to ForSysXR")
  stands_data <- sf::st_cast(stands_data,"POLYGON")
  #datafile = sf::st_read(system.file("extdata", "stands_data.shp", package="ForSysXR"))
  #assign('stands_data', datafile, envir = topenv())#.GlobalEnv)#envir = topenv())
}

