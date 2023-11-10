('Loading required R packages...')
pacman::p_load(
  sf,
  spdep,
  rlang,
  tidyverse,
  dplyr,
  spsUtil,
  stringr
)


# .onLoad = function (libname, pkgname) {
#  datafile = sf::st_read(system.file("extdata", "stands_data.shp", package="ForSysXR"))
#  assign('stands_data', datafile, envir = topenv())#.GlobalEnv)#envir = topenv())
# }


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to ForSysXR")
  stands_data <- sf::st_cast(stands_data,"POLYGON")
  stands_data_FBN <- sf::st_cast(stands_data_FBN,"POLYGON")
  #datafile = sf::st_read(system.file("extdata", "stands_data.shp", package="ForSysXR"))
  #assign('stands_data', datafile, envir = topenv())#.GlobalEnv)#envir = topenv())
}

