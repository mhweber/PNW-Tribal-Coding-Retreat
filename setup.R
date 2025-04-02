# Setup script that handles package installation and loading.
#
# A. check for CRAN available packages - install missing and load all
# B. check for GITHUB available packages - install missing and load all

installed = dplyr::as_tibble(installed.packages())

packages = list(
  cran = c("remotes", "dataRetrieval", "tmap", "tmaptools", "dplyr", "tigris", "sf", "mapview", "ggplot2",
           "spData", "terra", "stars", "devtools", "readr", "cowplot", "knitr", "rnaturalearth", "osmdata", 
           "tidyterra", "raster", "tidycencus", "ggspatial", "leaflet", "readxl"),
  github = c("AOI" = "mikejohnson51",
             "climateR" = "mikejohnson51",
             "zonal" = "mikejohnson51",
             "Rspatialworkshop" = "mhweber",
             "StreamCatTools" = "USEPA",
             "spDataLarge" = "nowosad")
)


#' Installation and loading from CRAN
for (package in packages$cran){
  if (!package %in% installed$Package) {
    cat("installing", package, "from CRAN\n")
    install.packages(package)
  }
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}


#' Installation and loading from github
for (package in names(packages$github)){
  pkg <- sprintf("%s/%s", packages$github[package], package)
  if (!package %in% installed$Package) {
    cat("installing", pkg, "from github\n")
    remotes::install_github(pkg)
  }
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}

cat("\n ***setup finished*** \n")

