# file to contain required functions for simulations
# and all package loading

library(sf)
library(Distance)
library(dsm)
library(dsims)
library(tidyverse)
library(sp)



#provided from
#https://dieghernan.github.io/201905_Cast-to-subsegments/

stdh_cast_substring <- function(x, to = "MULTILINESTRING") {
  ggg <- st_geometry(x)
  
  if (!unique(st_geometry_type(ggg)) %in% c("POLYGON", "LINESTRING")) {
    stop("Input should be  LINESTRING or POLYGON")
  }
  for (k in 1:length(st_geometry(ggg))) {
    sub <- ggg[k]
    geom <- lapply(
      1:(length(st_coordinates(sub)[, 1]) - 1),
      function(i)
        rbind(
          as.numeric(st_coordinates(sub)[i, 1:2]),
          as.numeric(st_coordinates(sub)[i + 1, 1:2])
        )
    ) %>%
      st_multilinestring() %>%
      st_sfc()
    
    if (k == 1) {
      endgeom <- geom
    }
    else {
      endgeom <- rbind(endgeom, geom)
    }
  }
  endgeom <- endgeom %>% st_sfc(crs = st_crs(x))
  if (class(x)[1] == "sf") {
    endgeom <- st_set_geometry(x, endgeom)
  }
  if (to == "LINESTRING") {
    endgeom <- endgeom %>% st_cast("LINESTRING")
  }
  return(endgeom)
}

# define last function to extract ds estimates is multiple strata used
last <- function(x) {return(x[length(x)])}


# given the argument fill (the covariate vector to use as the fill) and a name,
# return a geom_polygon object
# fill must be in the same order as the polygon data
grid_plot_obj <- function(shape, fill, name){
  
  # what data were supplied?
  names(fill) <- NULL
  row.names(fill) <- NULL
  data <- data.frame(fill)
  names(data) <- name
  
  # ! need to give the right name of the shapefile
  sp <- shape
  spdf <- SpatialPolygonsDataFrame(sp, data)
  spdf@data$id <- rownames(spdf@data)
  spdf.points <- fortify(spdf, region="id")
  spdf.df <- full_join(spdf.points, spdf@data, by="id")
  
  # store the x/y even when projected and labelled as "long" and "lat"
  spdf.df$x <- spdf.df$long
  spdf.df$y <- spdf.df$lat
  
  geom_polygon(aes_string(x="x",y="y",fill=name, group="group"), data=spdf.df)
}
