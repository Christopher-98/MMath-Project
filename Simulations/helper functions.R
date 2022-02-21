# file to contain required functions for simulations
# and all package loading

library(sf)
library(Distance)
library(dsm)
library(dsims)
library(tidyverse)
library(sp)
library(nngeo)

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


to_segments <-function(region, survey, transect.type) {
  # obtain region boundaries for use in calculating segment areas
  seg.region <- st_union(region@region)
  
  # section for it line transects to split into segments
  if (transect.type == 'line') {
    
    # extract transects
    samplers <- survey@transect@samplers
    
    # if transects are multilinestrings, split into individual linestrings
    samplers <- st_segments(samplers, progress = F)
    
    # split into segments of 2*truncation distance and separate
    # into individual line strings
    segs <- stdh_cast_substring(st_segmentize(samplers,
                                              dfMaxLength = 2*design.trunc),
                                to = "LINESTRING")
    
    segs$Effort <- as.numeric(st_length(segs))
    segs$Sample.Label <- 1:length(segs$transect)
    
    # create polygons based on truncation distance from line
    # Note: mitre may need set for eszigzagcom studies
    poly <- st_buffer(segs, dist = design.trunc, endCapStyle = 'FLAT')
    
    # Find the areas for each segment, using intersection with region
    # to account for study area edge profile
    segs$Area <- as.numeric(st_area(st_intersection(poly, seg.region)))
    
    segs <- st_centroid(segs)
    
  } else {
    
    # For point designs the segments are already defined
    segs <- survey@transect@samplers
    
    segs$Effort <- 1
    segs$Sample.Label <- segs$transect
    segs$transect <- NULL
    
    poly <- st_buffer(segs,design.trunc)
    
    segs$Area <- as.numeric(st_area(st_intersection(poly, seg.region)))
    
  }
  return(segs)
}

generate.dsm.data <- function(region, survey, transect.type){
  
  # extract observation data
  obsdata <- survey@dist.data[!is.na(survey@dist.data$object),]
  
  estimates$detections[j] <- nrow(obsdata)
  
  # generate segments for dsm
  segs <- to_segments(region, survey, transect.type)
  
  segdata <- cbind(as.data.frame(st_drop_geometry(segs)), st_coordinates(segs))
  
  # link obsdata to the segments
  obsdata <- st_as_sf(obsdata, coords = c('x','y'))
  
  # remove existing transects as sample labels
  obsdata$Sample.Label<- NULL
  
  #set crs for consistency
  st_crs(obsdata) <- st_crs(segs)
  
  # link observations to segments by nearest segment
  obsdata<- st_join(obsdata, segs, join = st_nearest_feature)
  obsdata <- st_drop_geometry(obsdata)
  
  # save only required columns
  obsdata <- obsdata[,c("object", "Sample.Label", "distance")]
  obsdata$size <- 1
  
  return(list(segdata = segdata,
              obsdata = obsdata))
}
