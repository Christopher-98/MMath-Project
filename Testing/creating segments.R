# segmentising data

library(dsims)
library(sf)
library(nngeo)


plot(survey)

fit <- analyse.data(analyses, data.obj = survey)

# segmentising data

source('helper functions.R')

# To extract coordinates to make segdata
survey.samp <- survey@transect@samplers

test.segs <- st_segments(survey.samp)

segments <- st_segmentize(test.segs,
              dfMaxLength = 2*design.trunc)
test.segs <- st_segments(segments)

segs <- stdh_cast_substring(st_segmentize(segments,
                                          dfMaxLength = 2*design.trunc), to = "LINESTRING")

segs$Effort <- as.numeric(st_length(segs))
segs$Sample.Label <- 1:length(segs$transect)

seg.lines <- segs

segs <- st_centroid(segs)

poly <- st_buffer(seg.lines, dist = design.trunc, endCapStyle = 'FLAT')

plot(poly)

poly <- st_intersection(poly, region@region)

plot(poly)

segs$Area <- as.numeric(st_area(poly))

seg.data <- cbind(as.data.frame(st_drop_geometry(segs)),
                 st_coordinates(segs))

obs.data <- survey@dist.data[!is.na(survey@dist.data$object),]

obs.data <- st_as_sf(obs.data, coords = c('x','y'))
# remove transects as sample lables
obs.data$Sample.Label<- NULL

st_crs(obs.data) <- st_crs(segs)

obs <- st_join(obs.data, segs, join = st_nearest_feature)

obs <- st_drop_geometry(obs)

obs <- obs[,c("object", "Sample.Label", "distance")]

obs$size <- 1

head(obs)


# create a prediction grid
# method from http://rfunctions.blogspot.co.uk/2014/12/how-to-create-grid-and-intersect-it.html

source('prediction grid.R')


###############################################################################
to_segments <-function(region, survey, transect.type) {
  # obtain region boundaries for use in calculating segment areas
  seg.region <- st_union(region@region)
  
  # section for it line transects to split into segments
  if (transect.type == 'line') {
    
    # extract transects
    samplers <- survey@transect@samplers
    
    # if transects are multilinestrings, split into individual linestrings
    samplers <- st_segments(samplers)
    
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

