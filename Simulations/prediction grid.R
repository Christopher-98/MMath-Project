
# create grid across the area of the region
predgrid <- st_make_grid(st_bbox(region@region),
                          cellsize = c(0.5*design.trunc, 0.5*design.trunc))

# crop grid to outline of region to allow ds comparison 
predgrid <- st_intersection(predgrid, region@region)

# convert to sf and calculate area of each grid segment
predgrid <- st_as_sf(predgrid)
predgrid$area <- as.numeric(st_area(predgrid))

# convert so spatial and find centres of each grid segment
pred.poly <- as(predgrid, 'Spatial')
predgrid <- st_centroid(predgrid)

# combine data as required by dsm
preddata <- cbind(as.data.frame(st_drop_geometry(predgrid)),
                  st_coordinates(predgrid))
