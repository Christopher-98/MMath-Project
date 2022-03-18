
# create grid across bounding box with resolution of  1/2 truncation distance
predgrid <- st_make_grid(st_bbox(region@region),
                          cellsize = c(design.trunc*0.5, design.trunc*0.5))

# for the purpose of testing, the grid is not cropped to the profile of the region.
# convert to sf and calculate area of each square
predgrid <- st_as_sf(predgrid)
predgrid$area <- as.numeric(st_area(predgrid))

# convert to spatial and find centres of each square
pred.poly <- as(predgrid, 'Spatial')
predgrid <- st_centroid(predgrid)

preddata <- cbind(as.data.frame(st_drop_geometry(predgrid)),
                  st_coordinates(predgrid))
