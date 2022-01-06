predgrid <- st_make_grid(st_bbox(region@region),
                          cellsize = c(design.trunc*0.5, design.trunc*0.5))

predgrid <- st_intersection(predgrid, region@region)

predgrid <- st_as_sf(predgrid)

predgrid$area <- as.numeric(st_area(predgrid))

predgrid <- st_centroid(predgrid)

preddata <- cbind(as.data.frame(st_drop_geometry(predgrid)),
                  st_coordinates(predgrid))
