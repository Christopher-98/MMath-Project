predgrid <- st_make_grid(st_bbox(region@region),
                          cellsize = c(design.trunc*0.5, design.trunc*0.5))

predgrid <- st_intersection(predgrid, region@region)

predgrid.poly <- st_as_sf(predgrid)

predgrid.poly$area <- as.numeric(st_area(predgrid.poly))

predgrid <- st_centroid(predgrid.poly)

preddata <- cbind(as.data.frame(st_drop_geometry(predgrid)),
                  st_coordinates(predgrid))
