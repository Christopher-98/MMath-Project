
vis.gam(dsm.mod,
        plot.type="contour",
        view=c("X","Y"),
        asp=1,
        type="response",
        n.grid=100)

plot(dsm.mod)
plot(density)
# predict 
dsm.pred <- predict(dsm.mod, preddata, preddata$area)

# make the plot
p <- ggplot() +
  grid_plot_obj(pred.poly, dsm.pred, "Density") +
  scale_fill_gradient(low="blue", high="green")+
  coord_equal() + theme_minimal()+
  labs(fill="Density")
p
plot(density)

dsm.cv <- dsm.var.gam(dsm.mod,
                      split(preddata, 1:nrow(preddata)),
                      off.set=preddata$area)

# coefficient of variation plot
p_cv <- ggplot() +
  grid_plot_obj(pred.poly, sqrt(dsm.cv$pred.var)/unlist(dsm.cv$pred), "CV") +
  scale_fill_gradient(low="blue", high="green")+
  coord_equal() + theme_minimal()+
  labs(fill="CV")
p_cv