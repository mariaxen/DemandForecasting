#Tune your Random forest parameters
plot.tune <- function(o, linear = TRUE) {
  x <- o$nodesize
  y <- o$mtry
  z <- o$mape
  so <- interp(x=x, y=y, z=z, linear = linear)
  idx <- which.min(z)
  x0 <- x[idx]
  y0 <- y[idx]
  filled.contour(x = so$x,
                 y = so$y,
                 z = so$z,
                 xlim = range(so$x, finite = TRUE) + c(-2, 2),
                 ylim = range(so$y, finite = TRUE) + c(-2, 2),
                 color.palette =
                   colorRampPalette(c("yellow", "red")),
                 xlab = "nodesize",
                 ylab = "mtry",
                 main = "MSE for nodesize and mtry\ncalibration dataset",
                 key.title = title(main = "MSE", cex.main = 1),
                 plot.axes = {axis(1);axis(2);points(x0,y0,pch="x",cex=1,font=2);
                   points(x,y,pch=16,cex=.25)})
}

#Import the parameters and results you achieved by tuning the model
o <- list()
o$nodesize <- c(rep(seq(50, 250, by=50), 7))
o$mtry     <- c(rep(4, 5), rep(6, 5), rep(8, 5), rep(10, 5), rep(12, 5), rep(14, 5), rep(16, 5))
o$mape     <- c(1197, 1209, 1219, 1232, 1246,  
                1182, 1175, 1175, 1182, 1189, 
                1193, 1176, 1170, 1169, 1172, 
                1207, 1186, 1175, 1174, 1172, 
                1221, 1199, 1186, 1182, 1181,
                1234, 1214, 1198, 1196, 1193, 
                1247, 1233, 1212, 1209, 1212)

o$mape     <- c(766, 896, 961, 1011,1048,  
                684, 813, 881, 928, 967, 
                648, 778, 845, 888, 925, 
                629, 758, 826, 871, 903, 
                617, 747, 813, 858, 890,
                608, 738, 806, 850, 882, 
                603, 734, 800, 846, 876)

## plot the surface
plot.tune(o)