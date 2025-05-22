

# Mapa Mundo
mundo <- world(path=".")
plot(mundo, xlim=c(-110,60), ylim=c(-80,40), col="light yellow", border="light gray")


points(Base_Colombia$longitud, Base_Colombia$latitud, col='red', pch=20)


# MAPEAR CON LINEAS
plot(vector_1, col = "red4")
lines(mundo, col='gray26', lwd=2)


# Mostrar mapa submuestreo

mapa_sub <- as.polygons(raster_1)
plot(mapa_sub, border='gray')
points(vector_1)

# selected points in red

points(raster_submuestreo, cex=1, col='red', pch='x')









