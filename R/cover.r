#MMM Sept 2014 - altered paths to reflect new ecomod polygon paths
cover= function(x,area="all", wd=wd <- file.path(project.datadirectory('aegis'),'data')){
  require(PBSmapping)

  borders= read.csv(file=file.path(wd,"Management_Areas","Fisheries","areaborders.csv"), head=T, sep=",")
  b=borders[which(borders$area==area),]

# read in shapefiles
#--------------------------------------
  land= importShapefile(file.path(wd,"Basemaps","Terrestrial","landmass_region"))
  coast=importShapefile(file.path(wd,"Basemaps","Marine","Coastline","coastline_polyline"))

#Overlay land and coastline such that any bad data (on land) is hidden
  addPolys(land, col="khaki", border="khaki")
  addLines(coast, col="black")
  abline(h=b$slat, lwd=3)
  abline(h=b$nlat, lwd=3)
  abline(v=-b$wlon, lwd=3)
  abline(v=-b$elon, lwd=3)

}