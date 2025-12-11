
show_image = function( fn ){
  par(mar=c(0.5, 0.5, 0.5, 0.5))
  o = png::readPNG(fn)
  res = dim(o)[2:1] # get the resolution, [x, y]
  # rasterImage requires a plot first
  plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',
    xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(o, 1, 1, res[1], res[2])
} 
