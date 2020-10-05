
  make.random.string = function(x="_") {
    rand.seq = aegis_floor(runif(1)*10^9)
    out = paste( x, rand.seq, sep="_")
    return(out)
  }


