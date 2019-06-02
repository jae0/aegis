
  bins.df = function( id, base, length.out=50 ) {
    x = switch( id,
      gf.mass = form.bins(base=base, x0=1/1000, x1=125000/1000, length.out=length.out ),
        # from 1g to 30000g or 1/1000 kg to 30000/1000 kg
      gf.len  = form.bins(base=base, x0=1, x1=1000, length.out=length.out  )  
        # from 1cm to 1000cm
    )
    return(x)
  }


