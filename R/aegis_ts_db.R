
  aegis_ts_db = function( db="", ref.year=2008  ) {

    if (db %in% c("climate", "climate.redo" ) ) {
      fn = file.path(  project.datadirectory("aegis"), "data", "climate.rdz")
      if (db=="climate") {
        return(read_write_fast(fn))
      }

      # old method ... manual db updates
      # climate = get.climate.data( file.path( project.datadirectory("aegis"), "data", "oceanclimate.csv"))

      seaice = osd.data( source="seaice.refresh" ) # daily
      icebergs =  osd.data( source="icebergs.refresh" )  # monthly
      rivsum =  osd.data( source="freshwater.runoff.quebec.refresh" ) # monthly
      hudsonbay =  osd.data( source="freshwater.runoff.into.hudsonbay.refresh" ) # monthly
      stjohn =  osd.data( source="freshwater.discharge.stjohn.refresh" ) # monthly
      t.sydney =  osd.data( source="t.sydney.ns.refresh" ) # monthly air tempratures at Sydney NS
      t.sable =  osd.data( source="t.sable.ns.refresh" ) # monthly air tempratures at Sydney NS
      t.halifax =  osd.data( source="t.halifax.ns.refresh" ) # monthly air tempratures at Sydney NS
      sable = osd.data(source="sable.meteorological.refresh" ) # EC climate/meteorological data
      nao = osd.data(source="nao.refresh" )
      halifax.harbour = osd.data(source="halifax.harbour.sst.refresh" )
      gulf.stream.front = osd.data(source="gulf.stream.front.refresh" )
      ss.slope.front = osd.data(source="ss.slope.front.refresh" )

      #  ... to do:
      # missaine bottom /top
      # emerald bottom / top
      # oxygen
      # nitrate,
      # chla
      # etc.
      # http://www.meds-sdmm.dfo-mpo.gc.ca/zmp/main_zmp_e.html

      seaice = osd.data( source="seaice" )
      seaice = tapply( seaice$sa.km2, years(seaice$date), sum, na.rm=T)
      seaice = data.frame( seaice.sa.km2=as.vector( seaice), yr = as.numeric( rownames( seaice ) ) )

      icebergs =  osd.data( source="icebergs" )
      icebergs =  tapply( icebergs$n.icebergs, years(icebergs$date), sum, na.rm=T )
      icebergs = data.frame( n.icebergs=as.vector( icebergs ), yr = as.numeric( rownames( icebergs ) ) )

      rivsum =  osd.data( source="freshwater.runoff.quebec" )
      rivsum = tapply( rivsum$discharge.m3.sec, years(rivsum$date), sum, na.rm=T)
      rivsum = data.frame( rivsum.discharge.m3.sec=as.vector( rivsum ), yr = as.numeric( rownames( rivsum ) ) )
      rivsum = rivsum[ -which(rivsum$yr==max(rivsum$yr)) ,]  # the last year is a partial record

      hudsonbay =  osd.data( source="freshwater.runoff.into.hudsonbay" )
      hudsonbay = tapply( hudsonbay$discharge.m3.sec, years(hudsonbay$date), sum, na.rm=T)
      hudsonbay = data.frame( hudsonbay.discharge.m3.sec=as.vector(hudsonbay), yr=as.numeric( rownames( hudsonbay ) ) )
      hudsonbay = hudsonbay[ -which(hudsonbay$yr==max(hudsonbay$yr)) ,]  # the last year is a partial record

      stjohn =  osd.data( source="freshwater.discharge.stjohn" )
      stjohn =  tapply( stjohn$discharge.m3.sec, years(stjohn$date), sum, na.rm=T)
      stjohn = data.frame( stjohn.discharge.m3.sec=as.vector(stjohn), yr=as.numeric( rownames( stjohn ) ) )
      stjohn = stjohn[ -which(stjohn$yr==min(stjohn$yr)) ,]  # the first year is a partial record
      stjohn = stjohn[ -which(stjohn$yr==max(stjohn$yr)) ,]  # the last year is a partial record

      t.sydney =  osd.data( source="t.sydney.ns" )
      t.sydney =  tapply( t.sydney$t.sydney.C, years(t.sydney$date), sum, na.rm=T)
      t.sydney =  data.frame( t.sydney.C=as.vector(t.sydney), yr=as.numeric( rownames( t.sydney ) ) )

      t.sable =  osd.data( source="t.sable.ns" )
      t.sable =  tapply( t.sable$t.sable.C, years(t.sable$date), sum, na.rm=T)
      t.sable =  data.frame( t.sable.C=as.vector(t.sable), yr=as.numeric( rownames( t.sable ) ) )

      t.halifax =  osd.data( source="t.halifax.ns" )
      t.halifax =  tapply( t.halifax$t.halifax.C, years(t.halifax$date), sum, na.rm=T)
      t.halifax =  data.frame( t.halifax.C=as.vector(t.halifax), yr=as.numeric( rownames( t.halifax ) ) )

      sable = osd.data(source="sable.meteorological" )
      sable.rad =  tapply( sable$rad.global.kJ.m2, sable$year, mean, na.rm=T ) * 24 * 365  # hourly mean .. convert to yearly total
      sable.rad =  data.frame( sable.rad.global.kJ.m2=as.vector(sable.rad), yr=as.numeric( rownames( sable.rad ) ) )
      sable.rad = sable.rad[ which(sable.rad$sable.rad.global.kJ.m2 > 3750000 ) ,]  # obvious outliers
      sable.rain =  tapply( sable$rain.mm, sable$year, mean, na.rm=T ) * 24 *365
      sable.rain =  data.frame( sable.rain.mm=as.vector(sable.rain), yr=as.numeric( rownames( sable.rain ) ) )
      sable = merge( sable.rain, sable.rad, by="yr" )

      nao = osd.data(source="nao" )
      nao$yr = years( nao$date)
      nao$date = NULL

      halifax.harbour = osd.data(source="halifax.harbour.sst" )
      halifax.harbour =  tapply( halifax.harbour$halifax.sst, years(halifax.harbour$date), sum, na.rm=T)
      halifax.harbour =  data.frame( halifax.harbour.sst=as.vector(halifax.harbour), yr=as.numeric( rownames( halifax.harbour ) ) )


      gulf.stream.front = osd.data(source="gulf.stream.front" )
      gulf.stream.front = gulf.stream.front[ which ( gulf.stream.front$lon == -62 ) , ]
      gulf.stream.front = gulf.stream.front[ which (! ( gulf.stream.front$lat < 0  )) , ]
      gulf.stream.front.mean = tapply( gulf.stream.front$lat, years(gulf.stream.front$date), mean, na.rm=T)
      gulf.stream.front.mean = data.frame( gulf.stream.front.lon62.lat.mean=as.vector( gulf.stream.front.mean ), yr=as.numeric( rownames( gulf.stream.front.mean ) ) )
      gulf.stream.front.sd = tapply( gulf.stream.front$lat, years(gulf.stream.front$date), sd, na.rm=T)
      gulf.stream.front.sd = data.frame( gulf.stream.front.lon62.lat.sd=as.vector( gulf.stream.front.sd ), yr=as.numeric( rownames( gulf.stream.front.sd ) ) )
      gulf.stream.front = merge( gulf.stream.front.mean,  gulf.stream.front.sd, by="yr" )


      ss.slope.front = osd.data(source="ss.slope.front" )
      ss.slope.front = ss.slope.front[ which ( ss.slope.front$lon == -62 ) , ]
      ss.slope.front = ss.slope.front[ which (! ( ss.slope.front$lat < 0  )) , ]
      ss.slope.front.mean = tapply( ss.slope.front$lat, years(ss.slope.front$date), mean, na.rm=T)
      ss.slope.front.mean = data.frame( ss.slope.front.lon62.lat.mean=as.vector( ss.slope.front.mean ), yr=as.numeric( rownames( ss.slope.front.mean ) ) )
      ss.slope.front.sd = tapply( ss.slope.front$lat, years(ss.slope.front$date), sd, na.rm=T)
      ss.slope.front.sd = data.frame( ss.slope.front.lon62.lat.sd=as.vector( ss.slope.front.sd ), yr=as.numeric( rownames( ss.slope.front.sd ) ) )
      ss.slope.front = merge( ss.slope.front.mean,  ss.slope.front.sd, by="yr" )

      climate = merge( seaice, icebergs, by="yr", all=T )
      climate = merge( climate, rivsum, by="yr", all=T  )
      climate = merge( climate, hudsonbay, by="yr", all=T  )
      climate = merge( climate, stjohn, by="yr", all=T  )
      climate = merge( climate, t.sydney, by="yr", all=T  )
      climate = merge( climate, t.sable, by="yr", all=T  )
      climate = merge( climate, t.halifax, by="yr", all=T  )
      climate = merge( climate, sable, by="yr", all=T  )
      climate = merge( climate, nao, by="yr", all=T  )
      climate = merge( climate, halifax.harbour, by="yr", all=T  )
      climate = merge( climate, gulf.stream.front, by="yr", all=T  )
      climate = merge( climate, ss.slope.front, by="yr", all=T  )

      read_write_fast(climate, file=fn )
      return(climate)
    }

    if ( db %in% c("cpi", "cpi.redo" ) ) {
      cpidir =  file.path( project.datadirectory("aegis"), "data", "CPI" )
      fn = file.path( cpidir, "cpi.csv")

      if (db=="cpi") {
        cpi = read.csv( fn, sep=",", header=T )
        cpi$cpi = cpi$cpi / cpi$cpi[ which(cpi$yr==ref.year) ]
        return(cpi)
      }

      # read stored data
      cpi = read.table( fn, sep=",", header=T )
      cpi$cpi = cpi$cpi / cpi$cpi[ which(cpi$yr==ref.year) ]

      # update data
      o = readLines( fn )
      print( "Incomplete:: need an automated update process" )

      # read_write_fast()

      return()


    }

    if (db %in% c("landings" ) ) {
      #####  Landings and landed values of fish ... DFO /economics stats
      # load landings and landed values from CSV files
      # these have been exported from spreadsheets that need to be updated
      # need an automated update mechanism
      infile = file.path(  project.datadirectory("aegis"), "data", "fish", "landings.all.modern.csv")
      ld = read.csv( infile, header=T, strip.white=T, stringsAsFactors=F ) # mt.live
      colnames(ld) = tolower(colnames(ld))
      ld$type = tolower( ld$type )
      for ( i in c("ns", "nb", "pei", "qc", "nfld", "atlantic", "bc", "canada" ) ) {
        ld[,i] = as.numeric(ld[,i])
        iii = which(! is.finite( ld[,i] ) )
        if (length(iii)>0) ld[iii,i] = 0
      }
      return (ld)
    }

    if (db %in% c("landedvalue" ) ) {
      #####  Landings and landed values of fish ... DFO /economics stats
      # load landings and landed values from CSV files
      # these have been exported from spreadsheets that need to be updated
      # need an automated update mechanism
      infile = file.path( project.datadirectory("aegis"), "data", "fish", "landedvalue.all.modern.csv")
      lv = read.csv( infile, header=T, strip.white=T, stringsAsFactors=F ) # K dollar
      colnames(lv) = tolower(colnames(lv))
      lv$type = tolower( lv$type )
      for ( i in c("ns", "nb", "pei", "quebec", "nfld.", "atlantic", "bc", "canada" ) ) {
        lv[,i] = as.numeric(lv[,i])
        iii = which(! is.finite( lv[,i] ) )
        if (length(iii)>0) lv[iii,i] = 0
      }
      return (lv )
    }

    if (db=="landedvalue.annual") {
      lv = aegis_ts_db( db="landedvalue", ref.year=ref.year )  # 1000 $ (CAD, inflation adjusted to refyear )
      lv$ns =  adjust.inflation(  x=lv$ns, yr=lv$year, reference.year=ref.year )
      lv$atlantic =  adjust.inflation(  x=lv$atlantic, yr=lv$year, reference.year=ref.year )

      ns = as.data.frame.matrix( xtabs( ns~ year+type, lv ) )
      names(ns) = paste( "landedvalue.ns", names(ns), sep="." )
      ns$yr = as.numeric( rownames(ns))

      atlantic = as.data.frame.matrix( xtabs( atlantic~ year+type, lv ) )
      names(atlantic) = paste( "landedvalue.atlantic", names(atlantic), sep="." )
      atlantic$yr = as.numeric( rownames(atlantic))

      landedvalue = merge( ns, atlantic, by="yr", sort=T )
      return(landedvalue)
    }

    if (db=="landings.annual") {
      ld = aegis_ts_db( db="landings", ref.year=ref.year )  ## metric tons

      ns = as.data.frame.matrix( xtabs( ns~ year+type, ld ) )
      names(ns) = paste( "landings.ns", names(ns), sep="." )
      ns$yr = as.numeric( rownames(ns))

      atlantic = as.data.frame.matrix( xtabs( atlantic~ year+type, ld ) )
      names(atlantic) = paste( "landings.atlantic", names(atlantic), sep="." )
      atlantic$yr = as.numeric( rownames(atlantic))

      landings = merge( ns, atlantic, by="yr", sort=T )
      return(landings)
    }


    if (db=="landedvalue.archive") {
      data.file = file.path( project.datadirectory("aegis"), "data", "landedvalues.csv")
      landedvalue = read.table(file=data.file, sep=";", header=T, as.is=T, strip.white=T)
      return (landedvalue)
    }

    if (db=="landings.archive") {
      data.file=file.path( project.datadirectory("aegis"), "data", "landings.csv")
      landings = read.table(file=data.file, sep=";", header=T, as.is=T, strip.white=T)
      return(landings)
    }


    if (db %in% c("landings.ns") ) {
      ld = aegis_ts_db( db="landings" ) # metric tons
      vars = c("year", "type", "ns")
      to.extract = c("groundfish~total", "pelagic~total", "shellfish~total", "other~total" )
      ld = ld[ which(ld$type %in% to.extract), vars ]
      return (ld)
    }

    if (db %in% c("landedvalue.ns") ) {
      lv = aegis_ts_db( db="landedvalue", ref.year=ref.year )
      vars = c("year", "type", "ns")
      to.extract = c("groundfish~total", "pelagic~total", "shellfish~total", "other~total" )
      lv = lv[ which(lv$type %in% to.extract), vars ]  # CAN
      lv$ns =  adjust.inflation(  x=lv$ns, yr=lv$year, reference.year=ref.year )
      return (lv)
    }

    if (db %in% c("demographics.redo" ) ) {
      ### goto:: http://www.gov.ns.ca/finance/communitycounts/dataview.asp?gnum=pro9012&gnum2=pro9012&chartid=&whichacct=&year2=&mapid=&ptype=&gtype=&yearid=2006&acctype=0&gname=&dcol=&group=&group1=&group2=&group3=&gview=3&table=table_d17&glevel=pro

      infile = file.path( project.datadirectory("aegis"), "data", "human.csv")
      demogr = read.table(file=infile, sep=",", header=T, as.is=T, strip.white=T, row.names = NULL)
      demogr$PCB.sealblubber = as.numeric( demogr$PCB.sealblubber )
      demogr$fish_harvesters_ns [ which( demogr$year %in% c(2003: 2007) ) ] = demogr$fish_harvesters_ns[ which(demogr$year==2002) ]
      return( demogr )
    }


    if ( db %in% c("GDP") ) {
      # GDP of NS from various sources, esp::
      # http://www.gov.ns.ca/econ/businessclimate/bci/indicator_view.asp?IndicatorID=19
      GDP = data.frame( cbind(
        yr = c( 1999:2007,2009:2013),
        percap = c( 24688, 26400, 27799, 28980, 30807, 31828, 33414, 33942, 35337,34753,35806,36073,35950,36042 )
      ) ) # reference year= 2004

      GDP$percap = adjust.inflation(  x=GDP$percap, yr=GDP$yr, reference.year=2004, reverse=T )  # return to an unadjusted state
      #GDP$percap = adjust.inflation(  x=GDP$percap, yr=GDP$yr, reference.year=ref.year )

      # historical from GPI NS report scaled ... need to rescale ... already adjusted
      gg = data.frame( cbind(
      yr = c(1980:1998),
      percap = c(
        100, 113.5, 118.9, 124.7, 130.6, 134.3, 139.9, 145.1, 147.8, 149.7, 147.4, 143.6,
        145.4, 144.7, 145.4, 147.8, 146.1, 148.1, 153.7)
      ))
      gg$percap = gg$percap / gg$percap[ which(gg$yr==1998)] # rescale to reference of 1998
      gg$percap = gg$percap * GDP$percap[ which(GDP$yr==1999) ]

      GDP = rbind( gg, GDP )
      return (GDP)

    }

    if (db %in% c("economic.data","economic.data.redo" ) ) {
      data.file=file.path( project.datadirectory("aegis"), "data", "economics.csv")
      economics = read.table(file=data.file, sep=",", header=T, as.is=T, strip.white=T)
      to.keep = c( "yr", "No.of.vessels.4vw", "Commercial.Licences.ns", "No.Fish.harvesters", "No.Fish.processors",
      "Employment.per.landedvalue.n.per.millions1997CAD", "Employment.per.landings.n.per.mt",  "GDP.Fish.Processing.millions.2005CAD",
      "GDP.Fishing.hunt.trap.millions.2002CAD", "GDP.NS.millions.2002CAD",  "GDP.fishing.percent", "GDP.Offshore.Oil.Gas.millions.2005CAD" )
      economics = economics[, to.keep ]
      economics$No.of.vessels.4vw = as.numeric( economics$No.of.vessels.4vw )
      economics$No.Fish.processors = as.numeric( economics$No.Fish.processors )
      return(economics)
    }


    if (db %in% c("percapita.landedvalue") ) {
      lvy = as.data.frame.table(  tapply( lv$ns*1000, lv$year, sum ) )
      names(lvy) = c("year", "landedvalue" )
      lvy$year = as.numeric( as.character( lvy$year ) )
      # load  number of fishers
      demogr = aegis_ts_db( db="demographics" )
      lvy = merge( lvy, demogr, by="year" )
      lvy$fish_harvesters_ns [ which( lvy$year %in% c(2003: 2007) ) ] = lvy$fish_harvesters_ns[ which(lvy$year==2002) ]
      lvy$percapita.landedvalue = lvy$landedvalue / lvy$fish_harvesters_ns
      return (lvy)
    }

    if (db %in% c("shrimp.timeseries", "shrimp.timeseries.redo") ) {
      # shrimp: directly contributed by Peter Koeller, but apparently available via VDC
      # data from Peter Koeller's aegis
      fn = file.path( project.datadirectory("aegis"), "data", "shrimp.rdz" )
      if (db=="shrimp.timeseries") {
        return ( read_write_fast(fn) )
      }
      shrimp = read.csv( file.path( project.datadirectory("aegis"), "data", "ESS_shrimp.csv"), header=T, stringsAsFactors=FALSE, na.strings=c("NA", "NAN", "NaN"))
      names(shrimp) = c("yr", "shrimp.abundance.index", "shrimp.size.sexchange.mm", "shrimp.exploitation.index", "shrimp.size.female", "shrimp.capelin.index" )

        # rv_cpue -- cpue of shrimp from dedicated shrimp trawls
        # sex_mm -- size at sex change -- lower size at sex change when growth rates are higher (density, temperature)
        # count -- number of shrimp per pound --- mean size estimate
        # exp_tot -- exploitation rates index
        # fem_size -- size of females -- index of exploitation of largest shrimp
        # ssjuly -- july SST in shrimp fishing areas "shrimp.sst.july"
        # capelin -- abundance of capelin --  a cold-adapted species

      read_write_fast( shrimp, file=fn )
      return (shrimp)
    }


    if (db %in% c("groundfish.timeseries", "groundfish.timeseries.redo" ) ) {

      outfn = file.path( project.datadirectory("aegis"), "data", "groundfish.ts.rdz" )
      if ( db=="groundfish.timeseries" ) {
        return(read_write_fast(outfn))
      }

      # data from groundfish data series
      set = groundfish_survey_db( DS="set" )  ## TODO --- convert to aegis_db

      variables = c( groundfish_variablelist("all"), "area")
      byyear = ts.getdata(set=set, fname="byear.4vw", from.file=F, variables=variables, plottimes="annual", regions="nafo.4vw", custom="normal")
      yrs = sort( unique( byyear$yr ) )
      vars = sort( unique( byyear$variable ) )
      nyrs = length(yrs)
      nvars = length(vars)
      Z = matrix(NA, ncol=nvars, nrow=nyrs)
      for (y in 1:nyrs) {
      for (v in 1:nvars) {
        i = which( byyear$yr==yrs[y] & byyear$variable==vars[v])
        if ( length(i)>0 ) {
          g =  byyear[ i, "mean" ]
          Z[y,v] = g
        }
      }}
      Z = as.data.frame(Z)
      colnames(Z) = paste("groundfish.stratified.mean", vars, sep="." )
      Z$yr = yrs
      read_write_fast( Z, file=outfn )
      return (Z)
    }


    if (db %in% c("species.area", "species.area.redo" )) {

      outfn = file.path( project.datadirectory("aegis"), "data", "species.area.ts.rdz" )
      if ( db=="species.area" ) {
        return(read_write_fast(outfn))
      }

			p = spatial_parameters( spatial_domain="SSE" )
      fns = list.files( file.path( project.datadirectory("aegis"), "data", "SSE","complete" ) )
      yrs = substring( fns, 4,7)
      yrs = as.numeric(yrs)
      yrs = sort( yrs[ which( is.finite( yrs) ) ] )

      res = data.frame( yr=yrs, C=NA, Z=NA, Npred=NA )
      for ( i in 1:length(yrs) ) {
        H = aegis_db( DS="complete", p=p, year=yrs[i] )
        res$C[i] = mean( H$C )
        res$Z[i] = mean( H$Z )
        #res$sar.rsq[i] = mean( H$sar.rsq )
        res$Npred[i] = mean( H$Npred )
      }

      read_write_fast( res, file=outfn )
      return (res)
    }


    if (db %in% c("sizespectrum", "sizespectrum.redo" )) {

      outfn = file.path( project.datadirectory("aegis"), "data", "sizespectrum.ts.rdz" )
      if ( db=="sizespectrum" ) {
        return(read_write_fast(outfn))
      }

      p = spatial_parameters( spatial_domain="SSE" )
      fns = list.files( file.path( project.datadirectory("aegis"), "data", "SSE",'complete' ) )
      yrs = substring( fns, 4,7)
      yrs = as.numeric(yrs)
      yrs = sort( yrs[ which( is.finite( yrs) ) ] )

      res = data.frame( yr=yrs, nss.rsquared=NA, nss.b1=NA, nss.shannon=NA )
      for ( i in 1:length(yrs) ) {
        H = aegis_db( DS="complete", p=p, year=yrs[i] )
        res$nss.rsquared[i] = mean( H$nss.rsquared )
      #  res$nss.b0[i] = mean( H$nss.b0 )
        res$nss.b1[i] = mean( H$nss.b1 )
        res$nss.shannon[i] = mean( H$nss.shannon )
      }

      read_write_fast( res, file=outfn )
      return (res)
    }


    if (db %in% c("metabolism", "metabolism.redo" )) {

      outfn = file.path( project.datadirectory("aegis"), "data", "metabolism.ts.rdz" )
      if ( db=="metabolism" ) {
        return(read_write_fast(outfn))
      }
      p = spatial_parameters( spatial_domain="SSE" )
      fns = list.files( file.path( project.datadirectory("aegis"), "data", "SSE","complete" ) )
      yrs = substring( fns, 4,7)
      yrs = as.numeric(yrs)
      yrs = sort( yrs[ which( is.finite( yrs) ) ] )

      res = data.frame( yr=yrs,  smr=NA, meanlen=NA, meanwgt=NA )
      for ( i in 1:length(yrs) ) {
        H = aegis_db( DS="complete", p=p, year=yrs[i] )
        #res$mr[i] = mean( H$mr )
        res$smr[i] = mean( H$smr )
        res$meanlen[i] = mean( H$len )
        res$meanwgt[i] = mean( H$mass )
        res$tmean[i] = mean( H$tmean )
      }

      read_write_fast( res, file=outfn )
      return (res)
    }


    if (db %in% c("species.composition", "species.composition.redo" )) {

      outfn = file.path( project.datadirectory("aegis"), "data", "species.composition.ts.rdz" )
      if ( db=="species.composition" ) {
        return(read_write_fast(outfn))
      }

      p = spatial_parameters( spatial_domain="SSE" )
      fns = list.files( file.path( project.datadirectory("aegis"), "data", "SSE","complete" ) )
      yrs = substring( fns, 4,7)
      yrs = as.numeric(yrs)
      yrs = sort( yrs[ which( is.finite( yrs) ) ] )

      res = data.frame( yr = yrs, ca1=NA, ca2=NA)#, pca1=NA, pca2=NA )
      for ( i in 1:length(yrs) ) {
        H = aegis_db( DS="complete", p=p, year=yrs[i] )
        res$ca1[i] = mean( H$ca1 )
        res$ca2[i] = mean( H$ca2 )
        #res$pca1[i] = mean( H$pca1 )
        #res$pca2[i] = mean( H$pca2 )
      }
      read_write_fast( res, file=outfn )
      return (res)
    }




    if (db %in% c("snowcrab.timeseries", "snowcrab.timeseries.redo" )) {

      outfn = file.path( project.datadirectory("aegis"), "data", "snowcrab.ts.rdz" )
      if ( db=="snowcrab.timeseries") {
        return(read_write_fast(outfn))
      }

      p = bio.snowcrab::load.environment()

      # trawl data
      tsdata =  snowcrab.timeseries.db( DS="biologicals" )  # this returns 1.96SE as "se"
      tsdata = tsdata[ tsdata$region=="cfaall", ]
      yrs = sort( unique( tsdata$year ) )
      vars = sort( unique( tsdata$variable ) )
      nyrs = length(yrs)
      nvars = length(vars)
      x = matrix(NA, ncol=nvars, nrow=nyrs)
      for (y in 1:nyrs) {
      for (v in 1:nvars) {
        i = which( tsdata$year==yrs[y] & tsdata$variable==vars[v])
        if ( length(i)>0 ) {
          g =  tsdata[ i, "mean" ]
          if( length (g) > 1) {
            print( "Duplicates found:")
            print( tsdata[i,] )
            g = g[1]
          }
          x[y,v] = g
        }
      }}
      x = as.data.frame(x)
      colnames(x) = paste("snowcrab.geometricmean", vars, sep="." )
      x$yr = yrs

      # abundance estimated from interpolation
      #turned off for 2014
      # p = parallel_run( p=p, runindex= list(y=p$years.to.model, v=c("R0.mass", "R1.no", "R2.no", "totno.female.berried", "totno.female.imm", "totno.female.mat") ) )

      #K = interpolation.db( DS="interpolation.simulation", p=p ) # to return the saved file
      #yrs = sort( unique( K$yr ) )
      #vars = sort( unique( K$vars ) )
      #nyrs = length(yrs)
      #nvars = length(vars)
      #Z = matrix(NA, ncol=nvars, nrow=nyrs)
      #for (y in 1:nyrs) {
      #for (v in 1:nvars) {
      #  i = which( K$yr==yrs[y] & K$vars==vars[v])
      #  if ( length(i)>0 ) {
      #    g =  sum(K[ i, "total" ], na.rm=T)  # sum over all areas
      #    Z[y,v] = g
      #  }
      #}}
      #Z = Z
      #Z = as.data.frame(Z)
      #
      #colnames(Z) = paste("snowcrab.kriged", vars, sep="." )
      #Z$yr = yrs

      # fishery data
      res = bio.snowcrab::fishery_data( toget="aggregated" )

      res$landings = res$landings / 1000
      res$effort = res$effort / 1000
      # rownames(res) = yrs
      colnames(res) = paste("snowcrab.fishery", colnames(res), sep="." )

      names(res)[which(names(res)=="snowcrab.fishery.yr") ] = "yr"

      res = merge(res, x, by="yr", all=T, sort=T)
      res = merge(res, Z, by="yr", all=T, sort=T)
      res = res[ which(res$yr <= max(Z$yr)) , ]

      # exploitation rates
      # post-2002 surveys have surveys in autumn
      res$snowcrab.exploitation.index = NA
      i = which(res$yr >= 2002 )
      j = which(res$yr <  2002 )
      res$snowcrab.exploitation.index[i] = res$snowcrab.fishery.landings[i] / (res$snowcrab.kriged.R0.mass[i] + res$snowcrab.fishery.landings[i]  )
      res$snowcrab.exploitation.index[j] = res$snowcrab.fishery.landings[j] / res$snowcrab.kriged.R0.mass[j]


      # temperatures
      td = NULL
      for (yy in 1970:max(p$yrs) ) {
       td =  rbind( td, snowcrab_stmv ( DS="K", p=p, v="R0.mass", y=yy ) )
      }

      tmean = as.data.frame.matrix ( xtabs( temp.region~yr+region, td ) )
      names( tmean ) = paste( "tmean.snowcrab", names( tmean ), sep="." )
      tmean$yr = as.numeric( rownames( tmean ) )

      tsa = as.data.frame.matrix ( xtabs( sa.region ~ yr+region, td ) )
      tsa$cfaall = tsa$cfa4x+ tsa$cfanorth + tsa$cfasouth

      names( tsa ) = paste( "sa.snowcrab", names( tsa ), sep="." )
      tsa$yr = as.numeric( rownames( tsa ) )

      res = merge( res, tmean, by="yr", all=T, sort=T)
      res = merge( res, tsa, by="yr", all=T, sort=T)

      read_write_fast( res, file=outfn )
      return (res)
    }


    if (db=="seal.timeseries") {
      data.file = file.path( project.datadirectory("aegis"), "data", "seals.csv")
      seal = read.table( data.file, sep=",", header=T,  as.is=T, strip.white=T)
      return(seal)
    }

    if (db=="plankton.timeseries") {
      data.file = file.path( project.datadirectory("aegis"), "data", "plankton.csv")
      plankton = read.table( data.file, sep=";", header=T,  as.is=T, strip.white=T)
      return(plankton)
    }



    if (db %in% c("aegis.all", "aegis.all.glue") ) {

      fn = file.path( project.datadirectory("aegis") , "data", "aegis.all.rdz" )
      if (db=="aegis.all" ) {
        return(read_write_fast(fn))
      }

      # refresh the survey data
      groundfish = aegis_ts_db( db="groundfish.timeseries" )
      snowcrab = aegis_ts_db( db="snowcrab.timeseries")
      climate = aegis_ts_db (db="climate" )
      shrimp = aegis_ts_db( db="shrimp.timeseries")

      sar = aegis_ts_db( db="species.area" )
      nss = aegis_ts_db( db="sizespectrum" )
      metab = aegis_ts_db( db="metabolism" )
      sc = aegis_ts_db( db="species.composition" )

      economics = aegis_ts_db( db="economic.data" )

      # hand constructed and updated ..
      plankton = aegis_ts_db( db="plankton.timeseries" )
      human = aegis_ts_db( db="demographics.redo" )
      climate = aegis_ts_db (db="climate" )

      seals = aegis_ts_db( db="seal.timeseries" )
      landedvalue = aegis_ts_db( db="landedvalue.annual", ref.year=2008 )
      landings = aegis_ts_db( db="landings.annual" )


      # merge all data
      out = merge( snowcrab, shrimp, by = "yr", all=T )
      out = merge( out, groundfish, by="yr", all=T )
      out = merge( out, plankton, by="yr", all=T )
      out = merge( out, human, by="yr", all=T )
      out = merge( out, climate, by="yr", all=T )
      out = merge( out, economics, by="yr", all=T )
      out = merge( out, seals, by="yr", all=T )
      out = merge( out, landings, by="yr", all=T )
      out = merge( out, landedvalue, by="yr", all=T )
      out = merge( out, sar,  by="yr", all=T )
      out = merge( out, nss,  by="yr", all=T )
      out = merge( out, metab,  by="yr", all=T )
      out = merge( out, sc,  by="yr", all=T )

      data = out[ order(out$yr), ]

      rownames(data) = data$yr
      colnames(data) = gsub( "[_~]", ".", tolower(colnames(data) ) )


      landings.Atlantic = c( "landingsCod.Atlantic.mt.live","landingsHaddock.Atlantic.mt.live","landingsRedfish.spp.Atlantic.mt.live",
        "landingsHalibut.Atlantic.mt.live","landingsFlatfishes.Atlantic.mt.live","landingsGreenland.turbot.Atlantic.mt.live",
        "landingsPollock.Atlantic.mt.live","landingsHake.Atlantic.mt.live","landingsCusk.Atlantic.mt.live","landingsCatfish.Atlantic.mt.live",
        "landingsSkate.Atlantic.mt.live","landingsDogfish.Atlantic.mt.live","landingsOther.Atlantic.mt.live",
        "landingstotal.groundfish.Atlantic.mt.live","landingsHerring.Atlantic.mt.live","landingsMackerel.Atlantic.mt.live",
        "landingsSwordfish.Atlantic.mt.live","landingsTuna.Atlantic.mt.live","landingsAlewife.Atlantic.mt.live",
        "landingsEel.Atlantic.mt.live","landingsSalmon.Atlantic.mt.live","landingsSmelt.Atlantic.mt.live","landingsCapelin.Atlantic.mt.live",
        "landingsOther.finifish.Atlantic.mt.live","landingsTOTAL.pelagic.Atlantic.mt.live","landingsClams.quahaug.Atlantic.mt.live",
        "landingsOyster.1.Atlantic.mt.live","landingsScallop.2.Atlantic.mt.live","landingsSquid.Atlantic.mt.live",
        "landingsMussel.3.Atlantic.mt.live","landingsLobster.Atlantic.mt.live","landingsShrimp.Atlantic.mt.live",
        "landingsCrab.Queen.Atlantic.mt.live","landingsCrab.Other.Atlantic.mt.live","landingsSea.urchin.Atlantic.mt.live",
        "landingsOther.shellfish.Atlantic.mt.live","landingsTOTAL.shellfish.Atlantic.mt.live",
        "landingsTOTAL.fish.shellfish.Atlantic.mt.live","landingsMarine.plants.Atlantic.mt.live","landingsLumpfish.roe.Atlantic.mt.live",
        "landingsMiscellaneous.other.Atlantic.mt.live","landingsTotal.misc.Atlantic.mt.live","landingstotal.all.seafood.Atlantic.mt.live"
      )


      landings.NS = c( "landingsCod.ns.mt.live","landingsHaddock.ns.mt.live","landingsRedfish.spp.ns.mt.live",
        "landingsHalibut.ns.mt.live","landingsFlatfishes.ns.mt.live","landingsGreenland.turbot.ns.mt.live",
        "landingsPollock.ns.mt.live","landingsHake.ns.mt.live","landingsCusk.ns.mt.live","landingsCatfish.ns.mt.live",
        "landingsSkate.ns.mt.live","landingsDogfish.ns.mt.live","landingsOther.ns.mt.live",
        "landingstotal.groundfish.ns.mt.live","landingsHerring.ns.mt.live","landingsMackerel.ns.mt.live",
        "landingsSwordfish.ns.mt.live","landingsTuna.ns.mt.live","landingsAlewife.ns.mt.live",
        "landingsEel.ns.mt.live","landingsSalmon.ns.mt.live","landingsSmelt.ns.mt.live","landingsCapelin.ns.mt.live",
        "landingsOther.finifish.ns.mt.live","landingsTOTAL.pelagic.ns.mt.live","landingsClams.quahaug.ns.mt.live",
        "landingsOyster.1.ns.mt.live","landingsScallop.2.ns.mt.live","landingsSquid.ns.mt.live",
        "landingsMussel.3.ns.mt.live","landingsLobster.ns.mt.live","landingsShrimp.ns.mt.live",
        "landingsCrab.Queen.ns.mt.live","landingsCrab.Other.ns.mt.live","landingsSea.urchin.ns.mt.live",
        "landingsOther.shellfish.ns.mt.live","landingsTOTAL.shellfish.ns.mt.live",
        "landingsTOTAL.fish.shellfish.ns.mt.live","landingsMarine.plants.ns.mt.live","landingsLumpfish.roe.ns.mt.live",
        "landingsMiscellaneous.other.ns.mt.live","landingsTotal.misc.ns.mt.live","landingstotal.all.seafood.ns.mt.live"
      )


      landings.totals.NS = c( "landingstotal.groundfish.ns.mt.live", "landingsTOTAL.pelagic.ns.mt.live", "landingsTOTAL.shellfish.ns.mt.live",
            "landingsTotal.misc.ns.mt.live", "landingstotal.all.seafood.ns.mt.live", "landings_grd.4vw",  "landings_pel.4vw", "landing_inv.4vw" )

      landings.totals.Atlantic = c( "landingstotal.groundfish.Atlantic.mt.live", "landingsTOTAL.pelagic.Atlantic.mt.live",
        "landingsTOTAL.shellfish.Atlantic.mt.live","landingsTotal.misc.Atlantic.mt.live", "landingstotal.all.seafood.Atlantic.mt.live" )


      landedvalue.NS = c("landedvalue.NS.Cod", "landedvalue.NS.Haddock", "landedvalue.NS.Redfish.spp.","landedvalue.NS.Halibut",
        "landedvalue.NS.Flatfishes", "landedvalue.NS.Greenland.turbot","landedvalue.NS.Pollock","landedvalue.NS.Hake",
        "landedvalue.NS.Cusk","landedvalue.NS.Catfish","landedvalue.NS.Skate","landedvalue.NS.Dogfish","landedvalue.NS.Other.groundfish",
        "landedvalue.NS.TOTAL.groundfish","landedvalue.NS.Herring","landedvalue.NS.Mackerel","landedvalue.NS.Swordfish","landedvalue.NS.Tuna",
        "landedvalue.NS.Alewife","landedvalue.NS.Eel","landedvalue.NS.Salmon","landedvalue.NS.Smelt","landedvalue.NS.Capelin",
        "landedvalue.NS.Other.pelagics","landedvalue.NS.TOTAL.pelagics","landedvalue.NS.Clams.quahaug","landedvalue.NS.Oyster.1",
        "landedvalue.NS.Scallop.2","landedvalue.NS.Squid","landedvalue.NS.Mussel.3","landedvalue.NS.Lobster","landedvalue.NS.Shrimp",
        "landedvalue.NS.Crab.Queen","landedvalue.NS.Crab.Other","landedvalue.NS.Sea.urchin","landedvalue.NS.Other.shellfish",
        "landedvalue.NS.TOTAL.shellfish","landedvalue.NS.TOTAL.shellandfinfish","landedvalue.NS.Marine.plants","landedvalue.NS.Lumpfish.roe",
        "landedvalue.NS.Miscellaneous","landedvalue.NS.Total.miscellaneous","landedvalue.NS.total.all"
      )

      landedvalue.Atlantic = c( "landedvalue.Atlantic.Cod","landedvalue.Atlantic.Haddock","landedvalue.Atlantic.Redfish.spp.",
        "landedvalue.Atlantic.Halibut","landedvalue.Atlantic.Flatfishes","landedvalue.Atlantic.Greenland.turbot","landedvalue.Atlantic.Pollock",
        "landedvalue.Atlantic.Hake","landedvalue.Atlantic.Cusk","landedvalue.Atlantic.Catfish","landedvalue.Atlantic.Skate",
        "landedvalue.Atlantic.Dogfish","landedvalue.Atlantic.Other.groundfish","landedvalue.Atlantic.TOTAL.groundfish",
        "landedvalue.Atlantic.Herring","landedvalue.Atlantic.Mackerel","landedvalue.Atlantic.Swordfish","landedvalue.Atlantic.Tuna",
        "landedvalue.Atlantic.Alewife","landedvalue.Atlantic.Eel","landedvalue.Atlantic.Salmon","landedvalue.Atlantic.Smelt",
        "landedvalue.Atlantic.Capelin","landedvalue.Atlantic.Other.pelagics","landedvalue.Atlantic.TOTAL.pelagics",
        "landedvalue.Atlantic.Clams.quahaug","landedvalue.Atlantic.Oyster1","landedvalue.Atlantic.Scallop.2","landedvalue.Atlantic.Squid",
        "landedvalue.Atlantic.Mussel.3","landedvalue.Atlantic.Lobster","landedvalue.Atlantic.Shrimp","landedvalue.Atlantic.Crab.Queen",
        "landedvalue.Atlantic.Crab.Other","landedvalue.Atlantic.Sea.urchin","landedvalue.Atlantic.Other.shellfish",
        "landedvalue.Atlantic.TOTAL.shellfish","landedvalue.Atlantic.TOTAL.shellandfinfish","landedvalue.Atlantic.Marine.plants",
        "landedvalue.Atlantic.Lumpfish.roe","landedvalue.Atlantic.Miscellaneous","landedvalue.Atlantic.Total.miscellaneous",
        "landedvalue.Atlantic.total.all"
      )

      landedvalue.totals.NS = c( "landedvalue.NS.TOTAL.groundfish", "landedvalue.NS.TOTAL.pelagics", "landedvalue.NS.TOTAL.shellfish",
            "landedvalue.NS.Total.miscellaneous", "landedvalue.NS.total.all", "landval_grd.4vw", "landval_pel.4vw", "landval_inv.4vw" )


      landedvalue.totals.Atlantic = c( "landedvalue.Atlantic.TOTAL.groundfish", "landedvalue.Atlantic.TOTAL.pelagics",
        "landedvalue.Atlantic.TOTAL.shellfish", "landedvalue.Atlantic.Total.miscellaneous", "landedvalue.Atlantic.total.all" )


      human = c( "No.of.vessels.4vw","Commercial.Licences.ns","No.Fish.harvesters","No.Fish.processors",
        "Employment.per.landedvalue.n.per.millions1997CAD","Employment.per.landings.n.per.mt","GDP.Fish.Processing.millions.2005CAD",
        "GDP.Fishing.hunt.trap.millions.2002CAD","GDP.NS.millions.2002CAD","GDP.fishing.percent","GDP.Offshore.Oil.Gas.millions.2005CAD",
        "Population.NovaScotia","Unemployment.rate","Demographics.0_19","Demographics.20_64","Demographics.65plus","no.highschool",
        "university","Nwells.drilled","seismic.2D","seismic.3D","PCB.sealblubber","PCB.Atlantic.Puffin.ppm.wet","No.Shellfish.closures",
        "No.Cruise.ships.Halifax","area_trawled.4vw"
      )


          climate = c( "snowcrab.bottom.temperature", "snowcrab.bottom.temperature.sd", "snowcrab.bottom.habitat.area",
            "ice_coverage.km.2", "T_bottom_emerald", "T_bottom_misaine", "SST_halifax", "vol_source_cil", "CIL.min.T",
            "Gulf.Stream.front.Ref.62lon", "Shelf.front.Ref.62lon", "NAO.index.mbar", "T_sable_annual", "storms",
            "groundfish.stratified.mean.oxyml", "groundfish.stratified.mean.sal", "groundfish.stratified.mean.temp", "nitrate"
          )

          snowcrab = c( "snowcrab.fishery.landings", #"snowcrab.exploitation.index",
            "snowcrab.geometricmean.cw.male.mat.mean" , "snowcrab.geometricmean.cw.fem.mat.mean",
            "snowcrab.geometricmean.R0.mass", "snowcrab.geometricmean.R1.no", "snowcrab.geometricmean.ma13.no", "snowcrab.geometricmean.ma12.no",
            "snowcrab.geometricmean.ma11.no", "snowcrab.geometricmean.ma10.no", "snowcrab.geometricmean.ma9.no",
            "snowcrab.geometricmean.fa10.no", "snowcrab.geometricmean.fa9.no", "snowcrab.geometricmean.fa8.no", "snowcrab.geometricmean.fa7.no",
            "snowcrab.geometricmean.totno.female.mat", "snowcrab.geometricmean.totno.female.imm"
       )

          ecosystem = c(  "seals.total", "groundfish.stratified.mean.shannon", "shrimp.capelin.index",
            "shrimp.size.female", "shrimp.exploitation.index", "shrimp.size.sexchange.mm", "shrimp.abundance.index",
            "cpr_colour", "cpr_diatoms", "cpr_dino", "cpr_cf1_4", "cpr_cf5_6", "cpr_ch", "cpr_para_pseudo",
            "groundfish.stratified.mean.totwgt.elasmobranchs", "groundfish.stratified.mean.totwgt.gadoid",
            "groundfish.stratified.mean.totwgt.flatfish", "groundfish.stratified.mean.ntaxa.elasmobranchs",
            "groundfish.stratified.mean.ntaxa.large.demersal", "groundfish.stratified.mean.ntaxa.small.demersal",
            "groundfish.stratified.mean.ntaxa.large.pelagic", "groundfish.stratified.mean.ntaxa.small.pelagic",
            "groundfish.stratified.mean.ntaxa.flatfish", "groundfish.stratified.mean.rmean.elasmobranchs",
            "groundfish.stratified.mean.rmean.gadoid", "groundfish.stratified.mean.rmean.flatfish",
            "groundfish.stratified.mean.rmean.large.demersal", "groundfish.stratified.mean.rmean.small.demersal",
            "groundfish.stratified.mean.rmean.large.pelagic", "groundfish.stratified.mean.rmean.small.pelagic",
            "groundfish.stratified.mean.mmean.elasmobranchs",
            "groundfish.stratified.mean.mmean.gadoid", "groundfish.stratified.mean.mmean.flatfish",
            "groundfish.stratified.mean.mmean.large.demersal", "groundfish.stratified.mean.mmean.small.demersal",
            "groundfish.stratified.mean.mmean.large.pelagic", "groundfish.stratified.mean.mmean.small.pelagic",
            "groundfish.stratified.mean.smrT", "groundfish.stratified.mean.sar.rsq",
            "groundfish.stratified.mean.nss.rsquared.all.50km.50day", "groundfish.stratified.mean.Z", "groundfish.stratified.mean.C",
            "groundfish.stratified.mean.Npred"
          )

        keyfactors = c(
        "landings.ns.all.grand.total",
        "landings.ns.groundfish.total",
        "landings.ns.pelagic.total",
        "landings.ns.shellfish.total",
        "landings.ns.other.total",
        "landings.ns.shellfish.crab.queen",
        "landedvalue.ns.groundfish.total",
        "landedvalue.ns.pelagic.total",
        "landedvalue.ns.shellfish.total",
        "landedvalue.ns.other.total",
        "landedvalue.ns.all.grand.total",
        "landedvalue.ns.shellfish.crab.queen",
        "population.novascotia", "demographics.65plus", "university",  "no.shellfish.closures",
        "employment.per.landedvalue.n.per.millions1997cad", "employment.per.landings.n.per.mt",
        "gdp.ns.millions.2002cad", "gdp.fish.processing.millions.2005cad", "gdp.fishing.hunt.trap.millions.2002cad",
        "gdp.offshore.oil.gas.millions.2005cad", "fish.harvesters.ns",
        "snowcrab.geometricmean.cw.fem.mat.mean", "snowcrab.geometricmean.totno.female.mat", "snowcrab.geometricmean.totno.female.imm",
        "snowcrab.geometricmean.cw.male.mat.mean",
        "snowcrab.geometricmean.R0.mass", "snowcrab.geometricmean.R1a.no", "snowcrab.fishery.landings",
        "tmean", "gulf.stream.front.lon62.lat.mean",
        "sa.snowcrab.cfaall",
        "nao",
        "groundfish.stratified.mean.oxyml", "groundfish.stratified.mean.temp", "groundfish.stratified.mean.sal",
        "nwells.drilled","seismic.2d","seismic.3d","pcb.sealblubber","pcb.atlantic.puffin.ppm.wet",
        "halifax.harbour.sst", "t.sable.c", "ss.slope.front.lon62.lat.mean", "seaice.sa.km2",
         "seals.total", "shrimp.capelin.index", "shrimp.abundance.index",
        "cpr.diatoms", "groundfish.stratified.mean.shannon", "cpr.colour",

        "groundfish.stratified.mean.totwgt.capelin",
        "groundfish.stratified.mean.totwgt.cod",
        "groundfish.stratified.mean.totwgt.elasmobranchs",
        "groundfish.stratified.mean.totwgt.gadoid",
        "groundfish.stratified.mean.totwgt.flatfish",
        "groundfish.stratified.mean.totwgt.large.demersal",
        "groundfish.stratified.mean.totwgt.small.demersal",
        "groundfish.stratified.mean.totwgt.large.pelagic",
        "groundfish.stratified.mean.totwgt.small.pelagic",
        "cpr.cf1.4","cpr.cf5.6", "cpr.dino",

        "smr", "ca1", "ca2", "meanwgt", "nss.b1", "nss.rsquared",

        # "groundfish.stratified.mean.rmean.small.pelagic",
        # "groundfish.stratified.mean.rmean.small.demersal",
        #"groundfish.stratified.mean.rmean.flatfish",
        #"groundfish.stratified.mean.rmean.elasmobranchs",
        #"groundfish.stratified.mean.rmean.gadoid",
        #"groundfish.stratified.mean.rmean.large.demersal",
        #"groundfish.stratified.mean.rmean.large.pelagic",

        "z",
        "c",
        "npred"

        )

        keyfactors = setdiff( keyfactors, c("landingsTotal.misc.ns.mt.live","landedvalue.NS.Total.miscellaneous" ) )

        to.log = c("snowcrab.geometricmean.totno.female.mat", "snowcrab.geometricmean.totno.female.imm" ,
        "snowcrab.geometricmean.r0.mass",  "snowcrab.geometricmean.r1.no" ,"snowcrab.fishery.landings", "seaice.sa.km2", "seals.total")


        keyfactors.names = matrix( c(
        "population.novascotia", "NS: Population size",
        "demographics.65plus",  "NS: % 65 and older",
        "university",   "NS: % attending university",
        "no.shellfish.closures", "No. shellfish closures",
        "employment.per.landedvalue.n.per.millions1997cad",  "Employment per total landed value",
        "employment.per.landings.n.per.mt", "Employment per total landings",
        "gdp.ns.millions.2002cad",  "GDP: NS total",
        "gdp.fish.processing.millions.2005cad",  "GDP: fish processing",
        "gdp.fishing.hunt.trap.millions.2002cad",  "GDP: fishing and hunting",
        "gdp.offshore.oil.gas.millions.2005cad", "GDP: oil and gas",
        "fish.harvesters.ns",  "No. fish harvesters",
        "snowcrab.geometricmean.cw.fem.mat.mean",  "Snow crab: mature female mean size",
        "snowcrab.geometricmean.totno.female.mat",  "Snow crab: mature female abundance",
        "snowcrab.geometricmean.totno.female.imm", "Snow crab: immature female abundance",
        "snowcrab.geometricmean.cw.male.mat.mean", "Snow crab: mature male mean size",
        "snowcrab.geometricmean.r0.mass", "Snow crab: mature male biomass",
        "snowcrab.geometricmean.r1.no", "Snow crab: male recruitment",
        "snowcrab.fishery.landings",  "Snow crab: landings",
        "tmean", "SSE: temperature mean",
        "sa.snowcrab.cfaall","Snow crab: habitat area",
        "gulf.stream.front.lon62.lat.mean", "Gulf stream front: lat@-62 lon",
        "nao","NAO index",
        "groundfish.stratified.mean.oxyml","RV: bottom oxygen",
        "groundfish.stratified.mean.temp","RV: bottom temperature",
        "groundfish.stratified.mean.sal", "RV: bottom salinity",
        "halifax.harbour.sst","Temperature: SST Halifax",
        "t.sable.c","Temperature: Sable Is.",
        "ss.slope.front.lon62.lat.mean" , "Shelf front: lat@-62 lon",
        "seaice.sa.km2","Ice coverage",
        "smr","RV: groundfish SMR",
        "mr","RV: groundfish MR",
        "seals.total", "Seal abundance",
        "shrimp.capelin.index", "Shrimp: capelin abundance index",
        "shrimp.abundance.index","Shrimp: abundance index",
        "cpr.diatoms","CPR: diatoms",
        "groundfish.stratified.mean.shannon","RV: Shannon index",
        "cpr.colour","CPR: colour",
        "groundfish.stratified.mean.rmean.small.pelagic","RV: condition small pelagics",
        "groundfish.stratified.mean.rmean.small.demersal", "RV: condition small demersals",
        "groundfish.stratified.mean.rmean.flatfish","RV: condition flatfish",
        "groundfish.stratified.mean.rmean.elasmobranchs","RV: condition elasmobranchs",
        "groundfish.stratified.mean.rmean.gadoid","RV: condition gadoids",
        "groundfish.stratified.mean.rmean.large.demersal","RV: condition large demersals",
        "groundfish.stratified.mean.rmean.large.pelagic","RV: condition large pelagics",
        "z","RV: species-area exponent",
        "c","RV: species-area intercept",
        "ca1","RV: species-composition axis 1",
        "ca2","RV: species-composition axis 2",
        "nss.b1", "RV: Normailised size spectra slope",
        "nss.rsquared", "RV: Normalised size spectra R-square",
        "meanwgt","RV: Mean body weight",
        "groundfish.stratified.mean.totwgt.capelin","RV: biomass capelin",
        "groundfish.stratified.mean.totwgt.cod", "RV: biomass cod",
        "groundfish.stratified.mean.totwgt.elasmobranchs", "RV: biomass elasmobranchs",
        "groundfish.stratified.mean.totwgt.gadoid","RV: biomass gadoids",
        "groundfish.stratified.mean.totwgt.flatfish", "RV: biomass flatfish",
        "groundfish.stratified.mean.totwgt.large.demersal","RV: biomass large demersals",
        "groundfish.stratified.mean.totwgt.small.demersal", "RV: biomass small demersals",
        "groundfish.stratified.mean.totwgt.large.pelagic", "RV: biomass large pelagics",
        "groundfish.stratified.mean.totwgt.small.pelagic", "RV: biomass small pelagics",
        "npred","RV: No. taxa predicted",
        "landings.ns.all.grand.total", "Landings: all",
        "landings.ns.groundfish.total", "Landings: groundfish",
        "landings.ns.pelagic.total", "Landings: pelagic",
        "landings.ns.shellfish.total", "Landings: shellfish",
        "landings.ns.other.total", "Landings: misc",
        "landings.ns.shellfish.crab.queen", "Landings: snow crab",
        "landedvalue.ns.groundfish.total",  "Landed value: groundfish",
        "landedvalue.ns.pelagic.total",  "Landed value: pelagics",
        "landedvalue.ns.shellfish.total", "Landed value: shellfish",
        "landedvalue.ns.other.total",  "Landed value: misc",
        "landedvalue.ns.all.grand.total", "Landed value: all",
        "landedvalue.ns.shellfish.crab.queen", "Landed value: snow crab",
        "nwells.drilled", "No. wells drilled",
        "seismic.2d", "Seismic 2D; km",
        "seismic.3d", "Seismic 3D, km^2",
        "pcb.sealblubber", "PCBs: seals",
        "pcb.atlantic.puffin.ppm.wet", "PCBs: puffins",
        "cpr.cf1.4","CPR: Calanus finmarchicus 1-4",
        "cpr.cf5.6","CPR: Calanus finmarchicus 5-6",
        "cpr.dino","CPR: dinoflagellates"
         ), ncol=2, byrow=T )



      aegis = list(data=data,
        landings.NS=landings.NS,
        landings.Atlantic=landings.Atlantic,
        landings.totals.NS=landings.totals.NS,
        landings.totals.Atlantic=landings.totals.Atlantic,
        landedvalue.NS = landedvalue.NS,
        landedvalue.Atlantic=landedvalue.Atlantic,
        landedvalue.totals.Atlantic=landedvalue.totals.Atlantic,
        landedvalue.totals.NS=landedvalue.totals.NS,
        snowcrab=snowcrab, human=human, ecosystem=ecosystem, climate=climate,
        keyfactors=keyfactors, to.log=to.log,
        keyfactors.names =keyfactors.names
      )

    read_write_fast( aegis , file=fn )

    return ( aegis )
  }


    return ("Data not found")

    # aegis_subset = function( X, type  ) {

    #   varlist = X[[ type ]]
    #   varlist = intersect( names(X$data), varlist)
    #   dat = X$data[, varlist ]

    #   for (i in 1:length( X$to.log)) dat[,i] = log10(dat[,i] + 1)

    #   if ( type=="keyfactors" ) {
    #     lkup = X$keyfactors.names
    #     for (i in 1:length( varlist )) {
    #       j = NULL
    #       j = which( tolower( lkup[,1]) == tolower( varlist[i]) )
    #       if (length(j)==1) names(dat)[i] = lkup[ j, 2]
    #     }
    #   }
    #   return (dat)
    # }

  }





