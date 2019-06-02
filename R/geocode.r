
geocode = function( civic.address="10900 Euclid Ave, Cleveland, USA" ) {
  
  q0 = "http://maps.google.com/maps/geo?q="
  q1 = gsub( "[[:punct:][:space:]]", "+", civic.address ) #replace punctuation and spaces with '+'
  q2 = "&output=kml&key="
  APIKEY = "ABQIAAAA5sNfz60epqKn0RoFraZGYhQ280tra-vRz4096Hpfbhn5LBr8DBSc3xPl-TDfLSEvPz5IV8JcKO5b7g"  # obtain API key from http://code.google.com/apis/maps/signup.html
  q3 = "..."

  # with an API key, 15,000 geocode requests in a 24 hour period are allowed 
  # but if they are done too quickly they will return an error code of 620
  # success will result in a status code of 200

  connection = url( paste( q0, q1, q2, APIKEY, q3, sep="") )
  result = readLines( connection )
  close( connection )

  
  return(result)
}
  


