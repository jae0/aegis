list_get = function(v, w, drop_na=TRUE) {
  out = unlist( sapply( v, function(x) x[[w]] ) ) 
  if (drop_na) out = out[ is.finite(out) ]
  out
}