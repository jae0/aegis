
duplicates.toremove = function(x) {
  which(x %in% x[which(duplicated(x))])
}

