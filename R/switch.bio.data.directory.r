switch.bio.data.directory = function(x=1) {
  #\\ NOTE:: this can also be done on the fly by manually the directory associated with a git branch
	if(x==1) bio.datadirectory <<- bio.datadirectory1
  if(x==2) bio.datadirectory <<- bio.datadirectory2
}
