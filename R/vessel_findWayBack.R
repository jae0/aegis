############################
vessel_findWayBack<-function(goalposind,fieldpointers, field){

# This function will follow the pointers from the goal position to the
# starting position
    n <-dim(fieldpointers)[1] # length of the field
    m <-dim(fieldpointers)[2]
    posind <- goalposind
    # convert linear index into [row column]
    
    py <- ((posind-1) %% n)+1
    px <- floor((posind-1)/n) +1

        
    # store initial position
    p <-c(py,px) 
    cost <-0
        # until we are at the starting position
    while (fieldpointers[posind] != 'S'){

    if (fieldpointers[posind] == 'L') # move left
    {
      px <- px-1;
      } else if  (fieldpointers[posind] == 'R') # move right
      {
        px <- px+1
  } else if  (fieldpointers[posind] == 'U') # move up
    { 
      py <- py - 1
  } else if (fieldpointers[posind] == 'D') # move down
  { 
    py <- py +1
  }
  
else if (fieldpointers[posind] == 'RU') # 
{
px <- px + 1
py <- py - 1
}
else if (fieldpointers[posind] == 'RD') # 
{
px <- px + 1
py <- py + 1
}
else if (fieldpointers[posind] == 'LU') # 
{
px <- px - 1
py <- py - 1
}

else if (fieldpointers[posind] == 'LD') # 
{
px <- px - 1
py <- py + 1
}


  pstep<-c(py, px)
p <-rbind(p, pstep)
cost = cost+field[py,px]
posind <-(px-1)*n+py

# end of this function
}




return(p)
}
############################