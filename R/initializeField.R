 initializeField <-function(n, wallpercent){
# This function will create a field with movement costs and walls, a start
# and goal position at random, a matrix in which the algorithm will store 
# f values, and a cell matrix in which it will store pointers
    # create the field and place walls with infinite cost
    field <- matrix(1,n,n) + matrix(10*runif(n*n, 0, 1), n, n)
    field<- field/min(field)  # minimum cost to travel 1 grid square is 1




    nsamples <- n*n*wallpercent/100
    indsamples <- matrix(sample(n,2*nsamples, replace = TRUE),nrow = nsamples)
    field[rbind(indsamples)] <- Inf

    # create random start position and goal position
    startpos<- matrix(sample(n,2,replace = TRUE),nrow = 1)
    cs = startpos[2]
    rs = startpos[1]
    startposind<-(cs-1)*n + rs
    

    goalpos<- matrix(sample(n,2,replace = TRUE),nrow =1) 
    cg = goalpos[2]
    rg = goalpos[1]
    goalposind<-(cg-1)*n + rg

    # force movement cost at start and goal positions to not be walls
    field[startposind] <- 0
    field[goalposind] <- 0
    # put not a numbers (NaN) in cost chart so A* knows where to look 
    costchart <- matrix(NA,n,n)
    # set the cost at the starting position to be 0
    costchart[startposind] <- 0
    # make fieldpointers as a cell array
    fieldpointers <- list()
    length(fieldpointers)<-n*n
    dim(fieldpointers)<-c(n,n)
        
    # set the start pointer to be "S" for start, "G" for goal
    fieldpointers[startposind] = 'S'
    fieldpointers[goalposind] = 'G'
    
    # everywhere there is a wall, put a 0 so it is not considered
    fieldpointers[rbind(indsamples)] = 0;
    # Create a list to output the variables
    IniField <-list("field" = field, "startposind" = startposind, "goalposind"=goalposind, "costchart" = costchart, "fieldpointers" = fieldpointers)
    return(IniField)}
    # end of this function