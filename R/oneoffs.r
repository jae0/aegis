
# Run commands without parentheses by changing class of the function into a class "command"


  class(search) <- c("command", class(search))
  class(searchpaths) <- c("command", class(searchpaths))

  class(quit)  <- c("command", class(quit))
  attr(quit, "default.args") <- list(save="no")
  q <- exit <- quit 

  class(ls) <- c("command", class(ls))
  attr(ls, "default.args") <- list(all = TRUE)

  clear <- function() { system("clear") ; NULL }
  class(clear) <- c("command", class(clear))


