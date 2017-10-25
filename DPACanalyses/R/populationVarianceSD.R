pop.var <- function(x) var(x) * (length(x)-1) / length(x)
pop.sd <- function(x) sqrt(pop.var(x))