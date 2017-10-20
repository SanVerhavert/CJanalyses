RaschProb <- function(a, b) #a=ability, b=difficulty
{
  exp(a-b)/(1+exp(a-b))
}