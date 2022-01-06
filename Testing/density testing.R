allprimes <- function(x) {
  ## check `x` variable is present
  stopifnot(!missing(x))
  
  ## check `x` is numeric
  stopifnot(is.numeric(x))
  
  ## check `x` is of length one
  stopifnot(length(x) == 1)
  
  ## check `x` is a whole number
  stopifnot(x %% 1 == 0)
  
  ## check `x` is larger than 2
  stopifnot(x > 2)
  
  # create vector of all numbers
  primes <- c(1:x)
  
  
  for (i in 2:floor(sqrt(x))) {
    # create sequence of all multiples of primes
    not_primes <- c(seq(i**2, x, i)) 
    
    # set all these values to NA
    primes[not_primes] <- rep(NA, length(not_primes))
  }
  # return vector without the NA values
  return(primes[!is.na(primes)])
}