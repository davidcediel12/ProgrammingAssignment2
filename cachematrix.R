## This script have two functions, one for create the matrix and the 
## respective methods, and 'cacheSolve' that use the matrix created 
## before to know if he inverse has been calculated yet, if the 
## inverse has been already calculated, the function only get this 
## value and return it, if the inverse hasn't been calculates, the
## function is going to use solve() for calculate and setInverse for 
## assign this value. 

## The function returns a list with 4 functions, set, that set the 
## content of the matrix, get, that returns the matrix, setInverse, 
## that is used in the first time when the inverse is not be calculated, 
## and getInverse, that returns the inverse.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      
      get <- function() x 
      set <- function(m) x <<- m
      
      getInverse <- function() inverse
      setInverse <- function(i) inverse <<- i
      list(set = set, get = get, 
           getInverse = getInverse, 
           setInverse = setInverse)
}


## The function returns the inverse, if this was calculated before, 
## only use this value and return it, if not, use  the builtin function 
## solve() to get the inverse, then use setInverse and return the calculated
## value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inver <- x$getInverse()
      if(!is.null(inver))
            return(inver)
      
      x$setInverse(solve(x$get(), ...))
      invisible(x$getInverse())
}

m <- makeCacheMatrix(matrix(rnorm(250000), nrow = 500, ncol = 500))

tini1 <- Sys.time()
cacheSolve(m)
tfin1 <- Sys.time()
cat("Time without cache", tfin1-tini1, "secs\n")

tini2 <- Sys.time()
cacheSolve(m)
tfin2 <- Sys.time()
cat("Time with cache", tfin2-tini2, "secs")