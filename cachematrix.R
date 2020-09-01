## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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