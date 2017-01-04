## The purpose of this code is save time for potentially time-consuming operations.
## We store the solutions of repeated computations as 'cache', so when they are to be computed,
## we can refer to the values in cache.

## First function creates an object which is a list of 4 functions. Its made to easily manipulate the next function.
## This uses its own environment where cache is stored, essentially.
## We are exploting the lexical scoping of R language.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(sol) inv <<- sol
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This checks if an inverse is already calculated before for the object, by checking the special environment.
## If cache is stored in the first place, that matrix is returned, and if its not, a new inverse is calculated and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'. Here, it is "sol".
  sol <- x$getinv()
  if(!is.null(sol)) {
    message("Getting cached data..")
    return(sol)
  }
  data <- x$get()
  sol <- solve(data, ...)
  x$setinv(sol)
  sol
}
