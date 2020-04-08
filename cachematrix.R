## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL                   ##set metrix m as null
  set <- function(y){       ## if set a new matrix, it will treat as y
    x <<- y                 ##assign new matrix as x
    m <<- NULL
  }
  get <- function() x       ##call out the latest input matrix
  setInverse <- function(inverse) m <<- inverse #calculate the inverse
  getInverse <- function() m  ## get the calculation result
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()    ## inv as the calculation result from the previous funchtion 
  if (!is.null(inv)) {     ## if there is no value for inv, function will call out the cached result from old calculation
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
