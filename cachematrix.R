## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this function implements a hypotetical matrix that will be used in the next function


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverter <- function(Inverter) m <<- Inverter
    getInverter <- function() m
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverter = setInverter,
         getInverter = getInverter)
    
}


## Write a short comment describing this function
##This function gets a matrix and compute its inverse
##It's assumed that the matrix is squared AND its determinant is different than zero
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x',
  m <- x$getInverter()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setInverter(m)
  m
}
