## We create a matrix object that can cache its inverse and a function that
## get it from the cache if possible. Otherwise the invers is computed

## First we create a matrix object that can cahce its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function()inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInv = setInv,
       getInv = getInv)
}

## This function gets the inverse from the cache or computes it if nesessary

cacheSolve <- function(x, ...) {
  
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getMatrix()
  inv <- solve(data)
  x$setInv(inv)
  inv
}
