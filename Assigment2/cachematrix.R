## Put comments here that give an overall description of what your
## functions do

## cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y){
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) invers <<- solveMatrix
  getInverse <- function() invers
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of the cached matrix
  invers <- x$getInverse()
  if(!is.null(invers)){
    message("getting cached data")
    return(invers)
  }
  data <- x$get()
  invers <- solve(data)
  x$setInverse(invers)
  invers      
}
