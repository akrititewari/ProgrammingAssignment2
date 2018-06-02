## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse 
##from the cache and save computation time


makeCacheMatrix <- function(x = matrix()) #This function creates a special "matrix" object that can cache its inverse
{
inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}




cacheSolve <- function(x, ...) #This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
{
        ## Return a matrix that is the inverse of 'x'
            inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv 
}
