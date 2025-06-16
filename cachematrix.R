## Put comments here that give an overall description of what your
## functions do
## MakeCacheMatrix

## Write a short comment describing this function
#makeCacheMatrix creates a special type of matrix that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
  x <<- y
  inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}


## Write a short comment describing this function
#This cache creates the inverse of the matrix. If the matrix was already computed in either make CacheMatrix or in CacheSolve,
#then this inverse will be obtained from the source rather than being recalculated.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
        message("Obtaining cached data")
        return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix,...)
        x$setInverse(inv)
}
