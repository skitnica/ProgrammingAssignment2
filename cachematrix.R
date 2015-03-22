## the functions in this script create a matrix object that stores a cached version of its inverse. 

## function fore creating a matrix object that stores a cached instance of its inverse
##     the object is mutable, allowing you to set a new value for the matrix, when this is done the cached inverse
##     will be cleared
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}


## function that takes the special matrix object created by makeCacheMatrix, if the inverse of the matrix
## has not been set yet, it will compute it, set the inverse and return the inverse, if the inverse was set before
## it will return the cached inverse.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  
  i <- solve(data)
  x$setinverse(i)
  i
}
