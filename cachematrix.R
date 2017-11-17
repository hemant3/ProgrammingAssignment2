## The function creates a special matrix object and cache its inverse

## this function creates a special matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##The function below computes the inverse of the special matrix created by 
## the makeCacheMatrix function above. In case of inverse being already calculated 
##with matrix being same, then it retrieves the inverse from cache otherwise, 
## calculates it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("Retrieving data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
  
}
