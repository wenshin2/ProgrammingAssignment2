## Functions to cache the inverse of a matrix

## Function to create a special "matrix", which caches the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) {
    i <<- inv
  }
  
  getInverse <- function() {
    i
  }
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Fetch the inverse matrix if it has been cached, compute it otherwise
## x has to be object created using makeCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("retrieving from cache")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}