## The functions store and also use the stored values to find the inverse of
##   a matrix.

## This stores the matrix inverse and is a list of 4 function that help call the 
## function inside when necessary

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function(){ 
    x
  }  
  setinverse <- function(inverse) {
    m <<- inverse
  }  
  getinverse <- function() {
    m
  }  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the matrix, but first searches in the 
## memory for a readymade value to avert computations

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m   
}
