## makeCacheMatrix creates a special matrix object that can cache its matrix
## cacheSolve computes inverse of special Matrix returned by makecacheMatrix

## creates a special matrix object that can cache its matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #initialising the inverse matrix
  set <- function(y) {
    x <<- y ## the matrix
    i <<- NULL
  }

  get <- function() x ## getting the matrix
  setinverse <- function(inverse) #setting the inverse
    i<<- inverse
  
  getinverse <- function() i ## getting the inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
} ## list of all functions in cacheMatrix


## returns value of the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) { ## if inverse is available, return its value
    message("getting cached data")
    return(m)
  }
  
  data <- x$get() ## get the value of the matrix
  
  m <- solve(data,...) ## calculating inverse of the matrix
  
  x$setinverse(m) ## setting the value of the inverse calculated
  
  m ## returning the value of the inverse
}
}
