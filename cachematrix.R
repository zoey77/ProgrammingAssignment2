## return the inverse of a matrix, 
##cache the inverse for future use until the original matrix is replaced

## take a matrix, and return a list of four functions
## set - take a matrix, replace the old input matrix with the new one, and reset the inverse to NULL
## get - return the input matrix
## setinverse - set inverse to the input (used in the next function)
## getinverse - return the stored inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## check if the inverse of the input is null,
## if it is, then calculate the inverse and store it in the input
## otherwise, return the stored inverse without additional calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
