## First we define the 'makeCachematrix' function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }

  getmatrix <- function() {
    x
  }

  getinverse <- function() {
    inv
  }
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }  

  list(setmatrix = setmatrix,
       getmatrix = getmatrix,
       getinverse = getinverse,
       setinverse = setinverse)
}


## Define the "cacheSolve" function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
##Check to see if the inverse is already cached
  
  if(!is.null(inv)) {
    return(inv)
  }
  
  ## Return a matrix that is the inverse of 'x'
  
  mat <- x$getmatrix()
  inv <- solve(mat)

  ##Cache the inverse
  x$setinverse(inv)
  
  inv
}
