## These 2 functions store and calculate the inverse of a matrix.

## This function creates a list containing 4 functions 
## to set and retrieve values of the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve #calculation of the inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function finds the inverse of the matrix, or if it 
## has already been calculated, gets the stored value of the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)  #calculation of the inverse
  x$setinv(m)
  m
}