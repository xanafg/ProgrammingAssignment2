## the function makeCacheMatrix creates a list containing 4 functions,
## 1. set: sets the value of the matrix
## 2. get: gets the value of the matrix
## 3. setinv: sets the value of the inverse
## 4. getinv: gets the value ot the inverse
## makeCacheMatrix creates an object to store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cachSolve= calculates the inverse of the matrix stored in makeCacheMatrix.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
