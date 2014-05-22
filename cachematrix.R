## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    invM <- NULL
    
    set <- function (newM) {
       x <<- newM
    }
    
    get <- function () x
    setInv <- function (inv) {
      invM <<- inv
    }
    getInv <- function () invM
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInv()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data) %*% data
      x$setInv(m)
      m
}
