## These functions calculate the inverse of a given matrix x
## But is not equals as function solve(), because it is calculate 
## only once and it is saved in the memory

## The function makeCacheMatrix saved the value of matrix and its inverse
## The function will return another functions:

## get: return the value of the matrix
## set: set a new matrix
## getInv: return the value of the inverse
## setInv: set the value of the inverse

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


## The function cacheSolve return the inverse of a certain matrix
## It is necessary passe the object matrix from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInv()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setInv(m)
      m
}
