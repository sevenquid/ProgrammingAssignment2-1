##  For JHU R Programming Assignment
##  By Haosha Wang - Elsa 
##
## Matrix inversion is a costly computation and there will be benefit to caching the inverse of a matrix
## rather than compute it repeatedly
## For this assignement, assume that the matrix supplied is always inverstible. 

## makeCacheMatrix creates a list containing a function to
## 1. Set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  invMtrx <- NULL
  set <- function(y){
    x <<- y
    invMtrx <- NULL
  }
  
  get <- function() x
  setInv <- function(inverse) invMtrx <<- inverse
  getInv <- function() invMtrx
  list(set = set, get = get,
      setInv = setInv,
      getInv = getInv)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMtrx <- x$getInv()
  if(!is.null(invMtrx)){
    message("Getting cached inversed matrix")
    return (invMtrx)
  }
  data <- x$get()
  invMtrx <- solve(data,...)
  x$setInv(invMtrx)
  invMtrx
}

