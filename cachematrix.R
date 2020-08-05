## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   mtx <- NULL
   setMatrix <- function(y) {
      x <<- y
      mtx <<- NULL
   }
   
   getMatrix <- function() x
   
   invertMatrix <- function(inverted) mtx <<- inverted
   
   getInversion <- function() mtx
   
   print(list(setMatrix = setMatrix, getMatrix = getMatrix, invertMatrix = invertMatrix, getInversion = getInversion))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   mtx <- x$getInversion()
   if(!is.null(mtx)) {
      message("Getting cached data")
      return(mtx)
   }
   data <- x$getMatrix()
   mtx <- solve(data)
   x$invertMatrix(mtx)
   mtx
}


myMatrix <- matrix(c(1,3,2,4,7,5,3,8,9), nrow = 3, ncol = 3)
