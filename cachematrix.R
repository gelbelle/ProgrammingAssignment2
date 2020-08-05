## The functions contained here work together to create a matrix, invert it, save the inversion to the cache, and then retrieve the inversion if it exists.

## Takes one argument named x, this argument must be a matrix and must be invertable.
## It first sets the matrix then inverts it

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


## This function takes one argument named x, this argument must be a matrix and it must be invertable.
## First it checks to see if the inversion of the matrix is already in the cache, if it is it returns the cached data, otherwise it computes the inverse of the special matrix object

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
