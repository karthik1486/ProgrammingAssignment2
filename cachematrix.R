## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {


    inv <- NULL
    
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    getMatrix <- function() x
    setMatrixInverse <- function(matrixInv) inv <<- matrixInv
    getMatrixInverse <- function() inv
    list(setMatrix=setMatrix,getMatrix=getMatrix,
         setMatrixInverse=setMatrixInverse,
         getMatrixInverse=getMatrixInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getMatrixInverse()
     # if the inverse has already been calculated
     if(!is.null(inv))
     {
         # get it from the cache and skips the computation.
         message("getting cached inverse")
         return(inv)
     }
     
     # otherwise, calculates the inverse 
     mat <- x$getMatrix()
     inv <- solve(mat, ...)
     
     # sets the value of the inverse in the cache via the setMatrixInv function.
     x$setMatrixInverse(inv)
     return(inv)
}
