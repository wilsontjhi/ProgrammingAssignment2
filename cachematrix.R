## The following 2 functions provide the capability to calculate 
## and cache the inverse of a matrix. 
## - The first function makeCacheMatrix produces a special "matrix", 
##   which encapsulates the functionalities to get/set the value of 
##   the matrix and to get/set the value of the inverse. 
## - The second function, cacheSolve, calculates and caches the inverse
##   of the special "matrix" created by the first function. The cacheSolve 
##   function first checks if the special "matrix" already has the 
##   cached result. If it is, it will return the cached result. Otherwise, 
##   it will calculate the inverse of the matrix and cached it in the 
##   special "matrix"
## ASSUMPTION: The matrix passed in to makeCacheMatrix is invertible.



## makeCacheMatrix creates a special "matrix", which is really a list of 
## functions to get/set the value of the matrix and to get/set the value 
## of the inverse of the matrix. It has one argument which is a matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverseResult <- NULL
    
    set <- function(y){
        x <<- y
        inverseResult <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverseResult <<- inverse
    getinverse <- function() inverseResult
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve accepts the special "matrix" as an argument and then 
## produce the inverse of the matrix. 
## If the special "matrix" does not contain the cached inverse result, 
## this function will calculate the inverse and cache it to the special "matrix"
## If the special "matrix" has the cached result, the function will 
## just return the cached result.

cacheSolve <- function(x, ...) {
    
    inverseResult <- x$getinverse()
    
    if(!is.null(inverseResult)){
        message("getting cached data")
        return(inverseResult)
    }
    
    ## no cached data, calculate the inverse and cache it
    data <- x$get()
    inverseResult <- solve(data, ...)
    x$setinverse(inverseResult)
    inverseResult
}
