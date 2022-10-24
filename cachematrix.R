## The following functions will create a matrix, its inverse and cache/store 
## the inverse. These functions work under the assumption that the matrix is  
## square and invertible. 

## makeCacheMatrix creates a square martix that can cache/store its inverse  
## after being ran through the cacheSolve function 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL 
    set <- function(y) { 
        x <<- y
        i <<- NULL
    }
    get <- function() x 
    setinverse <- function(matrix) i <<- matrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve calculates the inverse of a matrix or gives the cache/stored 
## value if it has already be calculated.  "x" is a matrix created by calling 
## the makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("Getting Cached Data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
