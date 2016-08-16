## Second programming assignment of the R Programming course
## Invert a matrix and cache its value. 
## Retrieve the inverted matrix from the cache if it exists.


## makeCacheMatrix: This function wraps a "matrix" object that can cache  
## its inverse. The matrix is supposed to be invertible.
## usage: set matrix: if m is a matrix, "a <- makeCacheMatrix(m)"
##        get matrix: a$get()
##        invert: cacheSolve(a)



makeCacheMatrix <- function(x = matrix()) { 
    ## initiate the inverse to null
    i <- NULL
    ## loads matrix from the parent environment
    set <- function(y){
        x <<- y
        ## initiate the inverse to null
        i <- NULL
    }
    ## returns the matrix
    get <- function() x
    # stores the inverse. Will return an error if the matrix is singular
    setinv <- function(solve) i<<-solve
    # returns the cached inverse. Will return NULL if cache empty
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
} 

## cacheSolve: this function returns the inverse of a matrix wrapped 
## by the makeCacheMatrix function,either by inverting it or by retrieving
## its value from the cache. 


cacheSolve <- function(x, ...) { 
    ## Return a matrix that is the inverse of 'x' 
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cache inverse")
        return(inv)
    }
    mat <- x$get()
    ## matrix singularity not tested (supposed invertible)
    inv <- solve(mat)
    x$setinv(inv)
    inv
}