## 
## Tim Dale
## Coursera R-programming
##
## Functions to wrap a matrix with the ability to compute and cache its inverse.
##
##




## 
## Create a wrapper for a matrix that will cache its inverse.  This allows us to save a lot
## of times when the inverse is subsequently needed: we simply use the cached copy rather
## than recomputing it.
##
makeCacheMatrix <- function(m = matrix()) {

    # holder of the inverse
    inverse <- NULL
    
    set <- function(y) {
        m <<- y
        inverse <<- NULL
    }
    
    ## return the matrix
    get <- function() m
    
    
    ## cache the inverse of the matrix
    setInverse <- function(i) inverse <<- i

    ## get the cached inverse of the matrix; will be NULL until it cached by setInverse()
    getInverse <- function() inverse
    
    ## get the internal functions of this cached matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse);
}



##
## Compute the inverse of a matrix.
##
## The inverse is computed once and cached.  Subsequent calls to this function will return
## the cached inverse, thus saving a lot of time.
##
cacheSolve <- function(x, ...) {    
        
    ## See if we have the inverse already computed and cached
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached inverse")
        return (inverse)
    }
    
    ## we don't, so compute it
    message("computing inverse")
    data <- x$get()
    inverse <- solve(data)
    
    ## cache it
    x$setInverse(inverse)
    
    ## return it
    inverse
}



