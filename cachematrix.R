## Put comments here that give an overall description of what your
## functions do

## This basically creates the object/matrix used by cacheSolve below.  Can't really say I understand it but then again I'm only human.  But the matrix created
## here can cache its inverse.  It is basically a copy of the one given in the assignment.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(ym) {
        x <<- ym
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
}


## This one calculates the inverse of the special matrix created above.  It only does so if it needs to, that is the inverse does not exist already.
## This is checked int he is.null function.  I wish I had had this when doing my discreet and not so discreet mathematics back in 1985....

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
