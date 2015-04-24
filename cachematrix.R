# These functions, used together, are an elaborate way to invert a matrix
# 
# To use these functions, define a matrix of interest, e.g.:
#
#    thing1<-matrix(c(1,2,3,4),nrow=2,ncol=2)
#
# Example call:
#
#    a<-makeCacheMatrix (thing1) 
#       This defines a list of functions:
#         a$set() which sets 'thing1'
#         a$get() which returns 'thing1'
#         a$setinv() which solves 'thing1' by means of 'cacheSolve'
#         a$getinv() which returns whatever the last call of a$setinv() returned
#    cacheSolve (a) 
#       First call - returns the calculated inverse of 'thing1'. 
#    cacheSolve (a)
#       Second call - returns whatever the first call got (without recalculating)

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
