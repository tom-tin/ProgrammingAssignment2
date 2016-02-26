## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly 
## (there are also alternatives to matrix inversion 
## that we will not discuss here). 

## Your assignment is to write a pair of functions
## that cache the inverse of a matrix.


## A tip for this, create a matrix z, 
## than a <- makeCacheMatrix(z) and pass a to the cacheSolve


## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inve) inv <<- inve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# makeVector <- function(x = numeric()) {
#     m <- NULL # mean
#     set <- function(y) { # to set a vector x that take value y, mean is NULL
#         x <<- y 
#         m <<- NULL # because set() method: m is NULL and this goes up 1 level
#     }
#     get <- function() x # return x, no need {} since 1 line, where x is taken
#     # from the set method, since set() use x<<-, x will move outside to the
#     # makeVector environment, so that now get() can return x
#     setmean <- function(mean) m <<- mean
#     getmean <- function() m
#     list(set = set, get = get,
#          setmean = setmean,
#          getmean = getmean) # the makeVector func return this list of 4 ele
# }



## Computing the inverse of a square matrix can be done 
## with the solve function in R. (imagine solve() plays the role of mean())
## For example, if X is a square invertible matrix, 
## then solve(X) returns its inverse.
## For this assignment, assume that the matrix supplied is always invertible.

## cacheSolve
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

# cachemean <- function(x, ...) {
#     m <- x$getmean()
#     if(!is.null(m)) {
#         message("getting cached data")
#         return(m) # you need to exit the function, so need to use return()
#     }
#     data <- x$get()
#     m <- mean(data, ...)
#     x$setmean(m)
#     m # automatically return m
# }

## EXAMPLE FOR USSAGE:
## x <- matrix(1:4, 2, 2)
## a <- makeCacheMatrix(x)
## b <- cacheSolve(a)
## z <- matrix(c(1, 0, 0, 1), 2, 2)
## a <- makeCacheMatrix(x)
## b <- cacheSolve(a)
