## Put comments here that give an overall description of what your
## functions do

## 'makeCacheMatrix' wraps matrix 'x' and
## returns list containing four functions:
## - set(y) -- change value of matrix
## - get() -- get value of matrix
## - set.inverse(new.inv) -- set inverse matrix
## - get.inverse() -- get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        set.inverse <- function(new.inv) {
                inv <<- new.inv
        }
        get.inverse <- function() {
                inv
        }
        list(set = set,
             get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}


## This function calculates inverse of wrapped matrix 'x',
## and tries to use cached result whenever it possible
## (e.g. we calculate it before, and matrix didn't change
## since that time)

cacheSolve <- function(x, ...) {
        inv <- x$get.inverse()
        if (!is.null(inv)) {
                ## Inverse is cached; using it
                ## print("Use cached argument")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$set.inverse(inv)
        inv
}
