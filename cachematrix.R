## This is a matrix that cashes an inverse value.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
## These values (x and inv are my universal variables operating outside of
##the function. These will store the value and test against the NULL
                x <<- y
                inv <<- NULL
        }
## This is a short hand of a get <- function(){x} and 'gets' the x which is
## a universal value equal to 'y'
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## By making sure that 'setInverse and 'getInverse' are the same, the
## 'cashSolve' will return the inverse of X if the inv isn't NULL, else it
## does the new calc.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Cashe data comming your way Chummer")
                return(inv)
        }

## here's the code re-doing the calc if the cashe doesn't already
## have an answer and then writing it to inv and calling

        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}