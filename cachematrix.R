## These two functions cache the inverse of a matrix making it available to be retrieved
## without any further calculation

## This function creates a special "matrix" object, i.e. a list of functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(x) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinverse <- function(solveMatrix) n <<- solveMatrix
        getinverse <- function() n
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function calculates the inverse fo the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        n <- x$getinverse()
        if(!is.null(n)) {
                message("Returning cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setinverse(n)
        n
}

## Verifiying that the functions work

set.seed(50)
my_mat <- matrix(round(runif(36, 0, 100),0), nrow = 6, ncol = 6)
my_mat

my_mat_cache <- makeCacheMatrix(my_mat)
cacheSolve(my_mat_cache)