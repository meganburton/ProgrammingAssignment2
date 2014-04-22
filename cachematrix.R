## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y = matrix()) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    solveInverse <- function() {
        inverse <<- solve(x)
        inverse
    }
    
    getInverse <- function() {
        inverse
    }
    
    list(set = set,
         get =  get,
         solveInverse = solveInverse,
         getInverse = getInverse)    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
