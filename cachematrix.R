## The two functions that I have written work together to solve for and 
## cache the inverse of a matrix. The first function, 'makeCacheMatrix',
## takes a matrix and returns a special matrix object that provides the
## information needed to solve for and cache the inverse. The second function, 
## 'cacheSolve', returns the inverse of the matrix either by solving the inverse, 
## or by retrieving it from the cache if it has already been calculated.  

## 'makeCacheMatrix' is a function that takes an argument x - which is a matrix. 
## If no argument is given, the defualt is an empty matrix. The goal 
## of this function is to calculate and store the inverse of x, which it does 
## by executing the 'solveInverse' function. 'makeCacheMatrix' includes two 'get' 
## functions that each return matrices. This function also includes a 'set' 
## function that allows a user to re-set the value of x. 

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    ## creating a variable, inverse, and initializing its value to be NULL.
    
    set <- function(y = matrix()) {
        x <<- y
        inverse <<- NULL
    }
    ## creating a function called 'set', which takes an argument, y, and 
    ## re-assigns it to be x. Allows the user to re-assign a value to x and 
    ## clear the cached inverse variable. y also defaults to an empty matrix 
    ## if it is not specified.
    
    get <- function() {
        x
    }
    ## the function 'get' returns x.
    
    solveInverse <- function(...) {
        inverse <<- solve(x, ...)
        inverse
    }
    ## 'solveInverse' computes the inverse of x using the 'solve' function 
    ## included in R. The inverse returned from the 'solve' function is then 
    ## assigned to the inverse variable created in line 11. This inverse is 
    ## then returned. Additional arguments may be passed in.
    
    getInverse <- function() {
        inverse
    }
    ## 'getInverse' returns the inverse calculated by 'solveInverse.' 
    
    list(set = set,
         get =  get,
         solveInverse = solveInverse,
         getInverse = getInverse)    
}
    ## creating a list that stores the values of 'set', 'get', 
    ## 'solveInverse' and 'getInverse' under their respective names.


## The goal of 'cacheSolve' is to retrieve the inverse of a function 
## either by calling the 'solveInverse' function from line 30-33, or 
## by getting the cached data. It requires an argument, x, but takes 
## additional arguments if specified. The first time through, 'cacheSolve' 
## will assign to a variable, inverse, whatever 'getInverse' returns. 
## If it's NULL, 'cacheSolve' will call 'solveInverse' from 'makeCacheMatrix.' 
## 'solveInverse' uses the 'solve' function to calculate the inverse of x.
## This value is then returned. The second time through, 'cacheSolve' will 
## try to get the inverse value of x from 'getInverse' just like before. 
## Since the inverse has been calculated and returned, 'getInverse' will 
## return the actual matrix. Since 'getInverse' is not NULL, the console
## will display the message 'Getting Cached Data' followed by the inverse 
## of x. 

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    ## creating a new variable, inverse, and assigning the value to be 
    ## whatever the 'getInverse' function returns.
    
    if(!is.null(inverse)) {
        message("Getting Cached Data")
        return(inverse)
    }
    ## if the inverse variable is not NULL then the message 'Getting Cached 
    ## Data' will be displayed on the console. Following the message, the 
    ## inverse matrix will be returned.
    
    x$solveInverse(...)
}
    ## if the inverse variable is NULL, R will not execute the body of the if 
    ## statement. Instead, it will come to this line and call the 
    ## 'solveInverse' function from 'makeCacheMatrix.' The inverse matrix of 
    ## x will be computed and returned. 
