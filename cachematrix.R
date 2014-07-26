## These functions enable the caching of matrix inversion
## result so that the inversion computation does not have
## to be done if a particular matrix has already been
## inverted.

## This function creates a special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        invM <- NULL
        set <- function(y) {
                x <<- y
                invM <<- NULL
                
        }
        get <- function() x
        setInv <- function(solve) invM <<- solve
        getInv <- function() invM

        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special matrix created by
##  the previous function. If the inverse already claculated then retrieves
##  the cached inverse instead of re-calculating.

cacheSolve <- function(x, ...) {
        # Check to see if inverse already calculated. If yes, then just return cached inverse
        invM <- x$getInv()
        if(!is.null(invM)) {
                message("returning cached data")
                return(invM)
        }
        
        #If inverse was not already calculated then need to do the calc and return the inverse
        data <- x$get()
        invM <- solve(data, ...)
        x$setInv(invM)
        invM
        
}

## Some other comments for my own understanding of these functions
##   - when call makeCacheMatrix passing it a matrix M, it stores the matrix. It does not calculate
##   -   the inverse, just a function to calculate the inverse. h <- makeCacheMatrix(M), then
##   -   h$get() is the stored matrix.

