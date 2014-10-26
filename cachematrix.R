## Author: Steve Burch  10/25/2014

##
## takes a matrix and builds a "special matrix", which is really
## as list of 4 functions, a matrix, and 2 "global" variables
##

## typical function calls
##
##  first, make some matrices
##    m1 <- matrix(1:4, 2,2)
##    m2 <- matrix(c(5,-1,3,19,67,0,2.1,4,-11), 3,3)
##
##  specialMatrix <- makeCacheMatrix(m1)
##   or, specialMatrix <- makeCacheMatrix(m2)


makeCacheMatrix <- function(x = matrix()) {

    
    oldM <- NULL            ## variable for previous matrix
    mInverse <- NULL        ## variable for matrix inverse
    
    set <- function(y) {
        
        ## do if/then if current matrix != previous
        ##   if newly passed in y matrix != oldM, save y in to oldM, clear mInverse
        if (!(is.matrix(oldM) && is.matrix(y) && dim(oldM) == dim(y) && all(oldM == y))) {

            message("matrices are NOT equal")
            
            oldM <<- y
            mInverse <<- NULL            
            x <<- y
        }
        else{
            
            message("matrices are equal")            
        }
    }
        
    get <- function() x
    getInverse <- function() mInverse
    setInverse <- function(inv) mInverse <<- inv
    
    list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}

##
## takes a "special matrix" 'x', extracts the matrix inside, calls set()
## in order to see if 'x' has changed since last call.
## gets the inverse of 'x' and if NULL, recomputes else uses cached value
## 
## example function calls
##
##  m1 <- matrix(1:4, 2,2)
##   or, m2 <- matrix(c(5,-1,3,19,67,0,2.1,4,-11), 3,3)
##
##  specialMatrix <- makeCacheMatrix(m1)
##  cacheSolve(specialMatrix)
##  repeat to read from cache:  cacheSolve(specialMatrix)
##       (then change matrix)
##  specialMatrix <- makeCacheMatrix(m2)
##  cacheSolve(specialMatrix)
##  repeat to read from cache:  cacheSolve(specialMatrix)

## returns the inverse of x$get()

cacheSolve <- function(x, ...) {
        
    m <- x$get()  ## get actual matrix from "special matrix"
    
    x$set(m)      ## pass it to set() to run old/new check, etc.
    
    inv <- x$getInverse()  ## inv should be in cache if matrix didn't change
    
    if (!is.null(inv)) {
        message("getting inverse from cached data")
        return(inv)
    }
    
	## cached inv is NULL, so calculate
    message("have to calculate inverse")
    inv <- solve(m)
    x$setInverse(inv)
    
    inv    
}
