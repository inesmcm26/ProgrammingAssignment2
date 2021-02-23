## This functions make it possible to go get the inverse of a matrix in
## cache instead of recalculating it, in order to save computational effort

## This function creates a special matrix, which is a list containing
## a function to set the value of the matrix; get the value of the matrix;
## set the value of the inverse matrix; get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    ## inverse is initialized NULL
    inv <- NULL
    
    ## create the object
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## get the object
    get <- function() x
    ## set inverse and get inverse "attribute"
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    ## return the special matrix object
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function calculates the inverse of a matrix. If it was stored in chache
## it gets the stores matrix and returns it. If not, calculates the inverse matrix
## and saves it in cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    
    ## getting object
    data <- x$get()
    inv <- solve(x)
    ## storing inverse matrix in cache
    x$setinverse(inv)
    inv
}
