## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix, similiar to makeVector, gets and sets the 
## original and inversed matrix with four basic functions:
##set the value of the matrix
##get the value of the matrix
##set the value of the inversed matrix
##get the value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
    
    xinv <- NULL ## stores the cached inversed matrix
    xchanged <- TRUE ## flag to control if the matrix has changed.
    ## the burden is that we have evaluate identical matrixes
    ## each time we set, but I assume is more efficient than inverse one.
    set <- function(y) {
        xchanged <<- identical(x,y) ## evaluates if the new matrix is different
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinverse <- function(inversed) {
        xinv <<- inversed
        xchanged <<- FALSE ## after a setinversed, automatically set the flag to not changed
    }
    getinverse <- function() xinv
    ischanged <- function() xchanged
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse,
         ischanged = ischanged)
}


## Write a short comment describing this function
## The following function calculates the inverse of the matrix 
## created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinverse()
    ## If there is a reversed matrix and the matrix has not changed, get cached
    if(!is.null(xinv) && !x$ischanged()) {
        message("getting cached data")
        return(xinv)
    }
    ## Else, inverse the matrix using function solve and set the inverse
    data <- x$get()
    xinv <- solve(data) 
    x$setinverse(xinv)
    xinv
}
