## makeCacheMatrix creates a list of functions that allow
## us to build matrices and store and retreive their inverses easily.
## Using R's lexical scoping ability, we are able to set
## 'inv' as a variable within our makeCacheMatrix function,
## and have functions defined within the makeCacheMatrix environment
## continue to refer to these variable values no matter how/when
## they are called.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    ## 'set' function takes a matrix object and assigns it
    ## to the 'x' value within the makeCacheMatrix
    ## environment
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## 'get' function takes the 'x' value within the
    ## makeCacheMatrix environment and returns it
    get <- function() x
    
    ## 'setinv' takes the value in 'newinv' and assigns
    ## it to the 'inv' variable within the
    ## makeCacheMatrix environment
    setinv <- function(newinv) inv <<- newinv
    
    ## 'getinv' takes the 'inv' value within the
    ## makeCacheMatrix environment and returns it
    getinv <- function() inv
    
    ## we return a list of these four functions
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve takes the list of functions created within
## makeCacheMatrix, and uses them to refer back to the
## values stored within the makeCacheMatrix environment.
## This allows us to check first to see if the inverse has already
## been stored away (i.e. cached) within the makeCacheMatrix
## environment, rather than repeatedly computing the inverse
## which can be computationally expensive. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    # test to see if we have a previously stored value of 'inv'
    # if so, we return it and exit the cacheSolve function
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, we take our matrix data, calculate
    # the inverse using solve(), and then store it away
    # in the 'inv' variable within the makeCacheMatrix
    # environment so that we don't have to calculate it
    # again. 
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}

## after running x <- makeCacheMatrix(some_matrix)
## and cacheSolve(x), the following must hold true:
## round(x$getinv %*% x$get) == diag(10)
## for every element in diag(10).
