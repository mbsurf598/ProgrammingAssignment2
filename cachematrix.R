## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

## Initialize the inverse property
    i <- NULL

    ## This is the method I used to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## This is the method I used the get the matrix
    get <- function() {
     ## This will return the matrix
     m
    }

    ## This is the method I used to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## This is the method I used to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## This will return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {

    ## This will return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## This will return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Setting the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}
