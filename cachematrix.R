## This is a pair of functions that compute the inverse of a matrix

## The first function makeCacheMatrix
## It creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {

	## Initialize the inverse
    invs <- NULL

	## Set the matrix
    set <- function(matrix) {
            m <<- matrix
            invs <<- NULL
    }

	## Get the matrix
    get <- function() {
    	m
    }

    ## Set the inverse of the matrix
    setInverse <- function(inverse) {
        invs <<- inverse
    }

    ## Get the inverse of the matrix
    getInverse <- function() {
        invs
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The second function cacheSolve
## Compute the inverse of the matrix from the first function
## Check the following before computing the inverse
## (1) If the inverse has already been calculated
## (2) The maxtric has not been changed
## If so, retrieve the inverse from the cache

cacheSolve <- function(mtx, ...) {

    ## Return an inverse matrix 'mtx'
    m <- mtx$getInverse()

    ## Check if it is already set and return the value if true
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }

    ## Otherwise, get the Matrix mtx
    data <- mtx$get()

    ## Calculate the inverse using solve() function
    m <- solve(data) %*% data

    ## Set the inverse to mtx
    mtx$setInverse(m)

    ## Return the matrix
    m
}
