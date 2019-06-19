## Functions to cache the inverse of a matrix

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly.
## Below are a pair of functions that cache the inverse of a matrix.
## Note that the matrix supplied is assumed to be always invertible.


## The first function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse. 
## The "matrix" is a list containing a functon to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. It first checks to see whether the inverse has been calculated (and the matrix has not changed). 
## If so, the cachesolve retrieves the inverse from the cache and skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets the value of the inverse in the cache via the setiverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {  ## check to see if the inverse has been calculated
        message("getting cached data")
        return(inv)  ## get the inverse from the cache
    }
    data <- x$get()
    inv <- solve(data, ...)  ## calculated the inverse
    x$setinverse(inv)   ## set the value of the inverse in the cache
    inv   ## return the inverse of the matrix
}

##This function will create a special matrix object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
	## Initialize the inverse property
	i <- NULL
	## Setting the matrix
	set <- function( matrix ) {
		m <<- matrix
		i <<- NULL
	}
	## Getting the matrix
	get <- function() {
		m
	}
	## Setting the inverse of the matrix
	setInverse <- function(inverse) {
		i <<- inverse
	}
	## Getting the inverse of the matrix
	getInverse <- function() {
		i
	}
	## Return a list of the methods
	list(set = set, get = get,
			 setInverse = setInverse,
			 getInverse = getInverse)
}


## This function will compute the inverse of the special matrix returned by the above function.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	## Return the inverse if its already set
	if( !is.null(m) ) {
		message("getting cached data")
		return(m)
	}
	## Getting the matrix from the object
	data <- x$get()
	## Calculating inverse using matrix multiplication
	m <- solve(data) %*% data
	## Setting the inverse to the object
	x$setInverse(m)
	## Returning matrix
	m
}
