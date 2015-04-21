## Following functions are used to create a special object that stores
## a matrix and cache its inversion

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	m_inversed <- NULL

    ## Store new matrix
	set <- function(y) {
		x <<- y
		m_inversed <<- NULL
	}

    ## Return stored matrix
	get <- function() {
        x
	}

    ## Store inversed matrix
    setinverse <- function(inversed) {
        m_inversed <<- inversed
    }

    ## Return inversed matrix
    getinverse <- function() {
        m_inversed
    }

	list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Check if there is inversed matrix calculated previously
    m_inversed <- x$getinverse()
    if(!is.null(m_inversed)) {
        ## Return cached inversed matrix
        message("getting cached data")
        return(m_inversed)
    }
    # Computes, caches, and returns the inverse of matrix 'x'
    m <- x$get()
    m_inversed <- solve(m)
    x$setinverse(m_inversed)
    m_inversed
}