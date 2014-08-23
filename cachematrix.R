## Matrix inversion is usually a costly computation.
## The following pair of functions can make a spectial matrix with its inversion cache.

## Make matrix to a cached matrix

makeCacheMatrix <- function(x = matrix()) {
		## Check if the input matrix is square
		if(nrow(x) != ncol(x)) {
				message("the input matrix must be square")
				return(NULL)
		}

		m <- NULL
		set <- function(y) {
				x <<- y
				m <<- NULL
		}

		get <- function() x
		setSolve <- function(solve) m <<- solve
		getSolve <- function() m

		list( set = set,
				get = get,
				setSolve = setSolve,
				getSolve = getSolve
		)
}


## Return the inverse of matrix by cache result if existed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
        		message("getting cached data")
        		return(m)
        }

        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)

        m
}
