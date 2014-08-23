## Put comments here that give an overall description of what your
## functions do

## Make matrix to a cached matrix

makeCacheMatrix <- function(x = matrix()) {
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
