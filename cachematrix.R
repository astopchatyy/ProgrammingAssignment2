## Implements matrix with cachable inversion operation
## functions do

## Makes matrix with cachable inversion

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setSolved <- function(solved ) s <<- solved 
	getSolved <- function() return(s)
	return(list(set = set, get = get,
		setSolved = setSolved, getSolved = getSolved))
}


## returns another CacheMatrix, that is inversion of argument matrix

cacheSolve <- function(x, ...) {
	if(!is.null(x$getSolved()))
	{
		return(x$getSolved())
	}
	s <- solve(x$get())

	res <- makeCacheMatrix(s)
	res$setSolved(x$get()) 
	x$setSolved(s)
	return(res)
}
