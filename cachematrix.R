## The functions will hel in calculating the inverse of a matrix and cache the result
## This will help in fast retreival of already calculated inverses from the cache

## Creates a matrix object that can cache its invers

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x

	setinverse <- function(inverse) inv <<- inverse

	getinverse <- function()inv
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse for new matrix and retrieve the inverse of already existing matrices from cache

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached inverse")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
