## makeCacheMatrix creates a special matrix object to hold (cache) a copy of 
## 		the inverted matrix 
## cacheSolve returns the inverse of matrix x, the first time this function is 
## 		called it calculates the inverse of the matrix x and stores the result 
## 		to be returned on subsequent calls
## 		

makeCacheMatrix <- function(x = matrix()) {
	inverseX <- NULL
	set <- function(y) {
		x <<- y
		inverseX <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inverseX <<- inverse
	getInverse <- function() inverseX
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## return the inverse of matrix x created using the makeCacheMatrix function
## if a cached inverse is available return that, otherwise compute it and save 
## it in the cache for later
cacheSolve <- function(x, ...) {
	inverseX <- x$getInverse()
	## if null then not found in cache, compute & put in cache
	if (is.null(inverseX)) {
		inverseX <- solve(x$get())
		x$setInverse(inverseX)
	}
	return(inverseX)
}
