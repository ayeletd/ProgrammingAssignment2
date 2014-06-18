## These functions allow you to create a matrix and cache it's inverse.
## Usage Example: 
## mm <- makeCacheMatrix(matrix(c(2,0,0,0.5), 2,2))
## mm$get()
## cacheSolve(mm)  // calculates the inverse
## cacheSolve(mm)  // returns cahced value
## mm$set(matrix(c(1,0,0,0.5), 2,2))
## cacheSolve(mm)  // calculates the inverse

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inv.mat) inv <<- inv.mat
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve would retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		# message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
