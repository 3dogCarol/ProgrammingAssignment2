## Put comments here that give an overall description of what your
## functions do
## The problem solved by this code is saving a matrix inverse in cache for future use.

## Write a short comment describing this function
## The first function generates 4 other functions used to cache the inverse and the matrix.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	is.matrix(x)
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(xinv) inv <<- xinv
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)	
}


## Write a short comment describing this function
## The second function either computes the inverse if necessary or finds the inverse cached in memory.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
   	inv <- x$getinv()
	if(!is.null(inv)){
		message("getting cached inverse")
		return(inv)
	}
	data <- x$get()
	print(data)
	inv <- solve(data,...)
	x$setinv(inv)
	inv
}
