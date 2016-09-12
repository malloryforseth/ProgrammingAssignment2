
## makeCacheMatrix is a function that creates and returns a list of
## functions including: set - sets the matrix, get - gets the matrix,
## setinverse - sets the inverse of the matrix,and getinverse- gets 
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y 
		inverse <<- NULL
	}
	get <- function () x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list (set=set, get=get, 
			setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve is a function that searches the cache for the inverse of
## a matrix. If the inverse is found in the cache, the function 
## returns the value from the cache. If the inverse is not in the 
## cache, the function calcuates the inverse of the matrix, adds it to
## the cache, and returns it. 

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
	if(!is.null(inverse)){
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data)
	x$setinverse(inverse)
	inverse
}

