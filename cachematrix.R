## This function creates a special "matrix" object that can cache its inverse, and have the required 
## functions to store and recover the original matrix, or its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
	set <- function (y)
	{
		x <<- y
		inv <<- NULL
	}
	
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function () inv
	list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function uses the object created with makeCacheMatrix function, and have two objetives:
##   1. Verify if the original matrix have inverse. If it haven't, the function stops and show a message.
##   2. If the original matrix have inverse, returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## A matrix have inverse if it checks two conditions:
	##  1. It is a square matrix (number of columns = number of rows)
	##  2. Its determinant is different of 0
	mtx <- x$get()
	if (ncol(mtx) != nrow(mtx))
		return(print("This matrix haven't inverse"))
	else if (det(mtx)==0)
		return(print("This matrix haven't inverse"))
	
	# If x matrix have inverse, the function calculates it
	inv <- x$getinv()
	if (!is.null(inv))
	{
		message("getting cache data")
		return (inv)
	}
	
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
