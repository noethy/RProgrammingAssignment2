## This funciton creates a special 'matrix' object that can cache its inverse.
## It uses the <<- operator such that the value assigned to an object is not exposed to the outside environment. 

## This function creates a list containing functions to set, get, setinv and gatinv. 

makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL  # restuls of inversion is stored in xinv
	set <- function(y){  # set function, to set a matrix to an object
		x <<- y
		xinv <<- NULL
	}
	get <- function() x  # return input matrix
	setinv <- function(inv) xinv <<- inv  # set inversion
	getinv <- function() xinv  # return to inversion of input matrix
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)   # return a list that contains these four functions
}


## This function computes the inverse of the special 'matrix' returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        xinv <- x$getinv() # return a matrix that is the inverse of object x
        if(!is.null(xinv)){   # if inversion exists
        	message("getting caches data")
        	return(xinv)  # return cached inversion
        }
        data <- x$get()  # otherwise, get matrix object by using x$get
        xinv <- inv(data, ...)  # solve it by using inv
        x$setinv(xinv)  # set the results to the object
        xinv  # return the solved results
}
