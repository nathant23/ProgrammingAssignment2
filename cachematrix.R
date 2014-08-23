
## Write a short comment describing this function

# The makeCacheMatrix function is passed a matrix as an argument.  
# The makeCacheMatrix stores the original matrix and returns a list which are functions.  
# The get() function will return the matrix that was passed as the original matrix (or reset by the set() function).
# The set function allows the user to change the matrix and resets the inverse.
# The setInverse stores a value in the inverse value.  This does not actually calculate the inverse it only stores a value.
# The getInverse returns the value stored in the inverse variable.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) inverse <<- inv
	getInverse <- function() inverse
	list(set = set, get = get,
	     setInverse = setInverse,
	     getInverse = getInverse)
}


## Write a short comment describing this function.

# The cacheSolve function is passed as an argument an object created by the makeCacheMatrix function.
# The function first checks if the inverse value is stored in the object passed as an argument.
# If the inverse value has already been stored it prints a message letting the user know it is being passed an already 
# calculated value and then returns the value.
# If the inverse has not already been stored it gets the matrix and calculates the inverse.
# It then stores the inverse value and then returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setInverse(inverse)
	inverse
}
