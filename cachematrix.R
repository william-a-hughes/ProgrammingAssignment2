makeCacheMatrix <- function(x = matrix()) { #create function to store matrix and its mean; initialize x
	m  <- NULL #initialize m	
	set <- function(y) { #create function to assign the values of x and m in the parent environment
		x <<- y
		m <<-  NULL
	}
	get <- function() x #retrieve x from parent environment
	setinverse <- function(inverse) m <<- inverse 
	getinverse <- function() m
	#return functions within a list to the parent environment
	list(set = set, get = get,
		setinverse = setinverse, getinverse = getinverse)

}

cacheSolve <-function(x,...) { #create function to find inverse of matrix based on m (mean) value from makeCacheMatrix()
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m

   }
