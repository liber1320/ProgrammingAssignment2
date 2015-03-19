makeCacheMatrix <- function(x = matrix()) { 		#initialization, argument is a matrix

 	m <- NULL					#setting m to null
  	set <- function(y) {				#argument y is passed to makeCacheMatrix
        	x <<- y
        	m <<- NULL
        }
	get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m    	
        list(set = set, get = get,			#the values are listed out
        	setsolve = setsolve,
        	getsolve = getsolve)
}

cacheSolve <- function(x, ...) {  	#function which requires makeCacheMatrix object

	m <- x$getsolve() 		#getting inverted matrix from makeCacheMatrix
        if(!is.null(m)) { 		# if m is not null, get value from  makeCacheMatrix
                message("getting cached data")
                return(m)
        }
        data <- x$get()			# otherwise calculate value after obtaining matrix 
        m <- solve(data, ...)
        x$setsolve(m)			#assignment m value to makeCacheMatrix m
        m				#returns inverted matrix
}
