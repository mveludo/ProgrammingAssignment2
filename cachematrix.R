## These 2 functions are used to calculate and cache the inverse matrix
## Usage example:
## 		source('cachematrix.R')
## 		ma3=rbind(c(1,3),c(5,6))
## 		m3 <- makeCacheMatrix(ma3)
## 		cacheSolve(m3)  # First time calculates inverse
## 		cacheSolve(m3)	# Second time gets the inverse from cache

##
## This function creates a special "matrix" object that can cache its inverse
##
makeCacheMatrix <- function(x = matrix()) {
		i <- NULL				# set i (inverse matrix) to NULL 
        
		set <- function(y) {	# defines a function to set the matrix
                x <<- y
                m <<- NULL
        }
		
        get <- function() x		# returns the matrix 'x'
		
        setInverse <- function(inverse) i <<- inverse	# function to set the inverse matrix
		
        getInverse <- function() i 					    # returns 'i'
		
        list(set = set, get = get,	# returns the functions set, get, setInverse and getInverse
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## will retrieve the inverse from the cache or else it will calculate the inverse of the special "matrix".
cacheSolve <- function(x, ...) {
		m <- x$getInverse()   	# Try to get the inverse Matrix from cache
        if(!is.null(m)) {		# Return the inverse of the matrix if already cached
			message("getting cached data")
			return(m)		# returns the inverse matrix
	} 
		else 
		{
		## If the inverse matrix is not in cache, calculate the inverse Matrix and cache it.
			data <- x$get()    			# Get the Matrix
			m <- solve(data)			# Calculate the inverse Matrix using solve()
			x$setInverse(m)				# Set the inverse to cache
			m							# returns the inverse matrix
		}
}
