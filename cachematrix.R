#Create an matrix object with cached inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
		# only clear cache when y != x
        set <- function(y) {
			if(!identical(x, y))
			{
                x <<- y
                inverse <<- NULL			
			}
        }
        get <- function() x
        setinverse <- function(z) inverse <<- z
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# Try get inverse from a makeCacheMatrix object.
# If the cache doesn't hit, calculate and save the result to cache, so that next time, cache can be hit.
cacheSolve <- function(x) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
