# This first function creates a special matrix that can cache its
# inverse. Assume all matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

# This part computes the inverse of the matrix returned by makeCacheMatrix()
# This first part checks the existence if the inverse in memory
cacheSolve <- function(x, ...) {
        m <-x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        # Else calculate the inverse of the matrix

        inv_matrix <- x$get()
        m <- solve(inv_matrix, ...)

        # Update the cache with the new inverted matrix value

        x$setinverse(m)

        # Returm the inverted matrix
        m
}