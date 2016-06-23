makeCacheMAtrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x 
        setinv<- function(inverse) inv <<- inverse
        getinv<- function() inv
        list(set = set, get = get,
             setinv= setinv,
             getinv = getinv)
        
}
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}
        
