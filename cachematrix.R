#the functions will be used to create a special matrix
#..and calculate a inverse matrix, checking before if it was already calculated

#this function creates the special matrix
makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

#this function checking is was already calculated the inverse matrix
#..if it has already been created, returns message warning
#..if it wasn't created, creates the inverse matrix
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
