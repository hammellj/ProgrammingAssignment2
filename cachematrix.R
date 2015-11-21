## A paired set of functions to create a matrix and cache for the inverse of said matrix. The second function tries to return the cached value of the function matrix, if it does not exist.
## Analogous to the vector function described in https://class.coursera.org/rprog-034/human_grading/view/courses/975108/assessments/3/submissions 


## Function holds a matrix (assigned in function), and the cached value of its inverse, set by the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## takes the makeCacheMatrix "object" and solves for the inverse. Returns the cached value if it exists.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
