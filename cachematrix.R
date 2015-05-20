## These fuctions enable the inverse of a matrix to be cached.

## This takes a matrix as an argument and returns a matrix of functions
## that can performed on the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    matrix(c(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse))
}

## This takes the makeCacheMatrix matrix as an argment.
## This checks to see if the inverse has been cached yet,it so it returns it
## else it caluculates the inverse, caches it, then returns it.

cacheSolve <- function(x, ...) {
    m <- x[[4]]() ##checks for chached data
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    x[[3]](solve(x[[2]]())) ## calls function 3 (set inverse) passing the inverse
    x[[4]]()## calls function 4 (get inverse)
        ## Return a matrix that is the inverse of 'x'
}
