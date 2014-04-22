## *makeCacheMatrix* and *cacheSolve* are the functions that allow to create an 
## inverse of a matrix. *makeCacheMatrix* creates a list with the functions 
## that allow to re-set the value of the cached matrix (set), a function giving 
## back the original matrix (get), the function that allows to save the inverse 
## matrix (setinverse) and the function accessing the inverse matrix 
## (getinverse). 
## *cacheSolve* uses the *makeCacheMatrix* as input and calculates the inverse 
## matrix of the original matrix. Being runned repeatedly it checkes if the 
## previous inverted matrix is still cached in *makeCacheMatrix* and then takes 
## this cached inverse matrix instead of making the calculations and gives the 
## message "getting cached data". However, if the matrix to be inverted has been 
## changed since the last run of *cacheSolve* the calculation of the inverse 
## will be produced and cached in *makeCacheMatrix* 

## *makeCacheMatrix* is the input function for *cacheSolve*. The input of this 
## function must be a matrix, it should be a square matrix to be inversible. 
## Example: i <- matrix(1:4, 2, 2)
##          a <- makeCacheMatrix(i)
##          a$get() shows i
##          a$set(i1) allows to change the original matrix
##          Once the function *cacheSolve* been run a$getinverse() shows the 
##          cached inverse matrix
##          a$setinverse() is used by *cacheSolve*

makeCacheMatrix <- function(x=matrix(, nrow = 0, ncol = 0)) {
    if(!is.matrix(x)) {
        message("x is no matrix")
    } 
    ## This is just a check if input is a matrix and a message if it is not.
    if(identical(nrow(x), ncol(x))) {} else {
        message("x must be square to be invertible. Do not apply *cacheSolve* 
                for this matrix")
    } 
    ## This part checks if input is a square matrix and gives a message if it is
    ## not as not square matrices are not invertible. The funktion still makes 
    ## the cache but it warns so the user do not apply *cacheSolve*
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    }

##*cacheSolve* uses the *makeCacheMatrix* as input and calculates the inverse 
## matrix of the original matrix. Being runned repeatedly it checkes if the 
## previous inverted matrix is still cached in *makeCacheMatrix* and then takes 
## this cached inverse matrix. If the matrix to be inverted has been changed 
## since the last run of *cacheSolve* the calculation of the inverse will be 
## produced and cached in *makeCacheMatrix* 
## Example:     b <- cacheSolve(a)

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
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
