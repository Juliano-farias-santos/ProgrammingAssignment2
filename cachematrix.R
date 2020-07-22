## Some matrix algebra computations are notoriously expensive, such as 
## calculating the inverse of a matrix. This functions take advantage of
## R scoping rules in order to make calculations faster. 

## This functions works the criteria of caching in R. 

## The first function creates an R object that stores a matrix and its
## inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## initializing objects: x is initialized as function argument and
    ## m is set to NULL, if it's necessary to store a new value for x
    ## this command will clean the last m's value. 
    set <- function(y = matrix()) {
        x <<- y
        m <<- NULL
    }
    ## set function saves the objects x and m in the parent environment
    ## in this way, once the functions is called, the variables defined
    ## in the body of fucntion won't be lost.
    get <- function() x
    ## this function takes advantage of the lexical scoping features in 
    ## in R. Since the symbol x is not defined within get, R recovery it
    ## from the parent environment of makeCacheMatrix
    setinverse <- function(solve) m <<- solve
    ## Since m is defined in the parent environment, to access it after
    ## setinverse completes, it's need to use the <<- operator to 
    ## modificate its value in parent environment.
    getinverse <- function() m
    ## recovery the value of m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    ## list the elements in the function's body is a way to access the
    ## saved objects easily
}


## The second function requires an argument that is returned by 
## makeCacheMatrix in order to retrive the inverse from the cached value
## that is stored in makeCacheMatrix object's environment. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        ## retrieves the value of x kept in the output object of 
        ## makeCacheMatrix
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        ## if m alread has a value saved inside it, the function returns
        ## the message: "getting cached data".
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
