## Coursera Data Science: R Programming 
## Week 3 Assignment; GitHub user: StefanoSanfilippo


## This function creates a special "matrix" object that can cache its inverse
## First, initialize the objects "x" and "invmat" 
makeCacheMatrix <- function(x = matrix()) {     ## x is initialized as a function argument, so no further initialization is required inside the function.
        invmat <- NULL                          ## invmat is set to NULL, initializing it as an object inside the makeCacheMatrix() environment to be used by the subsequent code in the function.
        setmat <- function(y) {                 ## We define the setmat function to assign a new value of the matrix in parent environment,
                x <<- y                         ## Assign the value of NULL to the object invmat in the parent
                invmat <<- NULL                 ## environment. This line of code clears any value of m that has been cached by a previous makeCacheMatrix() run.
        }
        get <- function() x                                 ## makeCacheMatrix defines the getter for x. Since the symbol x is not defined inside get(), R fetches it
                                                            ## from the parent environment of makeCacheMatrix.
        setinverse <- function(inverse) invmat <<- inverse  ## assign the input argument to the value of invmat in the parent environment using "<<-".
        getinverse <- function() invmat                     ## gets the value of invmat where called
        list(setmat = setmat, get = get,
        setinverse = setinverse, getinverse = getinverse)   ## The last section of the code assigns each of these functions as an element within a list()                                                       
                                                            ## and returns it to the parent environment.
                                                            ## You need this in order to refer to the functions with the $ operator
}
        
        
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache
        
cacheSolve <- function(x, ...) {        ## cacheSolve() begins with a single argument, x, and an ellipsis that allows the caller to pass additional arguments to the function
        invmat <- x$getinverse()        ## cacheSolve calls the getmean() function on the input object. 
        if(!is.null(invmat)) {          ## Then check if the result is NULL. If the value here is not equal to NULL, we have a valid cached value and we can return it to the parent environment.
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...)
        x$setinverse(invmat)
        invmat
}

##testing the functions
B <- matrix(c(1/2, -1/4, -1, 3/4),2,2)
        
B1 <- makeCacheMatrix(B)
cacheSolve(B1) #inverse returned after computation, no message 
        
cacheSolve(B1) #inverse returned from cache and message is printed here
        
B2 <- makeCacheMatrix(-B)
cacheSolve(B1)
cacheSolve(B2)