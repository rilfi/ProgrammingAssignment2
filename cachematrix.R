##As you know caching is the common approach in data processing give 
##several performance advantages. This program all about generating 
##the inverse of an invertible matrix in an optimized way using casing. 
##Our program contains two functions which are makeCacheMatrix and cacheSolve.
##In first step you should create an invertible matrix object
##Then Create an object of a function makeCacheMatrix with argument as an 
##invertible matrix object
##finally you can store inverse matrix in a matrix variable/object 
##by calling the function cacheSolve using object of makeCacheMatrix created 
##in previous step
##Eg:
##  c=rbind(c(1, -1/6), c(-1/6, 1))
##  mcm<-makeCacheMatrix(c)
##  inmatrix<-cacheSolve(mcm)

##In this function it provide getter and setter for matrix and inverse matrix
##You should give an invertible matrix object as an argument.
##This function will return an instance of makeCacheMatrix fuction.

makeCacheMatrix <- function(x = matrix()) {
 	  i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This is the function creating inverse matrix, and checking the for whether
## The same objects existing or not. Essentially you should pass the object of 
## the makeCacheMatrix function. It will check whether that inverse matrix for 
## the given invertible matrix is available in cache then it will avoid inversing
## and return the inverse matrix from cache otherwise it will create the fresh inverse 
## matrix and will store in cache

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }	
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}
