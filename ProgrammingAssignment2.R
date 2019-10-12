## 2 functions that can create a makeCacheMatrix object that can store its value, calculate
## the stored matrix's inverse, as well as store the value of its inverse.

## makeCacheMatrix makes an object that contains 4 functions and is capable of storing its value
## and the value of its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(mat)
        {
                x <<- mat
                m <<- NULL
        }
        get <- function() 
        {
                x
        }
        
        setInverse <- function(inv) 
        {
                m <<- inv
        }        
        getInverse <- function() 
        {
                m
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        

}


## cacheSolve can be passed a makeCacheMatrix object and can use the object's functions to 
## calculate the inverse of the matrix stored within the object. Then cacheSolve can be used
## to command the makeCacheMatrix object to store its own inverse or retrieve it.

cacheSolve <- function(x, ...) 
{
        inv<-x$getInverse()
        if (!is.null(inv))
        {
                message("getting cached data")
                return(inv)
        
        }
        mat<-x$get()
        inv<-solve(mat)
        x$setInverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'

        
}
