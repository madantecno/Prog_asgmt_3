## Program creates a two function to increase the evaluation time for inverse of a matrix
##by caching the result

## Creates CacheMatrix list which helps in caching the inverse matrix
##and retrieving when required

makeCacheMatrix <- function(x = matrix()) {
        
        inverse<-NULL
        
        set <- function(y){                                        ##function caches the value passed to it
                x<<-y
                inverse<<-NULL
        }
        
        get <- function() x                                        ##function Returns the Data set that was cached 
        setinverse<-function(temp_inverse) inverse<<-temp_inverse  ##Function Caches the inverse value passed to it
        
        getinverse<-function() inverse                             ##Function returns the cached inverse value
        getevn<- function() environment()                          ##Function returns the environment of the vector
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse,
             getevn = getevn)                                      ##Returns the list containing all that function which can be used 
        
}


## cacheSolve function returns the inverse of the matrix being passed to it
## It first check to see if the inverse is already available , If yes then returns it directly
## If the inverse is not present then calculates the inverse and caches the value and then returns

cacheSolve <- function(x, ...) {

        inverse<-x$getinverse()                                    ##gets the inverse currently cached for the matrix
        
        if(!(is.null(inverse))){                                   ##Checks if the inverse Cached is valid or Null
                message("Getting cached data")                     ##If Valid then returns the inverse
                return (inverse)
        }
        
        data<-x$get()                                              ##Retriving  the cached  Data set 
        
        inverse<-solve(data)                                       ##Calculate the inverse
        
        x$setinverse(inverse)                                      ##cache the result
        
        inverse                                                    ##return the result
        
}
