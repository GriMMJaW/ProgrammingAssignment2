## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
###This function creates the special vector which is a list of 4 functions set,get,setinverse and getinverse.
###In case trying to implement first run this function to create at special vector. Input to this function should be a inversible square matrix.
makeCacheMatrix <- function(x = matrix())
{
    inverse <- NULL
    set <- function(y) 
    {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {x}
    setinverse <- function() 
    {
        inverse <<- solve(x)
    }
    getinverse <- function() {inverse}
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
###This function first checks whether there is a special vector. If the above function is not executed correctly before executing this function an error will be thrown.
###This function further checks whether the inverse for the special vector is already calculated. If yes then it just access the cache to print the value. If no then it calculates the inverse using solve() function and prints the value.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        print(inverse)
    }
    else
    {
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse()
        print(inverse)
    }
}

##The above functions can be tested by implementing the following code.
### mydata is the input matrix for which the inverse is to be calculated.
### set.seed(2)
### mydata <- matrix(rnorm(4),2,2)

###Create the special vector
### a <- makeCacheMatrix(mydata)

### b <- cacheSolve(a)
###First Time on execution the following is obtained.
###       [,1]      [,2]
### [1,] -1.5692285 -2.204304
### [2,] -0.2566143 -1.245129

###Second Time on execution the following is obtained.

### getting cached data
###         [,1]      [,2]
### [1,] -1.5692285 -2.204304
### [2,] -0.2566143 -1.245129 


