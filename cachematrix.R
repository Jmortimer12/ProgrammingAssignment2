## This script is has two functions which will first create a list in order to store
##  the functions needed to create a matrix, set the value as the inverse of a 
## matrix, i.e., cache it the data, and finally fetch the data when necessary.
## The second function will either pull the cached data or calculate the 
## inverse matrix and cache it. 

## This matrix creates a list with the following functions
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { 
         ##creates a special matrix to capture the inverse
         MI <- NULL
         set <- function(y) {
                  x <<- y
                  MI <<- NULL
         } 
         get <- function() x
         setinverse <- function(Minverse) MI <<- Minverse
         getinverse <- function() MI
         list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## x is the matrix we will input to calculate the inverse

x <- matrix(1:4, 2, 2) 

## MCM stores the list created running makeCacheMatrix with x as an argument

MCM <- makeCacheMatrix(x)

##Cache Solve will retrieve the stored cache and check if it is equal to NULL
## if it isn't, the function will print the message and retreive the inverse
#if it is equal to NULL, the function will retrieve the data, calculate the inverse,
# Store it and then print it

cacheSolve <- function(x) {
         MI <- MCM$getinverse()
         if(!is.null(MI)){
                  message("getting cached data")
                  return(MI)
         }
         data <- MCM$get()
         MI <- solve(data)
         MCM$setinverse(MI)
         MI
}