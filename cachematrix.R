## Script has 2 functions:
## 1. makeCacheMatrix is a function that takes a matrix as a parameter
##    and returns a vector of 4 functions:
##    a) getMatrix
##    b) setMatrix
##    c) getInvMatrix
##    d) setInvMatrix
## 2. cacheSolve returns the inverse matrix by either calculating it or retrieving it from cache.
##    It takes the vector of matrix function as a parameter

## This function creates a special "matrix" object that can cache its inverse.
## It takes a matrix as a parameter and returns 4 functions that:
## set the matrix, get the matrix, set the inverse matrix and get the inverse matrix
makeCacheMatrix <- function(xMtx = NULL) {
    invMtx <- NULL
    setMatrix <- function(pMtx) {
        xMtx <<- pMtx
        invMtx <<- NULL
    }
    getMatrix <- function() xMtx
    setInvMatrix <- function(pInvMtx) invMtx <<- pInvMtx
    getInvMatrix <- function() invMtx
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}


## Function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. 
## If the inverse has already been calculated the function will return the inverse matrix
## stored in cache, and if not, it will calculate the inverse matrix, store it in cache
## by calling setInvMatrix function from makeCacheMatrix function and return it
## It takes the result of makeCacheMatrix function (list of 4 function) as the inout parameter

cacheSolve <- function(funcVec, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMtx <- funcVec$getInvMatrix()
        if(!is.null(invMtx)) {
            ## Inverse matrix is already cached and we'll return it
            message("Getting cached inverse matrix")
            return(invMtx)
        }
        ## If inverse matrix was not already cached, we'll calculate it and return
        tempMtx <- funcVec$getMatrix()
        ## Calculate the inverse matrix
        invMtx <- solve(tempMtx, ...)
        ## Storing the inverse matrix in cache
        funcVec$setInvMatrix(invMtx)
        ## Return the calculated value.matrix
        invMtx
}