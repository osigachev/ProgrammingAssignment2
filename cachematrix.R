## These functions cache the inverse (MatrInv) of a matrix (Matr) instead 
## of computing it repeatedly unless the matrix (Matr) is changed

## The following allows to get or modify the value of matrix (Matr)
## and to get or realculate the value of inverse (MatrInv)

makeCacheMatrix <- function(Matr = matrix()) {
        MatrInv <- NULL
        set <- function(X) {
                Matr <<- X
                MatrInv <<- NULL
        }
        get <- function() Matr
        setInvMatr <- function(X) MatrInv <<- X
        getInvMatr <- function() MatrInv
        list(set = set, get = get, 
             setInvMatr = setInvMatr, 
             getInvMatr = getInvMatr)        
}

## The following prints cashed inverse (MatrInv) of the matrix (Matr) or
## recalculates the inverse if the matrix has been changed

cacheSolve <- function(Matr, ...) {
        MatrInv <- Matr$getInvMatr()
        if (is.null(MatrInv)) {
                message('Getting Cached Data...')
                data <- Matr$get()
                MatrInv <- solve(data, ...)
                Matr$setInvMatr(MatrInv)
        } else {
                message('Printing Cached Inverse...')
        }
        return(MatrInv)
}
