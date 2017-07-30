## Put comments here that give an overall description of what your
## functions do

## Checks cache for extant matrix and either returns or creates matrix M

makeCacheMatrix <- function(M = matrix()) {
        W <- NULL
        set <- function(y) { #assigns values in cache
                M <<- y
                W <<- NULL
        }
        get <- function() M
        setW <- function(inverse) W <<- inverse
        getW <- function() W
        list(set = set, get = get,
             setW = setW,
             getW = getW)
}


## Checks cache for inverse of matrix M and either returns or generates inverse.

cacheSolve <- function(M, ...) {
        W = M$getW()
        if(!is.null(W)){ #checks for existing inverse mamtrix
                message("getting cached data")
                return(W)
        }
        mat.data = M$get() #crates matrix otherwise
        W = solve(mat.data, ...)
        M$setW(W)
        return(W)
}