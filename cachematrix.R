## Pair of functions that cache the inverse of a matrix in order to not
## recalculate matrix over and over when inversion needed.
## NOTE: makeCacheMatrix(x) has to be called prior to cacheSolve(x).

## 1. Declaring a variable 'm', which will be used to save inverse matrix;
## 2. Obtaining "raw" matrix to be inverted;
## 3. Assigning computed inverse matrix to m;
## 4. Obtaining the cached inverse matrix.

makeCacheMatrix <- function(x=matrix()) {
        m <- NULL
        get <- function() x
        setInvMatrix <- function(InvMatrix) m <<- InvMatrix
        getInvMatrix <- function() m

        list(get=get, setInvMatrix=setInvMatrix, getInvMatrix=getInvMatrix)
}

## If inversion of matrix exists (m matrix), inverted matrix from cachewill be
## printed out, otherwise calculation of inverted matrix will be executed. 
## NOTE: argument x for this function must be cached, i.e. a list returned from
##       calling makeCacheMatrix(x).

cacheSolve <- function(x) {
        m <- tmp_vec$getInvMatrix()
        if(!is.null(m)){
                message("Cached data found. Getting result... Done.")
                return(m)
        }
        else {
                data <- tmp_vec$get()
                m <- solve(data)
                tmp_vec$setInvMatrix(m)
                return(m)
        }
}