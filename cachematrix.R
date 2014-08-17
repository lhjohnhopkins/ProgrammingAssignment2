## Matrix inversion often uses a lot of computer time.
## These routines will invert and cache the inverted
## matrix avoiding the need for repeatedly inverting
## the same matrix over and over.
##
## To use:
## 1. Call the makeCacheMatrix() function optionally
##    with the matrix to be inverted (solve()) and cached.
##    save the return object in a variable.
## 2. If you didn't supply the matrix to be inverted
##    in step 1 above, supply it in the $set()
##    method of the returned object.
## 3. Return the inverted matrix by calling the
##    cacheSolve() function with the returned object
##    created in step 1 above.
##
## Methods to the returned object are:
##    $set(x)    set a (new) matrix
##    $get()     get the saved matrix
##    $setInv(x) set the inverted matrix (done by cacheSolve())
##    $getInv()  get the inverted matrix (will not be valid until cacheSolve()
##               is called at least once after $set() or makeCachematrix())
## Note that the setInv and getInv are not intended to be called by a user.
##
## NOTE: This is a programming assignment function and
## DOES NOT contain production required functionality.
## Specifically:
##  All passed matrices are assumed to be invertable.
##  No error checking is performed.
##  No optimzations have been considered.

## Create a cached matrix object
## cacheMat : the matrix (will be cached), default to an "empty" matrix
## returns an object with the cached matrix and a set of access methods
## for the cached matrix and it's inverse.
makeCacheMatrix <- function(cacheMat = matrix()) {
        cacheSol <- NULL    ## inverse is not (yet) cached

        ## save a new matrix
        set <- function(cacheMat) {
                cacheMat <<- cacheMat  ## set parent environment values
                cacheSol <<- NULL      ## as above, inv is not (yet) cached
        }

        ## get a saved matrix
        get <- function() {
                cacheMat   ## return cached matrix
        }

        ## set the solve()'ed matrix
        ## this should only be called by cacheSolve() or bad things
        ## are likely to happen
        setSol <- function(cacheSol) {
                cacheSol <<- cacheSol  ## set parent environment value
        }

        ## get the inverse matrix
        ## returns NULL if cacheSolve() has not calculated an inverse (yet)
        getSol <- function() {
                cacheSol
        }

        ## return the methods as a list, callable as CMObj$set(x) etc.
        list(set=set,
             get=get,
             setSol=setSol,
             getSol=getSol)
}


## solve() a cached matrix and cache the solve()'ed value
## CMObj : the cache matrix object
## ...   : optional paramaters passed to solve()
## returns solve() on CMObj's matrix, from cache if possible
cacheSolve <- function(CMObj, ...) {
        solMat <- CMObj$getSol()
        if(!is.null(solMat)) {
                ## Return the cached inverse
                return(solMat)
        }

        ## solve()'ed value is not cached, calculate it and cache it
        solMat <- solve(CMObj$get(), ...)
        CMObj$setSol(solMat)   ## cache it
        solMat  ## and return it
}
