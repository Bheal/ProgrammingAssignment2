## Put comments here that give an overall description of what your
## functions do

# Ans:       1. Create varible by inputing a matrix to makeCacheMatrix().
#          Suppose you name this variable org_m.
        org_m <- makeCacheMatrix(rbind(c(-1 ,2, -3), c(2,1,0), c(4,-2,5)))
#       Then call cacheSolve on this variable to find the inverse matrix.
        cacheSolve(org_m)
#       First time this call computes the inverse (and returns it) and 
#       caches the resulting inverse to save on computation cost.



## Write a short comment describing this function

# Ans:       2. org_m is a list of 4, 4 functions.
#       i) org_m$get() has the stored matrix.
#       ii) org_m$getinverse is NULL, since i is set to null.  This can
#               change in 1 of 2 ways, either run cacheSolve(org_m), OR
#       iii) Or explicitly give the command org_m$setinverse('value').
#               If org_m$inverse() is changed explicitily, it will 
#               lead to the 'value' being set as inverse.
#       iv) Similarly org_m$set('new_matrix') can be changed without 
#               calling makeCacheMatrix(), this saves computation cost
#               while it will also set/reset i to NULL
#      v) the `<<-` assignment transfers the search for the variable to 
#               environment. Therefor both of the nested functions in
#               makeCacheMatrix transfer the assignment of x and i from
#               nested functions to their parent environment, which is
#               makeCacheMatrix function.
#               
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <- NULL
        }
        get <- function()x
        setinverse <- function(inverse) i<<-inverse
        getinverse <- function() i
        list(set=set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Write a short comment describing this function
# Ans:  3.  First time cacheSolve(org_m) is run i will be NULL, thus 
#       (!is.null(i)) will evaluate to FALSE and inverse is computed
#       which is then stored in the $setinverse(inverse) function of the
#       list org_m.  Thus if cacheSolve(org_m) is called again, !is.null(i),
#       evaluated to TRUE and message"getting cached data" and i are returned.
cacheSolve <- function(x, ...) {
                i <- x$getinverse()
                if(!is.null(i)) {
                        message("getting cached data")
                        return(i)
                }
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
                i
}

