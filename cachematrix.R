## Caching can speed up calculation by allowing a variable
## to be calculated 1x and then retreived later without needing
## to be recalculated
##
##These functions calculate the inverse of a matrix
## and then cache the output.  Later, you can access the
## cached inverse value, or it will calculate the inverse if 
## not calculated previously 

## The first function calculates the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## initialize m
  set <- function(y) {
    x <<- y
    inv <<- NULL
    ## set the value of the matrix (clear cache)
    ## Note that <<-refers to changes in parent 
    ## or global environment, <-to changes to local
    ## environment
    }
  get <- function() x
  ##get function, x is called by fxn, could put on another
  ##line if using {}
  setinv <- function(solve) inv <<- solve
  ##set inv, again, inv is called (& set by inv in global
  ##environment, could put in {})
  getinv <- function() inv
  ##get inverse; inv is called by fxn
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
##make output into a list that can be called by the 
## '$' operator
}

## The second function will grab the cached inverse result, 
## if calculated. Otherwise the inverse is calculated 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  ##get cached mean
  if(!is.null(inv)) {
    message("getting cached data")
    ##if not null, send message
    return(inv)
    ##and get m from cache
  }
  data <- x$get()
  ##otherwise get data
  inv <- solve(data, ...)
  ##calculate mean of data
  x$setinv(inv)
  ##sets value of mean in cache
  inv
  ##and returns m
}

##  Test functions

## x<-matrix((1:4),2,2)  #create matrix x

## x                     #print x
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4

## m_inv<-makeCacheMatrix(x) #set up

## cacheSolve(m_inv)         #then solve
## 1st run calculates inverse
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## cacheSolve(m_inv)         #run again
## 2nd run grabs cached data
##getting cached data
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## x<-matrix((4:7),2,2)      #create new matrix x
## x                         #print x
##      [,1] [,2]
##[1,]    4    6
##[2,]    5    7

## cacheSolve(m_inv)         #run again
## 3rd run grabs cached data from 'previous' inverse
##getting cached data
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## m_inv<-makeCacheMatrix(x) #set up using new data
                             #this clears the cache

##cacheSolve(m_inv)          # then run
##This run calculates a new matrix inverse
##     [,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2

##cacheSolve(m_inv)          #run again
##This grabs the cached value for the 'new' inverse
##getting cached data
##     [,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2
