## THe second programming assignment will require you to write 
## an R function is able to cache potentially time-consuming computations. 

## For example, taking the mean of a numeric vector is typically
## a fast operation. However, for a very long vector, it may take too long
## to compute the mean, especially if it has to be computed repeatedly
## (e.g. in a loop). 

## If the contents of a vector are not changing, it may make sense
## to cache the value of the mean so that when we need it again, 
## it can be looked up in the cache rather than recomputed. 

## In this Programming Assignment will take advantage 
## of the scoping rules of the R language and how they can be manipulated
## to preserve state inside of an R object.

## Example: Catching the mean of a vector
## In this example we introduce the <<- operator which can be used 
## to assign a value to an object in an environment 
## that is different from the current environment. 
## For ex: x <<- y
## It's the 'superassignment' operator.  It does the assignment in the 
## enclosing environment. That is, starting with the enclosing frame, it 
## works its way up towards the global environment until it finds a 
## variable called x, and then assigns to it. If it never finds 
## an existing x, it creates one in the global environment. 

## The good use of superassignment is in conjuction with lexical scope, 
## where an environment stores state for a function or set of functions 
## that modify the state by using superassignment.
## Example of the good use is:

# make.accumulator<-function(){ 
#     a<-0 
#     function(x) { 
#         a<<-a+x 
#         a 
#     } 
# } 
# 
# > f<-make.accumulator() 
# > f(1) # 1st call, a is 0, x is 1, then a get 0+1=1 and this go up so a=1
# [1] 1 
# > f(1) # 2nd call, where a is already 1, so after this a=2
# [1] 2 
# > f(11) 
# [1] 13 
# > f(11) 
# [1] 24 

## The Evil and Wrong use is to modify variables in the global environment. 

## 

## Below are two functions that are used to create a special object
## that stores a numeric vector and cache's its mean.

## The first function, makeVector creates a special "vector", 
## which is really a list containing 4 functions:

## 1. set the value of the vector --> use function get
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeVector <- function(x = numeric()) {
    m <- NULL # mean
    set <- function(y) { # to set a vector x that take value y, mean is NULL
        x <<- y 
        m <<- NULL # because set() method: m is NULL and this goes up 1 level
    }
    get <- function() x # return x, no need {} since 1 line, where x is taken
    # from the set method, since set() use x<<-, x will move outside to the
    # makeVector environment, so that now get() can return x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean) # the makeVector func return this list of 4 ele
}

## The following function calculates the mean 
## of the special "vector" created with the above function. 
## However, it first checks to see if the mean has already been calculated.
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data
## and sets the value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m) # you need to exit the function, so need to use return()
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m # automatically return m
}


## HOW TO USE THIS:
## First, creat a normal vector, say x <- c(1,2,3)
## Then feed x to the makeVector function to get a special vector b:
## b <- makeVector(x)
## then use c <- cachemean(b), then c will be the mean of the normal vector x