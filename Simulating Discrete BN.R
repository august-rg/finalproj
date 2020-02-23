# load in the BN learn library

library("bnlearn")


#--------------------creating a simple BN structure by hand---------------------


test <- empty.graph(c("sex", "age", "companion", "eats"))
plot(test)


# we now want to add in an edge set
# we need to put the arcs into a matrix in order to do this
# take a look at the edge object to see what it looks like
# it is a 2*4 matrix with column names specifying where the arc starts and ends
edges <- matrix(c("sex", "eats",
                  "age", "eats",
                  "companion", "age",
                  "companion", "eats"),
                byrow = TRUE, ncol = 2,
                dimnames = list(NULL, c("from", "to")))


# below we assign our edges to the graph
# in our graph object, edges are reffered to as arcs
arcs(test) <- edges

# plot it to see that it did what we wanted
# note that R by default makes the graph directed
plot(test)

# now comes the challenging part, we want to use Bayes Theorem to store
# the conditional probabilities between edges
# I think that with data, we could tell R to learn the probabilities
# with the available data
# we want to create the conditional probability table by hand now
# we need to store the states of each node into individual objects

sex <- c("boy", "girl")
age <- c("<3", ">3")
companion <- c("yes", "no")

# this is as far as we were able to get on creating a graph by hand before
# getting highly confused by conditional probability... much more to come


# link below to how to create conditional probabilities by hand for a BN

#https://bookdown.org/robertness/causalml/docs/tutorial-probabilistic-modeling-with-bayesian-networks-and-bnlearn.html


#-------------------simulating marshmallow dataset------------------------------

# our goal below is to simulate a data set using the arbitrary conditional
# probabilities that we created above. From this data set, we will use
# the hill-climbing algorithm to learn the structure of the network

# variable names for our nodes
# each variable will have two factors
# sex: girl or boy
# age: <3 or >3
# companion in room: yes or no
# Eats marshmallow before 10 min: yes or no


# below we will make an empty dataframe
marshmallows <- data.frame(rep(NA, 100))

# now we can fill in our columns one by one

# first we put in the sex of the child, note that this is not randomized
# not really necessary since we will randomize all the other columns
marshmallows$sex <- append(rep("girl", 50), rep("boy", 50))

# lets take out the NA column
marshmallows$rep.NA..100. <- NULL

# now we want to build in new columns one by one
# lets build in the age column, we want in total for about half of the children
# to be older than three and half to be less than three, randomized
marshmallows$age <- sample(append(rep("<3", 50), rep(">3", 50)), replace = FALSE)

# lets add wheteher or not they have a companion, also randomized
marshmallows$companion <- sample(append(rep("yes", 50), rep("no", 50)), replace = FALSE)


# finally, we want to add in the conditional probability
# we will make an if statement for all of the possible outcomes
# this will be a conditional probability and will determine whether
# the child eats the marshmallow before 10 minutes or not
# when we make our network, we can check that the conditional
# probabilities match up with what we tell R 

# we make the empty column for outcome which we will populate using an if statement
# with "yes" or "no" to indicate if the ate their marshmallow before 10 minutes
marshmallows$outcome <- rep(NA, 100)

head(marshmallows)


#--------------Helper functions for populate.outcomes---------------------------

# we can make a function that tells us the length of the subset that we want
# we need to know this in order to simulate our data
# x takes the sex, y takes age, and z takes companion
subset.size <- function(x, y, z){
  length(marshmallows$sex[marshmallows$sex == x 
                          & marshmallows$age == y 
                          & marshmallows$companion == z])
}


# example
subset.size("boy", ">3", "no")



# takes vector length n, proportion p, two binary arguments x and y
# returns a vector of the speccified length with the count of x reflecting
# the proprtion p and the count of y reflecting the proportion 1-p
# if the resulting vector comes out longer than the specified length n
# the function only returns the first n terms
prop.vector <- function(n, p, x, y){
  x.count <- rep(x, round(n*p))
  y.count <- rep(y, round(n*(1-p)))
  return((sample(append(x.count, y.count)))[1:n])
}

prop.vector(10, 0.3, "boy", "girl")

#------------------main populate.outcomes function------------------------------


# in this case, we just want for R to leave the value alone if it is not specified
# therefore, we only need if statements and no else statements
# not to self: else statements are just added after each curly bracket
# also recall that else statements/commands are inside {}
# this loop tells R to replace put "yes" in marshmallows$outcome if all
# three conditions are met else do nothing
# this loop will be the foundation for populating the outcome
# column once we figure out how to simulate with conditional probability
populate.outcomes <- function(x, y, z, p){
  for(i in 1:length(marshmallows$sex)){
    if(marshmallows$sex[i] == x){
     if(marshmallows$age[i] == y){
       if(marshmallows$companion[i] == z){
        marshmallows$outcome[i] <- prop.vector(subset.size(x, y, z), p, "yes", "no")[i]
        print(prop.vector(subset.size(x, y, z), p, "yes", "no")[i])
        }
      } 
    } 
  }
}
 

populate.outcomes("boy", ">3", "no", 0.6)

# current problem: having the prop.vector inside of for loop makes a vector
# that is what we want kinda for first two entries but then is a bunch of NA values
# need to debug this so that we have full vector to select from 


















#----------------messing around-------------------------------------------------

prop.vector(10, 0.6, "yes", "no")[3]

populate.outcomes("girl", "<3", "no", 0.6)

marshmallows$outcome

# we can make a function that tells us the length of the subset that we want
# we need to know this in order to simulate our data
# x takes the sex, y takes age, and z takes companion
subset.size <- function(x, y, z){
  length(marshmallows$sex[marshmallows$sex == x 
                   & marshmallows$age == y 
                   & marshmallows$companion == z])
}


# takes vector length n, proportion p, two binary arguments x and y
# returns a vector of the speccified length with the count of x reflecting
# the proprtion p and the count of y reflecting the proportion 1-p
# if the resulting vector comes out longer than the specified length n
# the function only returns the first n terms
prop.vector <- function(n, p, x, y){
  x.count <- rep(x, round(n*p))
  y.count <- rep(y, round(n*(1-p)))
  return((sample(append(x.count, y.count)))[1:n])
}


prop.vector(subset.size(x, y, z), p, "yes", "no")


test <- prop.vector(100, 0.6, "yes", "no")

fire <- rep(NA, 10)
for(i in 1:10)
  fire[i] <- test[i]

foo <- sample(append(rep("yes", 60), rep("no", 40)))[1]










# populating the outcome column will be difficult to code since we need it
# to reflect conditional probabilities. We are still figuring this out. We
# will probably have more luck once we figure out the first part of this code



head(marshmallows)
