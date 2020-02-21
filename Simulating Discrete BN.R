# load in the BN learn library

library("bnlearn")

#-------------------simulating marshmallow dataset------------------------------

# variable names for our nodes
# each variable will have two factors
# sex: girl or boy
# age: <3 or >3
# companion in room: yes or no
# Eats marshmallow before 10 min: yes or no


# below we will make an empty dataframe
marshmallows <- data.frame(rep(NA, 100))

# now we can fill in our columns one by one
marshmallows$sex <- append(rep("girl", 50), rep("boy", 50))

# lets take out the NA column
marshmallows$rep.NA..100. <- NULL

# now we want to build in new columns one by one
# lets build in the age column, we want in total for about half of the children
# to be older than three and half to be less than three
marshmallows$age <- sample(append(rep("<3", 50), rep(">3", 50)), replace = FALSE)

# lets add wheteher or not they have a companion
marshmallows$companion <- sample(append(rep("yes", 50), rep("no", 50)), replace = FALSE)


# finally, we want to add in the conditional probability
# we will make an if statement for all of the possible outcomes
# this will be a conditional probability and will determine whether
# the child eats the marshmallow before 10 minutes or not
# when we make our network, we can check that the conditional
# probabilities match up with what we tell R 

# we make the empty column for outcome which we will populate with "yes" or "no"
# to indicate if the ate their marshmallow before 10 minutes
marshmallows$outcome <- rep(NA, 100)




# in this case, we just want for R to leave the value alone if it is not specified
# therefore, we only need if statements and no else statements
# this works also and is much more concise
# not to self: else statements are just added after each curly bracket
# also recall that else statements/commands are inside {}
for(i in 1:length(marshmallows$sex)){
  if(marshmallows$sex[i] == "girl"){
    if(marshmallows$age[i] == "<3"){
      if(marshmallows$companion[i] =="no"){
        marshmallows$outcome[i] <- "yes"
      }
    } 
  } 
}




# build in our own conditional probabilities
# use if statements to tell R how to construct the network for each column
# start with one and use if statements to fill in the next column


samp <- mean(rnorm(100, mean = 0.6, sd = 0.2))

test <- append(c(rep("yes", floor(100*samp))), c(rep("no", ceiling((100 - 100*samp)))))    
ranodmized_test <- sample(test)



#--------------------creating a simple BN structure by hand---------------------


test <- empty.graph(c("sex", "age", "companion", "eats"))
plot(test)


# we now want to add in an arc set
# we need to put the arcs into a matrix in order to do this
edges <- matrix(c("sex", "eats",
                  "age", "eats",
                  "companion", "age",
                  "companion", "eats"),
                byrow = TRUE, ncol = 2,
                dimnames = list(NULL, c("from", "to")))

# below we assign our edges to the graph
arcs(test) <- edges

# plot it to see that it did what we wanted
# note that it seems as though R by default makes the graph directed
# makes sense since we are in the BN package which is by defn directed graphs
plot(test)

# we want to create the conditional probability table by hand now
# we need to store the states of each node into individual objects

sex <- c("boy", "girl")
age <- c("<3", ">3")
companion <- c("yes", "no")





