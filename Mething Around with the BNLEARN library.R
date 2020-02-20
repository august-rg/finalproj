#----------Mething Around with BN Learn Library---------------------------------

# load in the "bnlearn" library for bayesian networs
library("bnlearn")


# load in the pre-loaded dataset coronary
data("coronary")

# assign coronary to an object
smoking <- coronary

# lets take a look at the data
head(smoking)
tail(smoking)


# we can learn the structure of the coronary dataset and store in an object
bn_smoking <- hc(smoking)
plot(bn_smoking)

# we need to remove the edge from m.work to family
bn_smoking$arcs <- bn_smoking$arcs[-which(bn_smoking$arcs[,'from'] == "M. Work"
                                           & bn_smoking$arcs[, 'to'] == "Family"),]


# this should have removed the node connecting m.work to family, lets check
plot(bn_smoking)

# we tell R to learn the structure of the BN
# the object below stores all of the conditional probability tables for each variable
# we can access each using fitted_bn$variable name
fitted_bn <- bn.fit(bn_smoking, data = smoking)

# we can use the cpquery function to make inferences from the network
?cpquery  # we can ask R how to use the cpquery function
# cpquery(bnstruct, event = event to predict, evidence = belief states)

# below we can ask what the likelihood of smoking is given that proteins are <3
cpquery(fitted_bn, event = (Smoking == 'no'), evidence = (Proteins == '<3'))


# we can ask more sophisticated questions if we have more evidence in our query
cpquery(fitted_bn, event = (Smoking == 'no'), 
        evidence = (Proteins == '<3' & Pressure == '>140'))



# AND, that is how we can use the BN Learn library on a somewhat simple
# dataset... note that this uses the code from 
# https://www.r-bloggers.com/bayesian-network-in-r-introduction/
