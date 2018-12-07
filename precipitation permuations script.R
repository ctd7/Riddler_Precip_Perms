library(gtools)
hui = 2           # Initial umbrellas at home
oui = 3 - hui     # Initial umbrellas at the office
x = c(0,1)        # No rain or rain
rain = permutations(n = 2, r = 10, v = x, repeats.allowed = T)  # Matrix of all permulations. Dimensions = 2^10,10
prob = 0          # Initializing probability of success to 0
suc = rep(1,2^10) # Vector to keep track of which of the rain permuations kept person dry. Code later will determine if not true
probp = 1         # Ongoing probability of each iteration

sz = dim(rain)    # Size of the rain matrix

for (i in 1:sz[1]) {  # This is going through every possible iteration
  probp = 1           # Resetting ongoing probability
  hu = hui            # Tracking the number of umbrellas at the house (hu) and office (ou). Reinitialized for every loop
  ou = oui
  for(j in 0:(sz[2]/2 - 1)) {  # Going through each workday
    
    if (rain[i,2*j+2] == 1) {  # Since the chain of rain when leaving the house is 50%, just checks if it is raining when leaving the office
      probp = probp *0.5 * 0.4 # Calculates the probability of these events occuring. Since it is conditional probability, must multiply.
    }
    
    if (rain[i,2*j+2] == 0) {
      probp = probp *0.5 * 0.6
    }
    
    hu = hu - rain[i,2*j+1]  # Checks how many umbrellas are the house and office
    ou = ou + rain[i,2*j+1]
    
    if (hu < 0 || ou < 0) {  # Essentially, if either goes below 0, then the person will get wet
      probp = 0 
      suc[i] = 0 # Says this iteration was unsucessful, and breaks the loop
      break
    }
    
    hu = hu + rain[i,2*j+2]  # Repeat the algorithim for returning home
    ou = ou - rain[i,2*j+2]
    
    if (hu < 0 || ou < 0) {
      probp = 0
      suc[i] = 0
      break
    }
  }
  prob = probp + prob # Disjointed probability so you add
}

rain2 = rain[which(suc == 1),] # Creates a new matrix with only the successful iterations. Serves as a check for intial run
sucn = sum(suc)  # Number of iterations that were succesful

sz = dim(rain2)
prob2 = 0
hu = rep(hui,sz[1])  # Vectors to see what the umbrella situations is at the end of the week
ou = rep(oui,sz[1])
odd <- function(x) x%%2 != 0   # Setting up odd and even functions
even <- function(x) x%%2 == 0

rain3 = rain2                   # Setting up probability matrix
rain3[,odd(1:sz[2])] = 0.5      # Setting all odd columns (raining when leaving the home) to 0.5
rain3[,even(1:sz[2])] = abs(rain3[,even(1:sz[2])]+.4 - 1)  # Setting all the even columns (leaving office) to appropriate values
probp2 = vector(mode = "double", length = sz[1])
probp2[1:length(probp2)] =  1

for (k in 1:sz[2]) {  # Conducting the product for conditinal probabilities
  probp2[1:length(probp2)] = probp2[1:length(probp2)] * rain3[,k]
  
  if (k <= 5) { 
    hu[1:length(hu)] = hu[1:length(hu)] - rain2[,2*(k-1) +1] + rain2[,2*k]
      } 
  
}
prob2 = sum(probp2)   # Getting the final product
ou = 3 - hu

