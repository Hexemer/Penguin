P <- 500
as.numeric(P)
P[1] <- 10
r <- 0.1
K = 100

#Discrete model (logistic map)
P[2] <- r*(1-(P[1]/K))*P[1]+P[1]
P[2]

P[3] <- r*(1-(P[2]/K))*P[2]+P[2]
P[3]

P[4] <- r*(1-(P[3]/K))*P[3]+P[3]
P[4]

#for loop in P
for (i in 2:500) {
  P[i] <- r * (1 - (P[i-1] / K)) * P[i-1] + P[i-1]
}
P

#Plot P
plot(P)
plot(P, type = "l")
plot(P, type = "l", col = "hotpink",
     xlab = "Time", ylab = "Population Size",
     main = "Logistic Growth")
#P(t+1) vs P(t)
plot(P[1:499],P[2:500])
plot(P[1:499], P[2:500], 
     type = "l", col = "hotpink2",
     xlab = "P(t)", 
     ylab = "P(t+1)",
     main = "P(t+1) vs P(t)")

#lines function
#create a vector from 0 to 150 with an increment 0.1
Pt <- seq(0,150,0.1)
Ptt <- r*(1-Pt/K)*Pt+Pt
lines(Pt, Ptt, col= "seagreen3")    
#draws the diagonal to show the equilibrium
lines(c(1,150), c(1,150), col = "dodgerblue")      
#creates the path using the diagonal
lines(c(P[1], rep(P[2:99], each=2)), y= c(rep(P[2:99], each=2), P[100]), col="mediumorchid2")
      
install.packages("deSolve")
library(deSolve)      
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)

#ode function
## y - initial state values for the ODE system, a vector. If y has a name attribute, the names will be used to label the output matrix
state <- c(P=10)
##times - time sequence for which output is wanted, the first value of times must be the initial time
#Simulate 100 time steps, and get values for every 0.1 interval
times <- seq(0,100,by=0.01)
#parameters passed to func.
parameters <- c(r=0.1, K=1000)

LG<- function(t,state,parameters) { ##logisitic grown function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state,parameters)),{ #"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    dP<- r*(1-P/K)*P #our logisitc equation governing the rate of change of P
    return(list(dP)) #return the rate of change
  })
}

out <- ode(y=state, times = times, func = LG, parms = parameters)
#convert it to a more convenient data
out.df<-data.frame(out)
plot(out.df, type = "l")

#Plot
ggplot(data = out.df) +
  geom_line(mapping = aes(x=time,y=P), color="seagreen") +
  geom_hline(yintercept = 0, color="midnightblue") +
  geom_vline(xintercept = 0, color="midnightblue") +
  labs(x = "Time", y = "P")

##Model Calibration
#Model without the parameter values
tmax <- 300
x <- numeric(tmax+1)
for(i in 2:(tmax+1)) {
  x[i] <- r*x[i-1]*(1-x[i-1]/K) +x[i-1]+rnorm(1,0,x[i-1]/100)
}

popLG <- read.csv("pop_LG_simul_noise_small.csv", header = TRUE)
ggplot(data = popLG) +
  geom_line(mapping = aes(x=time,y=P), color="violet") +
  geom_hline(yintercept = 0, color="midnightblue") +
  geom_vline(xintercept = 0, color="midnightblue") +
  labs(x = "Time", y = "P")
