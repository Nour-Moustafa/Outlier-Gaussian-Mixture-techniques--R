
require(caTools)
library(matrixStats)
library(dplyr)
library(plyr)

max_mu <- function (x)
{
  mu<- colMeans(as.matrix(x))
  max_mu= matrix(0,nrow=nrow(x),ncol=ncol(x))
  for(r in 1:nrow(x))
  {
    for( c in 1:ncol(x))
    {
      max_mu[r,c]= mu[c]
    }
  }
  return(max_mu)
}

#estimatining std
max_sd <- function (x)
{
  std<- colSds(as.matrix(x))
  max_std=matrix(0,nrow=nrow(x),ncol=ncol(x))
  for(r in 1:nrow(x))
  {
    for( c in 1:ncol(x))
    {
      max_std[r,c]= std[c]
    }
  }
  return(max_std)
}



# estimate density that will be the input of GMA-HMM
mix_density <- function (x,mu,std)
{
  den_mr=matrix(0,nrow=nrow(x),ncol=ncol(x))
  for(r in 1:nrow(x))
  {
    for( c in 1:ncol(x))
    {
      
      den_mr[r,c]<- rnorm(x[r,c],mu[r,c],std[r,c]) 
    }
  }
  return(den_mr)
}



mu=ddply( original_data[,1:161], .(original_data[,162]), max_mu) # estimate mu based on types
std=ddply(original_data[,1:161], .(original_data[,162]), max_sd) # estimate std based on types
dt_mu=as.matrix(mu[,1:161])    # 
dt_sd= as.matrix(std[,1:161])  #

dt_mu=dt_mu[!duplicated(dt_mu), ]
dt_sd= dt_sd[!duplicated(dt_sd), ]

#1000*161 simulate 10000 record with 28 varaibles
X <- seq(min(original_data),max(original_data),length = 1610000)       # x
simulated_attacks <-  abs(pnorm(X, mean = mean(dt_mu[2,]), sd = sd(dt_sd[2,]))) # attack
dim(simulated_attacks) <- c(10000,161)
label= rep(1, nrow(simulated_attacks)) # attack label
simulated_attacks=cbind(simulated_attacks,label)

simulated_normal <- abs(pnorm(X, mean = mean(dt_mu[1,]), sd = sd(dt_sd[1,]))) #normal
dim(simulated_normal) <- c(10000,161)
label= rep(0, nrow(simulated_normal)) # normal label
simulated_normal=cbind(simulated_normal,label)

simulated_data= rbind(simulated_attacks,simulated_normal)
simulated_data= as.data.frame(simulated_data)
head(simulated_data)

rm (label)
