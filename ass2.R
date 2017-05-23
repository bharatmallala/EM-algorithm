data = read.csv(file.choose())
#initialization step
mu<-c(30,35,45)
sd<-c(55,67,78)
probx_c=data.frame(0)
probc_x=data.frame(0)
s<-c()
N<-c()
newmu<-c()
newsd<-c()
newprobc<-c()
probc<-c(0.33,0.34,0.33)
#expectation step
expectation<-function()
{
  for(i in 1:nrow(data))
  {
    for(j in 1:length(mu)){
      
      
      probx_c[i,j] <- exp( -(data[i,1]-mu[j])^2 / (2*(sd[j])^2)) /sqrt(2*pi*(sd[j])^2)
    }
    
  }
  
  
  for(i in 1:nrow(data)){
    s[i]= 0
  }
  for(i in 1:nrow(data))
  {
    for(j in 1:NROW(mu)) {
      s[i]<-s[i]+(probc[j]*probx_c[i,j])}}
  
  for(i in 1:nrow(data)) {
    
    for(j in 1:length(mu)){
      probc_x[i,j] <- (probx_c[i,j]*probc[j])/s[i]
    }
  }
  
  for(j in 1:NROW(mu)){
    N[j] = 0
  }
  
  
    for(j in 1:NROW(mu))
      N[j] =sum(probc_x[,j]) 
    }

#maximization step
newmu[]
maximization<- function(probc_x,data,N,){
  for(i in 1:NROW(mu))
  {
    
      newmu[i]<- sum(probc_x[,i]*data)/N[i]
      
      newsd[i]<- sqrt((sum(probc_x[,i]*(data)^2))/N[i]-( sum(probc_x[,i]*data)/N[i])^2)
      
      newprobc[i]<- c(N[i]/nrow(data))
    
    
  }
  
  #calculating loglikelihood
  loglkh <- c(0)
  for(i in 1:nrow(data)){
    for (j in 1:length(mu)){
      loglkh<- loglkh + log(newprobc[j]*(exp((-(data[i,1]-newmu[j])^2)/(2*(newsd[j])^2)))/sqrt(2*pi*(newsd[j])^2))
      
    }
    
  }}

#runnig EM algorithm





