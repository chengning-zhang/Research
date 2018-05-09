rm(list=ls())
setwd("C:/Users/chengning/Desktop/R/research/presenceOnlyP450")
load("P450.RData")
setwd("C:/Users/chengning/Desktop/R/771/FinalProject")
#P450 assume this is true population
##shuffle the population data
set.seed(1000)
P450=P450[sample(1:nrow(P450)),]
#sum(P450$y==1) 624
#sum(P450$y==0) 331
##p(y=1)=624/955=.6534031
#########simulate presence data from y=1
p=.6534031
set.seed(1000)
index.presence=sample(which(P450$y==1),100,replace = T)
presence=P450[index.presence,]  

#########simulate absence data from whole population
set.seed(10000)
index.absence=sample(1:nrow(P450),400,replace = T)
absence=P450[index.absence,]
#sum(absence$y==1) p(y=1|s=1)=264/400=0.66 close to p(y=1)
index.test=setdiff((1:nrow(P450)),union(index.absence,index.presence))
P450.s=rbind(presence,absence)
P450.s$z=as.factor(c(rep(1,100),rep(0,400)))

P450.test=P450[index.test,] 

########################choose first 200 y=1 to be s=1,make the simulation data,try 200 later,
##wrong!!!not case control
#P450.simulate=P450[,1:8]
#P450.simulate$s=as.factor(c(rep(1,200),rep(0,755)))

##############################first let g(x) trained by NB
prediction.g<-function(instance,data,output=FALSE){ #data is training data, instance is
  ##dataframe one obs
  classvalues <-
    sort(unique(data[, ncol(data)]),decreasing = T)
  p1<- (sum(data[, ncol(data)] == classvalues[1])+1)/(nrow(data)+2) ###p1=p(s=1) in all cases, since I sort
  p2<-(sum(data[, ncol(data)] == classvalues[2])+1)/(nrow(data)+2) ###p2=p(s=0)
  
  #####1-8 columns don't have the factor (0,1)(1,2) problem. 
  for(l in 1:(ncol(data)-2) ){ ####in P450.s we have extra column which is y
    featurevalue <- unique(data[, l])
    k <- length(unique(data[, l]))
    
    p1<-p1*(sum(data[, l] == as.character(instance[l]) &
                  data[, ncol(data)] == classvalues[1])+1) / (sum(data[, ncol(data)] ==
                                                                    classvalues[1])+k)
    
    p2<-p2*(sum(data[, l] == as.character(instance[l]) &
                  data[, ncol(data)] == classvalues[2])+1) / (sum(data[, ncol(data)] ==
                                                                    classvalues[2])+k)
    
  }
  p<-p1+p2;p1<-p1/p;p2<-p2/p
  if(output==T)  cat("p(y=1|x)=",p1,"\n")
  return(p1)
}
#prediction.g(P450.simulate[1,],P450.simulate)

#########################Validation set as P450.s itself,Then P is the labeled examples,which is first 100
g=numeric(nrow(presence))
for(i in 1:length(g)){
  g[i]=prediction.g(P450.s[i,],P450.s)
}
c.hat.NB=mean(g) ###for seed 1000 and seed 10000 NB.hat.c=0.2359345
c.true=100/624  #### (differs a lot!!!) not a lot, but still overestimate c.true for unknown reason
#################################try different seed, to have different p450.s,Then estimate c.hat.NB 100
###times
c.hat.NB=numeric(100)
for(j in 1:100){
index.presence=sample(which(P450$y==1),100,replace = T)
presence=P450[index.presence,]  
index.absence=sample(1:nrow(P450),400,replace = T)
absence=P450[index.absence,]
index.test=setdiff((1:nrow(P450)),union(index.absence,index.presence))
P450.s=rbind(presence,absence)
P450.s$z=as.factor(c(rep(1,100),rep(0,400)))
P450.test=P450[index.test,] 
g=numeric(nrow(presence))
for(i in 1:length(g)){
  g[i]=prediction.g(P450.s[i,],P450.s)
}
c.hat.NB[j]=mean(g)
}
plot(c.hat.NB,ylim = c(0,0.3),main="NB estimate of c from 100 repeatition")
abline(h=c.true,col="red")
abline(h=mean(c.hat.NB),col="blue") ###show overestimate the true c
####################################################second estimate of c
g=numeric(955)
for(i in 1:955){
  g[i]=prediction.g(P450.simulate[i,],P450.simulate)
}
c.hat2=sum(g[1:100])/sum(g)

prediction.f<-function(instance,data,output=FALSE){
    
  if(output==T) cat("p(y=1|x)=",prediction.g(instance,data,output = T)/c.hat.NB,"\n")
   
  return(prediction.g(instance,data)/c.hat.NB)
}

prediction.f(P450.s[1,],P450.s) #####P(y=1|x)  >1, how to decide???In paper use 0.5c. Does logistic has same problem?Yes

#prediction=function(test,train){}
###########################################let us first try fitted accuracy instead of on test data,
###just test on unlabeled data, P450.s[101:500,]
f.NB=numeric(nrow(absence))
for(i in (nrow(presence)+1):(nrow(presence)+nrow(absence))){
  f.NB[i-nrow(presence)]=prediction.f(P450.s[i,],P450.s)
}

box.data.NB=data.frame(fitted=f.NB,y=absence$y)
box.data.NB=box.data.NB[order(box.data.NB$fitted),]
boxplot(fitted~y,data = box.data.NB,ylab="P(y=1|X)",xlab="true pre/abs",main="NB")###It is fucking good!!!
abline(h=3*c.hat.NB,col="red")
#plot(absence$y,f.NB)
(sum(as.numeric(f.NB>2*c.hat.NB)==absence$y))/nrow(absence)###accuracy .7875 fucking good!!!
## sum(as.numeric(box.data.NB[,1]>2*c.hat.NB)==box.data.NB$y)/400 0.7875 same
(sum(as.numeric(f.NB>1*c.hat.NB)==absence$y))/nrow(absence) ##0.73
###better than expected!!
####(actuallt, c estimate does not change accuracy since all data divided by same c estimate), 
###No !!! it changes the accuracy!!!!why?? relative magnititude same but threshold change, which change
##accuracY!!!!!!
##################################try logistics Does logistic has same problem? yes!
logitmodel=glm(z~.-y,family=binomial(link='logit'),data=P450.s)###always predict P of y=1!!!

c.hat.log=mean(logitmodel$fitted.values[1:nrow(presence)])

f.log=logitmodel$fitted.values[(1+nrow(presence)):(nrow(presence)+nrow(absence))]/c.hat.log

box.data.log=data.frame(fitted=f.log,y=absence$y)
box.data.log=box.data.log[order(box.data.log$fitted),]
boxplot(fitted~y,data = box.data.log,ylab="P(y=1|X)",xlab="true pre/abs",main="logistic")###It is fucking good!!!
abline(h=2*c.hat.log,col="red")

(sum(as.numeric(f.log>2*c.hat.log)==absence$y))/nrow(absence) ##0.7975
(sum(as.numeric(f.log>1*c.hat.log)==absence$y))/nrow(absence) ##0.7175
(sum(as.numeric(f.log>3*c.hat.log)==absence$y))/nrow(absence) ##0.785
######################################g(x) trained by TAN 
###########think later: when we have presence-only, do we have this contact matrix???? 
##I guess yes, since we know all x
###let us delete y in p450.s to make sure we don't modify code too much


prim <- function(train) {
  p <- ncol(train) - 1   ## p is the number of attributes, p=18
  V <- c(1:p)
  #E<-matrix(1,ncol=p,nrow=p);diag(E)<-0
  
  M<-matrix(c(245,   9,   0,   3,   0,   2,  65,   8,
              9, 218,  17,  17,  49,  10,  50,  17,
              0,  17, 175,  16,  25,  13,   0,  46,
              3,  17,  16, 194,  19,   0,   0,   3,
              0,  49,  25,  19, 199,  10,   0,   3,
              2,  10,  13,   0,  10, 249,  50,  74,
              65,  50,   0,   0,   0,  50, 262,  11,
              8,  17,  46,   3,   3,  74,  11, 175),ncol=8,byrow = T)
  diag(M) <-
    0
  ## mutual infor>0 find maximum, dont wanna diag otherwise loop
  Vnew <-
    c(1)
  Enew <-
    matrix(0, ncol = p, nrow = p)
  #index_Vnew<-c(1);#index records the index of Vnew in V
  #if Vnew("lymphote","no_of_node","block_of) index=c(1,18,2)
  while (setequal(V, Vnew) == 0) {
    index_i <- numeric(length(Vnew))
    max_inf <- numeric(length(Vnew))
    for (i in 1:length(Vnew)) {
      index <- order(M[, Vnew[i]], decreasing = T)
      index_i[i] <- setdiff(index, Vnew)[1]   ##type 1 ties
      max_inf[i] <- M[index_i[i], Vnew[i]]
      
    }
    #type 2 ties
    if (sum(max_inf == max(max_inf)) > 1) {
      index1 <- which(max_inf == max(max_inf))
      index1 <- which(Vnew == min(Vnew[index1]))
    } else{
      index1 <- order(max_inf, decreasing = T)[1]
    }
    Vnew <-
      c(Vnew, index_i[index1])
    Enew[index_i[index1], Vnew[index1]] <- 1
    
    Enew[Vnew[index1], index_i[index1]] <- (-1)
    #index_Vnew<-c(index_Vnew,index_i[index1])
    
    
  }
  return(Enew)
} ####determined by predictors only, not response y


parent <- function(train, l) {
  #lth feature, return its parent
  E <- prim(train)
  if (l == 1) {
    #cat(colnames(train)[l]," ",colnames(train)[ncol(train)])
    return(0)
  } else{
    parentindex <- which(E[, l] == (-1))
    return(parentindex)
  }
  
  
}

parentname <- function(train, l) {
  if (l == 1) {
    cat(colnames(train)[l], " ", colnames(train)[ncol(train)], "\n")
  } else{
    parentindex <- parent(train, l)
    
    cat(colnames(train)[l],
        " ",
        colnames(train)[parentindex] ,
        " ",
        colnames(train)[ncol(train)],
        "\n")
  }
  
}


prediction_tan.g <-
  function(instance, data,output=FALSE) { ####instance can be just predictors without response y
    classvalues <- sort(unique(data[, ncol(data)]),decreasing = T) ###p1 is P(y=1) since we sort!!   
    p1 <- (sum(data[, ncol(data)] == classvalues[1]) + 1) / (nrow(data) +
                                                               2)
    p2 <- (sum(data[, ncol(data)] == classvalues[2]) + 1) / (nrow(data) +
                                                               2)
    featurevalue <- unique(data[, 1])
    
    i = which(featurevalue == as.character(instance[1])) ##i is the index ###trouble in factor stuff
    k = length(featurevalue)
    
    p1 <- p1 * (sum(data[, 1] == featurevalue[i] &
                      data[, ncol(data)] == classvalues[1]) +
                  1) / (sum(data[, ncol(data)] ==
                              classvalues[1]) +
                          k)
    p2 <- p2 * (sum(data[, 1] == featurevalue[i] &
                      data[, ncol(data)] == classvalues[2]) +
                  1) / (sum(data[, ncol(data)] ==
                              classvalues[2]) +
                          k)
    #p1<-p1*post_tan(data,1)[i]
    #p2<-p2*post_tan(data,1)[k+i]
    ## p(c)P(x1|c)
    
    for (l in 2:(ncol(data) - 1)) { ###extra column in P450.s  or delete y in P450.s
      featurevalue <- unique(data[, l])
      i = which(featurevalue == as.character(instance[l])) ##i is the index ###trouble in factor stuff
      xq <- parent(data, l)
      parentvalue <- unique(data[, xq])
      q <- length(parentvalue)
      k = length(featurevalue)
      Q = which(parentvalue == as.character(instance[xq]))
      
      p1 <- p1 *
        (sum(
          data[, l] == featurevalue[i] &
            data[, ncol(data)] == classvalues[1] &
            data[, xq] == parentvalue[Q]
        ) +
          1) / (sum(data[, ncol(data)] ==
                      classvalues[1] &
                      data[, xq] == parentvalue[Q]) + k)
      p2 <- p2 *
        (sum(
          data[, l] == featurevalue[i] &
            data[, ncol(data)] == classvalues[2] &
            data[, xq] == parentvalue[Q]
        ) +
          1) / (sum(data[, ncol(data)] ==
                      classvalues[2] &
                      data[, xq] == parentvalue[Q]) + k)
      #p2<-p2*post_p(data,l)$posterior[k*q+(Q-1)*k+i]
    }
    p <- (p1 + p2)
    p1 <- p1 / p
    p2 <- p2 / p
    #options(digits = 12)
    data_chr<-data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
    instance<-c(t(instance))
    if(unique(data_chr[,ncol(data_chr)])[1]=="1"){ ### seems correct, make sure return p(y=1|x)
      if(output==T) cat("P(s=1|x)=",p1,"\n")
      return(p1)
    }else{
      if(output==T) cat("P(s=1|x)=",p1,"\n")
      return(p2)
    }
    
  }

#########################Validation set as P450.simulated itself,Then P is first 200 obs, find c.hat
g=numeric(nrow(presence))
for(i in 1:length(g)){
  g[i]=prediction_tan.g(P450.s[i,-9],P450.s[,-9])
}
c.hat.TAN=mean(g)
c.true=100/624  #### differs a lot!!!! more than NB and logis

prediction_tan.f<-function(instance,data,output=FALSE){
  
  if(output==T) cat("p(y=1|x)=",prediction_tan.g(instance,data,output = T)/c.hat.TAN,"\n")
  
  return(prediction_tan.g(instance,data)/c.hat.TAN)
}


f.TAN=numeric(nrow(absence))
for(i in (nrow(presence)+1):(nrow(presence)+nrow(absence))){
  f.TAN[i-nrow(presence)]=prediction.f(P450.s[i,-9],P450.s[,-9])
}

box.data.TAN=data.frame(fitted=f.TAN,y=absence$y)
box.data.TAN=box.data.TAN[order(box.data.TAN$fitted),]
boxplot(fitted~y,data = box.data.TAN,ylab="P(y=1|X)",xlab="true pre/abs",main="TAN")###It is fucking good!!!
abline(h=2*c.hat.TAN,col="red")

(sum(as.numeric(f.TAN>2*c.hat.TAN)==absence$y))/nrow(absence)###accuracy 0.7975 fucking good!!!
(sum(as.numeric(f.TAN>3*c.hat.TAN)==absence$y))/nrow(absence)##0.75 
###1*c.hat.TAN/0.7575



