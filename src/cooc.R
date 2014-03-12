###cooc = co-occurrence analysi function
###MKL 12Mar2014

## You can input these functions by running source('pathtothefilehere/cooc.R')

## r00 = both observation and species totals are allowed to vary (this is the least conservative)
## r0 = observation totals are constrained
## c0 = species totals are constrained
## r1 = both observation and species totals are constrained (most conservative model)

library(vegan)

###Support function for re-organizing output
getout <- function(x){
  out <- unlist(x$oecosimu[c(5,6,1,3,7)])
  names(out)[2] <- 'c.score'
  return(out)
}

cs <- function(x){nestedchecker(x)[[1]][1]}

sim.plot <- function(x,col='darkgrey'){
  lines(density(x),col=col)
}

cooc <- function(x='row=obs,col=spp',nits=1000,plot.sim=TRUE,col=c(1,2,3,4)){
  x[x!=0] <- 1
  sim <- list()
  sim[[1]] <- oecosimu(x,cs,method='r00',burnin=100,thin=10,nsimul=nits) 
  sim[[2]] <- oecosimu(x,cs,method='r0',burnin=100,thin=10,nsimul=nits)  
  sim[[3]] <- oecosimu(x,cs,method='c0',burnin=100,thin=10,nsimul=nits) 
  sim[[4]] <- oecosimu(x,cs,method='r1',burnin=100,thin=10,nsimul=nits) 
  out <- data.frame(do.call(rbind,lapply(sim,getout)))
  out[,2:4] <- apply(out[,2:4],2,function(x) round(as.numeric(x),5))
  if (plot.sim){
    min.x <- min(c(sim[[1]][[2]][[4]],sim[[2]][[2]][[4]],sim[[3]][[2]][[4]],sim[[4]][[2]][[4]],
                   sim[[1]][[2]][[6]],sim[[2]][[2]][[6]],sim[[3]][[2]][[6]],sim[[4]][[2]][[6]]))
    max.x <- max(c(sim[[1]][[2]][[4]],sim[[2]][[2]][[4]],sim[[3]][[2]][[4]],sim[[4]][[2]][[4]],
                   sim[[1]][[2]][[6]],sim[[2]][[2]][[6]],sim[[3]][[2]][[6]],sim[[4]][[2]][[6]]))
    max.y <- max(c(density(sim[[1]][[2]][[4]])$y,density(sim[[2]][[2]][[4]])$y,
                   density(sim[[3]][[2]][[4]])$y,density(sim[[4]][[2]][[4]])$y))
    plot(0,xlim=c((min.x-(min.x*0.1)),(max.x+(max.x*0.1))),ylim=c(0,(max.y+(max.y*0.1))),
         ylab='Density',xlab='C-Score')
    for (i in 1:length(sim)){
      lines(density(sim[[i]][[2]][[4]]),col=col[i])
      lines(x=c(sim[[i]][[2]][[6]],sim[[i]][[2]][[6]]),y=c(0,max.y),col='black',lty=2)
    }
    legend('topright',legend=c('r00','r0','c0','r1'),col=col,lty=1)
  }
  return(out)
}
cooc(dune)
