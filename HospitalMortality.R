# Hospital Infant Mortality Rates using JAGS code

# read in libraries needed 
library(R2jags)
library(coda) # for diagnostics 
library(ggplot2)
library(ggthemes)

df <- read.table('hospmort.dat', header=TRUE)
df
hospNum <- c(1,2,3,4,5,6,7,8,9,10,11,12)
df$hosp <- hospNum
df

mdl0 <- "
 model {
    for (i in 1:12){
      deaths[i] ~ dbin(p[i],n[i])
              p[i] ~ dbeta(a[i], b[i])
                  a[i] ~ dgamma(2, 0.1)
                  b[i] ~ dgamma(2, 0.1)
    }
 }
"

writeLines(mdl0,'HospMort0.txt')
n <- df$numops
deaths <- df$deaths
data.jags <- c('deaths', 'n')
parms0 <- c('p')

mort.sim0 <- jags(data=data.jags, inits=NULL, parameters.to.save = parms0, 
                 model.file = 'HospMort0.txt',n.iter=11500, n.burnin = 1000, 
                 n.chains = 4, n.thin = 2)
mort.sim0

sims <- as.mcmc(mort.sim0)
effectiveSize(sims)
autocorr.diag(sims)
raftery.diag(sims)

#only one to worry is the first p because it is 0.... # 2.74 for p1 and 2.03 for deviance

#### got the model to work, now fix where the chains will be at, change lambdas to p's and check the info
chains <- as.matrix(sims)
head(chains)
plot(density(chains[,1]))
p1 <- chains[,2]
p10 <- chains[,3]
p11 <- chains[,4]
p12 <- chains[,5]
p2 <- chains[,6]
p3 <- chains[,7]
p4 <- chains[,8]
p5 <- chains[,9]
p6 <- chains[,10]
p7 <- chains[,11]
p8 <- chains[,12]
p9 <- chains[,13]
### all of these chains are 21000 in length 
list1 <- rep(1, length(p1))
list2 <- rep(2, length(p1))
list3 <- rep(3, length(p1))
list4 <- rep(4, length(p1))
list5 <- rep(5, length(p1))
list6 <- rep(6, length(p1))
list7 <- rep(7, length(p1))
list8 <- rep(8, length(p1))
list9 <- rep(9, length(p1))
list10 <- rep(10, length(p1))
list11 <- rep(11, length(p1))
list12 <- rep(12, length(p1))
ggdata <- data.frame(p.chain = c(p1, p2, p3, p4, p5, p6, p7, p8, p9,
                                      p10, p11, p12), 
                     p.num = c(list1, list2, list3, list4, list5, list6, list7, list8, list9, 
                                      list10, list11, list12) )
###  NEED TO FIX THE AXIS ON THIS DENSITY PLOT WITH THE DECIMALS AND THE X AXIS BUSINESS 
g <- ggplot(ggdata, aes(p.chain, colour = factor(p.num)))
# This plot shows a nice comparison of all the posterior distributions of the infant mortality rates and number of
# deaths by hospital 
g + geom_density() + theme_fivethirtyeight()+ 
  labs(colour = "Hospital ID", title = "Posterior Distribution of Infant Mortality", 
       subtitle = "Number of Deaths by Hospital") + 
          scale_x_continuous(breaks = seq(0, 1, .04))

p1MCMC <- as.mcmc(ggdata[ggdata$p.num==1,])
p2MCMC <- as.mcmc(ggdata[ggdata$p.num==2,])
p3MCMC <- as.mcmc(ggdata[ggdata$p.num==3,])
p4MCMC <- as.mcmc(ggdata[ggdata$p.num==4,])
p5MCMC <- as.mcmc(ggdata[ggdata$p.num==5,])
p6MCMC <- as.mcmc(ggdata[ggdata$p.num==6,])
p7MCMC <- as.mcmc(ggdata[ggdata$p.num==7,])
p8MCMC <- as.mcmc(ggdata[ggdata$p.num==8,])
p9MCMC <- as.mcmc(ggdata[ggdata$p.num==9,])
p10MCMC <- as.mcmc(ggdata[ggdata$p.num==10,])
p11MCMC <- as.mcmc(ggdata[ggdata$p.num==11,])
p12MCMC <- as.mcmc(ggdata[ggdata$p.num==12,])

HPDinterval(p1MCMC) #Highest Posterior Density interval
quantile(p1, c(.025,0.5, 0.975)) #Equal tail interval 

HPDinterval(p2MCMC)
quantile(p2, c(.025,0.5, 0.975))

HPDinterval(p3MCMC)
quantile(p3, c(.025,0.5, 0.975))

HPDinterval(p4MCMC)
quantile(p4, c(.025,0.5, 0.975))

HPDinterval(p5MCMC)
quantile(p5, c(.025,0.5, 0.975))

HPDinterval(p6MCMC)
quantile(p6, c(.025,0.5, 0.975))

HPDinterval(p7MCMC)
quantile(p7, c(.025,0.5, 0.975))

HPDinterval(p8MCMC)
quantile(p8, c(.025,0.5, 0.975))

HPDinterval(p9MCMC)
quantile(p9, c(.025,0.5, 0.975))

HPDinterval(p10MCMC)
quantile(p10, c(.025,0.5, 0.975))

HPDinterval(p11MCMC)
quantile(p11, c(.025,0.5, 0.975))

HPDinterval(p12MCMC)
quantile(p12, c(.025,0.5, 0.975))

