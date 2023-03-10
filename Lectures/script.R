library(INLA)
N = 100 

x = rnorm(N, mean=6,sd=2)
y = rnorm(N, mean=5*x+1,sd=1) 


#look at the data
plot(x, y, pch = 20, bty = "n")
grid()


data = data.frame(x = x, y = y)

mod1  = inla(y ~ x, data = data)
mod1_lm = lm(y ~ x, data = data)


# compare the results
summary(mod1)
summary(mod1_lm)

names(mod1)

mod1$summary.fixed
mod1$summary.random
mod1$marginals.fixed

plot(mod1$marginals.fixed$`(Intercept)`, type = "l", bty = "n")
plot(mod1$marginals.fixed$x, type = "l", bty = "n")

mod1$summary.hyperpar




# Let us complicate things a little

mod1_bis  = inla(y ~ x, data = data, 
                 control.predictor = list(compute = TRUE),
                 control.compute = list(dic = T))



# summary.linear.predictor
# summary.fitted.values

# marginals.linear.predictor
# marginals.fitted.values


mod1$summary.linear.predictor
mod1_bis$summary.linear.predictor


# Your turn: 

# Iris data

# Petal.Length ~ 1 + Petal.Width

# Add a random effect:

formula = Petal.Length ~ 1 + Petal.Width + f(Species, model = "iid")

mod2 = inla(formula, data = iris)



# AR models ---------------------------------------------------------------

require(maptools)
require(lattice)


library(readr)
dataNMMAPS <- read_csv("https://sites.google.com/a/r-inla.org/stbook/NMMAPSraw.csv?attredirects=0&d=1")

formula <-  pm10 ~ 1 + temperature
model.linear <- inla(formula,family="gaussian",data=dataNMMAPS)
round(model.linear$summary.fixed[,1:5],3)

plot(model.linear$marginals.fixed[[1]],type="l",main="",ylab="",xlab=expression(beta[0]), bty = "n")
plot(model.linear$marginals.fixed[[2]],type="l",main="",ylab="",xlab=expression(beta[1]), bty = "n")

summary(lm(formula,data=dataNMMAPS))



# Change the prior for beta0 and beta1
model.linear <- inla(formula,family="gaussian", data = dataNMMAPS,
                     control.fixed=list(mean=0, prec=1, mean.intercept=0, prec.intercept=0.0001))
summary(model.linear)

# Note that when the model includes more than one fixed effect, a list has to be specified.
# For example, control.fixed=list(mean=list(a=1, b=2, default=0)) assigns prior
# mean equal to 1 for fixed effect a and equal to 2 for b; all the other fixed effects have a zero prior mean.
# The same holds for the precision prec.


# Change the prior for the precision
# By default, a noninformative logGamma prior is assumed on the logarithm
# of the precision, which is equivalent to assume a Gamma prior on the precision; 
# if we want to specify, for instance, a standard Normal(0, 1)
# prior on the logarithm of the precision we should write:
  
model.linear <- inla(formula,family="gaussian", data=dataNMMAPS,
                     control.family=list(hyper=list(prec=list(prior="gaussian",param=c(0,1)))))



# fitted 
model.linear <- inla(formula,family="gaussian",
                     data=dataNMMAPS,
                     control.predictor=list(compute=TRUE))

res.lin <- (dataNMMAPS$pm10 - model.linear$summary.fitted.values$mean) / model.linear$summary.fitted.values$sd
round(model.linear$summary.fitted.values[1:5,1:5],3) 

plot(res.lin, ylim = c(-150,300), main = "", xlab="Days",
     ylab=expression((y[i] - hat(mu[i]))/hat(sigma[i])), bty = "n")

# Iclude month effect in the model
month <- substring(as.character(dataNMMAPS$date), first=4, last=6)
dataNMMAPS$month <- factor(month,
                           levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
formula.inla2 <- pm10 ~ 1 + temperature + month
model.linear2 <- inla(formula.inla2,family="gaussian",
                      data=dataNMMAPS,
                      control.predictor = list(compute = TRUE))

round(model.linear2$summary.fixed[,1:5],3)
res.lin2 <- (dataNMMAPS$pm10 - model.linear2$summary.fitted.values$mean) / model.linear2$summary.fitted.values$sd

plot(res.lin2,ylim=c(-50,150),main="",xlab="Days",
     ylab=expression((y[i] - hat(mu[i]))/hat(sigma[i])),bty="n")

# Inlcude time-effect in the model
formula.inla3 <- pm10 ~  1 +  temperature + month + 
  f(id, model="ar1") #hyper = list(prec = list(prior="loggamma",param=c(1,0.01))))
model.linear3 <- inla(formula.inla3,family="gaussian",
                      data=dataNMMAPS,
                      control.predictor = list(compute = TRUE))

round(model.linear3$summary.fixed[,1:5],3)
res.lin3 <- (dataNMMAPS$pm10 - model.linear3$summary.fitted.values$mean) / model.linear3$summary.fitted.values$sd

plot(res.lin3,ylim=c(-25,25),main="",xlab="Days",
     ylab=expression((y[i] - hat(mu[i]))/hat(sigma[i])),bty="n")


# plot the random effect
plot(model.linear3$summary.random$id$mean,bty="n",
     xlab="Days",ylab=expression(PM[10]),pch=10,cex=0.4,ylim=c(min(model.linear3$summary.random$id$"0.025quant"),max(model.linear3$summary.random$id$"0.975quant")))



# Changing the Likelihood -------------------------------------------------

# download the data from:
# https://sites.google.com/a/r-inla.org/stbook/sheffield_data.zip?attredirects=0&d=1


Stroke <- read.csv("sheffield_data/Stroke.csv")
Stroke$Adjusted.prob <- Stroke$stroke_exp/Stroke$pop
Stroke$logit.adjusted.prob <- log(Stroke$Adjusted.prob/(1-Stroke$Adjusted.prob))                            

formula.inla <- y ~ 1 + factor(NOx) + factor(Townsend) +
  offset(logit.adjusted.prob)
model.logistic <- inla(formula.inla, family="binomial", Ntrials=pop, data=Stroke)

round(model.logistic$summary.fixed[,1:5],3)

prob.stroke <- inla.tmarginal(function(x) exp(x)/(1+exp(x)), model.logistic$marginals.fixed[[1]])
plot(prob.stroke, type= "l", bty = "n")

inla.zmarginal(prob.stroke)
inla.emarginal(exp, model.logistic$marginals.fixed$"factor(NOx)2")


## For you: seeds data with two different links -> logit and probit


# CAR ---------------------------------------------------------------------

data(Scotland)

g = system.file("demodata/scotland.graph", package="INLA")


#u = 0.2/0.31

u = 10
alpha = 0.9
phi.u = 0.5
phi.alpha = 2/3 ## prob(phi < phi.u) = phi.alpha

formula = Counts ~  f(Region,
                      model = "bym",
                      graph=g ) + I(X/10)

mod.mean = inla(formula,family="poisson",E=E,data=Scotland, control.predictor = list(compute= T), 
                control.compute = list(dic = T))


library(SpatialEpi)
data(scotland)
scotland.map <- scotland$spatial.polygon



cols <-viridis::viridis(30, direction = -1)
lcols <- cut(Scotland$Counts, breaks = seq(min(Scotland$Counts), max(Scotland$Counts), length.out = 31),# probs = c(0.25,.5,.75)),
             labels = cols)

legend.col <- function(col, lev){
  
  opar <- par
  n <- length(col)
  bx <- par("usr")
  
  box.cx <- c(bx[2] + (bx[2] - bx[1]) / 1000,
              bx[2] + (bx[2] - bx[1]) / 1000 + (bx[2] - bx[1]) / 50)
  box.cy <- c(bx[3], bx[3])
  box.sy <- (bx[4] - bx[3]) / n
  
  xx <- rep(box.cx, each = 2)
  
  par(xpd = TRUE)
  for(i in 1:n){
    
    yy <- c(box.cy[1] + (box.sy * (i - 1)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i)),
            box.cy[1] + (box.sy * (i - 1)))
    polygon(xx, yy, col = col[i], border = col[i])
    
  }
  par(new = TRUE)
  plot(0, 0, type = "n",
       ylim = c(min(lev), max(lev)),
       yaxt = "n", ylab = "",
       xaxt = "n", xlab = "",
       frame.plot = FALSE)
  axis(side = 4, las = 2, tick = FALSE, line = .25)
  par <- opar
}


par(bty="n")
plot(scotland.map, col = as.character(lcols), main= "Raw Counts")
legend.col(col = viridis::viridis(30), lev = Scotland$Counts)


lcols <- cut(Scotland$Counts/Scotland$E, breaks = seq(min(Scotland$Counts/Scotland$E), max(Scotland$Counts/Scotland$E), length.out = 31),
             labels = cols)
plot(scotland.map, col = as.character(lcols), main = "Standardized Mortality Ratio")
legend.col(col = viridis::viridis(30), lev = Scotland$Counts/Scotland$E)


inlaMargs<- mod.mean$marginals.linear.predictor

install.packages("geostatsp")
library(geostatsp)
ep = geostatsp::excProb(inlaMargs, threshold=0)>0.95  


colors = viridis::viridis(2, direction = 1, option = "A", alpha = 0.6, begin = .2, end = .7)[ep+1]
plot(scotland.map, col = colors, main = "Detected by Exceedence probability")
legend("topleft", legend = c("Low Risk", "High Risk"), fill = viridis::viridis(2, direction = 1, option = "A", alpha = 0.6, begin = .2, end = .7), bty = "n" )
