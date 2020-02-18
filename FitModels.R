library(rjags)

load(file = "data_1999.rdata")

## LOAD AND CLEAN DATA
dat <- dat99[sample(nrow(dat99), size = 0.2*nrow(dat99)),]

N <- nrow(dat)
X <- dat[,c("G18", "G19", "MEMGREEK", "G10", "SEX", "A3")]
names(X) <- c("Dad.School", "Mom.School", "Greek", "HS.Drink", "Sex", "Grade")
# HS_Drink 1 -- never, 2,3 -- Infrequent, 4,5,6,7 -- Frequent
# Year 1 fresh, 2 soph, 3, 4, 5 (fifth year), 6 grad student
# Race
# Religion
X$Dad.School <- factor(X$Dad.School, levels = c(1,2,3,4), labels = c(0,0,0,1))
X$Mom.School <- factor(X$Mom.School, levels = c(1,2,3,4), labels = c(0,0,0,1))
X$HS.Drink <- factor(X$HS.Drink, levels = c(1,2,3,4,5,6,7), labels = c("Never","Infreq","Infreq","Freq","Freq","Freq","Freq"))
X$Sex <- factor(X$Sex, levels = c(0,1), labels = c("Male", "Female"))
X$Grade <- factor(X$Grade, levels = c(1,2,3,4,5,6), labels = c("Fresh", "Soph", "Junior", "Senior", "Fifth", "Grad"))
X$First.Gen <- factor(as.numeric((X$Dad.School == 0)&(X$Mom.School == 0)))
X$Greek <- factor(X$Greek)
summary(X)

drink_problems <- dat[,c(paste0("DRPROB", c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N")))]
M <- ncol(drink_problems) # number of drinking problems

GPA <- factor(dat$F4, levels = seq(9,1, by = -1), 
              labels = c("D", "C-", "C", "C+", "B-", "B", "B+", "A-", "A"),
              ordered = TRUE)

## REMOVE MISSING VALUES FOR NOW
YYX <- cbind(drink_problems,GPA,X)
YYX <- na.omit(YYX)
drink_problems <- YYX[,1:M]
GPA <- YYX[,M+1]
X <- YYX[,(M + 2):ncol(YYX)]

## Get Final Model Matrix
X <- model.matrix(~First.Gen*Greek + HS.Drink + Sex + Grade, data = X)




## FIT MODELS

## --- Drinking Problem Model -----
## Get drink_prob_model_string from file JagsModels.R 
## Model takes data X, Y, N, p, M, K
# X is model matrix for logistic regression
# Y is M-dimensional drink-problem indicator response
# K is number of latent drinking problem levels

drink_prob_model <- jags.model(textConnection(drink_prob_model_string), 
                    data = list(X=X, Y=drink_problems, N=nrow(drink_problems), p=ncol(X), M=ncol(drink_problems), K=2))

samp <- coda.samples(drink_prob_model, variable.names=c("beta", "xi"), n.iter=1000)
summary(samp)

## --- Academic Performance Model ---
## Rank Regression Model
Y <- GPA
X <- X[,-1] # No Intercept Needed For Rank Regression
XtX <- t(X)%*%X
XtXi <- solve(XtX)
V <- chol(XtXi)

n <- nrow(X)
g <- n # For Zellner g-prior


## GIBBS SAMPLING ROUTINE
S <- 1000 # number of simulations

## Data Storage
# Don't need to store samples of Z
BETA <- NULL

## Starting Values for Gibbs Sampling
Z <- 6*(rank(Y) - mean(rank(Y)))/n
beta <- rep(0,11) # First.Gen, Greek, and First.Gen:Greek

## Sample
for(s in 1:S){
  # Sample beta
  w <- rnorm(11)
  w <- t(V)%*%w
  w <- g*w/(1 + g)
  beta <- w + (g/(1 + g))*XtXi%*%t(X)%*%Z
  
  # Sample Z
  for(i in 1:n){
    a <- max(c(Z[Y < Y[i]], -Inf))
    b <- min(c(Z[Y > Y[i]], Inf))
    
    z_expected <- X[i,]%*%beta
    l <- pnorm(a, z_expected)
    u <- pnorm(b, z_expected)
    
    p <- runif(1, l, u)
    
    Z[i] <- qnorm(p, mean = z_expected)
  }
  
  BETA <- rbind(BETA, t(beta))
}


colMeans(BETA) # Mean Coefficients (Positive indicates improved GPA)
par(mfrow = c(1,3))
for(i in 1:3){plot(density(BETA[,i]), lwd = 2, col = "blue", main = paste("Effect on GPA: ", c("First Gen", "Greek", "Inter"))[i], xlab = NA); abline(v = 0, col = "red")}

## But these Traceplots Suggest Burnin Should be Dropped
plot(BETA[,1])
plot(BETA[,2])
plot(BETA[,3])

## So Trim Beta
BETA_trim <- BETA[300:1000,]
for(i in 1:3){plot(density(BETA_trim[,i]), lwd = 2, col = "blue", main = paste("Effect on GPA: ", c("First Gen", "Greek", "Inter"))[i], xlab = NA); abline(v = 0, col = "red")}





