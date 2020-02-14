## Model takes data X, Y, N, p, M, K
drink_prob_model_string <- "model{
  # Data likelihood
  for(m in 1:M){
    for(n in 1:N){
      Y[n,m] ~ dbern(xi[z[n] + 1,m])
    }
  }
  
  # latent variable likelihood
  for(i in 1:N){
    z[i] ~ dbern(q[i])
    logit(q[i]) <- inprod(X[i,],beta)
  }

  # priors
  for(j in 1:p){
    beta[j] ~ dt(0, 1/2.5^2, 1)
  }
  
  for(h in 1:K){
    for(m in 1:M){
      xi[h,m] ~ dbeta(0.5, 0.5)
    }
  }
}"