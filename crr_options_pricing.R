call_payoff<- function(S,K){
  return(max(S-K,0))
}
put_payoff <- function(S,K){
  return(max(K-S,0))
}

European_CRR <- function(S, K, r, sigma, t, n, payoff_fun){
  # S - asset price
  # K - strike price
  # r - continues discount rate
  # sigma - volatility
  # t - maturity (in years)
  # n - number of steps
  # payoff_fun - pay-off function (for example call (S-K)+)
  u<-exp(sigma*sqrt(t/n))
  d<-exp(-sigma*sqrt(t/n))
  p<-(exp(r*t/n)-d)/(u-d)
  price<-numeric(n+1)
  prob<-numeric(n+1)
  for(i in 0:n){
    price[i+1]<-payoff_fun(S*u^(n-i)*d^i,K)
    prob[i+1]<-choose(n,i)*p^(n-i)*(1-p)^i
  }
  return(exp(-(r*t))*sum(prob*price))
}

European_CRR(100, 110, 0.06, 0.3, 1, 55, put_payoff)
European_CRR(100, 110, 0.06, 0.3, 1, 55, call_payoff)

American_CRR <- function(S, K, r, sigma, t,n, payoff_fun){
  u<-exp(sigma*sqrt(t/n))
  d<-exp(-sigma*sqrt(t/n))
  p<-(exp(r*t/n)-d)/(u-d)
  price<-numeric(n+1)
  for(i in 0:n){
    price[i+1]<- payoff_fun(S*u^(n-i)*d^i,K)
  }
  for(i in rev(1:n)){
    temp_price_vec<-numeric(i)
    for(j in 1:i){
      binomial_price <- exp(-r*t/n)*(p*price[j]+(1-p)*price[j+1])
      exercise_price <-  payoff_fun(S*u^(i-j)*d^j, K)
      temp_price_vec[j]<-max(binomial_price,exercise_price)
    }
    price<-temp_price_vec
  }
  return(price[1])
}
American_CRR(100, 110, 0.06, 0.3, 1, 55, put_payoff)
American_CRR(100, 110, 0.06, 0.3, 1, 55, call_payoff)
