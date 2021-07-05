# Chapter 5-a
# kakyo
# 2021-06-25

#Parameter
sigma = 0.2
S0 = 15000
K = 14000 
T = 2 
r = 0.01　# risk free interest rate
q=0 #原資産の配当利回り
Put_price=0
M=60
N=2
type='put'


Coupon = function (sigma,out,S0,K,r,T,q,type,N,M){
BS_model = function(sigma,out,S0,K,r,T,q,type){
  d1 = ( log(S0/K) + ( r - q + sigma^2/2 )*T ) / (sigma*T^(1/2))
  Nd1 = pnorm(d1,mean = 0,sd=1)
  d2 = d1- sigma*T^(1/2)
  Nd2 = pnorm(d2,mean = 0,sd=1)
  if (type == "call"){
    out = S0*exp(-q*T)*Nd1-K*exp(-r*T)*Nd2}
  else{
    out = K*exp(-r*T)*(1-Nd2) - S0*exp(-q*T)*(1-Nd1)
  }
  return(out)
}
Put_price_2=BS_model(sigma,out,S0,K,r,T,q,type)
put=((100-M)/K)*Put_price_2
dbond=100/(1+r)^T
pvan=optiRum::PV(r,N,-1)
coupon=(100-dbond+put)/pvan
return(coupon)
}

Coupon(sigma,Put_price,S0,K,r,T,q,'Put',N,M)

#K
K_n=seq(14000,20000)
c=c()
for(i in 1:6001){
  c[i]=Coupon(sigma,Put_price,S0,K_n[i],r,T,q,'Put',N,M)
}
plot(K_n,c,type='l',xlab='K',ylab='C')

#M
M_n=seq(0,100)
c=c()
for(i in 1:101){
  c[i]=Coupon(sigma,Put_price,S0,K,r,T,q,'Put',N,M_n[i])
}

plot(M_n,c,type='l',xlab='M',ylab='C')

#r
r_n=seq(0.01,0.1,0.001)
c=c()
for(i in 1:91){
  c[i]=Coupon(sigma,Put_price,S0,K,r_n[i],T,q,'Put',N,M)
}
plot(r_n,c,type='l',xlab='r',ylab='C')

#sigma
sigma_n=seq(0.05,0.3,0.001)
c=c()
for(i in 1:251){
  c[i]=Coupon(sigma_n[i],Put_price,S0,K,r,T,q,'Put',N,M)
}
plot(sigma_n,c,type='l',xlab='sigma',ylab='C')

