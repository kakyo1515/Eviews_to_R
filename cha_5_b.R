library(quantmod)
library(reshape2)
library(ggplot2)

#Nikkei 225データの入手
getSymbols("^N225",src="yahoo",from="2011-1-1",to="2012-1-18")
stock_data=N225$N225.Close
#Weekly dataに変更
stock_weekly=period.apply(stock_data, INDEX = seq(1,255,5), FUN = first)
View(stock_weekly)
# 定数と初期値の決定
# K:行使価格; sigma:volatility; r:金利; Cycle:リバランス頻度; intfund:初期投資額
K=10000;sigma=0.2
r=0.01;intfund=10000;Cycle=52

portfolio_cal=function(y,K,sigma,r,intfund,Cycle){
time=length(y) #データ(保険運用)期間
tau=time/Cycle　#残存期間数
ror=na.omit(y/lag(y,1)) #株式投資収益率
w=c() #リスク資産投資比率ベクトル
riskyasset=c()　#危険資産額ベクトル
riskfreeasset=c()　#安全資産額ベクトル
portfolio=intfund #ポートフォリオ価値ベクトル
riskyassetonly=intfund　#危険資産だけに投資した時の価値ベクトル
#毎期で計算
for(t in 1:(time-1)){
  S=y[t]
  d1=(log(S/K)+(r+sigma^2/2)*tau)/(sigma*sqrt(tau))
  Nd1=pnorm(d1)
  d2=d1-sigma*sqrt(tau)
  Nd2=pnorm(d2)
  #Black–Scholesでput optionの価値Pを計算
  P=K*exp(-r*tau)*pnorm(-d2)-S*pnorm(-d1)
  w[t]=(S*Nd1)/(S+P) #リスク資産投資比率=株価*ヘッジ比率/ポートフォリオ価値
  tau=tau-1/Cycle #残存期間数-1
  riskyasset[t]=portfolio[t]*w[t] #危険資産への投資
  riskfreeasset[t]=portfolio[t]-riskyasset[t]　#安全資産への投資
  riskyasset[t+1]=riskyasset[t]*ror[t] #期末危険資産額
  riskfreeasset[t+1]=riskfreeasset[t]*(1+r/Cycle)　#期末安全資産額
  portfolio[t+1]=riskyasset[t+1]+riskfreeasset[t+1]　#期末ポートフォリオ価値
  riskyassetonly[t+1]=riskyassetonly[t]*ror[t] #全て危険資産に投資した時の期末価値
}
return(list('portfolio'=portfolio,'riskyasset'=riskyasset,
            'riskfreeasset'=riskfreeasset,
            'riskyassetonly'=riskyassetonly,'w'=w))
}

result=portfolio_cal(stock_weekly,K=10000,sigma,r=0.01,intfund,Cycle)

#Figure 5.6 
#A　ポートフォリオ保険ありとなしの運用成果比較
plot(result$portfolio,type = 'l',ylim=c(7000,11000),xlab = '',ylab = '')
lines(result$riskyassetonly,type = 'l',col='red')
legend(x = "topright", legend=c('Fund with PI', 'RiskyAsset only'),
       col=c("black", "red"),lty=c(1,1),cex=1)

#Bポートフォリオ保険ありファンドとその内容　
plot(result$portfolio,type = 'l',ylim=c(0,12000),xlab = '',ylab = '')
lines(result$riskyasset,type = 'l',col='red')
lines(result$riskfreeasset,type = 'l',col='blue')
legend(x = "topright", legend=c('Fund with PI', 'RiskyAsset','RiskfreeAsset'),
       col=c("black", "red",'blue'),lty=c(1,1),cex=1)

#Figure 5.7
barplot(result$w)


#CPPI
#初期値の設定
stock0=6000　#危険資産(株式)
cash0=4000 #安全資産(現金・短期債券)
floor=9000　#フロアー(最低保証額)
m=4　#乗数 (1/25%)
r=0.01　#risk free interest rate
Cycle=52 #運用期間
#ベクトル
stock=stock0　
cash=cash0
portfolio=stock+cash
floor_series=floor
stockonly=portfolio
ror=na.omit(stock_weekly/lag(stock_weekly,1)) #株式投資収益率

for(t in 2:52){
  portfolio[t]=stock[t-1]*ror[t-1]+cash[t-1]*(1+r/Cycle) #ポートフォリオ期末価値
  stock[t]=(portfolio[t]-floor)*m #危険資産(株式)への投資
  cash[t]=portfolio[t]-stock[t]　#安全資産(現金・短期債券)への投資
  stockonly[t]=stockonly[t-1]*ror[t-1] #全て危険資産に投資した時の期末価値
}

rate_stock=stock/portfolio
rate_cash=cash/portfolio

#Figure 5.9
#A ポートフォリオ保険をかけたファンドとかけなかったファンド
plot(portfolio,type = 'l',ylim=c(7500,10500))
abline(h=floor_series,col='red')
lines(stockonly,type = 'l',col='blue')

#B ポートフォリオ保険をかけたの構成金額
plot(portfolio,type = 'l',ylim=c(0,12000))
lines(stock,type = 'l',col='blue')
lines(cash,type = 'l',col='red')

#Figure 5.10　ポートフォリオ保険をかけたファンドの株式と短期債券の投資比率
number=seq(1,52)
data=data.frame(number,rate_cash,rate_stock)
data_2=melt(data,id.var='number') 

ggplot(data_2,aes(number,value,fill=variable))+
  geom_bar(stat="identity",position="stack")+
  theme_bw()+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL))

