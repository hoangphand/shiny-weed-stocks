# source("C:\\Users\\GCE Employee\\Desktop\\personal\\weeds\\mult-lines-same-graph.R")
# Hoang, this is an example of how you can show many functions on a single
# plot. We will make a lot of use of this in the near future
# This example uses the file 'export-3-months.csv' which is in the files of our Slack group
# Note that the format for read.table differs between Microsoft and Linux
dat1=read.table("export-3-months.csv",header=T, sep=',') 
dat=data.frame(dat1)
dat$symbol_id=as.character(dat$symbol_id)
dat$date2=as.Date(dat$date)
get_sym=function(x){ y=switch(x,  '3'='SBUX', '9'='APHA.TO', '6'='N.V', '7'='HMMJ.TO'); return(y) }
tmp=unlist(lapply(dat$symbol_id, get_sym))
dat$sym=tmp
colnames(dat)
# [1] "symbol_id" "date"      "open"      "high"      "low"       "close"    
# [7] "volume"    "date2"     "sym"   
SBUX=dat[dat$sym=='SBUX', c(8, 4)];
colnames(SBUX)=c('date', 'sbux')
APHA.TO=dat[dat$sym=='APHA.TO', c(8, 4)]
colnames(APHA.TO)=c('date', 'apha')

N.V=dat[dat$sym=='N.V', c(8, 4)]
colnames(N.V)=c('date', 'namaste')

HMMJ.TO=dat[dat$sym=='HMMJ.TO', c(8, 4)]
colnames(HMMJ.TO)=c('date', 'hmmj')

d2=merge(SBUX, merge(APHA.TO, merge(N.V, HMMJ.TO, by="date", all=T), by="date", all=T), by="date", all=T)
colnames(d2)
# [1] "date"    "sbux"    "apha"    "namaste" "hmmj"  
cat("Summary of the data frame d2 so far:\n") 
print(summary(d2))
# Sys.sleep(5)
pairs(d2[,-1])
cat("Correlation matrix of d2:\n") 
print(round(cor(d2[,-1], use='pair'), 2))
# Sys.sleep(5)

# Plot all four of these on the same plot. Normalize and look at percent changes 
ncol.now=ncol(d2)
d2[,(ncol.now+1): (2*ncol.now-1)]=NA
# For each column of stock prices, use the first non-missing price to normalize
# the other prices
scale.by.first.not.zero=function(x){
	min.not.missing=min(which(!(is.na(x) | x==0)))
	first.non.missing=x[min.not.missing]
	return(100*(x-first.non.missing)/first.non.missing)
}
for(i in (ncol.now+1): (2*ncol.now-1)) d2[,i]=scale.by.first.not.zero(d2[,i-ncol.now+1])
colnames(d2)[(ncol.now+1): (2*ncol.now-1)]=paste(colnames(d2)[2:ncol.now], ".2", sep='')
min.y=min(d2[,(ncol.now+1): (2*ncol.now-1)], na.rm=T)
max.y=max(d2[,(ncol.now+1): (2*ncol.now-1)], na.rm=T)
par(mar=c(5, 5, 2, 1))
plot(d2[,1], d2[,ncol.now+1], pch=16, col=1, cex=1.6, xlab="Date",
	ylab="% Change", main="", ylim=c(min.y, max.y))
lines(d2[,1], d2[,ncol.now+1], col=1, lwd=2)
for(i in (ncol.now+2): (2*ncol.now-1)){
	points(d2[,1], d2[,i], pch=16, col=i-ncol.now)
	lines(d2[,1], d2[,i], lwd=2,col=i-ncol.now)
}
legend(d2[round(0.8*nrow(d2)), 1], 0.1*min.y+0.9*max.y, colnames(d2)[2:ncol.now],
	pch=16, col=1:ncol.now, bg="ivory2", cex=1.)
	