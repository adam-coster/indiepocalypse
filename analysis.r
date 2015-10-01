library('stringr')
library('Cairo')
library('MASS')
library('mixtools')
library('Hmisc')

files = list.files('.','*.tsv')
games = read.table('indie.tsv',quote="",sep='\t',header=T,stringsAsFactors=F,fill=T)
games[,'publisher'] = 'Indie'
games[,'group'] = 'Indie'

for(file in files){
  publisher = str_extract(file,"^[^\\.]+")
  if(file != 'indie.tsv'){
    new_data = read.table(file,quote="",sep='\t',header=T,stringsAsFactors=F,fill=T)
    games = games[!(games$Idx %in% new_data$Idx), ]
  }
}

# CLEAN UP #
# Delete rows with inconsistent date format (mostly "MMM D[D], YYYY")
games=games[grep("^\\w\\w\\w \\d+, \\d\\d\\d\\d$",games$Released),]

# Bin into month and year, restrict to 2013-2014
months = 1:12
names(months) = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

games[,'release.month'] = months[tolower(str_extract(games$Released,"^\\w\\w\\w"))]
games[,'release.year'] = as.numeric(str_extract(games$Released,"\\d\\d\\d\\d$"))
games = games[games$release.year %in% c(2013,2014,2015),]

# get number of owners
games[,'owners.numeric'] = as.numeric(str_replace_all(str_extract(games$Owners,"^[\\d,]+"),',',''))

# remove F2P and games under 2.99
games = games[games$Price != 'Free',]
games[,'price.numeric'] = round(as.numeric(str_replace(games$Price,"\\$","")))
games = games[games$price.numeric>=3,]

# pull out the userscore
games[,'rating'] = as.numeric(str_replace(games$Userscore,"% \\([\\dNA/%]+\\)",""))
games$rating[is.na(games$rating)] = 0

# THE INDIEPOCOLYPSE! AHHHHH!
games[,'yearmonth'] = games$release.month
games$yearmonth[games$release.year==2014] = games$yearmonth[games$release.year==2014] + 12
games$yearmonth[games$release.year==2015] = games$yearmonth[games$release.year==2015] + 24
games = games[games$yearmonth<32,]

released = 1:31
are_good = 1:31
for(i in 1:31){
  includes = games$yearmonth==i
  released[i] = sum(includes)
  are_good[i] = sum(includes & games$rating>69)
}

# Rate of release
CairoPDF('steam_release_rate',width=9,height=3)
par(mfrow=c(1,2))
prefixes = c('','Good')
minima   = c(-1, 69)
for(i in 1:2){
  h = hist(games$yearmonth[games$rating>minima[i]],seq((1:31)-.5),plot=F)
  plot(h$mids,h$counts,main=paste(prefixes[i],"Indie games released by month"),xlab="Release Date",xaxt='n',
       ylab="Games released",type='l',pch=16,col='dodgerblue',lwd=3)
  axis(1,at=c(1,13,25,31),label=c('2013','2014','2015','July'))
}
dev.off()

CairoPDF('steam_success_rate',width=6,height=9)
hists = list('2013'=0,'2014'=0,'2015'=0)
colors = c('red','dodgerblue','black')
years = 2013:2015
par(mfrow=c(3,2))
for(i in 1:3){
  y=years[i]
  col = colors[i]
  owners = games$owners.numeric[games$owners.numeric>0 & games$release.year==y]
  hist(owners, 100, main=paste("Indie success (",y,')',sep=''),xlab="Owners per Game",ylab="Number of Games" )
  
  owners = log10(games$owners.numeric[games$owners.numeric>0 & games$release.year==y])
  h=hist(owners, 20,plot=F )
  plot(h$mids,h$density, main=paste("Indie success (transformed, ",y,')',sep=''),xlab="log(Owners per Game)",ylab="Fraction of Games")
  normfit = fitdistr(owners,'normal')
  normaldat = dnorm(h$mids,mean=normfit$estimate[1],sd=normfit$estimate[2])
  lines(h$mids,normaldat,col=col,lwd=2)
  legend('topright',c(paste('mean =',round(normfit$estimate[1],2)),
                      paste('sd   =',round(normfit$estimate[2],2))),
         text.col=col,bty='n')
  hists[[y]]=h
}
dev.off()

CairoPDF('steam_swamp',width=4,height=3)
plot(hists[[2015]]$mids,hists[[2015]]$counts,type='l',lwd=3,yaxt='n',
     main="Indie success decreases by year",xlab="log(Owners per Game)",col=colors[3],
     ylab="Number of Games" )
lines(hists[[2013]]$mids,hists[[2013]]$counts,type='l',lwd=3,col=colors[1])
lines(hists[[2014]]$mids,hists[[2014]]$counts,type='l',lwd=3,col=colors[2])
axis(2,at=c(0,50,100))
legend('topright',c('2013','2014','2015'),text.col=colors,bty='n')
dev.off()

CairoPDF('steam_success_quality',width=4,height=4)
plot(NA,NA,ylim=c(0,35),xlim=c(2.5,6.5),ylab="Number of Games",
     xlab="log(Owned)",main=paste("Success of good indie games",sep=""))
for(i in 1:3){
  y = years[i]
  # Need distributions of 'success' for good and bad games for both years
  good = log10(games$owners.numeric[games$owners.numeric>0 & games$release.year==y & games$rating>69])
  bad  = log10(games$owners.numeric[games$owners.numeric>0 & games$release.year==y & games$rating<70])
  h.good = hist(good,seq(from=2.5,to=7,by=.25),plot=F)
  h.bad  = hist(bad ,seq(from=2.5,to=7,by=.25),plot=F)
  ymax   = max(c(h.good$counts,h.bad$counts))
  xlims  = range(c(good,bad))
  lines(h.good$mids,h.good$counts,ylim=c(0,ymax),xlim=c(2.5,6.5),lwd=2,col=colors[i])
  #lines(h.bad$mids,h.bad$counts,lwd=2,lty=3,col=colors[i])
}
#legend('topright',c('good','others'),bty='n',text.col='gray',col='gray',lty=c(1,3))
legend('topleft',c('2013','2014','2015'),text.col=colors,bty='n')
dev.off()

CairoPDF("Indiepocolypse",width=12,height=5)
par(mfrow=c(1,2))
postfixes = c('','(good games)')
for(i in 1:2){
  x = games$yearmonth[games$rating>minima[i]]
  y = log10(games$owners.numeric[games$rating>minima[i]])
  plot(x,y,pch=16,cex=.5,col=rgb(0,0,0,.3),ylim=c(2,7),
       main=paste("Success vs. release month",postfixes[i]),xlab="Release Date",ylab="log(Owners)", xaxt='n')
  axis(1,at=c(1,13,25,31),label=c('2013','2014','2015','July'))
  medians = 1:31
  upperq  = 1:31
  lowerq  = 1:31
  total   = 1:31
  for(j in 1:31){
    medians[j] = median(games$owners.numeric[games$yearmonth==j & games$rating>minima[i]])
    upperq[j]  = quantile(games$owners.numeric[games$yearmonth==j & games$rating>minima[i]],.75)
    lowerq[j]  = quantile(games$owners.numeric[games$yearmonth==j & games$rating>minima[i]],.25)
    total[j]   = sum(games$owners.numeric[games$yearmonth==j & games$rating>minima[i]])
  }
  points(1:31,log10(medians),pch=16,cex=1,col='red')
  points(1:31,log10(upperq),pch=3,cex=1,col='red')
  points(1:31,log10(lowerq),pch=3,cex=1,col='red')
  points(1:31,log10(total),pch=15,cex=1,col='blue')
  fit = lm(y~x)
  slope = fit$coefficients[2]
  intercept = fit$coefficients[1]
  rsq   = summary(fit)$r.squared
  #abline(b=slope,a=intercept)
  correlation = rcorr(x,y,'pearson')$r[1,2]
  pvalue      = rcorr(x,y,'pearson')$P[1,2]
  legend('bottomleft',c(paste('pc =',round(correlation,3)),paste('-log(P) =',round(-log10(pvalue),2))),bty='n')
}

dev.off()
