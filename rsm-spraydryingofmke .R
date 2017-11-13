library(rsm)
library(desirability)
require(graphics)
library(alr3)
wt_catcher = 473.28   #units in grams

data = read.table("/Users/mac/Desktop/spray drying polyphenol data/mkespraydryingdata-physicalproperties.csv", header=TRUE, sep=",")

#getting the feedrate curve for the pump
feedrate.wt <- data$wt_extract/data$t_batch
feedrate.lm <- lm(feedrate.wt~feedrate, data= data)
summary(feedrate.lm)
 data$feedrate <-round( sapply( data$feedrate, function(x) predict.lm(feedrate.lm, data.frame(feedrate=x))))
 
 #standard curve determination
stdcrv = read.table("/Users/mac/Desktop/spray drying polyphenol data/stdcurve_rsm.csv", header=TRUE, sep=",")
analysis <- as.factor(stdcrv$Analysis)
date <-as.factor(stdcrv$date)
conc.soln <- stdcrv$wt.std*stdcrv$conc.stock/(stdcrv$wt.solvent+stdcrv$wt.std*(1-stdcrv$conc.stock))
dfanl <- by(stdcrv[,5:11], analysis, function(x) data.frame(x))      #segregates in terms of analysis
reduc <- (dfanl$TAA[,2:4] - dfanl$TAA[,5:7])/dfanl$TAA[,2:4]*100
stdcrv.new = data.frame(analysis, conc.soln, aveabs =rowMeans(rbind(reduc,dfanl$TPC[,2:4]),na.rm =TRUE))
cft <- by(stdcrv.new, list(analysis, date), function(x) lm(conc.soln~aveabs, data = x))
sapply(cft, summary)


 #physicochemical properties of the feed extract
 feed.dat <- read.table("/Users/mac/Desktop/spray drying polyphenol data/rsmexperiment-feedproperties.csv", header=TRUE, sep=",")
 feed.dat$sample_conc <- feed.dat$sample_wt/feed.dat$solvent_wt
feed.dat$tpc <- sapply(feed.dat$TP.abs, function(x) predict.lm(cft[["TPC","161212"]], data.frame(aveabs=x)))/feed.dat$sample_conc #g/g soln 
reduc_feed <- (feed.dat$TAA.abs_i-feed.dat$TAA.abs_f)/feed.dat$TAA.abs_i*100
feed.dat$taa<- sapply(reduc_feed, function(x) predict.lm(cft[["TAA","161216"]], data.frame(aveabs=x)))/250.29/10^-6*100/feed.dat$sample_conc # mu trolox/100g soln
feed.dat$TS <- (feed.dat$ts.wt_sample_f-feed.dat$ts.wt_dish)/(feed.dat$ts.wt_sample_i-feed.dat$ts.wt_dish)
feed.dat$tss <- (feed.dat$smpl.wtfinal -feed.dat$tss.dpf)/feed.dat$smpl.wt
feedproperties <- colMeans(feed.dat[,c("tpc","taa","TS","pH","tss")], na.rm=T)
feedproperties$sd <- sapply(feed.dat[,c("tpc","taa","TS","pH","tss")], function(x) sd(x, na.rm=T))

 
#rsm and anova for physical properties of MKE-spray dried
data$wt_product <- data$wt_product - wt_catcher
yield <- data$wt_product/(data$wt_extract*feedproperties$TS)*100
data[,"Yield"] <- yield
yield.rsm <- rsm(Yield~SO(feedrate,P, tempinlet, dairvol), data=data)
yield.lm <-  lm(Yield~tempinlet+ dairvol + feedrate+P+I(tempinlet^2)+ I(dairvol^2) +I(feedrate^2)+I(P^2)+tempinlet:dairvol+ tempinlet:feedrate +tempinlet:P+dairvol:feedrate+dairvol:P+feedrate:P, data=data)

hg.wt_powderinit <- data[,"hg.wt_powderinit"]
hg.wt_container <- data[,"hg.wt_container"]
hg.wt_powderfinal <- data[,"hg.wt_powderf"]

data[,"hg"] <- (hg.wt_powderfinal-hg.wt_powderinit)/(hg.wt_powderinit-hg.wt_container)*100
hg.rsm <- rsm(hg~FO(feedrate,P,tempinlet,dairvol), data=data)
hg.lm <-  lm(hg~tempinlet+ dairvol + feedrate+P+tempinlet:dairvol+ tempinlet:feedrate +tempinlet:P+dairvol:feedrate+dairvol:P+feedrate:P, data=data)
anova(hg.lm)
summary(hg.rsm)

mc <- (data$mc.wt_sampleinit-data$mc.wt_samplef)/(data$mc.wt_sampleinit-data$mc.wt_crucible)*100
mc.rsm <- rsm(mc~SO(feedrate,P,tempinlet,dairvol), data=data)
mc.lm <-  lm(mc~ tempinlet+ dairvol + feedrate+P+tempinlet:dairvol+ tempinlet:feedrate +tempinlet:P+dairvol:feedrate+dairvol:P+feedrate:P+I(tempinlet^2)+I(dairvol^2)+I(feedrate^2)+I(P^2), data=data)
anova(mc.lm)
summary(mc.rsm)
pureErrorAnova(mc.lm)

#rsm and anova for TPC and TAA
data.chem <- read.table("/Users/mac/Desktop/spray drying polyphenol data/mkespraydryingdata.csv", header=TRUE, sep=",")
data.chem$feedrate <- data$feedrate
wt_powder <- data.chem[,c("wt.powder_1", "wt.powder_2", "wt.powder_3")] 
wt_solvent <- data.chem[,c("wt.solvent_1", "wt.solvent_2", "wt.solvent_3")]
wt_solndil <- data.chem[,c("wt.soln_dil1", "wt.soln_dil2", "wt.soln_dil3")]     #weight of sample solution to be diluted
wt_solventdil <- data.chem[,c("wt.solvent_dil1", "wt.solvent_dil2", "wt.solvent_dil3")]   #weight of solvent added for dilution
TP.abs <- data.chem[,c("TP.abs_1", "TP.abs_2","TP.abs_3")]
TAA.abs_0 <- data.chem[,c("TAA.abs_01", "TAA.abs_02","TAA.abs_03")]
TAA.abs_1 <- data.chem[,c("TAA.abs_11", "TAA.abs_12","TAA.abs_13")]

dilute.sampleconc <-wt_powder[1:5,1:3]/1000/(wt_powder [1:5,1:3]/1000+wt_solvent [1:5,1:3])*wt_solndil [1:5,1:3]/wt_solventdil [1:5,1:3]      #wt.powder is in mg and wt.solvent in g
undiluted.sampleconc <-wt_powder[6:28,]/1000/wt_solvent[6:28,]
sampleconc <- rbind(dilute.sampleconc, undiluted.sampleconc)    # units in g/g
colnames(sampleconc) <- c("conc_1", "conc_2", "conc_3") 
data.chem[,7:18] <- c()
data.chem <- as.data.frame(append(data.chem, sampleconc, after = 6))

#predicting total polyphenol content 
tpc <- apply(TP.abs, 2, function(x) predict.lm(cft[["TPC","161212"]], data.frame(aveabs=x)))/data.chem[,c("conc_1","conc_2","conc_3")]*1000
colnames(tpc) <- c("tpc_1","tpc_2", "tpc_3")

#predicting total antioxidant activity 
reduction <- (TAA.abs_0-TAA.abs_1)/TAA.abs_0*100
taa <- apply(reduction, 2, function(x) predict.lm(cft[["TAA","161213"]], data.frame(aveabs=x)))/250.29/10^-6*100/data.chem[,c("conc_1","conc_2","conc_3")]
colnames(taa) <- c("taa_1", "taa_2", "taa_3")

#rsm and anova of stability test
data.stability <- read.table("/Users/mac/Desktop/spray drying polyphenol data/accelerated stability test- rsmmke.csv", header=TRUE, sep=",")
sample.wt <- data.stability[,c("sample.wt_t1", "sample.wt_t2")]
solvent.wt <- data.stability[,c("solvent.wt_t1", "solvent.wt_t2")]
TP.abs <- data.stability[,c("TP.abs_t1", "TP.abs_t2")]
date <-sapply(factor(data.stability$stdcrv.used), toString)
sample.conc <- sample.wt/solvent.wt
tpc.stability <- data.frame(
						code= data.stability$code, week=data.stability$week, 
						apply(TP.abs, 2, function(x) mapply(function(x,y) predict.lm(cft[["TPC",y]], 														data.frame(aveabs=x)), x ,date))/sample.conc*1000
						)
colnames(tpc.stability)[3:4] <- c("TPC_t1","TPC_t2")
stability.profile <- by(tpc.stability, list(tpc.stability$code), data.frame)


#response surface methodology
data.new <- data.frame(data.chem[,1:6], tpc=round(rowMeans(tpc, na.rm=T),2), taa=round(rowMeans(taa, na.rm=T),0), Yield=round(data$Yield,2))

##coding
data.coded <- data.frame(apply(data.new[,3:6], 2, function(x) (x-median(x))/(max(x)-min(x))*4), data.new[,7:9])
data.coded$feedrate <- round(data.coded$feedrate)
coded.tpc.lm <- lm(tpc~tempinlet+ dairvol + feedrate+P+I(tempinlet^2)+ I(dairvol^2) +I(feedrate^2)+I(P^2)+tempinlet:dairvol+ tempinlet:feedrate +tempinlet:P+dairvol:feedrate+dairvol:P+feedrate:P, data=data.coded)
coded.taa.lm <- lm(taa/1000~tempinlet+ dairvol + feedrate+P+I(tempinlet^2)+ I(dairvol^2) +I(feedrate^2)+I(P^2)+tempinlet:dairvol+ tempinlet:feedrate +tempinlet:P+dairvol:feedrate+dairvol:P+feedrate:P, data=data.coded)
coded.yield.lm <-  lm(yield~tempinlet+ dairvol + feedrate+P+I(tempinlet^2)+ I(dairvol^2) +I(feedrate^2)+I(P^2)+tempinlet:dairvol+ tempinlet:feedrate +tempinlet:P+dairvol:feedrate+dairvol:P+feedrate:P, data=data.coded)
summary(coded.tpc.lm)
summary(coded.taa.lm)
summary(coded.yield.lm)



write.table(data.new, file= "/Users/mac/Desktop/spray drying polyphenol data/summary-mkersm.csv", sep="," ,row.names=F)
tpc.rsm <- rsm(tpc~SO(tempinlet, dairvol,feedrate, P), data=data.new)
tpc.lm <- lm(tpc~tempinlet+ dairvol + feedrate+P+I(tempinlet^2)+ I(dairvol^2) +I(feedrate^2)+I(P^2)+tempinlet:dairvol+ tempinlet:feedrate +tempinlet:P+dairvol:feedrate+dairvol:P+feedrate:P, data=data.new)
taa.rsm <- rsm(taa~SO(tempinlet, dairvol,feedrate, P), data=data.new)
taa.lm <- lm(taa~tempinlet+ dairvol + feedrate+P+I(tempinlet^2)+ I(dairvol^2) +I(feedrate^2)+I(P^2)+tempinlet:dairvol+ tempinlet:feedrate +tempinlet:P+dairvol:feedrate+dairvol:P+feedrate:P, data=data.new)
summary(yield.rsm)
summary(tpc.rsm)
summary(taa.rsm)
tpc.aov<- pureErrorAnova(tpc.lm)
taa.aov <- pureErrorAnova(taa.lm)
yield.aov <- pureErrorAnova(yield.lm)
aov.list <- list(tpc.aov, taa.aov, yield.aov)
cor.test(data.new$tpc, data.new$taa, method="pearson")
rownames.new <- c("x1", "x2", "x3", "x4", "x1^2", "x2^2", "x3^2"," x4^2","x1:x2", "x1:x3", "x1:x4", "x2:x3", "x2:x4","x3:x4")

#desirability function
YieldD <- dMax(35,63)
tpcD <- dMax(30, 85)
taaD <- dMax(40, 120)
overallD <- dOverall(YieldD,tpcD, taaD)

rsmOpt <- function(x, dObject, space="square") 
{
	tpc <- predict(coded.tpc.lm, data.frame(tempinlet=x[1], feedrate=x[2], P=x[3], dairvol=x[4]))
	taa <- predict(coded.taa.lm, data.frame(tempinlet=x[1], feedrate=x[2], P=x[3], dairvol=x[4]))
	yield <- predict(coded.yield.lm, data.frame(tempinlet=x[1], feedrate=x[2], P=x[3], dairvol=x[4]))	
	
	out <- predict(dObject, data.frame(tpc=tpc, taa=taa, yield=yield))
	
	if(space=="circular")
	{
		if(sqrt(sum(x^2))> 2) out <- 0	
	} else if (space == "square") if(any(abs(x) > 2)) out <- 0
	out
}

##optimization by Nelder-mean simplex method
searchgrid <- expand.grid(tempinlet = seq(-2, 2, length = 5), feedrate = seq(-2, 2, length = 5),dairvol = seq(-2, 2, length = 5), P = seq(-2, 2, length = 5))

for(i in 1:dim(searchgrid)[1])
{
	tmp <- optim(as.vector(searchgrid[i,]),
				rsmOpt,
				dObject = overallD,
				space = "circular",
				control = list(fnscale = -1), method="Nelder-Mead")
	if( i == 1)
		{
			best <- tmp
		} else {
			if(tmp$value > best$value) best <- tmp
		}
}
print(best)
##optimum values
mapply(function(x,y) x/4*(max(y)-min(y)) + median(y), best$par, data.new[,3:6] )
optimval <-c(

	tpc.optim = predict(coded.tpc.lm, data.frame(tempinlet = best$par[1], feedrate = best$par[2], dairvol = best			$par[3], P= best$par[4])),
	taa.optim = predict(coded.taa.lm, data.frame(tempinlet = best$par[1], feedrate = best$par[2], dairvol = best			$par[3], P= best$par[4])),
	yield.optim = predict(coded.yield.lm, data.frame(tempinlet = best$par[1], feedrate = best$par[2], dairvol = best		$par[3], P= best$par[4]))

)
write.table(optimval, file= "/Users/mac/Desktop/spray drying polyphenol data/optimval-rsm.csv", sep=",", row.names=F)


#generating img files for graphs
##tpc rsm and contour plots
svg(filename="/Users/mac/Desktop/spray drying polyphenol data/tpc-rsm.svg", width =6, height =6, onefile =T )
par(cex.axis=0.7,cex.lab=1,las=3, mgp = c(3,5,0))
persp(tpc.rsm, tempinlet~dairvol, contour= T, theta=-40, phi=20, r=5, zlab = "TPC (mg GAE/g)", xlabs=list( "drying air flow rate (m^3/min)","inlet air temperature (°C)" ), atpos=0 )
dev.off()

svg(filename="/Users/mac/Desktop/spray drying polyphenol data/tpc-contour.svg", width =6, height =6, onefile =T )
par(mgp=c(2.5,1,0),cex.axis=1.25, cex.lab=1.2)
contour(tpc.rsm, tempinlet~dairvol,  xlabs =c("inlet air temperature (°C)",expression("drying air flowrate"~(m^{3}~min^{-1}))), atpos=0, labcex=1) 
dev.off()

##taa rsm and contour plots
svg(filename="/Users/mac/Desktop/spray drying polyphenol data/taa-rsm.svg", width =6, height =6, onefile =T )
par(cex.axis=0.7, cex.lab=1, las=3)
persp(taa.rsm, tempinlet~dairvol, contour= T, theta=-30, phi=20, r=5, zlab = "TAA(umol Trolox/100 g) x 10^-3", xlabs = c( "drying air flow rate (m^3/min)","inlet air temperature (°C)"))
dev.off()

svg(filename="/Users/mac/Desktop/spray drying polyphenol data/taa-contour.svg", width =6, height =6, onefile =T )
par(mgp=c(2.5,1,0),cex.axis=1.25, cex.lab=1.2)
contour(taa.rsm, tempinlet~dairvol,  xlabs =c("inlet air temperature (°C)",expression("drying air flowrate"~(m^{3}~min^{-1}))), atpos=0, labcex=1) 
dev.off()

svg(filename="/Users/mac/Desktop/spray drying polyphenol data/yield-rsmtemp-dairvol.svg", width =6, height =6, onefile =T )
par(cex.axis=0.7, cex.lab=1, las=3)
persp(yield.rsm, tempinlet~dairvol, contour= T, theta=-30, r=5, zlab="Yield (%)", xlabs= c( "drying air flow rate (m^3/min)","inlet air temperature (°C)") )
dev.off()

svg(filename="/Users/mac/Desktop/spray drying polyphenol data/yield-contourtemp-dairvol.svg", width =6, height =6, onefile =T )
par(mgp=c(2.5,1,0),cex.axis=1.25, cex.lab=1.2)
contour(yield.rsm, tempinlet~dairvol,  xlabs =c("inlet air temperature (°C)",expression("drying air flowrate"~(m^{3}~min^{-1}))), atpos=0, labcex=1) 
dev.off()


svg(filename="/Users/mac/Desktop/spray drying polyphenol data/yield-rsmtemp-feedrate.svg", width =6, height =6, onefile =T )
par(cex.axis=0.7, cex.lab=1, las=3)
persp(yield.rsm, tempinlet~feedrate, contour= T, theta=-30, r=5, zlab="Yield (%)", xlabs= c( "feed flow rate (g/min)","inlet air temperature (°C)"))
dev.off()

svg(filename="/Users/mac/Desktop/spray drying polyphenol data/yield-contourtemp-feedrate.svg", width =6, height =6, onefile =T )
par(mgp=c(2.5,1,0),cex.axis=1.25, cex.lab=1.2)
contour(yield.rsm, tempinlet~feedrate,  xlabs =c("inlet air temperature (°C)",expression("feed flow rate"~(g~min^{-1}))), atpos=0, labcex=1) 
dev.off()


svg(filename="/Users/mac/Desktop/spray drying polyphenol data/yield-rsmtemp-pressure.svg", width =6, height =6, onefile =T )
par(cex.axis=0.7, cex.lab=1, las=3)
persp(yield.rsm,tempinlet~P, contour= T, theta=-30, r=5, zlab="Yield (%)", xlabs= c("atomizing pressure(MPa)","inlet air temperature (°C)"))
dev.off()

svg(filename="/Users/mac/Desktop/spray drying polyphenol data/yield-contourtemp-pressure.svg", width =6, height =6, onefile =T )
par(mgp=c(2.5,1,0),cex.axis=1.25, cex.lab=1.2)
contour(yield.rsm, tempinlet~P,  xlabs =c("inlet air temperature (°C)","atomizing pressure(MPa)"), atpos=0, labcex=1) 
dev.off()



#anova summary
anova.table =apply(data.frame(row.names=rownames.new, tpc.pvalue =anova(tpc.lm)$P[1:14],  taa.pvalue =anova(taa.lm)$P[1:14], yield.pvalue =anova(yield.lm)$P[1:14]), c(1,2), function(x) round(x, digits =3))
write.table(anova.table, file= "/Users/mac/Desktop/spray drying polyphenol data/anova.table-rsm.csv", sep=",", row.names=F)


