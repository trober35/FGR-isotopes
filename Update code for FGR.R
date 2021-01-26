
library(FSA)
library(magrittr)
library(dplyr)
library(plotrix)
library(Matching)
library(nnet)
library(ggplot2)
library(ggpubr)
library(tidyr)



fish<-CHANCE_WORKING_FULL_TOPES


lat = subset(fish, fish$species == "LAT")
bbt = subset(fish, fish$species == "BBT")
brc = subset(fish, fish$species == "BRC")
koe = subset(fish, fish$species == "KOE")
whs = subset(fish, fish$species == "WHS")
utc = subset(fish, fish$species == "UTC")
smb = subset(fish, fish$species == "SMB")
rbt = subset(fish, fish$species == "RBT")
chiro= subset(fish, fish$species == "CHIRO")
zoo = subset(fish, fish$species == "ZOO")
snails = subset(fish, fish$species == "SNAILS")
crf = subset(fish, fish$species == "CRF")
amp= subset(fish,fish$species =="AMP")

plot(NULL, xlim=c(-40,-20), ylim= c(5,20), xlab="Carbon", ylab="Nitrogen", xaxt="n", yaxt="n",main="", type="n")
axis(1, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)
axis(2, at=c(5,10,15,20),las=1, cex.axis=10/10)

points(bbt$C, bbt$N, col="brown", pch=2)
points(brc$C, brc$N, col="goldenrod", pch=3)
points(koe$C, koe$N, col="red", pch=4)
points(whs$C, whs$N, col="green", pch=5)
points(utc$C, utc$N, col="purple", pch=6)
points(smb$C, smb$N, col="orange", pch=7)
points(rbt$C, rbt$N, col="gray", pch=8)
points(chiro$C, chiro$N, col="gray", pch=9)
points(zoo$C, zoo$N, col="dark blue", pch=10)
points(crf$C, crf$N, col="black", pch=11)
points(snails$C, snails$N, col="blue", pch=12)
points(amp$C,amph$N, col="dark red",pch=13)







musclelat<-filter(lat, tissue=="muscle")
liverlat<-filter(lat, tissue=="liver")

latpuppies<-filter(lat,length<=482)
latpups<-filter(lat,length %in%483:711)
lattrophy<-filter(lat,length>=711 )


latIFm<-filter(musclelat, region=="inflow")
latIFl<-filter(liverlat, region=="inflow")
latOHm<-filter(musclelat, region=="open_hills")
latOHl<-filter(liverlat, region=="open_hills")
latCm<-filter(musclelat, region=="canyon")
latCl<-filter(liverlat, region=="canyon")

puppiesmusc<-filter(musclelat,length<=482)
pupsmusc<- filter(musclelat,length %in%483:711)
trophymusc<- filter(musclelat, length>=711)

summerlatm<-filter(musclelat,month %in% 6:9)
summerlatl<-filter(liverlat,month %in% 6:9)
falllatm<-filter(musclelat,month %in% 10:11)
falllatl<-filter(liverlat,month %in% 10:11)
winterlatm<-filter(musclelat,month %in% 1:3)
winterlatl<-filter(liverlat,month %in% 1:3)
springlatm<-filter(musclelat,month %in% 4:5)
springlatl<-filter(liverlat,month %in% 4:5)

LATpuppySuM<-filter(summerlatm,length<=482) 
LATpuppyFaM<-filter(falllatm,length<=482)
LATpuppyWiM<-filter(winterlatm,length<=482)
LATpuppySpM<-filter(springlatm,length<=482)
LATpupSuM<- filter(summerlatm,length %in%483:711)
LATpupFaM<- filter(falllatm,length %in%483:711)
LATpupWiM<- filter(winterlatm,length %in%483:711)
LATpupSpM<- filter(springlatm,length %in%483:711)
LATtrophySuM<- filter(summerlatm, length>=711)
LATtrophyFaM<- filter(falllatm, length>=711)
LATtrophyWiM<- filter(winterlatm, length>=711)
LATtrophySpM<- filter(springlatm, length>=711)

LATpuppySuL<-filter(summerlatl,length<=482) 
LATpuppyFaL<-filter(falllatl,length<=482)
LATpuppyWiL<-filter(winterlatl,length<=482)
LATpuppySpL<-filter(springlatl,length<=482)
LATpupSuL<- filter(summerlatl,length %in%483:711)
LATpupFaL<- filter(falllatl,length %in%483:711)
LATpupWiL<- filter(winterlatl,length %in%483:711)
LATpupSpL<- filter(springlatl,length %in%483:711)
LATtrophySuL<- filter(summerlatl, length>=711)
LATtrophyFaL<- filter(falllatl, length>=711)
LATtrophyWiL<- filter(winterlatl, length>=711)
LATtrophySpL<- filter(springlatl, length>=711)
###################################################### LAT C and N plots #############################################
plot(NULL, xlim=c(-38,-23), ylim= c(10,20), xlab="Carbon", ylab="Nitrogen", xaxt="n", yaxt="n",main="C vs. N all lat topes muscle", type="n")
axis(1, at=c(-38, -34, -30, -26, -24),las=1, cex.axis=10/10)
axis(2, at=c(10,12,14,16,18,20),las=1, cex.axis=10/10)

points(latIFm$C, latIFm$N, pch=16, col="blue")
points(latOHm$C, latOHm$N, pch=17, col="red")
points(latCm$C, latCm$N, pch=18, col="cyan")

legend(-25, 20, legend=c("IF" , "OH", "Can"), 
       pch=c(16, 17, 18), col=c("blue", "red", "cyan")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)


plot(NULL, xlim=c(-37,-22), ylim= c(10,20), xlab="Carbon", ylab="Nitrogen", xaxt="n", yaxt="n",main="C vs. N all lat topes liver", type="n")
axis(1, at=c( -36, -33, -30, -27, -24),las=1, cex.axis=10/10)
axis(2, at=c(10,15,20),las=1, cex.axis=10/10)

points(latIFl$C, latIFl$N, pch=16, col="black")
points(latOHl$C, latOHl$N, pch=17, col="green")
points(latCl$C, latCl$N, pch=18, col="purple")

legend(-25, 20, legend=c("IF" , "OH", "Can"), 
       pch=c(16, 17, 18), col=c("black", "green", "purple")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)


################################ LAT Carbon vs Sulfur ################################################################

plot(NULL, xlim=c(-40,-20), ylim= c(-2,8), xlab="Carbon", ylab="Sulfur", xaxt="n", yaxt="n",main="C vs. S all topes muscle", type="n")
axis(1, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)

points(latIFm$C, latIFm$S, pch=16, col="blue")
points(latOHm$C, latOHm$S, pch=17, col="red")
points(latCm$C, latCm$S, pch=18, col="cyan")

legend(-22, 8, legend=c("IF" , "OH", "Can"), 
       pch=c(16, 17, 18), col=c("blue", "red", "cyan")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
#######  liver #########

plot(NULL, xlim=c(-40,-20), ylim= c(-2,8), xlab="Carbon", ylab="Sulfur", xaxt="n", yaxt="n",main="C vs. S all topes liver", type="n")
axis(1, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)

points(latIFl$C, latIFl$S, pch=16, col="black")
points(latOHl$C, latOHl$S, pch=17, col="green")
points(latCl$C, latCl$S, pch=18, col="purple")

legend(-22, 8, legend=c("IF" , "OH", "Can"), 
       pch=c(16, 17, 18), col=c("black", "green", "purple")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)

###################################################### length classes vs. C  #######################################

puppiesmusc<-filter(musclelat,length<=482)
pupsmusc<- filter(musclelat,length %in%483:711)
trophymusc<- filter(musclelat, length>=711)

plot(NULL, xlim=c(0,1200), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT muscle vs. length", type="n")
axis(1, at=c(0,200,400,600,800, 1000,1200),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(puppiesmusc$length, puppiesmusc$C, pch=16, col="blue")
points(pupsmusc$length, pupsmusc$C, pch=17, col="darkblue")
points(trophymusc$length, trophymusc$C, pch=18, col="goldenrod")

legend(1000, -20, legend=c("Puppies" , "Pups", "Trophy"), 
       pch=c(16, 17, 18), col=c("blue", "darkblue", "goldenrod")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)

############################### muscle puppiesvs. C   ###########################

plot(NULL, xlim=c(0,482), ylim= c(-40, -25), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT puppies muscle vs. length", type="n")
axis(1, at=c(0,100, 200, 300, 400, 500),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(puppiesmusc$length, puppiesmusc$C, pch=16, col="blue")

########################## muscle pups vs. C   #############################

plot(NULL, xlim=c(460, 750), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT pups muscle vs. length", type="n")
axis(1, at=c(460, 500, 550, 600, 650, 700, 750),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(pupsmusc$length, pupsmusc$C, pch=17, col="darkblue")

#################### muscle trophy vs. C ###############################
plot(NULL, xlim=c(700,1100), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT trophy muscle vs. length", type="n")
axis(1, at=c(700,800,900,1000,1100),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(trophymusc$length, trophymusc$C, pch=18, col="goldenrod")


############################### liver length vs. C###############
puppiesliv<-filter(liverlat,length<=482)
pupsliv<- filter(liverlat,length %in%483:711)
trophyliv<- filter(liverlat, length>=711)

plot(NULL, xlim=c(0,1200), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT liver length vs. carbon", type="n")
axis(1, at=c(0,200,400,600,800, 1000,1200),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(puppiesliv$length, puppiesliv$C, pch=16, col="blue")
points(pupsliv$length, pupsliv$C, pch=17, col="darkblue")
points(trophyliv$length, trophyliv$C, pch=18, col="goldenrod")

legend(950, -20, legend=c("Puppies" , "Pups", "Trophy"), 
       pch=c(16, 17, 18), col=c("blue", "darkblue", "goldenrod")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)

############################### muscle puppies vs. C   ###########################

plot(NULL, xlim=c(100,500), ylim= c(-40, -25), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT puppies liver C vs. length", type="n")
axis(1, at=c(100, 200, 300, 400, 500),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(puppiesliv$length, puppiesliv$C, pch=16, col="blue")

########################## muscle pups vs. C   #############################

plot(NULL, xlim=c(460, 750), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT pups liver C vs. length", type="n")
axis(1, at=c(460, 500, 550, 600, 650, 700, 750),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(pupsliv$length, pupsliv$C, pch=17, col="darkblue")

#################### muscle trophy vs. C ###############################
plot(NULL, xlim=c(700,1100), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT trophy liver C vs. length", type="n")
axis(1, at=c(700,800,900,1000,1100),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(trophyliv$length, trophyliv$C, pch=18, col="goldenrod")

#################  season separation in muscle and liver values   #############################################################


plot(NULL, xlim=c(100,500), ylim= c(-40, -25), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT puppies summer muscle C vs. length", type="n")
axis(1, at=c(100, 200, 300, 400, 500),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(LATpuppySuM$length, LATpuppySuM$C, pch=16, col="blue")
points(LATpuppySuL$length, LATpuppySuL$C, pch=17, col="red")  # liver if you want to compare to one another

################ fall   ##########################
plot(NULL, xlim=c(100,500), ylim= c(-40, -25), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT puppies fall muscle C vs. length", type="n")
axis(1, at=c(100, 200, 300, 400, 500),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(LATpuppyFaM$length, LATpuppyFaM$C, pch=16, col="blue")
points(LATpuppyFaL$length, LATpuppyFaL$C, pch=17, col="red")   # liver if you want to compare one to another

##################### winter  ####################

plot(NULL, xlim=c(100,500), ylim= c(-40, -25), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT puppies fall muscle C vs. length", type="n")
axis(1, at=c(100, 200, 300, 400, 500),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(LATpuppyWiM$length, LATpuppyWiM$C, pch=16, col="blue")
 points(LATpuppyWiL$length, LATpuppyWiL$C, pch=17, col="red")  # liver if you want to compare

##################   spring  ######################

plot(NULL, xlim=c(100,500), ylim= c(-40, -25), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT puppies fall muscle C vs. length", type="n")
axis(1, at=c(100, 200, 300, 400, 500),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(LATpuppySpM$length, LATpuppySpM$C, pch=16, col="blue")
#\ points(LATpuppySpL$length, LATpuppySpL$C, pch=16, col="red")  # liver if you want 

################### pups in individual seasons  ###########################################################################################################

plot(NULL, xlim=c(460, 750), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT pups summer muscle C vs. length", type="n")
axis(1, at=c(460, 500, 550, 600, 650, 700, 750),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(LATpupSuM$length, LATpupSuM$C, pch=16, col="blue")
 points(LATpupSuL$length, LATpupSuL$C, pch=17, col="red")  # liver if you want to compare to one another

################ fall   ##########################
plot(NULL, xlim=c(460, 750), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT pups muscle fall C vs. length", type="n")
axis(1, at=c(460, 500, 550, 600, 650, 700, 750),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(LATpupFaM$length, LATpupFaM$C, pch=16, col="blue")
points(LATpupFaL$length, LATpupFaL$C, pch=17, col="red")   # liver if you want to compare one to another

##################### winter  ####################

plot(NULL, xlim=c(460, 750), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT pups muscle winter C vs. length", type="n")
axis(1, at=c(460, 500, 550, 600, 650, 700, 750),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(LATpupWiM$length, LATpupWiM$C, pch=16, col="blue")
points(LATpupWiL$length, LATpupWiL$C, pch=17, col="red")  # liver if you want to compare

##################   spring  ######################

plot(NULL, xlim=c(460, 750), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT pups muscle spring C vs. length", type="n")
axis(1, at=c(460, 500, 550, 600, 650, 700, 750),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(LATpupSpM$length, LATpupSpM$C, pch=16, col="blue")
points(LATpupSpL$length, LATpupSpL$C, pch=16, col="red")  # liver if you want 

##################################################################################  trophy size class ##########################################################

plot(NULL, xlim=c(700,1100), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT trophy summer muscle C vs. length", type="n")
axis(1, at=c(700,800,900,1000,1100),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)


points(LATtrophySuM$length, LATtrophySuM$C, pch=16, col="blue")
points(LATtrophySuL$length, LATtrophySuL$C, pch=17, col="red")  # liver if you want to compare to one another

################ fall   ##########################
plot(NULL, xlim=c(700,1100), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT trophy fall muscle C vs. length", type="n")
axis(1, at=c(700,800,900,1000,1100),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(LATtrophyFaM$length, LATtrophyFaM$C, pch=16, col="blue")
points(LATtrophyFaL$length, LATtrophyFaL$C, pch=17, col="red")   # liver if you want to compare one to another

##################   spring  ######################

plot(NULL, xlim=c(700,1100), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT trophy spring muscle C vs. length", type="n")
axis(1, at=c(700,800,900,1000,1100),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

points(LATtrophySpM$length, LATtrophySpM$C, pch=16, col="blue")
 points(LATtrophySpL$length, LATtrophySpL$C, pch=16, col="red")  # liver if you want 



############################################################# sulfur stuff  ###################################################
par(mai=c(1,1,1,1), bg="white", fg="black")


plot(NULL, xlim=c(-40,-20), ylim= c(-2,8), xlab="Carbon", ylab="Sulfur", xaxt="n", yaxt="n",main="", type="n")

axis(1, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)

points(bbt$C, bbt$S, col="brown", pch=2)
points(brc$C, brc$S, col="goldenrod", pch=3)
points(koe$C, koe$S, col="red", pch=4)
points(whs$C, whs$S, col="green", pch=5)
points(utc$C, utc$S, col="purple", pch=6)
points(smb$C, smb$S, col="orange", pch=7)
points(rbt$C, rbt$S, col="gray", pch=8)
points(chiro$C, chiro$S, col="gray", pch=9)
points(zoo$C, zoo$S, col="dark blue", pch=10)
points(crf$C, crf$S, col="black", pch=11)
points(snails$C, snails$S, col="blue", pch=12)
points(amp$C,amp$S, col="red",pch=13)


######      separate regions muscle and liver ######################################


plot(NULL, xlim=c(180,800), ylim= c(0,8), xlab="Length in mm", ylab=expression(paste(delta^{34}, "S")),main="LAT Length vs.Sulfur",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(musclelat$length, musclelat$S, pch=16, col="blue")

plot(NULL, xlim=c(180,800), ylim= c(0,8), xlab="Length in mm", ylab=expression(paste(delta^{34}, "S")),main="LAT Length (LIVER) vs.Sulfur",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(liverlat$length, liverlat$S, pch=17, col="red")
#canyon
plot(NULL, xlim=c(180,1100), ylim= c(0, 8), xlab="Length in mm", ylab=expression(paste(delta^{34}, "S")),main="LAT Length vs.Sulfur Canyon muscle",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(latCm$length, latCm$S,pch= 16, col="blue")

plot(NULL, xlim=c(180,1100), ylim= c(0, 8), xlab="Length in mm", ylab=expression(paste(delta^{34}, "S")),main="LAT Length vs.Sulfur Canyon liver",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(latCl$length, latCl$S,pch= 17, col="red")

#inflow
plot(NULL, xlim=c(180,1000), ylim= c(0,8), xlab="Length in mm", ylab=expression(paste(delta^{34}, "S")),main="LAT Length vs.Sulfur Inflow muscle",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(latIFm$length, latIFm$S, pch=16, col="darkblue")

plot(NULL, xlim=c(180,1000), ylim= c(0,8), xlab="Length in mm", ylab=expression(paste(delta^{34}, "S")),main="LAT Length vs.Sulfur Inflow liver",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(latIFl$length, latIFl$S, pch=17, col="red")
#open hills
plot(NULL, xlim=c(180,1100), ylim= c(0,8), xlab="Length in mm", ylab=expression(paste(delta^{34}, "S")),main="LAT Length vs.Sulfur Open Hills muscle",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(latOHm$length, latOHm$S, pch=16, col="goldenrod")

plot(NULL, xlim=c(180,1100), ylim= c(0,8), xlab="Length in mm", ylab=expression(paste(delta^{34}, "S")),main="LAT Length vs.Sulfur Open Hills liver",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(latOHl$length, latOHl$S, pch=17, col="red")

############################### muscle puppies vs. S  ###########################

plot(NULL, xlim=c(100,500), ylim= c(0,8), xlab="Length", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="LAT puppies liver vs. length", type="n")
axis(1, at=c(100, 200, 300, 400, 500),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)
points(puppiesliv$length, puppiesliv$S, pch=16, col="red")
# points(puppiesmusc$length, puppiesmusc$S, pch=17, col="blue")

########################## muscle pups vs. S   #############################

plot(NULL, xlim=c(460, 750), ylim= c(0,8), xlab="Length", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="LAT pups liver C vs. length", type="n")
axis(1, at=c(460, 500, 550, 600, 650, 700, 750),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)

points(pupsliv$length, pupsliv$S, pch=17, col="red")
#points(pupsmusc$length, pupsmusc$S, pch=16, col="darkblue")
#################### muscle trophy vs. S ###############################
plot(NULL, xlim=c(700,1100), ylim= c(0,8), xlab="Length", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="LAT trophy liver C vs. length", type="n")
axis(1, at=c(700,800,900,1000,1100),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)

points(trophymusc$length, trophymusc$S, pch=18, col="blue")
#points(trophyliv$length, trophyliv$S, pch=18, col="red")

#################  seasonal separation in muscle and liver S values   #############################################################


plot(NULL, xlim=c(100,500), ylim= c(0,8), xlab="Length", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="LAT puppies summer M&L vs. length", type="n")
axis(1, at=c(100, 200, 300, 400, 500),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)

points(LATpuppySuM$length, LATpuppySuM$S, pch=16, col="blue")
points(LATpuppySuL$length, LATpuppySuL$S, pch=17, col="red")  # liver if you want to compare to one another
legend(400, 8, legend=c("Muscle", "Liver"), 
       pch=c(16, 17), col=c("blue", "red")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
################ fall   ##########################
plot(NULL, xlim=c(100,500), ylim= c(0,8), xlab="Length", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="LAT puppies FALL M&L vs. length", type="n")
axis(1, at=c(100, 200, 300, 400, 500),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)


points(LATpuppyFaM$length, LATpuppyFaM$S, pch=16, col="blue")
points(LATpuppyFaL$length, LATpuppyFaL$S, pch=17, col="red")   # liver if you want to compare one to another
legend(400, 8, legend=c("Muscle", "Liver"), 
       pch=c(16, 17), col=c("blue", "red")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
##################### winter  ####################

plot(NULL, xlim=c(100,500), ylim= c(0,8), xlab="Length", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="LAT puppies WINTER M&L vs. length", type="n")
axis(1, at=c(100, 200, 300, 400, 500),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)


points(LATpuppyWiM$length, LATpuppyWiM$S, pch=16, col="blue")
 points(LATpuppyWiL$length, LATpuppyWiL$S, pch=17, col="red")  # liver if you want to compare
legend(400, 8, legend=c("Muscle", "Liver"), 
       pch=c(16, 17), col=c("blue", "red")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
##################   spring  ######################

plot(NULL, xlim=c(100,500), ylim= c(0,8), xlab="Length", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="LAT puppies SPRING M&L vs. length", type="n")
axis(1, at=c(100, 200, 300, 400, 500),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)


points(LATpuppySpM$length, LATpuppySpM$S, pch=16, col="blue")
 points(LATpuppySpL$length, LATpuppySpL$S, pch=17, col="red")  # liver if you want 
legend(400, 8, legend=c("Muscle", "Liver"), 
       pch=c(16, 17), col=c("blue", "red")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
################### pups in individual seasons  ###########################################################################################################

plot(NULL, xlim=c(460, 750), ylim= c(0,8), xlab="Length", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="LAT pups summer M&L vs. length", type="n")
axis(1, at=c(460, 500, 550, 600, 650, 700, 750),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)

points(LATpupSuM$length, LATpupSuM$S, pch=16, col="blue")
points(LATpupSuL$length, LATpupSuL$S, pch=17, col="red")  # liver if you want to compare to one another
legend(680, 8, legend=c("Muscle", "Liver"), 
       pch=c(16, 17), col=c("blue", "red")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
################ fall   ##########################
plot(NULL, xlim=c(460, 750), ylim= c(0,8), xlab="Length", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="LAT pups FALL M&L vs. length", type="n")
axis(1, at=c(460, 500, 550, 600, 650, 700, 750),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)

points(LATpupFaM$length, LATpupFaM$S, pch=16, col="blue")
points(LATpupFaL$length, LATpupFaL$S, pch=17, col="red")   # liver if you want to compare one to another
legend(680, 8, legend=c("Muscle", "Liver"), 
       pch=c(16, 17), col=c("blue", "red")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
##################### winter  ####################

plot(NULL, xlim=c(460, 750), ylim= c(0,8), xlab="Length", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="LAT pups WINTER M&L vs. length", type="n")
axis(1, at=c(460, 500, 550, 600, 650, 700, 750),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)

points(LATpupWiM$length, LATpupWiM$S, pch=16, col="blue")
 points(LATpupWiL$length, LATpupWiL$S, pch=17, col="red")  # liver if you want to compare
legend(680, 8, legend=c("Muscle", "Liver"), 
       pch=c(16, 17), col=c("blue", "red")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
##################   spring  ######################

plot(NULL, xlim=c(460, 750), ylim= c(0,8), xlab="Length", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="LAT pups SPRING M&L vs. length", type="n")
axis(1, at=c(460, 500, 550, 600, 650, 700, 750),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)

points(LATpupSpM$length, LATpupSpM$S, pch=16, col="blue")
 points(LATpupSpL$length, LATpupSpL$S, pch=17, col="red")  # liver if you want 
legend(680, 8, legend=c("Muscle", "Liver"), 
       pch=c(16, 17), col=c("blue", "red")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
##################################################################################  trophy size class ##########################################################

plot(NULL, xlim=c(700,1100), ylim= c(0,8), xlab="Length", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="LAT trophy SUMMER M&L vs. length", type="n")
axis(1, at=c(700,800,900,1000,1100),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)


points(LATtrophySuM$length, LATtrophySuM$S, pch=16, col="blue")
 points(LATtrophySuL$length, LATtrophySuL$S, pch=17, col="red")  # liver if you want to compare to one another
legend(1000, 8, legend=c("Muscle", "Liver"), 
       pch=c(16, 17), col=c("blue", "red")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
################ fall   ##########################
plot(NULL, xlim=c(700,1100), ylim= c(0,8), xlab="Length", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="LAT trophy FALL M&L vs. length", type="n")
axis(1, at=c(700,800,900,1000,1100),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)

points(LATtrophyFaM$length, LATtrophyFaM$S, pch=16, col="blue")
points(LATtrophyFaL$length, LATtrophyFaL$S, pch=17, col="red")   # liver if you want to compare one to another
legend(1000, 8, legend=c("Muscle", "Liver"), 
       pch=c(16, 17), col=c("blue", "red")
       , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
##################   spring  ######################

plot(NULL, xlim=c(700,1100), ylim= c(0,8), xlab="Length", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="LAT trophy SPRING M&L vs. length", type="n")
axis(1, at=c(700,800,900,1000,1100),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)

points(LATtrophySpM$length, LATtrophySpM$S, pch=16, col="blue")
 points(LATtrophySpL$length, LATtrophySpL$S, pch=17, col="red")  # liver if you want 
 
 legend(1000, 8, legend=c("Muscle", "Liver"), 
        pch=c(16, 17), col=c("blue", "red")
        , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
 
######################################################  ONE GRAPH THREE REGIONS ###################### 
 plot(NULL, xlim=c(0,1200), ylim= c(0,8), xlab="Length", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="LAT liver length vs. SULFUR", type="n")
 axis(1, at=c(0,200,400,600,800, 1000,1200),las=1, cex.axis=10/10)
 axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)
 
 points(puppiesliv$length, puppiesliv$S, pch=16, col="blue")
 points(pupsliv$length, pupsliv$S, pch=17, col="darkblue")
 points(trophyliv$length, trophyliv$S, pch=18, col="goldenrod")
 
 legend(800, 8, legend=c("Puppies" , "Pups", "Trophy"), 
        pch=c(16, 17, 18), col=c("blue", "darkblue", "goldenrod")
        , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)

 ####### EACH SEASON
 plot(NULL, xlim=c(-38,-23), ylim= c(0,8), xlab="CARBON", ylab=expression(paste(delta^{34}, "S")), xaxt="n", yaxt="n",main="C vs. N all lat topes muscle", type="n")
 axis(1, at=c(-38, -34, -30, -26, -24),las=1, cex.axis=10/10)
 axis(2, at=c(0,2,4,6,8),las=1, cex.axis=10/10)
 
 points(latIFm$C, latIFm$S, pch=16, col="blue")
 points(latOHm$C, latOHm$S, pch=17, col="red")
 points(latCm$C, latCm$S, pch=18, col="cyan")
 
 legend(-25, 20, legend=c("IF" , "OH", "Can"), 
        pch=c(16, 17, 18), col=c("blue", "red", "cyan")
        , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
 
########################################################## SEX ######################
 malelatpuppies<- filter(latpuppies,sex=='m')
 femalelatpuppies<- filter(latpuppies,sex=='f')
 malelatpups<- filter(latpups,sex=='m')
 femalelatpups<- filter(latpups,sex=='f')
 malelattrophy<- filter(lattrophy,sex=='m')
 femalelattrophy<- filter(lattrophy,sex=='f')
 
 malelatpuppies<- filter(malelatpuppies,tissue=='liver')
 femalelatpuppies<- filter(femalelatpuppies,tissue=='liver')
 malelatpups<- filter(malelatpups,tissue=='liver')
 femalelatpups<- filter(femalelatpups,tissue=='liver')
 malelattrophy<- filter(malelattrophy,tissue=='liver')
 femalelattrophy<- filter(femalelattrophy,tissue=='liver')
 
 
 
 ### puppies ###
 plot(NULL, xlim=c(-38,-23), ylim= c(10,20), xlab="Carbon", ylab="Nitrogen", xaxt="n", yaxt="n",main="Male vs Female LAT puppies", type="n")
 axis(1, at=c(-38, -34, -30, -26, -24),las=1, cex.axis=10/10)
 axis(2, at=c(10,12,14,16,18,20),las=1, cex.axis=10/10)
 
 points(malelatpuppies$C, malelatpuppies$N, pch=16, col="blue")
 points(femalelatpuppies$C, femalelatpuppies$N, pch=17, col="red")

 
 legend(-25, 20, legend=c("Male" , "Female"), 
        pch=c(16, 17), col=c("blue", "red")
        , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
###  pups  ###
 
 plot(NULL, xlim=c(-37,-22), ylim= c(10,20), xlab="Carbon", ylab="Nitrogen", xaxt="n", yaxt="n",main="Male vs. Female LAT pups", type="n")
 axis(1, at=c( -36, -33, -30, -27, -24),las=1, cex.axis=10/10)
 axis(2, at=c(10,15,20),las=1, cex.axis=10/10)
 
 points(malelatpups$C, malelatpups$N, pch=16, col="blue")
 points(femalelatpups$C, femalelatpups$N, pch=17, col="red")

 
 legend(-25, 20, legend=c("Male" , "Female"), 
        pch=c(16, 17), col=c("blue", "red")
        , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
 ### trophy ###
 
 plot(NULL, xlim=c(-37,-22), ylim= c(10,20), xlab="Carbon", ylab="Nitrogen", xaxt="n", yaxt="n",main="Male vs. Female LAT trophy", type="n")
 axis(1, at=c( -36, -33, -30, -27, -24),las=1, cex.axis=10/10)
 axis(2, at=c(10,15,20),las=1, cex.axis=10/10)
 
 points(malelattrophy$C, malelattrophy$N, pch=16, col="blue")
 points(femalelattrophy$C, femalelattrophy$N, pch=17, col="red")
 
 
 legend(-25, 20, legend=c("Male" , "Female"), 
        pch=c(16, 17), col=c("blue", "red")
        , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
 
 ########### length vs M & F   ###############################################
 
 plot(NULL, xlim=c(200,500), ylim= c(-40, -25), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT puppies muscle vs. length", type="n")
 axis(1, at=c( 200, 300, 400, 500),las=1, cex.axis=10/10)
 axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)
 
 points(malelatpuppies$length, malelatpuppies$C, pch=16, col="blue")
 points(femalelatpuppies$length, femalelatpuppies$C, pch=17, col="red")
 legend(450, -25, legend=c("Male" , "Female"), 
        pch=c(16, 17), col=c("blue", "red")
        , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
 
 plot(NULL, xlim=c(460, 750), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT pups liver C vs. length", type="n")
 axis(1, at=c(460, 500, 550, 600, 650, 700, 750),las=1, cex.axis=10/10)
 axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)
 
 points(malelatpups$length, malelatpups$C, pch=16, col="blue")
 points(femalelatpups$length, femalelatpups$C, pch=17, col="red")
 legend(700, -20, legend=c("Male" , "Female"), 
        pch=c(16, 17), col=c("darkblue", "red")
        , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
 
 plot(NULL, xlim=c(700,1100), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="LAT trophy muscle vs. length", type="n")
 axis(1, at=c(700,800,900,1000,1100),las=1, cex.axis=10/10)
 axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)
 
 points(malelattrophy$length, malelattrophy$C, pch=16, col="blue")
 points(femalelattrophy$length, femalelattrophy$C, pch=17, col="red")
 legend(-25, 20, legend=c("Male" , "Female"), 
        pch=c(16, 17), col=c("blue", "red")
        , bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)
 
 
 
 