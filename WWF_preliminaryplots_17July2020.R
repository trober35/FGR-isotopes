

fish=read.table("Data/Isotope_FirstLook.txt", header = T)
ages<-read.csv("Data/AgesToLink.csv", header=T)

fish.aged<-merge(fish, ages, by="id_number")

head(fish)
dim(fish)

lat = subset(fish, fish$species == "LAT")
bbt = subset(fish, fish$species == "BBT")
brc = subset(fish, fish$species == "BRC")
koe = subset(fish, fish$species == "KOE")
whs = subset(fish, fish$species == "WHS")
utc = subset(fish, fish$species == "UTC")
smb = subset(fish, fish$species == "SMB")
rbt = subset(fish, fish$species == "RBT")

#####Carbon vs Nitrogen

plot(NULL, xlim=c(-40,-20), ylim= c(8,18), xlab=expression(paste(delta^{13}),"Carbon"), ylab="Nitrogen", xaxt="n", yaxt="n",main="", type="p")

points(lat$C, lat$N, col="blue",pch=16)
points(bbt$C, bbt$N, col="brown", pch=17)
points(brc$C, brc$N, col="goldenrod", pch=18)
points(koe$C, koe$N, col="red", pch=19)
points(whs$C, whs$N, col="green", pch=20)
points(utc$C, utc$N, col="purple", pch=21)
points(smb$C, smb$N, col="orange", pch=22)
points(rbt$C, rbt$N, col="gray", pch=8)

axis(1, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)
axis(2, at=c(5,10,15,20),las=1, cex.axis=10/10)

legend(-40, 15, legend=c("LAT", "BBT", "BRC", "KOE", "WHS", "UTC", "SMB", "RBT"), 
	pch=c(1,2,3,4,5,6,7,8), col=c("blue", "brown",
	 "goldenrod", "red", "green", "purple","orange", "gray")
	, bty="n", cex=1, pt.cex=2, lty=NULL, merge=FALSE, trace=FALSE)

#####Carbon vs Sulfur

plot(NULL, xlim=c(-40,-20), ylim= c(-2,8), xlab="Carbon", ylab="Sulfur", xaxt="n", yaxt="n",main="", type="n")

points(lat$C, lat$S, col="blue")
points(bbt$C, bbt$S, col="brown", pch=2)
points(brc$C, brc$S, col="yellow", pch=3)
points(koe$C, koe$S, col="red", pch=4)
points(whs$C, whs$S, col="green", pch=5)
points(utc$C, utc$S, col="purple", pch=6)
points(smb$C, smb$S, col="orange", pch=7)
points(rbt$C, rbt$S, col="gray", pch=8)

axis(1, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)

legend(-40, 5, legend=c("LAT", "BBT", "BRC", "KOE", "WHS", "UTC", "SMB", "RBT"), 
	pch=c(1,2,3,4,5,6,7,8), col=c("blue", "brown",
	 "yellow", "red", "green", "purple","orange", "gray")
	, bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)

#####Nitrogen vs Sulfur

plot(NULL, xlim=c(5,20), ylim= c(-2,8), xlab="Nitrogen", ylab="Sulfur", xaxt="n", yaxt="n",main="", type="n")

points(lat$N, lat$S, col="blue")
points(bbt$N, bbt$S, col="brown", pch=2)
points(brc$N, brc$S, col="yellow", pch=3)
points(koe$N, koe$S, col="red", pch=4)
points(whs$N, whs$S, col="green", pch=5)
points(utc$N, utc$S, col="purple", pch=6)
points(smb$N, smb$S, col="orange", pch=7)
points(rbt$N, rbt$S, col="gray", pch=8)

axis(1, at=c(5,10,15,20),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)

legend(6, 5, legend=c("LAT", "BBT", "BRC", "KOE", "WHS", "UTC", "SMB", "RBT"), 
	pch=c(1,2,3,4,5,6,7,8), col=c("blue", "brown",
	 "yellow", "red", "green", "purple","orange", "gray")
	, bty="n", cex=1, pt.cex=1, lty=NULL, merge=FALSE, trace=FALSE)


#####LAT LENGTH VS CARBON

plot(NULL, xlim=c(0,800), ylim= c(-40, -20), xlab="Length", ylab="Carbon", xaxt="n", yaxt="n",main="", type="n")

points(lat$length, lat$C)

lat.age<- subset(fish.aged, fish.aged$species == "LAT")

lat.age.canyon<-subset(lat.age, lat.age$region=="canyon")
lat.age.inflow<-subset(lat.age, lat.age$region=="inflow")
lat.age.oh<-subset(lat.age, lat.age$region=="open_hills")

points(lat.age.canyon$length, lat.age.canyon$C, cex=(lat.age.canyon$age)/8)
points(lat.age.inflow$length, lat.age.inflow$C, cex=(lat.age.inflow$age)/8, col="darkblue")
points(lat.age.oh$length, lat.age.oh$C, cex=(lat.age.oh$age)/8, col="goldenrod")


axis(1, at=c(0,200,400,600,800),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)

#####LAT LENGTH VS NITROGEN

plot(NULL, xlim=c(0,800), ylim= c(5,20), xlab="Length", ylab="Nitrogen", xaxt="n", yaxt="n",main="", type="n")

points(lat$length, lat$N)

axis(1, at=c(0,200,400,600,800),las=1, cex.axis=10/10)
axis(2, at=c(5,10,15,20),las=1, cex.axis=10/10)

#####LAT LENGTH VS SULFUR

plot(NULL, xlim=c(0,800), ylim= c(-2,8), xlab="Length", ylab="Sulfur", xaxt="n", yaxt="n",main="", type="n")

points(lat$length, lat$S,pch=16,col='blue')

axis(1, at=c(0,200,400,600,800),las=1, cex.axis=10/10)
axis(2, at=c(-2,0,2,4,6,8),las=1, cex.axis=10/10)



