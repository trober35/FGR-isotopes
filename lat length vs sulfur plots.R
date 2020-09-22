#lat length vs S
plot(NULL, xlim=c(180,800), ylim= c(0,8), xlab="Length in mm", ylab=expression(paste(delta^{34}, "S")),main="Length vs.sulfur",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)


points(lat$length, lat$S, pch=16, col="blue")

points(lat.canyon$length, lat.canyon$S, cex=(lat.canyon$age)/8)
points(lat.inflow$length, lat.inflow$S, cex=(lat.inflow$age)/8, pch=16, col="darkblue")
points(lat.oh$length, lat.oh$S, cex=(lat.oh$age)/8, pch=16, col="goldenrod")

lat.canyon<-subset(lat, lat$region=="canyon")
lat.inflow<-subset(lat, lat$region=="inflow")
lat.oh<-subset(lat, lat$region=="open_hills")

#sulfur vs length separated by region
plot(NULL, xlim=c(180,800), ylim= c(0,8), xlab="Length in mm", ylab=expression(paste(delta^{34}, "S")),main="LAT Length vs.Sulfur",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat$length, lat$S, pch=16, col="blue")
#canyon
plot(NULL, xlim=c(180,800), ylim= c(0, 8), xlab="Length in mm", ylab=expression(paste(delta^{13}, "C")),main="LAT Length vs.Sulfur Canyon",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.canyon$length, lat.canyon$S, cex=(lat.canyon$age)/8,pch= 16, col="red")
#inflow
plot(NULL, xlim=c(180,800), ylim= c(0,8), xlab="Length in mm", ylab=expression(paste(delta^{13}, "C")),main="LAT Length vs.Sulfur Inflow",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.inflow$length, lat.inflow$S, cex=(lat.inflow$age)/8, pch=16, col="darkblue")
#open hills
plot(NULL, xlim=c(180,800), ylim= c(0,8), xlab="Length in mm", ylab=expression(paste(delta^{13}, "C")),main="LAT Length vs.Sulfur Open Hills",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.oh$length, lat.oh$S, cex=(lat.oh$age)/8, pch=16, col="goldenrod")

#all together
plot(NULL, xlim=c(180,800), ylim= c(0,8), xlab="Length in mm", ylab=expression(paste(delta^{34}, "S")),main="LAT Length vs.Sulfur",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.canyon$length, lat.canyon$S, pch= 16, col="red")
points(lat.inflow$length, lat.inflow$S, cex=(lat.inflow$age)/8, pch=16, col="darkblue")
points(lat.oh$length, lat.oh$S, cex=(lat.oh$age)/8, pch=16, col="goldenrod")

legend(200,8, legend=c("Inflow","Open Hills", "Canyon"), pch=c(16,16,16), col=c("red","dark blue", "goldenrod"))


#CARBON VS SULFUR separated  by sampling date.


lat.june<-subset(lat, lat$date=="43641"|lat$date=="43642")
lat.july<-subset(lat, lat$date=="43663"|lat$date=="43669"|lat$date=="43670"|lat$date=="43662")
lat.august<-subset(lat, lat$date=="43704"|lat$date=="43706")


#june
plot(NULL, xlim=c(-35,-20), ylim= c(0,8), xlab="Length in mm", ylab=expression(paste(delta^{34}, "S")),main="LAT Length vs.Sulfur",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.june$C, lat.june$S,pch= 16, col="red")

#july
plot(NULL, xlim=c(-35,-20), ylim= c(0,8), xlab="Length in mm", ylab=expression(paste(delta^{34}, "S")),main="LAT Length vs.Sulfur",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.july$C, lat.july$S,pch= 16, col="blue")
#august
plot(NULL, xlim=c(-36,-20), ylim= c(0,8), xlab="Length in mm", ylab=expression(paste(delta^{34}, "S")),main="LAT Length vs.Sulfur",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.august$C, lat.august$S,pch= 16, col="goldenrod")

#all together
plot(NULL, xlim=c(-35,-20), ylim= c(0,8), xlab=expression(paste(delta^{13}, "C")), ylab=expression(paste(delta^{34}, "S")),main="LAT Carbon vs.Sulfur",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.june$C, lat.june$S,pch= 16, col="red")
points(lat.july$C, lat.july$S,pch= 16, col="blue")
points(lat.august$C, lat.august$S,pch= 16, col="goldenrod")


legend(-35,8, legend=c("June","July", "August"), pch=c(16,16,16), col=c("red","dark blue", "goldenrod"))


# C vs S separatd by region

#inflow
plot(NULL, xlim=c(-35,-20), ylim= c(0,8), xlab=expression(paste(delta^{13}, "C")), ylab=expression(paste(delta^{34}, "S")),main="LAT Carbon vs.Sulfur Inflow",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.inflow$C, lat.inflow$S,pch= 16, col="red")

#open hills
plot(NULL, xlim=c(-35,-20), ylim= c(0,8), xlab=expression(paste(delta^{13}, "C")), ylab=expression(paste(delta^{34}, "S")),main="LAT Carbon vs.Sulfur Open Hills",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.oh$C, lat.oh$S,pch= 17, col="blue")
#canyon
plot(NULL, xlim=c(-36,-20), ylim= c(0,8), xlab=expression(paste(delta^{13}, "C")), ylab=expression(paste(delta^{34}, "S")),main="LAT Carbon vs.Sulfur Canyon",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.canyon$C, lat.canyon$S,pch= 18, col="goldenrod")

#all together
plot(NULL, xlim=c(-35,-20), ylim= c(0,8), xlab=expression(paste(delta^{13}, "C")), ylab=expression(paste(delta^{34}, "S")),main="LAT Carbon vs.Sulfur",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.inflow$C, lat.inflow$S,pch= 16, col="red")
points(lat.oh$C, lat.oh$S,pch= 17, col="blue")
points(lat.canyon$C, lat.canyon$S,pch= 15, col="goldenrod")


legend(-35,8, legend=c("Inflow","Open Hills", "Canyon"), pch=c(16,17,15), col=c("red","dark blue", "goldenrod"))






