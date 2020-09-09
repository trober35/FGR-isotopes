plot( NULL,xlim=c(-38,-20), ylim= c(8,18), ylab=expression(paste(delta^{15}, "N")), xlab=expression(paste(delta^{13}, "C"))
     ,main= "Carbon vs. Nitrogen",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
par(mai=c(1,1,1,1), bg="white", fg="black")

points(lat$C, lat$N, col="blue",pch=16)
points(bbt$C, bbt$N, col="brown", pch=17)
points(brc$C, brc$N, col="goldenrod", pch=18)
points(koe$C, koe$N, col="red", pch=19)
points(whs$C, whs$N, col="green", pch=20)
points(utc$C, utc$N, col="purple", pch=16)
points(smb$C, smb$N, col="orange", pch=22)
points(rbt$C, rbt$N, col="gray", pch=8)


legend(-38, 13,legend=c("LAT", "BBT", "BRC", "KOE", "WHS", "UTC", "SMB", "RBT"), 
       pch=c(16,17,18,19,20,16,22,8), col=c("blue", "brown",
                                     "goldenrod", "red", "green", "purple","orange", "gray")
       , bty="n", cex=1, pt.cex=2, lty=NULL, merge=FALSE, trace=FALSE)

#carbon vs length

plot(NULL, xlim=c(180,800), ylim= c(-36, -23), xlab="Length in mm", ylab=expression(paste(delta^{13}, "C")),main="Length vs.Carbon",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)

points(lat$length, lat$C, pch=16, col="blue")

lat.age<- subset(fish.aged, fish.aged$species == "LAT")

lat.age.canyon<-subset(lat.age, lat.age$region=="canyon")
lat.age.inflow<-subset(lat.age, lat.age$region=="inflow")
lat.age.oh<-subset(lat.age, lat.age$region=="open_hills")

points(lat.age.canyon$length, lat.age.canyon$C, cex=(lat.age.canyon$age)/8)
points(lat.age.inflow$length, lat.age.inflow$C, cex=(lat.age.inflow$age)/8, col="darkblue")
points(lat.age.oh$length, lat.age.oh$C, cex=(lat.age.oh$age)/8, col="goldenrod")


axis(1, at=c(0,200,400,600,800),las=1, cex.axis=10/10)
axis(2, at=c(-40, -35, -30, -25, -20),las=1, cex.axis=10/10)







