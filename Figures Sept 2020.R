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

par(mai=c(1,1,1,1), bg="white", fg="black")


#Carbon vs Nitrogen

plot( NULL,xlim=c(-38,-20), ylim= c(8,18), ylab=expression(paste(delta^{15}, "N")), xlab=expression(paste(delta^{13}, "C"))
      ,main= "Carbon vs. Nitrogen",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
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

#Carbon vs length in different regions

lat.canyon<-subset(lat, lat$region=="canyon")
lat.inflow<-subset(lat, lat$region=="inflow")
lat.oh<-subset(lat, lat$region=="open_hills")

#canyon
plot(NULL, xlim=c(180,800), ylim= c(-36, -23), xlab="Length in mm", ylab=expression(paste(delta^{13}, "C")),main="LAT Length vs.Carbon in Canyon",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.canyon$length, lat.canyon$C, cex=(lat.canyon$age)/8,pch=17)
#inflow
plot(NULL, xlim=c(180,800), ylim= c(-36, -23), xlab="Length in mm", ylab=expression(paste(delta^{13}, "C")),main="LAT Length vs.Carbon in Inflow",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.inflow$length, lat.inflow$C, cex=(lat.inflow$age)/8, pch=16, col="brown")
#open hills
plot(NULL, xlim=c(180,800), ylim= c(-36, -23), xlab="Length in mm", ylab=expression(paste(delta^{13}, "C")),main="LAT Length vs.Carbon in OH",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.oh$length, lat.oh$C, cex=(lat.oh$age)/8, pch=16, col="forestgreen")

#all together
plot(NULL, xlim=c(180,800), ylim= c(-36, -23), xlab="Length in mm", ylab=expression(paste(delta^{13}, "C")),main="LAT Length vs.Carbon",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.canyon$length, lat.canyon$C, cex=(lat.canyon$age)/8,pch=17)
points(lat.inflow$length, lat.inflow$C, cex=(lat.inflow$age)/8, pch=16, col="brown")
points(lat.oh$length, lat.oh$C, cex=(lat.oh$age)/8, pch=18, col="forestgreen")

legend(200,-24, legend=c("Canyon","Inflow", "Open Hills"), pch=c(17,16,18),col = c("black","brown","forestgreen"))



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
plot(NULL, xlim=c(-35,-20), ylim= c(0,8), xlab=expression(paste(delta^{13}, "C")), ylab=expression(paste(delta^{34}, "S")),main="LAT Carbon vs.Sulfur June Sampling",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.june$C, lat.june$S,pch= 16, col="red")

#july
plot(NULL, xlim=c(-35,-20), ylim= c(0,8), xlab=expression(paste(delta^{13}, "C")), ylab=expression(paste(delta^{34}, "S")),main="LAT Carbon vs.Sulfur July Sampling",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.july$C, lat.july$S,pch= 16, col="blue")
#august
plot(NULL, xlim=c(-36,-20), ylim= c(0,8), xlab=expression(paste(delta^{13}, "C")), ylab=expression(paste(delta^{34}, "S")),main="LAT Carbon vs.Sulfur August Sampling",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.august$C, lat.august$S,pch= 16, col="goldenrod")

#all together
plot(NULL, xlim=c(-35,-20), ylim= c(0,8), xlab=expression(paste(delta^{13}, "C")), ylab=expression(paste(delta^{34}, "S")),main="LAT Carbon vs.Sulfur ",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.june$C, lat.june$S,pch= 16, col="red")
points(lat.july$C, lat.july$S,pch= 16, col="blue")
points(lat.august$C, lat.august$S,pch= 16, col="goldenrod")


legend(-35,8, legend=c("June","July", "August"), pch=c(16,16,16), col=c("red","dark blue", "goldenrod"))


# C vs S separatd by region

#inflow
plot(NULL, xlim=c(-35,-20), ylim= c(0,8), xlab=expression(paste(delta^{13}, "C")), ylab=expression(paste(delta^{34}, "S")),main="Inflow LAT Carbon vs.Sulfur",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.inflow$C, lat.inflow$S,pch= 16, col="red")

#open hills
plot(NULL, xlim=c(-35,-20), ylim= c(0,8), xlab=expression(paste(delta^{13}, "C")), ylab=expression(paste(delta^{34}, "S")),main="Open Hills LAT Carbon vs.Sulfur",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.oh$C, lat.oh$S,pch= 17, col="blue")
#canyon
plot(NULL, xlim=c(-36,-20), ylim= c(0,8), xlab=expression(paste(delta^{13}, "C")), ylab=expression(paste(delta^{34}, "S")),main="Canyon LAT Carbon vs.Sulfur",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.canyon$C, lat.canyon$S,pch= 18, col="goldenrod")

#all together
plot(NULL, xlim=c(-35,-20), ylim= c(0,8), xlab=expression(paste(delta^{13}, "C")), ylab=expression(paste(delta^{34}, "S")),main="LAT Carbon vs.Sulfur",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
points(lat.inflow$C, lat.inflow$S,pch= 16, col="red")
points(lat.oh$C, lat.oh$S,pch= 17, col="blue")
points(lat.canyon$C, lat.canyon$S,pch= 15, col="goldenrod")


legend(-35,8, legend=c("Inflow","Open Hills", "Canyon"), pch=c(16,17,15), col=c("red","dark blue", "goldenrod"))


# Simmr Plots
library(simmr)

LAT<-simmr_data_2020
prey<-isoprey1
preys<-isoprey2
TEFs<-TEFs_simmr

LAT_simmr = simmr_load(mixtures = as.matrix(LAT[, 1:2]),
                       source_names = prey$Sources,
                       source_means = prey[,2:3],
                       source_sds = prey[,5:6],
                       group= NULL)

plot(LAT_simmr,xlab = expression(paste(delta^13, "C (\u2030)",
                                       sep = "")), 
     ylab = expression(paste(delta^15, "N (\u2030)",
                             sep = "")), 
     title = 'LAT C vs. N')

LAT_simmr_out = simmr_mcmc(LAT_simmr)
summary(LAT_simmr_out, type = 'diagnostics',
        group = 1)


posterior_predictive(LAT_simmr_out)
prior_viz(LAT_simmr_out)
plot(LAT_simmr_out, type = 'histogram')

#simmr with C and S
LATCS<- csmixtures2020
mix<-as.matrix(LATCS[,1:2])
preys<-isoprey2
mix
LAT_simmrS = simmr_load( mixtures = mix,
                         source_names = preys$Sources,
                         source_means = preys[,2:3],
                         source_sds = preys[,4:5],
                         group = NULL)

plot(LAT_simmrS, xlab = expression(paste(delta^13, "C (\u2030)",
                                         sep = "")), 
     ylab = expression(paste(delta^34, "S (\u2030)",
                             sep = "")), 
     title = 'LAT C vs S')

LAT_simmr_out = simmr_mcmc(LAT_simmrS)
summary(LAT_simmr_out, type = 'diagnostics',
        group = 1)


posterior_predictive(LAT_simmr_out)
prior_viz(LAT_simmr_out)
plot(LAT_simmr_out, type = 'histogram')



