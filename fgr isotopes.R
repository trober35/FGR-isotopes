library(simmr)
system.file("extdata", "geese_data.xls", package = "simmr")
library(readxl)
path = system.file("extdata", "geese_data.xls", package = "simmr")
geese_data = lapply(excel_sheets(path), read_excel, path = path)



system(paste('open',path))
targets = geese_data[[1]]
sources = geese_data[[2]]
TEFs = geese_data[[3]]
concdep = geese_data[[4]]



geese_simmr = simmr_load(mixtures = as.matrix(targets[, 1:2]),
                         source_names = sources$Sources,
                         source_means = sources[,2:3],
                         source_sds = sources[,4:5],
                         correction_means = TEFs[,2:3],
                         correction_sds = TEFs[,4:5],
                         concentration_means = concdep[,2:3],
                         group = as.factor(paste('Day', targets$Time)))

plot(geese_simmr, group = 1:8)


geese_simmr_out = simmr_mcmc(geese_simmr)
summary(geese_simmr_out, type = 'diagnostics',
        group = 1)


posterior_predictive(geese_simmr_out, group = 5)
prior_viz(geese_simmr_out)
plot(geese_simmr_out, type = 'histogram')





# Start of fish data

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
