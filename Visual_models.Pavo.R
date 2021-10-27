library(readxl)
library(purrr)
library(pavo)
library(tcltk)

bombina.data<-as.rspec(read.csv("./data/habituation_&_experiment_metadata.csv"))

# Visual modeling"
sensdata(visual = "star", achromatic = "st.dc", plot = TRUE, ylab = 'Absorbance')
irradiance.data<-read.csv("./data/Irradianz Klimakammern 15.6.18.xlsx.csv")
mean.irradiance<-aggspec(as.rspec(irradiance.data), by=2,FUN=mean)

flx.illum<-irrad2flux(mean.irradiance)
pdf("./plots/irradiance_spectra_Klimakammern.pdf")
plot(as.rspec(irradiance.data),type = 's')
dev.off()


# Calculate the average spectra for the three substrate types
substr.natural<-as.rspec(read.csv2("./data/Substrat_natürlich.csv"), lim = c(300, 700))
substr.natural.sm <- procspec(substr.natural,fixneg = "zero", opt = 'smooth', span = 0.2)
substr.natural.avg <- aggspec(substr.natural.sm, by = 6, FUN = mean)
colnames(substr.natural.avg)<-c("wl","substrate_natural")

substr.light<-as.rspec(read.csv2("./data/Substrat_hell.csv"), lim = c(300, 700))
substr.light.sm <- procspec(substr.light,fixneg = "zero", opt = 'smooth', span = 0.2)
substr.light.avg <- aggspec(substr.light.sm, by = 6, FUN = mean)
colnames(substr.light.avg)<-c("wl","substrate_light")

substr.dark<-as.rspec(read.csv2("./data/Substrat_dunkel.csv"), lim = c(300, 700))
substr.dark.sm <- procspec(substr.dark,fixneg = "zero", opt = 'smooth', span = 0.2)
substr.dark.avg <- aggspec(substr.dark.sm, by = 6, FUN = mean)
colnames(substr.dark.avg)<-c("wl","substrate_dark")

all.data<-Reduce(merge, list (substr.natural.avg, substr.light.avg, substr.dark.avg, bombina.data))
substrate.all.data<-cbind(substr.natural.sm, substr.light.sm[-1], substr.dark.sm[-1])
names(substrate.all.data)<-c("wl",rep("substrate_natural",6),rep("substrate_light",6), rep("substrate_dark",6))
substrate.names<-c(rep("substrate_natural",6),rep("substrate_light",6), rep("substrate_dark",6))

#Spectral reflectance for the visible range of wavelengths (nm) of the natural Liekwegen substrate (natural) and two artificial substrates (light, dark) used in the experiment
pdf("./plots/Spectral reflectance of substrates.pdf")
sust.plot<-aggplot(substrate.all.data, substrate.names, ylim = c(0, 25), FUN.center = mean, alpha = 0.8, legend = TRUE, shadecol =  c("#666666","#CCCCCC", "#000000"), lcol =  c("#666666","#CCCCCC", "#000000"))
dev.off()

# Visual model and color and brightness contrasts
vismod.all<- vismodel(all.data, visual = "star", achromatic = "st.dc", qcatch= "fi", illum = flx.illum, scale = 1, relative = FALSE)
contrast.all<-coldist(vismod.all, noise = "neural", achro = T, n = c (1, 3.5, 5.5, 5), weber = 0.1, weber.achro = 0.34, subset="substrate_")

write.csv2(contrast.all,"./data/contrast_all-data.starling.csv", row.names=FALSE)
# Edit this document in Excel to append all metadata information, saved as: "contrast_all-data.starling.csv"
