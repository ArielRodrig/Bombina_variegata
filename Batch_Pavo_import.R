library(readxl)
library(purrr)
library(pavo)
library(tcltk)
library(dplyr)

dir<-tk_choose.dir(getwd(), "Choose a suitable folder (./data)")
setwd(dir)
for(infile in dir(dir, pattern=".xls")) 
{
filename<- toString(infile)


path <- filename
data <- path %>%
  excel_sheets() %>%
  set_names() %>% 
  map_df(~ read_excel(path = path, sheet = .x, range = "A1:Q2049", col_names = TRUE), .id = "individual")
head(data, n = 10)

clean.data<-as.data.frame(data[-c(4:5, 7:8, 10:11, 13:14, 16:17)])
colnames(clean.data)<-c("individual", "wavelength", "rep_1", "rep_2","rep_3","rep_4","rep_5","rep_6")

results<-list()
for (i in 1:max(as.numeric(clean.data$individual))) {
ind<-clean.data[clean.data$individual == as.character(i),]	
temp<- as.rspec(ind[-1], lim = c(300, 700))
colnames(temp)<-c("wavelength", paste(as.character(i),1:6,sep="."))
results[[i]]<- temp[-1]
}

pavo.data<- cbind(temp[1],as.data.frame(do.call("cbind", results)))
specs<-as.rspec(pavo.data)
#Extract individual labels
indiv <- do.call(rbind, strsplit(names(specs), "\\."))[, 1]
spectra.sm <- procspec(specs,fixneg = "zero", opt = 'smooth', span = 0.2)
mspectra <- aggspec(spectra.sm, by = 6, FUN = mean)
plot(mspectra)
write.csv(mspectra,as.character(paste(filename,"csv", sep=".")), row.names = F)
rm(indiv)
rm(pavo.data)
rm(mspectra)
rm(spectra.sm)
rm(specs)
}
