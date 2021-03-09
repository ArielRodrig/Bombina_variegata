data <- read.csv("./data/Substrate_contrasts_Field.csv")

library(ggplot2)
# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


data$deltaLDorsal<-abs(data$deltaLDorsal)

library(multcomp)
library(ggfortify)
library(stargazer)

#Calculations on deltaSDorsal by Target population
# Doberg (all toads against Doberg background)
doberg<-data[data$Target_Population=="D",]
deltaSDorsal.model1<-lm(deltaSDorsal ~ 1 + Source_Population, data=doberg) #, REML=FALSE
autoplot(deltaSDorsal.model1)
summary(deltaSDorsal.model1)
anova(deltaSDorsal.model1)
summary(glht(deltaSDorsal.model1, linfct = mcp(Source_Population = "Tukey")))

#Liekwegen
Liekwegen<-data[data$Target_Population=="L",]
deltaSDorsal.model1<-lm(deltaSDorsal ~ 1 + Source_Population, data=Liekwegen) #, REML=FALSE
autoplot(deltaSDorsal.model1)
summary(deltaSDorsal.model1)
anova(deltaSDorsal.model1)
summary(glht(deltaSDorsal.model1, linfct = mcp(Source_Population = "Tukey")))

#Messingsberg
Messingsberg<-data[data$Target_Population=="M",]
deltaSDorsal.model1<-lm(deltaSDorsal ~ 1 + Source_Population, data=Messingsberg) #, REML=FALSE
autoplot(deltaSDorsal.model1)
summary(deltaSDorsal.model1)
anova(deltaSDorsal.model1)
summary(glht(deltaSDorsal.model1, linfct = mcp(Source_Population = "Tukey")))

#Calculations on deltaLDorsal by Target background
# Doberg (all toads against Doberg background)
doberg<-data[data$Target_Population=="D",]
deltaLDorsal.model1<-lm(abs(deltaLDorsal) ~ 1 + Source_Population, data=doberg) #, REML=FALSE
autoplot(deltaLDorsal.model1)
summary(deltaLDorsal.model1)
anova(deltaLDorsal.model1)
summary(glht(deltaLDorsal.model1, linfct = mcp(Source_Population = "Tukey")))

#Liekwegen
Liekwegen<-data[data$Target_Population=="L",]
deltaLDorsal.model1<-lm(abs(deltaLDorsal) ~ 1 + Source_Population, data=Liekwegen) #, REML=FALSE
autoplot(deltaLDorsal.model1)
summary(deltaLDorsal.model1)
anova(deltaLDorsal.model1)
summary(glht(deltaLDorsal.model1, linfct = mcp(Source_Population = "Tukey")))

#Messingsberg
Messingsberg<-data[data$Target_Population=="M",]
deltaLDorsal.model1<-lm(abs(deltaLDorsal) ~ 1 + Source_Population, data=Messingsberg) #, REML=FALSE
autoplot(deltaLDorsal.model1)
summary(deltaLDorsal.model1)
anova(deltaLDorsal.model1)
summary(glht(deltaLDorsal.model1, linfct = mcp(Source_Population = "Tukey")))

#plot
p1<-ggplot(data=data, aes(x=Source_Population, y=deltaSDorsal, fill=Source_Population)) + geom_boxplot(outlier.colour = NA)  +  geom_jitter(width = 0.1)+
facet_wrap(~Target_Population) + labs(title = "Background substrate", x = "Origin", y= "Dorsal ΔS")+ theme_classic() + theme(legend.position="none")
p2<-ggplot(data=data, aes(x=Source_Population, y=abs(deltaLDorsal), fill=Source_Population)) + geom_boxplot(outlier.colour = NA)  +  geom_jitter(width = 0.1)+
facet_wrap(~Target_Population) + labs(title = "Background substrate", x = "Origin", y= "Dorsal ΔL")+ theme_classic() + theme(legend.position="none")
pdf("./plots/Origin_population_vs_Backgrounds.pdf")
multiplot(p2, p1,cols=1)
dev.off()


#Calculations on deltaLVentralB ventral black spots by Target population
# Doberg
deltaLVentralB.doberg<-lm(abs(deltaLVentralB) ~ 1 + Source_Population, data=doberg) #, REML=FALSE
autoplot(deltaLVentralB.doberg)
summary(deltaLVentralB.doberg)
anova(deltaLVentralB.doberg)
summary(glht(deltaLVentralB.doberg, linfct = mcp(Source_Population = "Tukey")))

#Liekwegen
deltaLVentralB.Liekwegen<-lm(abs(deltaLVentralB) ~ 1 + Source_Population, data=Liekwegen) #, REML=FALSE
autoplot(deltaLVentralB.Liekwegen)
summary(deltaLVentralB.Liekwegen)
anova(deltaLVentralB.Liekwegen)
summary(glht(deltaLVentralB.Liekwegen, linfct = mcp(Source_Population = "Tukey")))

#Messingsberg
deltaLVentralB.Messingsberg<-lm(abs(deltaLVentralB) ~ 1 + Source_Population, data=Messingsberg) #, REML=FALSE
autoplot(deltaLVentralB.Messingsberg)
summary(deltaLVentralB.Messingsberg)
anova(deltaLVentralB.Messingsberg)
summary(glht(deltaLVentralB.Messingsberg, linfct = mcp(Source_Population = "Tukey")))

#Calculations on deltaSVentralB ventral black spots by source population
# Doberg
deltaSVentralB.doberg<-lm(abs(deltaSVentralB) ~ 1 + Source_Population, data=doberg) #, REML=FALSE
autoplot(deltaSVentralB.doberg)
summary(deltaSVentralB.doberg)
anova(deltaSVentralB.doberg)
summary(glht(deltaSVentralB.doberg, linfct = mcp(Source_Population = "Tukey")))

#Liekwegen
deltaSVentralB.Liekwegen<-lm(abs(deltaSVentralB) ~ 1 + Source_Population, data=Liekwegen) #, REML=FALSE
autoplot(deltaSVentralB.Liekwegen)
summary(deltaSVentralB.Liekwegen)
anova(deltaSVentralB.Liekwegen)
summary(glht(deltaSVentralB.Liekwegen, linfct = mcp(Source_Population = "Tukey")))


#Messingsberg
deltaSVentralB.Messingsberg<-lm(abs(deltaSVentralB) ~ 1 + Source_Population, data=Messingsberg) #, REML=FALSE
autoplot(deltaSVentralB.Messingsberg)
summary(deltaSVentralB.Messingsberg)
anova(deltaSVentralB.Messingsberg)
summary(glht(deltaSVentralB.Messingsberg, linfct = mcp(Source_Population = "Tukey")))
# summary
stargazer(deltaLVentralB.doberg,deltaLVentralB.Liekwegen,deltaLVentralB.Messingsberg,deltaSVentralB.doberg,deltaSVentralB.Liekwegen,deltaSVentralB.Messingsberg,
 type="html", digits=2, report=('vc*p'), out="ANOVA.VentralB.doc") #p=c(0.05,0.01,0.001)

#Calculations on deltaLVentralY ventral yellow spots by source population
# Doberg
deltaLVentralY.doberg<-lm(abs(deltaLVentralY) ~ 1 + Source_Population, data=doberg) #, REML=FALSE
autoplot(deltaLVentralY.doberg)
summary(deltaLVentralY.doberg)
anova(deltaLVentralY.doberg)
summary(glht(deltaLVentralY.doberg, linfct = mcp(Source_Population = "Tukey")))

# Liekwegen
deltaLVentralY.Liekwegen<-lm(abs(deltaLVentralY) ~ 1 + Source_Population, data=Liekwegen) #, REML=FALSE
autoplot(deltaLVentralY.Liekwegen)
summary(deltaLVentralY.Liekwegen)
anova(deltaLVentralY.Liekwegen)
summary(glht(deltaLVentralY.Liekwegen, linfct = mcp(Source_Population = "Tukey")))

# Messingsberg
deltaLVentralY.Messingsberg<-lm(abs(deltaLVentralY) ~ 1 + Source_Population, data=Messingsberg) #, REML=FALSE
autoplot(deltaLVentralY.Messingsberg)
summary(deltaLVentralY.Messingsberg)
anova(deltaLVentralY.Messingsberg)
summary(glht(deltaLVentralY.Messingsberg, linfct = mcp(Source_Population = "Tukey")))

#Calculations on deltaSVentralY ventral yellow spots by source population
# Doberg
deltaSVentralY.doberg<-lm(abs(deltaSVentralY) ~ 1 + Source_Population, data=doberg) #, REML=FALSE
autoplot(deltaSVentralY.doberg)
summary(deltaSVentralY.doberg)
anova(deltaSVentralY.doberg)
summary(glht(deltaSVentralY.doberg, linfct = mcp(Source_Population = "Tukey")))

# Liekwegen
deltaSVentralY.Liekwegen<-lm(abs(deltaSVentralY) ~ 1 + Source_Population, data=Liekwegen) #, REML=FALSE
autoplot(deltaSVentralY.Liekwegen)
summary(deltaSVentralY.Liekwegen)
anova(deltaSVentralY.Liekwegen)
summary(glht(deltaSVentralY.Liekwegen, linfct = mcp(Source_Population = "Tukey")))

# Messingsberg
deltaSVentralY.Messingsberg<-lm(abs(deltaSVentralY) ~ 1 + Source_Population, data=Messingsberg) #, REML=FALSE
autoplot(deltaSVentralY.Messingsberg)
summary(deltaSVentralY.Messingsberg)
anova(deltaSVentralY.Messingsberg)
summary(glht(deltaSVentralY.Messingsberg, linfct = mcp(Source_Population = "Tukey")))

#Summary
stargazer(deltaLVentralY.doberg,deltaLVentralY.Liekwegen,deltaLVentralY.Messingsberg,deltaSVentralY.doberg,deltaSVentralY.Liekwegen,deltaSVentralY.Messingsberg, 
type="html", digits=2, report=('vc*p'),out="ANOVA.VentralY.doc")
#plot
p3<-ggplot(data=data, aes(x=Source_Population, y=abs(deltaLVentralB), fill=Source_Population)) + geom_boxplot(outlier.colour = NA)  +  geom_jitter(width = 0.1)+
	facet_wrap(~Target_Population) + labs(title = "Background substrate", x = "Origin", y= "Black ΔL")+ theme_classic() + theme(legend.position="none")
p4<-ggplot(data=data, aes(x=Source_Population, y=abs(deltaSVentralB), fill=Source_Population)) + geom_boxplot(outlier.colour = NA)  +  geom_jitter(width = 0.1)+
	facet_wrap(~Target_Population) + labs(title = "Background substrate", x = "Origin", y= "Black ΔS")+ theme_classic() + theme(legend.position="none")
p5<-ggplot(data=data, aes(x=Source_Population, y=abs(deltaLVentralY), fill=Source_Population)) + geom_boxplot(outlier.colour = NA)  +  geom_jitter(width = 0.1)+
	facet_wrap(~Target_Population) + labs(title = "Background substrate", x = "Origin", y= "Yellow ΔL") + theme_classic() + theme(legend.position="none")
p6<-ggplot(data=data, aes(x=Source_Population, y=abs(deltaSVentralY), fill=Source_Population)) + geom_boxplot(outlier.colour = NA)  +  geom_jitter(width = 0.1)+
	facet_wrap(~Target_Population) + labs(title = "Background substrate", x = "Origin", y= "Yellow ΔS")+ theme_classic() + theme(legend.position="none")
pdf("./plots/Source_population_vs_Backgrounds_Ventral_Black_and_Yellow.pdf")
multiplot(p3, p4, p5, p6,cols=1)
dev.off()



# Ventral contrast between Yellow and Black

deltaSVB.VY<-lm(deltaSVB.VY ~ 1 + Source_Population, data=data[data$Origin=="Native",]) #, REML=FALSE
autoplot(deltaSVB.VY)
summary(deltaSVB.VY)
anova(deltaSVB.VY)
#summary(glht(deltaSVB.VY, linfct = mcp(Source_Population = "Tukey")))

deltaLVB.VY<-lm(deltaLVB.VY ~ 1 + Source_Population, data=data[data$Origin=="Native",]) #, REML=FALSE
autoplot(deltaLVB.VY)
summary(deltaLVB.VY)
anova(deltaLVB.VY)

# Summary
stargazer(deltaLVB.VY,deltaSVB.VY,
 type="html", digits=2, report=('vc*p'), out="ANOVA.Black_vs_Yellow.doc") #p=c(0.05,0.01,0.001)


# plot
p7<-ggplot(data=data[data$Origin=="Native",], aes(x=Source_Population, y=deltaSVB.VY, fill=Source_Population)) + geom_boxplot(outlier.colour = NA)  +  geom_jitter(width = 0.1)+
labs(x = "Origin", y= "ΔS")+ theme_classic(base_size = 24) + theme(legend.position="none")
p8<-ggplot(data=data[data$Origin=="Native",], aes(x=Source_Population, y=abs(deltaLVB.VY), fill=Source_Population)) + geom_boxplot(outlier.colour = NA)  +  geom_jitter(width = 0.1)+
labs(x = "Origin", y= "ΔL")+ theme_classic(base_size = 24) + theme(legend.position="none")

pdf("./plots/Ventral_Black_vs_Yellow_contrasts.pdf")
multiplot(p8, p7,cols=1)
dev.off()


