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


library(multcomp)
library(ggfortify)
library(stargazer)



#Calculations on dL by Target background
# Doberg (all toads against Doberg background)
doberg<-data[data$Target_Population=="D",]
dL.doberg<-lmer(dL ~ 1 + Source_Population + (1|Frog_ID), data=doberg) #, REML=FALSE
autoplot(dL.doberg)
summary(dL.doberg)
anova(dL.doberg)
summary(glht(dL.doberg, linfct = mcp(Source_Population = "Tukey")))

#Liekwegen
Liekwegen<-data[data$Target_Population=="L",]
dL.model1<-lm(abs(dL) ~ 1 + Source_Population, data=Liekwegen) #, REML=FALSE
autoplot(dL.model1)
summary(dL.model1)
anova(dL.model1)
summary(glht(dL.model1, linfct = mcp(Source_Population = "Tukey")))

#Messingsberg
Messingsberg<-data[data$Target_Population=="M",]
dL.model1<-lm(abs(dL) ~ 1 + Source_Population, data=Messingsberg) #, REML=FALSE
autoplot(dL.model1)
summary(dL.model1)
anova(dL.model1)
summary(glht(dL.model1, linfct = mcp(Source_Population = "Tukey")))

#plot
p1<-ggplot(data=data, aes(x=Source_Population, y=dS, fill=Source_Population)) + geom_boxplot(outlier.colour = NA)  +  geom_jitter(width = 0.1) +
facet_wrap(~Target_Population) + labs(title = "Background substrate", x = "Origin", y= "Dorsal ΔS")+ theme_classic() + theme(legend.position="none") +
geom_hline(yintercept=1, linetype="dashed", color = "gray")

p2<-ggplot(data=data, aes(x=Source_Population, y=dL, fill=Source_Population)) + geom_boxplot(outlier.colour = NA)  +  geom_jitter(width = 0.1)+
facet_wrap(~Target_Population) + labs(title = "Background substrate", x = "Origin", y= "Dorsal ΔL")+ theme_classic() + theme(legend.position="none") +
geom_hline(yintercept=1, linetype="dashed", color = "gray")
pdf("./plots/Origin_population_vs_Backgrounds.pdf")
multiplot(p2, p1,cols=1)
dev.off()

#Calculations on dS by Target population
# Doberg (all toads against Doberg background)
doberg<-data[data$Target_Population=="D",]
dS.model1<-lm(dS ~ 1 + Source_Population, data=doberg) #, REML=FALSE
autoplot(dS.model1)
summary(dS.model1)
anova(dS.model1)
summary(glht(dS.model1, linfct = mcp(Source_Population = "Tukey")))

#Liekwegen
Liekwegen<-data[data$Target_Population=="L",]
dS.model1<-lm(dS ~ 1 + Source_Population, data=Liekwegen) #, REML=FALSE
autoplot(dS.model1)
summary(dS.model1)
anova(dS.model1)
summary(glht(dS.model1, linfct = mcp(Source_Population = "Tukey")))

#Messingsberg
Messingsberg<-data[data$Target_Population=="M",]
dS.model1<-lm(dS ~ 1 + Source_Population, data=Messingsberg) #, REML=FALSE
autoplot(dS.model1)
summary(dS.model1)
anova(dS.model1)
summary(glht(dS.model1, linfct = mcp(Source_Population = "Tukey")))
