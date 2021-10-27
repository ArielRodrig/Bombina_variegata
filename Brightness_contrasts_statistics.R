library(multcomp)
library(ggplot2)

# Analisis of the brightness contrasts
data<-read.csv("./data/contrast_all-data.starling.csv")
data$treatment <- factor(data$treatment, levels = c("natural","light","dark"))
data$factor_day<-data$day
data$factor_day[data$factor_day<5]<- 1
data$factor_day<-as.factor(data$factor_day)
data.all<-data


# Comparing the contrast against a given substrate in habituation AND experiment phases
natural<-data.all[data.all$patch1=="substrate_natural" & data.all$treatment=="natural",]
light<-data.all[data.all$patch1=="substrate_light" & data.all$treatment=="light",]
dark<-data.all[data.all$patch1=="substrate_dark"& data.all$treatment=="dark",]

library(DescTools)
natural.mod <- aov(dL ~ factor_day, data = natural)
DunnettTest(dL ~ factor_day, data = natural, control="1", conf.level=0.95)
natural.dunnett<-glht(natural.mod, linfct = mcp(factor_day = "Dunnett"), base = "1")
confint(natural.dunnett)
plot(natural.dunnett)

light.mod <- aov(dL ~ factor_day, data = light)
DunnettTest(dL ~ factor_day, data = light, control="1", conf.level=0.95)
light.dunnett<-glht(light.mod, linfct = mcp(factor_day = "Dunnett"), base = "1")
confint(light.dunnett)
plot(light.dunnett)

dark.mod <- aov(dL ~ factor_day, data = dark)
DunnettTest(dL ~ factor_day, data = dark, control="1", conf.level=0.95)
dark.dunnett<-glht(dark.mod, linfct = mcp(factor_day = "Dunnett"), base = "1")
confint(dark.dunnett)
plot(dark.dunnett)

### Plots
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

p1<-ggplot(natural, aes(x = factor_day, y = dL, group = interaction(factor_day,group), fill=group)) + ggtitle("Natural substrate") + xlab("Days") + scale_fill_manual(values = c("#666666", "#FFFFFF")) + geom_boxplot(position = position_dodge(0.9)) + theme_bw(base_size = 12) + labs(fill = "treatment")
p2<-ggplot(light, aes(x =factor_day, y = dL, group = interaction(factor_day,group), fill=group)) + ggtitle("Light substrate") + xlab("Days") + scale_fill_manual(values = c("#CCCCCC", "#FFFFFF"))  + geom_boxplot(position = position_dodge(0.9)) + theme_bw(base_size = 12) + labs(fill = "treatment")
p3<-ggplot(dark, aes(x = factor_day, y = dL, group = interaction(factor_day,group), fill=group)) + ggtitle("Dark substrate") + xlab("Days") + scale_fill_manual(values = c("#000000", "#FFFFFF"))  + geom_boxplot(position = position_dodge(0.9)) + theme_bw(base_size = 12) + labs(fill = "treatment")

pdf("./plots/boxplots_days_vs_average-reference-substrates.pdf")
multiplot(p1, p2, p3, cols=1)
dev.off()
