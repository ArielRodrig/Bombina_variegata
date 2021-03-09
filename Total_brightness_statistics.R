library(pavo)
day1<-as.rspec(read.csv("./data/Unken 29.5.18.xlsx.csv"))
day2<-as.rspec(read.csv("./data/Unken 30.5.18.xlsx.csv"))
day3<-as.rspec(read.csv("./data/Unken 31.5.18.xlsx.csv"))
day4<-as.rspec(read.csv("./data/Unken 1.6.18.xlsx.csv"))
day7<-as.rspec(read.csv("./data/Unken 4.6.18.xlsx.csv"))
day8<-as.rspec(read.csv("./data/Unken 5.6.18.xlsx.csv"))
day9<-as.rspec(read.csv("./data/Unken 6.6.18.xlsx.csv"))
day10<-as.rspec(read.csv("./data/Unken 7.6.18.xlsx.csv"))
day11<-as.rspec(read.csv("./data/Unken 8.6.18.xlsx.csv"))

full.data<-cbind(day1,day2[-1],day3[-1],day4[-1], day7[-1], day8[-1], day9[-1], day10[-1], day11[-1])
data<-as.rspec(full.data)
spec.summary<-summary(data)
indiv.data<-read.csv("./data/individual_data.csv")
merged<-cbind(indiv.data,spec.summary)
write.csv(merged,"./data/Total_Brightness_data.OK.csv")

library(ggplot2)
merged$treatment <- factor(merged$treatment, levels = c("natural","light","dark"))

pdf("./plots/Total_brightness_plot.OK.pdf", width=6, height=4)
ggplot(merged, aes(x=day, y=B1, group = interaction(day, treatment), fill = treatment)) + annotate("text", x = 2.5, y = 4500, label="habituation") + annotate("text", x = 9, y = 4500, label="experiment") +
  geom_boxplot(outlier.colour = NA) + theme_classic(base_size = 12) + ylim(50,5000) + scale_fill_manual(values = c("grey50", "grey90","grey0")) + labs(y="total brightness")+
  stat_summary(fun.y = median, geom = 'line',  aes(group = treatment, colour = treatment), position = position_dodge(width = 0.9)) + scale_colour_manual(values = c("#CCCCCC","#666666", "#000000")) + 
  annotate("rect", xmin = 0, xmax = 5, ymin = 100, ymax = 5000, alpha = .5)
dev.off()



# Analysis of the effect of experimental substrate treatments on total brightness of the toads 

library(lme4)
library(lmerTest)
library(report)
library(psycho)
library(performance)
experiment.data<-merged[merged$experiment=="experiment",]
#str(experiment.data)
fit <- lmer(B1 ~ day + treatment + (1|individual), data=experiment.data)
summary(fit)
anova(fit)
check_model(fit)
report(fit)
#  We fitted a linear mixed model (estimated using REML and nloptwrap optimizer) to predict B1 with day and treatment (formula = B1 ~ day + treatment). The model included individual as random effects (formula = ~1 | individual). Standardized parameters were obtained by fitting the model on a standardized version of the dataset. Effect sizes were labelled following Funder's (2019) recommendations.The model's total explanatory power is substantial (conditional R2 = 0.53) and the part related to the fixed effects alone (marginal R2) is of 0.45. The model's intercept, corresponding to B1 = 0, day = 0, treatment = natural and individual = 0, is at 2290.95 (SE = 449.86, 95% CI [1409.24, 3172.66], p < .001). Within this model:
#  
#    - The effect of day is negative and can be considered as tiny and not significant (beta = -39.44, SE = 46.20, 95% CI [-129.99, 51.12], std. beta = -0.06, p = 0.393).
#    - The effect of treatmentlight is positive and can be considered as very large and significant (beta = 1225.67, SE = 227.03, 95% CI [780.70, 1670.65], std. beta = 1.24, p < .001).
#    - The effect of treatmentdark is negative and can be considered as small and not significant (beta = -257.89, SE = 227.03, 95% CI [-702.86, 187.09], std. beta = -0.26, p = 0.256).
#  

fitnoInd <- lm(B1 ~ day + treatment, data=experiment.data)
anova(fit,fitnoInd)
