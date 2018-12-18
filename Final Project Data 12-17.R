final_data<-read.csv("522_data.csv")
attach(final_data)
summary(age)
summary(gender)
summary(location.1)
library(dplyr)

location_counts<-table(final_data$location.1)
barplot(location_counts, main="Research Locations",
        xlab="Location", col=c("darkblue","red","green","purple"), cex.names = 0.8)

cheating_percent<-c(.103,.161,.147,.142)
barplot(cheating_percent, main="Frequency of Cheating Behavior",
        xlab="Condition", col=c("darkblue","red","green","purple"), cex.names = 0.8, names.arg = c("Depleted Gain","Depleted Loss","Non-Depleted Gain","Non-Depleted Loss"))

#Analysis for DV1
##Proportion test
table(cheated, depleted)
prop.test(x=c(9,8), n=c(70,62))

table(cheated, gain_frame)
prop.test(x=c(9,8), n=c(59,73))

##Non-parametrics test
wilcox.test(cheated~depleted)
wilcox.test(cheated~gain_frame)
kruskal.test(cheated, condition)

# Analysis for DV2
dv2 <- subset(final_data, cheated==1)
wilcox.test(dv2$amt_cheat~dv2$depleted)
wilcox.test(dv2$amt_cheat~dv2$gain_frame)
kruskal.test(dv2$amt_cheat, dv2$condition)

#Additional analyses
summary(glm(final_data$cheated~factor(final_data$gender)*factor(final_data$depleted)*factor(final_data$gain_frame), family='binomial'))
