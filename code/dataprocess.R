##############################################################
#### LSAC data
##############################################################

library(haven);             # Imports SPSS (.sav) data
library(stringr)           # Looking for specific string ('word' or 'parts of word')
library(dplyr)
library(psych)
library(ltm)
library(RColorBrewer)
library(plot.matrix)


setwd ("G:/My Drive/LSAC/data/Survey data/SPSS") #My local working directory, likely not yours :-)

###########################################
### Preliminary data management ###########
###########################################

#Main thing this chunk of code does is to merge data from male and female parents
#and, for each participant, randomly sample either the male or female parent

test.0 <- read_spss("lsacgrb0.sav") #This is what the file was called in the Wave 9 data release
test.0$use <- 0 
test.0$use[test.0$zf02m2 == 2] <- 1
test.0$use[test.0$zf02m3 == 1 | test.0$zf02m3 == -9] <- 1
test.0 <- subset(test.0, test.0$use == 1)

mom.data <- test.0[,c(
  "hicid",
  "cohort",
  "wave",
  "aweight",
  "aweights",
  "stratum",
  "pcodes",
  "zf12m2",
  "ascagew",
  "zf02m1",
  "apa03m1", 
  "apa03m2", 
  "apa03m3", 
  "apa03m4", 
  "apa03m5", 
  "apa03m6")]

names(mom.data)[(ncol(mom.data)-5):ncol(mom.data)] <- c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")
mom.data$mom <- 1

dad.data <- test.0[,c(
  "hicid",
  "cohort",
  "wave",
  "aweight",
  "aweights",
  "stratum",
  "pcodes",
  "zf12m2",
  "ascagew",
  "zf02m1",
  "apa03f1", 
  "apa03f2", 
  "apa03f3", 
  "apa03f4", 
  "apa03f5", 
  "apa03f6")]

names(dad.data)[(ncol(dad.data)-5):ncol(dad.data)] <- c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")
dad.data$mom <- 0

alltogether <- rbind(mom.data, dad.data)

#sampling steps
set.seed(55555)
sampled <- alltogether %>% group_by(hicid) %>% sample_n(1)


###########################################################
#### Assessing endorsement rates
########################################################

#We want to see if we need to collapse categories -- this entails visualizing endorsement rates for each response option
#We will be creating Figure 4 in the text

#Get rid of missing data 
sampled$Item1[sampled$Item1 < 1] <- NA
sampled$Item2[sampled$Item2 < 1] <- NA
sampled$Item3[sampled$Item3 < 1] <- NA
sampled$Item4[sampled$Item4 < 1] <- NA
sampled$Item5[sampled$Item5 < 1] <- NA
sampled$Item6[sampled$Item6 < 1] <- NA

#This creates Figure 4 in the text

endors.mat <- matrix(NA, 6, 5)
for (a in 1:5) {
  endors.mat[1,a] <- length(subset(sampled$Item1, sampled$Item1 == a))
  endors.mat[2,a] <- length(subset(sampled$Item2, sampled$Item2 == a))
  endors.mat[3,a] <- length(subset(sampled$Item3, sampled$Item3 == a))
  endors.mat[4,a] <- length(subset(sampled$Item4, sampled$Item4 == a))
  endors.mat[5,a] <- length(subset(sampled$Item5, sampled$Item5 == a))
  endors.mat[6,a] <- length(subset(sampled$Item6, sampled$Item6 == a))
}

blue_palette <- colorRampPalette(colors = c("white", "#6593b7", "#326f9f", "#004b88"))(20)
plot(endors.mat, digits=0, text.cell=list(cex=1.25), fmt.cell = "%.0f", 
     col = n_palette,
     main = "Number of Participants Endorsing Each Response Option for Each Item",
     xlab = "Response Option",
     ylab = "Item",
     key = NULL) 


#We now collapse all items except item 3 to 3-category.

sampled$Item1[sampled$Item1 < 3] <- 3
sampled$Item2[sampled$Item2 < 3] <- 3
#sampled$Item3[sampled$Item3 < 3] <- 3
sampled$Item4[sampled$Item4 < 3] <- 3
sampled$Item5[sampled$Item5 < 3] <- 3
sampled$Item6[sampled$Item6 < 3] <- 3

sampled$Item1 <- sampled$Item1 - 3
sampled$Item2 <- sampled$Item2 - 3
#sampled$Item3 <- sampled$Item3 - 3
sampled$Item4 <- sampled$Item4 - 3
sampled$Item5 <- sampled$Item5 - 3
sampled$Item6 <- sampled$Item6 - 3


sampled$male <- 2 - sampled$zf02m1

sampled$agegroup <- NA
sampled$agegroup[sampled$ascagew <= quantile(sampled$ascagew, .25, na.rm = TRUE)] <- 1
sampled$agegroup[sampled$ascagew <= quantile(sampled$ascagew, .5, na.rm = TRUE) & sampled$ascagew > quantile(sampled$ascagew, .25, na.rm = TRUE)] <- 2
sampled$agegroup[sampled$ascagew <= quantile(sampled$ascagew, .75, na.rm = TRUE) & sampled$ascagew > quantile(sampled$ascagew, .5, na.rm = TRUE)] <- 3
sampled$agegroup[sampled$ascagew <= quantile(sampled$ascagew, 1, na.rm = TRUE) & sampled$ascagew > quantile(sampled$ascagew, .75, na.rm = TRUE)] <- 4

#Commented out, but this is what would write out the data
#write.table(sampled, "G:/My Drive/LSAC/analyses/monograph/LSAC_monograph.csv", sep = ",", row.names = FALSE, col.names = FALSE, na = "-55555")


#########################################
## Graphics #############################
#########################################

#First, we need to do a whole bunch of data management
#Item 3 is the only one we're not collapsing to 3 categories

for.polychoric <- sampled
for.polychoric$Item3.orig <- for.polychoric$Item3
for.polychoric$Item3[for.polychoric$Item3 < 3] <- 3
for.polychoric$Item3 <- for.polychoric$Item3 - 3

#For all of the items we center around the middle value for the purposes of getting mean plots on an interpretable scale
for.polychoric$Item1.scale <- for.polychoric$Item1 - 1
for.polychoric$Item2.scale <- for.polychoric$Item2 - 1
for.polychoric$Item3.scale <- for.polychoric$Item3.orig - 3 
for.polychoric$Item4.scale <- for.polychoric$Item4 - 1
for.polychoric$Item5.scale <- for.polychoric$Item5 - 1
for.polychoric$Item6.scale <- for.polychoric$Item6 - 1

age.25 <- subset(for.polychoric, for.polychoric$ascagew < 32)
age.50 <- subset(for.polychoric, (for.polychoric$ascagew >= 32 & for.polychoric$ascagew < 40))
age.75 <- subset(for.polychoric, (for.polychoric$ascagew >= 40 & for.polychoric$ascagew < 48))
age.100 <- subset(for.polychoric, for.polychoric$ascagew >=48)

male.parents <- subset(for.polychoric, for.polychoric$mom == 0)
female.parents <- subset(for.polychoric, for.polychoric$mom == 1)

male.children <- subset(for.polychoric, for.polychoric$male == 1)
female.children <- subset(for.polychoric, for.polychoric$male == 0)

fp.poly <- psych::polychoric((female.parents[,c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")]))$rho
mp.poly <- psych::polychoric((male.parents[,c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")]))$rho

fc.poly <- psych::polychoric((female.children[,c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")]))$rho
mc.poly <- psych::polychoric((male.children[,c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")]))$rho

a25.poly <- psych::polychoric((age.25[,c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")]))$rho
a50.poly <- psych::polychoric((age.50[,c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")]))$rho
a75.poly <- psych::polychoric((age.75[,c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")]))$rho
a100.poly <- psych::polychoric((age.100[,c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6")]))$rho


a25.poly[!lower.tri(a25.poly)] <- NA
a50.poly[!lower.tri(a50.poly)] <- NA
a75.poly[!lower.tri(a75.poly)] <- NA
a100.poly[!lower.tri(a100.poly)] <- NA

fp.poly[!lower.tri(fp.poly)] <- NA
mp.poly[!lower.tri(mp.poly)] <- NA
fc.poly[!lower.tri(fc.poly)] <- NA
mc.poly[!lower.tri(mc.poly)] <- NA

######################################################################

## Get polychoric correlation matrices (Figure 6)

# Have to do a few tricky steps to get all the colors on the same range for each correlation matrix
values <- round(seq(.4, .9, by = .01),2)
col.index <- function(x) {
  which(values == round(x,2))
}
cor.palette <- viridis(length(values))
#cor.palette <- cor.palette[length(cor.palette):1]

col.range <- function(y) {
  cor.palette[col.index(min(y, na.rm = TRUE)):col.index(max(y, na.rm = TRUE))]
}


#Top panel - plot for age
layout(mat = matrix(c(1, 2, 5, 3, 4, 5), nrow = 3, ncol = 2),
       heights = c(2, 2, .5), # Heights of the two rows
       widths = c(2, 2)) # Widths of the two columns
par(mar=c(5,3,3,3))
plot(fp.poly, border = NA, col = col.range(fp.poly), key = NULL, ylab = "", xlab = "", main = "Female parent")
plot(mp.poly, border = NA, col = col.range(mp.poly), key=NULL, ylab = "", xlab = "", main = "Male parent")
plot(fc.poly, border = NA, col = col.range(fc.poly), key = NULL, ylab = "", xlab = "", main = "Female child")
plot(mc.poly, border = NA, col = col.range(mc.poly), key=NULL, ylab = "", xlab = "", main = "Male child")

par(mar=c(0,3,0,3))
plot(c(0,1),c(0,2),type = 'n', axes = F,xlab = '', ylab = '')
text(x = seq(0,1,l=5), y=1.5, labels = seq(.4,.9,l=5))

legend.image <- as.raster(t(matrix(cor.palette)))
rasterImage(legend.image, 0, 0, 1,1)
dev.off()

layout(mat = matrix(c(1, 2, 5, 3, 4, 5), nrow = 3, ncol = 2),
       heights = c(2, 2, .5), # Heights of the two rows
       widths = c(2, 2)) # Widths of the two columns

par(mar=c(5,3,3,3))
plot(a25.poly, border = NA, col = col.range(a25.poly), key = NULL, ylab = "", xlab = "", main = "First quartile of child age")
plot(a50.poly, border = NA, col = col.range(a50.poly), key=NULL, ylab = "", xlab = "", main = "Second quartile of child age")
plot(a75.poly, border = NA, col = col.range(a75.poly), key = NULL, ylab = "", xlab = "", main = "Third quartile of child age")
plot(a100.poly, border = NA, col = col.range(a100.poly), key=NULL, ylab = "", xlab = "", main = "Fourth quartile of child age")

par(mar=c(0,3,0,3))
plot(c(0,1),c(0,2),type = 'n', axes = F,xlab = '', ylab = '')
text(x = seq(0,1,l=5), y=1.5, labels = seq(.4,.9,l=5))

legend.image <- as.raster(t(matrix(cor.palette)))
rasterImage(legend.image, 0, 0, 1,1)
dev.off()


###############################################################

# Last thing we do is create Figure 5, the plots of all the means

# Subset data -- note that this is centered so that the middle response option is 0
mean.25 <- colMeans(age.25[,c("Item1.scale", "Item2.scale", "Item3.scale", "Item4.scale", "Item5.scale", "Item6.scale")], na.rm = TRUE)
mean.50 <- colMeans(age.50[,c("Item1.scale", "Item2.scale", "Item3.scale", "Item4.scale", "Item5.scale", "Item6.scale")], na.rm = TRUE)
mean.75 <- colMeans(age.75[,c("Item1.scale", "Item2.scale", "Item3.scale", "Item4.scale", "Item5.scale", "Item6.scale")], na.rm = TRUE)
mean.100 <- colMeans(age.100[,c("Item1.scale", "Item2.scale", "Item3.scale", "Item4.scale", "Item5.scale", "Item6.scale")], na.rm = TRUE)

mean.fc <- colMeans(female.children[,c("Item1.scale", "Item2.scale", "Item3.scale", "Item4.scale", "Item5.scale", "Item6.scale")], na.rm = TRUE)
mean.mc <- colMeans(male.children[,c("Item1.scale", "Item2.scale", "Item3.scale", "Item4.scale", "Item5.scale", "Item6.scale")], na.rm = TRUE)

mean.fp <-  colMeans(female.parents[,c("Item1.scale", "Item2.scale", "Item3.scale", "Item4.scale", "Item5.scale", "Item6.scale")], na.rm = TRUE)
mean.mp <- colMeans(male.parents[,c("Item1.scale", "Item2.scale", "Item3.scale", "Item4.scale", "Item5.scale", "Item6.scale")], na.rm = TRUE)

par(mfrow = c(3,1), mar = c(2,4.5,1,3))
qual.palette <- turbo(4)
plot(1:6, mean.25, type = "l", col = qual.palette[1], lwd = 1.25, xaxt = "n", ylab = "Mean response", xlab = "", cex.lab = 1.25)
lines(1:6, mean.50, col = qual.palette[2])
lines(1:6, mean.75, col = qual.palette[3])
lines(1:6, mean.100, col = qual.palette[4])
axis(1, at = 1:6, labels = c("Item 1", "Item 2", "Item3", "Item 4", "Item 5", "Item 6"), cex.axis = 1.25)
legend("topright", fill = qual.palette, legend = c("1Q Age", "2Q Age", "3Q Age", "4Q Age"), horiz = TRUE, bty = "n")

qual.palette <- turbo(2)
plot(1:6, mean.fc, type = "l", col = qual.palette[1], lwd = 1.25, xaxt = "n", ylab = "Mean response", xlab = "", cex.lab = 1.25)
lines(1:6, mean.mc, col = qual.palette[2])
axis(1, at = 1:6, labels = c("Item 1", "Item 2", "Item3", "Item 4", "Item 5", "Item 6"), cex.axis = 1.25)
legend("topright", fill = qual.palette, legend = c("Female Child", "Male Child"), horiz = TRUE, bty = "n")

plot(1:6, mean.fp, type = "l", col = qual.palette[1], lwd = 1.25, xaxt = "n", ylab = "Mean response", xlab = "", ylim = c(0, 1.4), cex.lab = 1.25)
lines(1:6, mean.mp, col = qual.palette[2])
axis(1, at = 1:6, labels = c("Item 1", "Item 2", "Item3", "Item 4", "Item 5", "Item 6"), cex.axis = 1.25)
legend("topright", fill = qual.palette, legend = c("Female Parent", "Male Parent"), horiz = TRUE, bty = "n")

write.csv(cbind(age.modelcomp, childgender.modelcomp, parentgender.modelcomp), "modelcomp.csv")