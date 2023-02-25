##############################################################
#### Plots of simulated data for demonstration
##############################################################


#Get nice fonts for the plots
library(showtext)
font_add_google(name = "Montserrat",   # Name of the font on the Google Fonts site
                family = "qs") # Name you want to use to call the font
showtext_auto()

#Example with a single group just to show how items are calculated
#This creates Table 1 in the text
Eta.ij <- rnorm(100)
Omega.ij <- 1.5*Eta.ij + .25
mu.ij <- 1/(1 + exp(Omega.ij))
y.ij <- rep(NA, length(mu.ij))
for (p in 1:length(mu.ij)) {
  y.ij[p] <- rbinom(1,1,mu.ij[p])
}
Example.Data <- cbind(Eta.ij, Omega.ij, mu.ij, y.ij)

par(mfrow = c(2,2))

#Example with items for trace lines
#Uniform DIF
eta <- seq(-3, 3, by = .01)
l <- 1.2
nu.1 <- 1
nu.2 <- -.5
#Continuous items
y.1.u.continuous <- l*eta + nu.1
y.2.u.continuous <- l*eta + nu.2
plot(eta, y.1.u.continuous, type = "l", xlab = expression(eta[i]), ylab = expression("E(Y|"*eta[i]*")"), cex.lab = 1.1, col = "#674EA7")
title("Continuous Items", family = "montserrat", cex.main = 1.2, font = 1, line = 0.3)
lines(eta, y.2.u.continuous, type = "l", col = "#CC0000")
legend("topleft", col = c("#674EA7", "#CC0000"), legend = c(expression(nu*" = 1"), expression(nu*" = -.5")), y.intersp = .65, lwd = 1, bty = "n")
#Binary items
y.1.u.binary <- 1/(1+exp(-y.1.u.continuous))
y.2.u.binary <- 1/(1+exp(-y.2.u.continuous))
plot(eta, y.1.u.binary, type = "l", xlab = expression(eta[i]), ylab = expression("E(Y|"*eta[i]*")"), cex.lab = 1.1, col = "#674EA7")
title("Binary Items", family = "montserrat", cex.main = 1.2, font = 1, line = 0.3)
lines(eta, y.2.u.binary, type = "l", col = "#CC0000")
legend("topleft", col = c("#674EA7", "#CC0000"), legend = c(expression(nu*" = 1"), expression(nu*" = -.5")), y.intersp = .65, lwd = 1, bty = "n")

#Ordinal items
eta <- seq(-3, 3, by = .01)
l <- 1.6
t.1.1 <- -.3
t.1.2 <- 0.05
t.1.3 <- .3
t.1.4 <- 2.6
t.2.1 <- t.1.1 + .8
t.2.2 <- t.1.2 + .8
t.2.3 <- t.1.3 + .8
t.2.4 <- t.1.4 + .8
p.1.1 <- 1/(1+exp(-t.1.1 + l*eta))
p.1.2 <- 1/(1+exp(-t.1.2 + l*eta))
p.1.3 <- 1/(1+exp(-t.1.3 + l*eta))
p.1.4 <- 1/(1+exp(-t.1.4 + l*eta))
p.2.1 <- 1/(1+exp(-t.2.1 + l*eta))
p.2.2 <- 1/(1+exp(-t.2.2 + l*eta))
p.2.3 <- 1/(1+exp(-t.2.3 + l*eta))
p.2.4 <- 1/(1+exp(-t.2.4 + l*eta))

p.1.1.cat <- p.1.1
p.1.2.cat <- p.1.2 - p.1.1
p.1.3.cat <- p.1.3 - p.1.2
p.1.4.cat <- p.1.4 - p.1.3
p.1.5.cat <- 1 - p.1.4

p.2.1.cat <- p.2.1
p.2.2.cat <- p.2.2 - p.2.1
p.2.3.cat <- p.2.3 - p.2.2
p.2.4.cat <- p.2.4 - p.2.3
p.2.5.cat <- 1 - p.2.4

#plot(eta, p.1.1.cat, type = "l", col = "red")
#lines(eta, p.2.1.cat, type = "l", lty = 2, col = "red")
#lines(eta, p.1.2.cat, type = "l", lty = 1, col = "blue")
#lines(eta, p.2.2.cat, type = "l", lty = 2, col = "blue")
#lines(eta, p.1.3.cat, type = "l", lty = 1, col = "purple")
#lines(eta, p.2.3.cat, type = "l", lty = 2, col = "purple")
#lines(eta, p.1.4.cat, type = "l", lty = 1, col = "green")
#lines(eta, p.2.4.cat, type = "l", lty = 2, col = "green")
#lines(eta, p.1.5.cat, type = "l", lty = 1, col = "orange")
#lines(eta, p.2.5.cat, type = "l", lty = 2, col = "orange")

e.1 <- 1*p.1.1.cat + 2*p.1.2.cat + 3*p.1.3.cat + 4*p.1.4.cat + 5*p.1.5.cat
e.2 <- 1*p.2.1.cat + 2*p.2.2.cat + 3*p.2.3.cat + 4*p.2.4.cat + 5*p.2.5.cat

plot(eta, e.1, type = "l", xlab = expression(eta[i]), ylab = expression("E(Y|"*eta[i]*")"), cex.lab = 1.1, col = "#674EA7")
title("Ordinal Items", family = "montserrat", cex.main = 1.2, font = 1, line = 0.3)
lines(eta, e.2, type = "l", col = "#CC0000")
legend("topleft", col = c("#674EA7", "#CC0000"), legend = c(expression(tau*"= [-3, .05, .3, 2.6]"), expression(tau*"= [-2.1, .85, 1.1, 3.4]")), y.intersp = .65, lwd = 1, bty = "n")

#Nonuniform DIF
eta <- seq(-3, 3, by = .01)
l.1 <- 1.2
l.2 <- 2.0
nu <- .5
#Continuous items
y.1.n.continuous <- l.1*eta + nu
y.2.n.continuous <- l.2*eta + nu
plot(eta, y.1.n.continuous, type = "l", xlab = expression(eta[i]), ylab = expression("E(Y|"*eta[i]*")"), cex.lab = 1.1, col = "#674EA7")
title("Continuous Items", family = "montserrat", cex.main = 1.2, font = 1, line = 0.3)
lines(eta, y.2.n.continuous, type = "l", col = "#CC0000")
legend("topleft", col = c("#674EA7", "#CC0000"), legend = c(expression(lambda*" = 1"), expression(lambda*" = 2.0")), y.intersp = .65, lwd = 1, bty = "n")
#Binary items
y.1.n.binary <- 1/(1+exp(-y.1.n.continuous))
y.2.n.binary <- 1/(1+exp(-y.2.n.continuous))
plot(eta, y.1.n.binary, type = "l", xlab = expression(eta[i]), ylab = expression("E(Y|"*eta[i]*")"), cex.lab = 1.1, col = "#674EA7")
title("Binary Items", family = "montserrat", cex.main = 1.2, font = 1, line = 0.3)
lines(eta, y.2.n.binary, type = "l", col = "#CC0000")
legend("topleft", col = c("#674EA7", "#CC0000"), legend = c(expression(lambda*" = 1"), expression(lambda*" = 2.0")), y.intersp = .65, lwd = 1, bty = "n")

#Ordinal items
#DIF in thresholds
eta <- seq(-3, 3, by = .01)
l.1 <- 1.2
l.2 <- 2.0
t.1.1 <- -.3
t.1.2 <- 0.05
t.1.3 <- .3
t.1.4 <- 2.6
t.2.1 <- t.1.1
t.2.2 <- t.1.2
t.2.3 <- t.1.3
t.2.4 <- t.1.4
p.1.1 <- 1/(1+exp(-t.1.1 + l.1*eta))
p.1.2 <- 1/(1+exp(-t.1.2 + l.1*eta))
p.1.3 <- 1/(1+exp(-t.1.3 + l.1*eta))
p.1.4 <- 1/(1+exp(-t.1.4 + l.1*eta))
p.2.1 <- 1/(1+exp(-t.2.1 + l.2*eta))
p.2.2 <- 1/(1+exp(-t.2.2 + l.2*eta))
p.2.3 <- 1/(1+exp(-t.2.3 + l.2*eta))
p.2.4 <- 1/(1+exp(-t.2.4 + l.2*eta))

p.1.1.cat <- p.1.1
p.1.2.cat <- p.1.2 - p.1.1
p.1.3.cat <- p.1.3 - p.1.2
p.1.4.cat <- p.1.4 - p.1.3
p.1.5.cat <- 1 - p.1.4
e.1 <- p.1.1.cat*1 + p.1.2.cat*2 + p.1.3.cat*3 + p.1.4.cat*4 + p.1.5.cat*5

p.2.1.cat <- p.2.1
p.2.2.cat <- p.2.2 - p.2.1
p.2.3.cat <- p.2.3 - p.2.2
p.2.4.cat <- p.2.4 - p.2.3
p.2.5.cat <- 1 - p.2.4
e.2 <- p.2.1.cat*1 + p.2.2.cat*2 + p.2.3.cat*3 + p.2.4.cat*4 + p.2.5.cat*5

#plot(eta, p.1.1.cat, type = "l", col = "red")
#lines(eta, p.2.1.cat, type = "l", lty = 2, col = "red")
#lines(eta, p.1.2.cat, type = "l", lty = 1, col = "blue")
#lines(eta, p.2.2.cat, type = "l", lty = 2, col = "blue")
#lines(eta, p.1.3.cat, type = "l", lty = 1, col = "purple")
#lines(eta, p.2.3.cat, type = "l", lty = 2, col = "purple")
#lines(eta, p.1.4.cat, type = "l", lty = 1, col = "green")
#lines(eta, p.2.4.cat, type = "l", lty = 2, col = "green")
#lines(eta, p.1.5.cat, type = "l", lty = 1, col = "orange")
#lines(eta, p.2.5.cat, type = "l", lty = 2, col = "orange")

e.1 <- 1*p.1.1.cat + 2*p.1.2.cat + 3*p.1.3.cat + 4*p.1.4.cat + 5*p.1.5.cat
e.2 <- 1*p.2.1.cat + 2*p.2.2.cat + 3*p.2.3.cat + 4*p.2.4.cat + 5*p.2.5.cat


plot(eta, e.1, type = "l", xlab = expression(eta[i]), ylab = expression("E(Y|"*eta[i]*")"), cex.lab = 1.1, col = "#674EA7")
title("Ordinal Items", family = "montserrat", cex.main = 1.2, font = 1, line = 0.3)
lines(eta, e.2, type = "l", col = "#CC0000")
legend("topleft", col = c("#674EA7", "#CC0000"), legend = c(expression(lambda*"= 1.2"), expression(lambda*"= 2.0")), y.intersp = .65, lwd = 1, bty = "n")

#DIF in thresholds
eta <- seq(-3, 3, by = .01)
l <- 1.2
t.1.1 <- -.4
t.1.2 <- 0
t.1.3 <- .2
t.1.4 <- .3
t.2.1 <- -.3
t.2.2 <- .05
t.2.3 <- .3
t.2.4 <- 2.6
p.1.1 <- 1/(1+exp(-t.1.1 + l*eta))
p.1.2 <- 1/(1+exp(-t.1.2 + l*eta))
p.1.3 <- 1/(1+exp(-t.1.3 + l*eta))
p.1.4 <- 1/(1+exp(-t.1.4 + l*eta))
p.2.1 <- 1/(1+exp(-t.2.1 + l*eta))
p.2.2 <- 1/(1+exp(-t.2.2 + l*eta))
p.2.3 <- 1/(1+exp(-t.2.3 + l*eta))
p.2.4 <- 1/(1+exp(-t.2.4 + l*eta))

p.1.1.cat <- p.1.1
p.1.2.cat <- p.1.2 - p.1.1
p.1.3.cat <- p.1.3 - p.1.2
p.1.4.cat <- p.1.4 - p.1.3
p.1.5.cat <- 1 - p.1.4

p.2.1.cat <- p.2.1
p.2.2.cat <- p.2.2 - p.2.1
p.2.3.cat <- p.2.3 - p.2.2
p.2.4.cat <- p.2.4 - p.2.3
p.2.5.cat <- 1 - p.2.4

plot(eta, p.1.1.cat, type = "l", col = "red")
lines(eta, p.2.1.cat, type = "l", lty = 2, col = "red")
lines(eta, p.1.2.cat, type = "l", lty = 1, col = "blue")
lines(eta, p.2.2.cat, type = "l", lty = 2, col = "blue")
lines(eta, p.1.3.cat, type = "l", lty = 1, col = "purple")
lines(eta, p.2.3.cat, type = "l", lty = 2, col = "purple")
lines(eta, p.1.4.cat, type = "l", lty = 1, col = "green")
lines(eta, p.2.4.cat, type = "l", lty = 2, col = "green")
lines(eta, p.1.5.cat, type = "l", lty = 1, col = "orange")
lines(eta, p.2.5.cat, type = "l", lty = 2, col = "orange")

e.1 <- 1*p.1.1.cat + 2*p.1.2.cat + 3*p.1.3.cat + 4*p.1.4.cat + 5*p.1.5.cat
e.2 <- 1*p.2.1.cat + 2*p.2.2.cat + 3*p.2.3.cat + 4*p.2.4.cat + 5*p.2.5.cat

plot(eta, e.1, type = "l", ylim = c(1,5))
lines(eta, e.2)

