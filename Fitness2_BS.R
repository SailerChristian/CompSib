################################# Start Bernhard #######################################################
########################################################################################################

library(asreml)
library(pascal)
rm(list=ls())
setwd("C:/Users/Bernhard Schmid/Desktop/Data9June16onwards/4.6-Ver?ffentl9June16/4.6.3a-Papers(in-prog.)/S-Z/Sailer")
#load("data.RData")
X <- read.csv("CompSib_inputDataNew.csv",sep=";")

dim(X)
str(X)
# Fitness1: product of Fertility and Germination rates:
X$fert <- X$SeedCount/(X$Ovules)
X$rate <- X$Germination_YES/(X$Germination_YES+X$Germination_NO)  #NOTE: different from Germination!
# Fitness2: product of Fertility and Germination rate, including germination rates for plants without seeds:
# set fertility for all NA to zero:
X$fert2 <- X$fert
X$fert2[is.na(X$fert2)] <- 0
# set germination rate for all NA to zero:
X$rate2 <- X$rate
X$rate2[is.na(X$rate2)] <- 0
cbind(X$Totalbm,X$fert,X$fert2,X$rate,X$rate2)
X$Fitness_2 <- (X$fert2*X$rate2)
# exclude values with Totalbm being NA:
X$Fitness2 <- X$Fitness_2*as.numeric(X$Totalbm>0)
cbind(X$Totalbm,X$Fitness_2,X$Fitness2)

hist(X$Fitness2,breaks=20)

# As we are aware, this variable has a lot of zeroes and therefor the correct analysis was to separate
# it into probability of reproduction and fitness of those reproducing.
# But still we now do analyze Fitness2:

# Remove the NA's from the response variable:
no_nas <- {is.na(X$Fitness2)==FALSE}
X_noNA <- X[no_nas,]
dim(X_noNA)   #n=156
mean(X_noNA$Fitness2)
str(X_noNA)
X_noNA$GrassF <- factor(X_noNA$Grass)
X_noNA$WithinF <- factor(X_noNA$Within)
X_noNA$Box <- factor(X_noNA$Boxname)
############## lm with Fitness (log does not give better residuals!):
lm1 <- lm(terms(Fitness2~
                  GrassF+WithinF+GrassF:WithinF+Box+
                  (Dad+Cross)+Reproduction+(Dad+Cross):Reproduction+Line+
                  (Dad+Cross+Reproduction+Line):GrassF+
                  (Dad+Cross+Reproduction+Line):WithinF+
                  Box:Line
                ,keep.order=T)
            ,data=X_noNA)
anova(lm1)
par(mfrow=c(2,2))
plot(lm1)
############## same with asreml:
asreml1 <- asreml.nvc(fixed=Fitness2~
                        GrassF+WithinF+GrassF:WithinF+
                        Dad+Cross+Reproduction+Dad:Reproduction+Cross:Reproduction+
                        Dad:GrassF+Cross:GrassF+Reproduction:GrassF+
                        Dad:WithinF+Cross:WithinF+Reproduction:WithinF
                      ,random=~Box+Line+GrassF:Line+WithinF:Line+Box:Line
                      ,family=asreml.gaussian(link="identity",dispersion=NA),
                  ,keep.order=T,
                  ,data=X_noNA)
asreml1 <- update(asreml1)
test.asreml(asreml1)
#summary(asreml1)$varcomp
#coefficients(asreml1)
plot(asreml1)  #residuals look very nice!
############## simplified model with asreml:
asreml2 <- asreml.nvc(fixed=Fitness2~
                        GrassF+WithinF+GrassF:WithinF+
                        Dad+Cross+Reproduction+Dad:Reproduction+Cross:Reproduction+
                        Dad:GrassF+Cross:GrassF+Reproduction:GrassF+
                        Dad:WithinF+Cross:WithinF+Reproduction:WithinF
                      ,random=~Box+Line
                      ,family=asreml.gaussian(link="identity",dispersion=NA),
                      ,keep.order=T,
                      ,data=X_noNA)
test.asreml(asreml2)
############## simplified model with Dad and Cross combined in Cross:
asreml3 <- asreml.nvc(fixed=Fitness2~
                        GrassF+WithinF+GrassF:WithinF+
                        Cross+Reproduction+Cross:Reproduction+
                        Cross:GrassF+Reproduction:GrassF+
                        Cross:WithinF+Reproduction:WithinF
                      ,random=~Box+Line
                      ,family=asreml.gaussian(link="identity",dispersion=NA),
                      ,keep.order=T,
                      ,data=X_noNA)
test.asreml(asreml3)
plot(asreml3)  #residuals look very nice!
############## predicted values for simplified model with asreml:
# Test Cross:
asreml3.pv <- predict(asreml3,classify="Cross",sed=T)
asreml3.pv$predictions

################################## End Bernhard #####################################################
#####################################################################################################
