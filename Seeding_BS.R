################################# Start Bernhard #######################################################
########################################################################################################

library(asreml)
library(pascal)
rm(list=ls())
setwd("C:/Users/Bernhard Schmid/Desktop/Data9June16onwards/4-Forschung9June16/4.6-Veröffentl9June16/4.6.3a-Papers(in-prog.)/S-Z/Sailer")
#load("data.RData")
X <- read.csv("CompSib_inputDataNew.csv",sep=";")

dim(X)
str(X)
X$seeding <- as.numeric(X$SeedCount>0)
cbind(X$seeding,X$SeedCount)

# Remove the NA's from the response variable:
no_nas <- {is.na(X$seeding)==FALSE}
X_noNA <- X[no_nas,]
dim(X_noNA)   #n=129
mean(X_noNA$seeding)
str(X_noNA)
X_noNA$GrassF <- factor(X_noNA$Grass)
X_noNA$WithinF <- factor(X_noNA$Within)
X_noNA$Box <- factor(X_noNA$Boxname)

############## lm with seeding:
lm1 <- lm(terms(seeding~
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
############## logistic model with asreml:
totals <- as.numeric(X_noNA$seeding>=0)
asreml1 <- asreml.nvc(fixed=seeding~
                        GrassF+WithinF+GrassF:WithinF+
                        Dad+Cross+Reproduction+Dad:Reproduction+Cross:Reproduction+
                        Dad:GrassF+Cross:GrassF+Reproduction:GrassF+
                        Dad:WithinF+Cross:WithinF+Reproduction:WithinF
                      ,random=~Box+Line+GrassF:Line+WithinF:Line+Box:Line
                      ,family=asreml.binomial(dispersion=NA)
                      ,weights=totals
                      ,keep.order=T
                      ,data=X_noNA)
asreml1 <- update(asreml1)
test.asreml(asreml1)
#summary(asreml1)$varcomp
#coefficients(asreml1)
plot(asreml1)
############## simplified model with asreml:
asreml2 <- asreml.nvc(fixed=seeding~
                        GrassF+WithinF+GrassF:WithinF+
                        Dad+Cross+Reproduction+Dad:Reproduction+Cross:Reproduction+
                        Dad:GrassF+Cross:GrassF+Reproduction:GrassF+
                        Dad:WithinF+Cross:WithinF+Reproduction:WithinF
                      ,random=~Box+Line
                      ,family=asreml.binomial(dispersion=NA)
                      ,weights=totals
                      ,keep.order=T
                      ,data=X_noNA)
asreml2 <- update(asreml2)
test.asreml(asreml2)
############## simplified model with Dad and Cross combined in Cross:
asreml3 <- asreml.nvc(fixed=seeding~
                        GrassF+WithinF+GrassF:WithinF+
                        Cross+Reproduction+Cross:Reproduction+
                        Cross:GrassF+Reproduction:GrassF+
                        Cross:WithinF+Reproduction:WithinF
                      ,random=~Box+Line
                      ,family=asreml.binomial(dispersion=NA)
                      ,weights=totals
                      ,keep.order=T
                      ,data=X_noNA)
asreml3 <- update(asreml3)
test.asreml(asreml3)
############## predicted values for simplified model with asreml:
# Test Reproduction:
asreml3.pv <- predict(asreml3,classify="Reproduction")
asreml3.pv$predictions
# Test Reproduction:Within:
asreml3.pv <- predict(asreml3,classify="Reproduction:WithinF",sed=T)
asreml3.pv$predictions

################################## End Bernhard #####################################################
#####################################################################################################
