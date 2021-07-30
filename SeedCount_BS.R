################################# Start Bernhard #######################################################
########################################################################################################

library(asreml)
library(pascal)
rm(list=ls())
setwd("C:/Users/Bernhard Schmid/Desktop/Data9June16onwards/4-Forschung9June16/4.6-Veröffentl9June16/4.6.3a-Papers(in-prog.)/S-Z/Sailer")
#load("data.RData")
X <- read.csv("CompSib_inputDataNew.csv",sep=";")

no_nas <- is.na(X$SeedCount)==FALSE
X_noNA <- X[no_nas,]
X_SeedCountPositive <- X_noNA[X_noNA$SeedCount>0,]
str(X_SeedCountPositive)
X_SeedCountPositive$GrassF <- factor(X_SeedCountPositive$Grass)
X_SeedCountPositive$WithinF <- factor(X_SeedCountPositive$Within)
X_SeedCountPositive$Box <- factor(X_SeedCountPositive$Boxname)

############## lm with sqrt(SeedCount):
lm1 <- lm(terms(sqrt(SeedCount)~
            GrassF+WithinF+GrassF:WithinF+Box+
            (Dad+Cross)+Reproduction+(Dad+Cross):Reproduction+Line+
            (Dad+Cross+Reproduction+Line):GrassF+
            (Dad+Cross+Reproduction+Line):WithinF+
            Box:Line
            ,keep.order=T)
            ,data=X_SeedCountPositive)
anova(lm1)
par(mfrow=c(2,2))
plot(lm1)
############## same with asreml:
asreml1 <- asreml.nvc(fixed=sqrt(SeedCount)~
                        GrassF+WithinF+GrassF:WithinF+
                        Dad+Cross+Reproduction+Dad:Reproduction+Cross:Reproduction+
                        Dad:GrassF+Cross:GrassF+Reproduction:GrassF+
                        Dad:WithinF+Cross:WithinF+Reproduction:WithinF
                  ,random=~Box+Line+GrassF:Line+WithinF:Line+Box:Line
                  ,family=asreml.gaussian(link="identity",dispersion=NA)
                  ,keep.order=T
                  ,data=X_SeedCountPositive)
asreml1 <- update(asreml1)
test.asreml(asreml1)
#summary(asreml1)$varcomp
#coefficients(asreml1)
plot(asreml1)
############## simplified model with asreml:
asreml2 <- asreml.nvc(fixed=sqrt(SeedCount)~
                        GrassF+WithinF+GrassF:WithinF+
                        Dad+Cross+Reproduction+Dad:Reproduction+Cross:Reproduction+
                        Dad:GrassF+Cross:GrassF+Reproduction:GrassF+
                        Dad:WithinF+Cross:WithinF+Reproduction:WithinF
                      ,random=~Box+Line
                      ,family=asreml.gaussian(link="identity",dispersion=NA)
                      ,keep.order=T
                      ,data=X_SeedCountPositive)
test.asreml(asreml2)
############## simplified model with Dad and Cross combined in Cross:
asreml3 <- asreml.nvc(fixed=sqrt(SeedCount)~
                        GrassF+WithinF+GrassF:WithinF+
                        Cross+Reproduction+Cross:Reproduction+
                        Cross:GrassF+Reproduction:GrassF+
                        Cross:WithinF+Reproduction:WithinF
                      ,random=~Box+Line
                      ,family=asreml.gaussian(link="identity",dispersion=NA)
                      ,keep.order=T
                      ,data=X_SeedCountPositive)
test.asreml(asreml3)
############## predicted values for simplified model with asreml:
# Test Grass:
asreml3.pv <- predict(asreml3,classify="GrassF")
asreml3.pv$predictions
# Test Cross:
asreml3.pv <- predict(asreml3,classify="Cross",sed=T)
asreml3.pv$predictions
# Test Grass:Cross:
asreml3.pv <- predict(asreml3,classify="GrassF:Cross",sed=T)
asreml3.pv$predictions

################################## End Bernhard #####################################################
#####################################################################################################
