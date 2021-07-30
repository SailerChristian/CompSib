################################# Start Bernhard #######################################################
########################################################################################################

library(asreml)
library(pascal)
rm(list=ls())
setwd("C:/Users/Bernhard Schmid/Desktop/Data9June16onwards/4-Forschung9June16/4.6-Veröffentl9June16/4.6.3a-Papers(in-prog.)/S-Z/Sailer")
#load("data.RData")
X <- read.csv("CompSib_inputDataNew.csv",sep=";")
str(X)
dim(X)

hist(X$Ovules,breaks=100)
# INFLATED in 0 --> plants not producing any ovules should be excluded from analysis!
# Remove the NA's and zeros from the response variable:
positive <- {is.na(X$Ovules)==FALSE} & {(X$Ovules>0)==TRUE}
X_noNA <- X[positive,]
dim(X_noNA)  #n=96
str(X_noNA)
hist(X_noNA$Ovules,breaks=20)
X_noNA$GrassF <- factor(X_noNA$Grass)
X_noNA$WithinF <- factor(X_noNA$Within)
X_noNA$Box <- factor(X_noNA$Boxname)
############## lm with sqrt(Ovules):
lm1 <- lm(terms(sqrt(Ovules)~
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
asreml1 <- asreml.nvc(fixed=sqrt(Ovules)~
                        GrassF+WithinF+GrassF:WithinF+
                        Dad+Cross+Reproduction+Dad:Reproduction+Cross:Reproduction+
                        Dad:GrassF+Cross:GrassF+Reproduction:GrassF+
                        Dad:WithinF+Cross:WithinF+Reproduction:WithinF
                      ,random=~Box+Line+GrassF:Line+WithinF:Line+Box:Line
                      ,family=asreml.gaussian(link="identity",dispersion=NA)
                      ,keep.order=T
                      ,data=X_noNA)
asreml1 <- update(asreml1)
test.asreml(asreml1)
#summary(asreml1)$varcomp
#coefficients(asreml1)
plot(asreml1)
############## simplified model with asreml:
asreml2 <- asreml.nvc(fixed=sqrt(Ovules)~
                        GrassF+WithinF+GrassF:WithinF+
                        Dad+Cross+Reproduction+Dad:Reproduction+Cross:Reproduction+
                        Dad:GrassF+Cross:GrassF+Reproduction:GrassF+
                        Dad:WithinF+Cross:WithinF+Reproduction:WithinF
                      ,random=~Box+Line
                      ,family=asreml.gaussian(link="identity",dispersion=NA)
                      ,keep.order=T
                      ,data=X_noNA)
test.asreml(asreml2)
############## simplified model with Dad and Cross combined in Cross:
asreml3 <- asreml.nvc(fixed=sqrt(Ovules)~
                        GrassF+WithinF+GrassF:WithinF+
                        Cross+Reproduction+Cross:Reproduction+
                        Cross:GrassF+Reproduction:GrassF+
                        Cross:WithinF+Reproduction:WithinF
                      ,random=~Box+Line
                      ,family=asreml.gaussian(link="identity",dispersion=NA)
                      ,keep.order=T
                      ,data=X_noNA)
test.asreml(asreml3)
############## predicted values for simplified model with asreml:
# Test Grass:Cross:
asreml3.pv <- predict(asreml3,classify="GrassF:Cross",sed=T)
asreml3.pv$predictions
############## above with Dad contrast:
asreml3 <- asreml.nvc(fixed=sqrt(Ovules)~
                        GrassF+WithinF+GrassF:WithinF+
                        Dad+Cross+Reproduction+Dad:Reproduction+Cross:Reproduction+
                        Dad:GrassF+Cross:GrassF+Reproduction:GrassF+
                        Dad:WithinF+Cross:WithinF+Reproduction:WithinF
                      ,random=~Box+Line
                      ,family=asreml.gaussian(link="identity",dispersion=NA)
                      ,keep.order=T
                      ,data=X_noNA)
test.asreml(asreml3)

################################## End Bernhard #####################################################
#####################################################################################################
