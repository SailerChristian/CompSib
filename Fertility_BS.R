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

hist(X$SeedCount,breaks=20)
hist(X$ShellCount,breaks=20)

# Remove the NA's and zeros from the response variable:
no_nas <- {is.na(X$SeedCount)==FALSE} & {is.na(X$ShellCount)==FALSE} &
          {(X$SeedCount>0)==TRUE} & {(X$ShellCount>0)==TRUE}
X_noNA = X[no_nas,]
dim(X_noNA)   #n=95
X_noNA$fert <- X_noNA$SeedCount/(X_noNA$SeedCount+X_noNA$ShellCount)
hist(X_noNA$fert,breaks=20)
mean(X_noNA$fert)
cbind(X_noNA$SeedCount,X_noNA$ShellCount,X_noNA$fert)
str(X_noNA)
X_noNA$GrassF <- factor(X_noNA$Grass)
X_noNA$WithinF <- factor(X_noNA$Within)
X_noNA$Box <- factor(X_noNA$Boxname)
############## lm with asin(sqrt(fert)):
lm1 <- lm(terms(asin(sqrt(fert))~
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
asreml1 <- asreml.nvc(fixed=asin(sqrt(fert))~
                        GrassF+WithinF+GrassF:WithinF+
                        Dad+Cross+Reproduction+Dad:Reproduction+Cross:Reproduction+
                        Dad:GrassF+Cross:GrassF+Reproduction:GrassF+
                        Dad:WithinF+Cross:WithinF+Reproduction:WithinF
                      ,random=~Box+Line+GrassF:Line+WithinF:Line+Box:Line
                      ,family=asreml.gaussian(link="identity",dispersion=NA)
                  ,keep.order=T
                  ,data=X_noNA)
test.asreml(asreml1)
#summary(asreml1)$varcomp
#coefficients(asreml1)
plot(asreml1)
############## simplified model with asreml:
asreml2 <- asreml.nvc(fixed=asin(sqrt(fert))~
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
asreml3 <- asreml.nvc(fixed=asin(sqrt(fert))~
                        GrassF+WithinF+GrassF:WithinF+
                        Cross+Reproduction+Cross:Reproduction+
                        Cross:GrassF+Reproduction:GrassF+
                        Cross:WithinF+Reproduction:WithinF
                      ,random=~Box+Line
                      ,family=asreml.gaussian(link="identity",dispersion=NA)
                      ,keep.order=T
                      ,data=X_noNA)
test.asreml(asreml3)
############## same with binomial errors:
totals <- X_noNA$SeedCount+X_noNA$ShellCount
totals
asreml4 <- asreml.nvc(fixed=fert~
                        GrassF+WithinF+GrassF:WithinF+
                        Cross+Reproduction+Cross:Reproduction+
                        Cross:GrassF+Reproduction:GrassF+
                        Cross:WithinF+Reproduction:WithinF
                      ,random=~Box+Line
                      ,family=asreml.binomial(dispersion=NA)
#                      ,family=asreml.negative.binomial(dispersion=NA)
                      ,weights=totals   # with this significances increase
                      ,keep.order=T
                      ,data=X_noNA)
test.asreml(asreml4)
plot(asreml4)
############## predicted values for simplified model with asreml:
# Test Grass:
asreml4.pv <- predict(asreml4,classify="GrassF")
asreml4.pv$predictions
# Test Reproduction:Grass:
asreml4.pv <- predict(asreml4,classify="Reproduction:GrassF",sed=T)
asreml4.pv$predictions

################################## End Bernhard #####################################################
#####################################################################################################
