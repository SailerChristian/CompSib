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

hist(X$Germination_YES,breaks=20)
hist(X$Germination_NO,breaks=20)
hist(X$Ovules,breaks=20)

# Remove the NA's and zeros from the response variable:
no_nas <- {is.na(X$Germination_YES)==FALSE} & {is.na(X$Germination_NO)==FALSE} & {(X$Ovules>0)==TRUE}
X_noNA <- X[ no_nas, ]
dim(X_noNA)   #n=93
X_noNA$rate <- X_noNA$Germination_YES/(X_noNA$Germination_YES+X_noNA$Germination_NO)  #NOTE: different from Germination!
hist(X_noNA$rate,breaks=20)
mean(X_noNA$rate)
cbind(X_noNA$Germination_YES,X_noNA$Germination_NO,X_noNA$rate)
str(X_noNA)
X_noNA$GrassF <- factor(X_noNA$Grass)
X_noNA$WithinF <- factor(X_noNA$Within)
X_noNA$Box <- factor(X_noNA$Boxname)

############## lm with asin(sqrt(rate)):
lm1 <- lm(terms(asin(sqrt(rate))~
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
asreml1 <- asreml.nvc(fixed=asin(sqrt(rate))~
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
asreml2 <- asreml.nvc(fixed=asin(sqrt(rate))~
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
asreml3 <- asreml.nvc(fixed=asin(sqrt(rate))~
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
totals <- X_noNA$Germination_YES+X_noNA$Germination_NO
totals
asreml4 <- asreml.nvc(fixed=rate~
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
# Test Reproduction:
asreml4.pv <- predict(asreml4,classify="Reproduction")
asreml4.pv$predictions
# Test Reproduction:Within:
asreml4.pv <- predict(asreml4,classify="Reproduction:WithinF",sed=T)
asreml4.pv$predictions

################################## End Bernhard #####################################################
#####################################################################################################
