### load libraries
library(data.table)
library(ggplot2)
library(Hmisc)
library(MASS)
library(ResourceSelection)
library(rmeta)
library(ROCR)




### Birth Weight data
data(birthwt)
bwt <- with(birthwt, {
  race <- factor(race, labels = c("white", "black", "other"))
  ptd <- factor(ptl > 0)
  ftv <- factor(ftv)
  levels(ftv)[-(1:2)] <- "2+"
  data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0),
             ptd, ht = (ht > 0), ui = (ui > 0), ftv)
})


### Exploratory data analysis




### Fit a logistic regression model
options(contrasts = c("contr.treatment", "contr.poly"))
m1 = glm(low ~ ., binomial, bwt)



### model discrimination
mypred = prediction(m1$fitted.values, m1$y)
roc.perf = performance(mypred, measure = "tpr", x.measure = "fpr")
auc.perf = performance(mypred, measure = "auc")
plot(roc.perf, col = "orange", lwd = 2,
     main = paste ( "AUC = ", round(auc.perf@y.values[[1]], 2), sep="" )  )
abline(a = 0, b = 1, lty = 2)


### model calibration
myhl = hoslem.test(m1$y, m1$fitted.values, g=10)
mycalib = data.frame(  
            predictionbin = factor(  c( rownames(myhl$observed), 
                                        rownames(myhl$observed)),
                                    levels = rownames(myhl$observed)),
            observedfreq = c(myhl$observed[,1], myhl$observed[,2]),
            birthweight = factor( c( rep("normal", 10), rep("low", 10) ),
                                  levels = c("normal", "low")) )
ggplot(data=mycalib, 
         aes(x=predictionbin, y=observedfreq, fill = birthweight)) +
      geom_bar(stat="identity", position=position_dodge()) +
      theme(axis.text.x=element_text(angle=30, hjust=1) ) +
     scale_fill_manual(values = c("royalblue", "orange")) +
     ylab("Observed frequency") +
     xlab("Prediction probability bin") +
     ggtitle( paste("pvalue.HL = ", round(myhl$p.value, 4), sep=""))

