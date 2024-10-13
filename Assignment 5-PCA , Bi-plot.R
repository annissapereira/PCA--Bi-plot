# Load necessary libraries
library(HSAUR) #heptathlon dataset
library(psych)
library(devtools)
library(ggbiplot)
library(dplyr)
library(nnet)

# Data Loading
data("heptathlon")
str(heptathlon)
summary(heptathlon)
head(heptathlon)

# Convert the score variable into categorical bins for biplot
heptathlon$scoreCat <- as.factor(cut(heptathlon$score,
                                     breaks = 3, 
                                     labels = c("Low", "Medium", "High")))
#check the way these bins were created
heptathlon_scores <- heptathlon %>%
  select(score, scoreCat)
heptathlon_scores
table(heptathlon$scoreCat)

# Partition the data 
set.seed(1702) 
splt <- sample(2, nrow(heptathlon), replace = TRUE, prob = c(0.8, 0.2))
training <- heptathlon[splt == 1, ]
testing <- heptathlon[splt == 2, ]

head(training)

# Correlation analysis and plots
pairs.panels(training[,-c(8, 9)], 
             gap = 0, 
             bg = c("red", "green", "blue")[as.numeric(training$scoreCat)], 
             pch = 21)

# Principal Component Analysis
pc <- prcomp(training[,-c(8, 9)], center = TRUE, scale. = TRUE)  

attributes(pc)
pc$center
pc$scale
print(pc)
summary(pc)

# Orthogonality of PCs
pairs.panels(pc$x, 
             gap = 0, 
             bg = c("red", "green", "blue")[as.numeric(training$scoreCat)], 
             pch = 21)

# Biplot
g <- ggbiplot(pc, 
              obs.scale = 1, 
              var.scale = 1, 
              groups = training$scoreCat,  
              ellipse = TRUE,
              circle = TRUE, 
              ellipse.prob = 0.68)
g <- g + scale_color_manual(name = "Score Categories", 
                            values = c("red", "green", "blue")) + 
  theme(legend.direction = 'horizontal', 
        legend.position = 'top')
g  

# Predict using PC scores:
trg <- predict(pc, training)  
trg <- data.frame(trg)  
trg$scoreCat <- training$scoreCat 

tst <- predict(pc, testing)  
tst <- data.frame(tst)
tst$scoreCat <- testing$scoreCat 

#model building - fitting the multinomial logistic regression model, here we use the package nnet
trg$scoreCat <- relevel(trg$scoreCat, ref = "High")
mymodel <- multinom(scoreCat ~ PC1 + PC2+PC3, data = trg)
summary(mymodel)

#Confusion Matrix for training data:
p <- predict(mymodel, trg)
tab <- table(p, trg$scoreCat)
tab
1 - sum(diag(tab))/sum(tab)

# Confusion matrix for the testing data
p1 <- predict(mymodel, newdata = tst)
tab1 <- table(Predicted = p1, Actual = tst$scoreCat)
tab1
1 - sum(diag(tab1))/sum(tab1)

#The model might be overfitting hence we are getting the values for accuracy as 0, 
#we need to further investigate into this

