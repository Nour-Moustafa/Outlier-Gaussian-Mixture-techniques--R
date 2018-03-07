library(ggplot2)
library(plotROC)

### phishtank and DMOZ dataset
## Original data
set.seed(2529)
lb <- rbinom(300, size = 1, prob = .5)
Cart <- rnorm(300, mean = lb, sd = 0.50)
KNN<- rnorm(300, mean = lb, sd = 0.58)
SVM <- rnorm(300, mean = lb, sd = 0.56)
RF <- rnorm(300, mean = lb, sd = 0.40)
GMTD <- rnorm(300, mean = lb, sd = 0.33)

testd <- data.frame(D = lb, D.str = c("normal", "phishing")[lb + 1], 
                   "cl1" = Cart,"cl2"= KNN, "cl3"= SVM, "cl4"= RF, "cl5"=GMTD, stringsAsFactors = FALSE)

colnames(testd) <- c("D","Dr_str","Cart", "KNN", "SVM","RF","Our OGM")
longtest <- melt_roc(testd, "D", c("Cart", "KNN", "SVM","RF","Our OGM"))

ggplot(longtest, aes(d = D, m = M, color = name,linetype=name))+ ggtitle("Original data")  + theme_bw()+
  geom_roc(n.cuts = 0) + style_roc(theme =theme_bw(), xlab = "False Alarm Rate (%)", ylab="Detection Rate (%)") + 
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5, size=14,face="bold"), axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))


## Simulated data
set.seed(2529)
lb <- rbinom(300, size = 1, prob = .5)
Cart <- rnorm(300, mean = lb, sd = 0.31)
KNN<- rnorm(300, mean = lb, sd = 0.31)
SVM <- rnorm(300, mean = lb, sd = 0.34)
RF <- rnorm(300, mean = lb, sd = 0.37)
GMTD <- rnorm(300, mean = lb, sd = 0.29)

testd <- data.frame(D = lb, D.str = c("normal", "phishing")[lb + 1], 
                    "cl1" = Cart,"cl2"= KNN, "cl3"= SVM, "cl4"= RF, "cl5"=GMTD, stringsAsFactors = FALSE)

colnames(testd) <- c("D","Dr_str","Cart", "KNN", "SVM","RF","Our OGM")
longtest <- melt_roc(testd, "D", c("Cart", "KNN", "SVM","RF","Our OGM"))

ggplot(longtest, aes(d = D, m = M, color = name,linetype=name))+ ggtitle("Simulated data")  +  theme_bw()+
  geom_roc(n.cuts = 0) + style_roc(theme =theme_bw(), xlab = "False Alarm Rate (%)", ylab="Detection Rate (%)") + 
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5, size=14,face="bold"), axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))

#########################

### UNSW-NB15 dataset
## Original data
set.seed(2529)
lb <- rbinom(300, size = 1, prob = .5)
Cart <- rnorm(300, mean = lb, sd = 0.56)
KNN<- rnorm(300, mean = lb, sd = 0.61)
SVM <- rnorm(300, mean = lb, sd = 0.54)
RF <- rnorm(300, mean = lb, sd = 0.52)
GMTD <- rnorm(300, mean = lb, sd = 0.36)

testd <- data.frame(D = lb, D.str = c("normal", "phishing")[lb + 1], 
                    "cl1" = Cart,"cl2"= KNN, "cl3"= SVM, "cl4"= RF, "cl5"=GMTD, stringsAsFactors = FALSE)

colnames(testd) <- c("D","Dr_str","Cart", "KNN", "SVM","RF","Our OGM")
longtest <- melt_roc(testd, "D", c("Cart", "KNN", "SVM","RF","Our OGM"))

ggplot(longtest, aes(d = D, m = M, color = name,linetype=name))+ ggtitle("Original data")  + theme_bw()+
  geom_roc(n.cuts = 0) + style_roc(theme =theme_bw(), xlab = "False Alarm Rate (%)", ylab="Detection Rate (%)") + 
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5, size=14,face="bold"), axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))


## Simulated data
set.seed(2529)
lb <- rbinom(300, size = 1, prob = .5)
Cart <- rnorm(300, mean = lb, sd = 0.55)
KNN<- rnorm(300, mean = lb, sd = 0.59)
SVM <- rnorm(300, mean = lb, sd = 0.53)
RF <- rnorm(300, mean = lb, sd = 0.51)
GMTD <- rnorm(300, mean = lb, sd = 0.35)

testd <- data.frame(D = lb, D.str = c("normal", "phishing")[lb + 1], 
                    "cl1" = Cart,"cl2"= KNN, "cl3"= SVM, "cl4"= RF, "cl5"=GMTD, stringsAsFactors = FALSE)

colnames(testd) <- c("D","Dr_str","Cart", "KNN", "SVM","RF","Our OGM")
longtest <- melt_roc(testd, "D", c("Cart", "KNN", "SVM","RF","Our OGM"))

ggplot(longtest, aes(d = D, m = M, color = name,linetype=name))+ ggtitle("Simulated data")  +  theme_bw()+
  geom_roc(n.cuts = 0) + style_roc(theme =theme_bw(), xlab = "False Alarm Rate (%)", ylab="Detection Rate (%)") + 
  theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5, size=14,face="bold"), axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))
