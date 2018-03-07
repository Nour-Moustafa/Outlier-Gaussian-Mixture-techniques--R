f=`unsw-normal`
UN=nsl.train.vectors[1:44,1]
UN1=nsl.test.test[1:44,1]

par(mfrow=c(1,2))

n1 = length(UN)
probabilities1 = (1:n1)/(n1+1)
normal.quantiles1 = qnorm(probabilities1, mean(UN, na.rm = T), sd(UN, na.rm = T))
plot(sort(normal.quantiles1), sort(UN) , xlab = '',lwd=2,
     ylab = 'Quantiles of normal NSLKDD  ', main = '',col=4)
abline(0,1,col=2,lwd=2)

n2 = length(UN1)
probabilities2 = (1:n2)/(n2+1)
normal.quantiles1 = qnorm(probabilities2, mean(UN1, na.rm = T), sd(UN1, na.rm = T))
plot(sort(normal.quantiles1), sort(UN1) , xlab = '',lwd=2,
     ylab = 'Quantiles of abnormal NSLKDD  ', main = '',col=4)
abline(0,1,col=2,lwd=2)
