X_qi <- c(114,81,87,114,113,87,111,89,93,108,99,93,100,95,93,95,106,108)

hist(X_qi)
par(new=TRUE)
plot(dnorm(100,5),xlim=c(80,115),ylim=c(0,5))

ks.test(X_qi,'pnorm')

lillie.test(X_qi)

shapiro.test(X_qi)

jarque.bera.test(X_qi)
