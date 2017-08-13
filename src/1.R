source("func.R")
tau= seq(0, 2, 0.004)
mpt = mean_processing_time(tau,2)
mpt[[2]] <- mpt[[2]]/length(dataset1$V1)

#Effects of different aggregation interval lengths τ and normalized arrival rates α on the mean processing time E[D].
plot(tau,mpt[[1]][1,],type="l",col="red", ylim=c(0,max(mpt[[1]])),  xlab= expression(tau), ylab="E[D]", main="Effects of different aggregation interval lengths τ and normalized arrival rates α = 2 on the mean processing time E[D].")
lines(tau,mpt[[1]][2,],col="green")
lines(tau,mpt[[1]][3,],col="blue")
lines(tau,mpt[[1]][4,],col="dark green")

#Effects of different aggregation interval lengths τ and normalized arrival rates α on the mean processing time E[D].
plot(tau,mpt[[2]][1,], type="l", col="red", ylim=c(0,max(mpt[[2]])),  xlab= expression(tau), ylab = expression(~ Pb), main="Effects of different aggregation interval lengths τ and normalized arrival rates α = 2 on the packet loss probability p b .")
lines(tau,mpt[[2]][2,],col="green")
lines(tau,mpt[[2]][3,],col="blue")
lines(tau,mpt[[2]][4,],col="dark green")

mpt1 = mean_processing_time(tau,0.33)
mpt1[[2]] <- mpt1[[2]]/length(dlklataset1$V1)

#Effects of different aggregation interval lengths τ and normalized arrival rates α on the mean processing time E[D].
plot(tau,mpt1[[1]][1,],type="l",col="red", ylim=c(0,max(mpt1[[1]])),  xlab= expression(tau), ylab="E[D]", main="Effects of different aggregation interval lengths τ and normalized arrival rates α =0.33 on the mean processing time E[D].")
lines(tau,mpt1[[1]][2,],col="green")
lines(tau,mpt1[[1]][3,],col="blue")
lines(tau,mpt1[[1]][4,],col="dark green")

#Effects of different aggregation interval lengths τ and normalized arrival rates α on the mean processing time E[D].
plot(tau,mpt1[[2]][1,], type="l", col="red", ylim=c(0,max(mpt1[[2]])),  xlab= expression(tau), ylab = expression(~ Pb), main="Effects of different aggregation interval lengths τ and normalized arrival rates α = 0.33 on the packet loss probability p b .")
lines(tau,mpt1[[2]][2,],col="green")
lines(tau,mpt1[[2]][3,],col="blue")
lines(tau,mpt1[[2]][4,],col="dark green")

mpt2 = mean_processing_time(tau,0.66)
mpt2[[2]] <- mpt1[[2]]/length(dlklataset1$V1)


#Effects of different aggregation interval lengths τ and normalized arrival rates α on the mean processing time E[D].
plot(tau,mpt2[[1]][1,],type="l",col="red", ylim=c(0,max(mpt2[[1]])),  xlab= expression(tau), ylab="E[D]", main="Effects of different aggregation interval lengths τ and normalized arrival rates α =0.66 on the mean processing time E[D].")
lines(tau,mpt2[[1]][2,],col="green")
lines(tau,mpt2[[1]][3,],col="blue")
lines(tau,mpt2[[1]][4,],col="dark green")

#Effects of different aggregation interval lengths τ and normalized arrival rates α on the mean processing time E[D].
plot(tau,mpt2[[2]][1,], type="l", col="red", ylim=c(0,max(mpt2[[2]])),  xlab= expression(tau), ylab = expression(~ Pb), main="Effects of different aggregation interval lengths τ and normalized arrival rates α = 0.66 on the packet loss probability p b .")
lines(tau,mpt2[[2]][2,],col="green")
lines(tau,mpt2[[2]][3,],col="blue")
lines(tau,mpt2[[2]][4,],col="dark green")



