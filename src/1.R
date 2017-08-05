library(queuecomputer);
n <- 1e3
tau <- 5.0
arrivals <- cumsum(rexp(n))
timeout <- 0.0
service <- c()
departures <- c()
for (i in 1:n){
  service[i] <- 0
  if(arrivals[i] > timeout){
    timeout <- arrivals[i] + tau * rexp(1)
    service[i] <- tau
  }
  departures[i] <- timeout
}
queuedata <- queue_lengths(arrivals, service, departures)
average_queue(queuedata$times, queuedata$queuelength)+1

