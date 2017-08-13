queue_limited <- function(arrivals, service, interrupt, fetch_time, max_wait, adjust = 1){
  
  queue_times <- 0
  output <- rep(NA, length(arrivals))
  for(i in 1:length(arrivals)){
    if((max(queue_times, arrivals[i]) - arrivals[i]) < max_wait){
      queue_times <- max(queue_times, arrivals[i]) + service[i] * adjust
      output[i] <- queue_times
    }
  }
  print('Drop complete!')
  queue_times <- 0
  next_interrupt <- 1
  for(i in 1:length(arrivals)){
    if(is.na(output[i]) == FALSE){
      start_time <- 0
      service_time <- 0
      if((next_interrupt <= length(interrupt)) && (max(queue_times, arrivals[i]) > interrupt[next_interrupt]) && (max(queue_times, arrivals[i]) < interrupt[next_interrupt]+fetch_time)){
        start_time <- interrupt[next_interrupt]+fetch_time
        if(next_interrupt < length(interrupt)){
          next_interrupt <- next_interrupt + 1
        }
      }else{
        start_time <- max(queue_times, arrivals[i])
      }
      service_time <-service[i]
      while((next_interrupt <= length(interrupt)) && (interrupt[next_interrupt] < (start_time + service_time))){
        service_time <- service_time - (interrupt[next_interrupt] - start_time)
        start_time <- interrupt[next_interrupt] + fetch_time
        next_interrupt <- next_interrupt + 1
      }
      queue_times <- start_time + service_time
      output[i] <- queue_times
    }
  }
  return(output)
}


queue_batch_nbin <- function(arrivals, tau , c){
  
  queue_times <- 0
  output <- rep(NA, length(arrivals))
  interrupt <- c()
  j <- 1
  for(i in 1:length(arrivals)){
    if(queue_times < arrivals[i]){
      queue_times <- arrivals[i] + rnbinom(1, mu = tau, size = c^2-(tau^(-1)))
      interrupt[j] <- queue_times
      j <- j + 1
    }
    output[i] <- queue_times
    
  }
  return(list(output, interrupt))
}
queue_batch_geo <- function(arrivals, tau){
  
  queue_times <- 0
  output <- rep(NA, length(arrivals))
  interrupt <- c()
  j <- 1
  for(i in 1:length(arrivals)){
    if(queue_times < arrivals[i]){
      queue_times <- arrivals[i] + rgeom(1, 1/tau)
      interrupt[j] <- queue_times
      j <- j + 1
    }
    output[i] <- queue_times
    
  }
  return(list(output, interrupt))
}
queue_batch_pois <- function(arrivals, tau){
  
  queue_times <- 0
  output <- rep(NA, length(arrivals))
  interrupt <- c()
  j <- 1
  for(i in 1:length(arrivals)){
    if(queue_times < arrivals[i]){
      queue_times <- arrivals[i] + rpois(1, lambda = tau)
      interrupt[j] <- queue_times
      j <- j + 1
    }
    output[i] <- queue_times
    
  }
  return(list(output, interrupt))
}

mean_processing_time <- function(tau, alpha){
  times <- 10
  dataset1 = read.table("packetMinute2.txt")
  peripheralArrivals <- dataset1$V1-dataset1$V1[1]
  processinsServiceTime <- dataset1$V2/(mean(dataset1$V2)/mean(diff(dataset1$V1)))
  output <- rep(NA, length(tau))
  mean_time_output <- matrix(data=NA,nrow=4,ncol=length(tau))
  lost_rate_output <- matrix(data=NA,nrow=4,ncol=length(tau))
  for(i in 1:length(tau)){
    sumo <- rep(0,8)
    for(j in 1:times){
      print(i)
      print(j)
      peripheralservice <- rep(NA, length(dataset1$V1))
      peripheralDepartures <-rep(NA, length(dataset1$V1))
      
      output_pq1 <- queue_batch_pois(peripheralArrivals,10)
      peripheralDepartures1 <- output_pq1[[1]]
      interrupts1 <-output_pq1[[2]]
      
      output_pq2 <- queue_batch_geo(peripheralArrivals,10)
      peripheralDepartures2 <- output_pq2[[1]]
      interrupts2 <-output_pq2[[2]]
      
      output_pq3 <- queue_batch_nbin(peripheralArrivals,10,0.5)
      peripheralDepartures3 <- output_pq3[[1]]
      interrupts3 <-output_pq3[[2]]
      
      output_pq4 <- queue_batch_nbin(peripheralArrivals,10,2)
      peripheralDepartures4 <- output_pq4[[1]]
      interrupts4 <-output_pq4[[2]]
      
      ceteralArrival1 <- peripheralDepartures1
      ceteralArrival2 <- peripheralDepartures2
      ceteralArrival3 <- peripheralDepartures3
      ceteralArrival4 <- peripheralDepartures4
      
      
      centralDepartures1 <- queue_limited(ceteralArrival1, processinsServiceTime,interrupts1,1, 15.0, alpha)  
      centralDepartures2 <- queue_limited(ceteralArrival2, processinsServiceTime,interrupts2,1, 15.0, alpha)  
      centralDepartures3 <- queue_limited(ceteralArrival3, processinsServiceTime,interrupts3,1, 15.0, alpha)  
      centralDepartures4 <- queue_limited(ceteralArrival4, processinsServiceTime,interrupts4,1, 15.0, alpha)
      sumo[1] <- sumo[1] + mean(centralDepartures1 - peripheralArrivals , na.rm = TRUE)
      sumo[2] <- sumo[2] + mean(centralDepartures2 - peripheralArrivals , na.rm = TRUE)
      sumo[3] <- sumo[3] + mean(centralDepartures3 - peripheralArrivals , na.rm = TRUE)
      sumo[4] <- sumo[4] + mean(centralDepartures4 - peripheralArrivals , na.rm = TRUE)
      sumo[5] <- sumo[5] + sum(is.na(centralDepartures1))
      sumo[6] <- sumo[6] + sum(is.na(centralDepartures2))
      sumo[7] <- sumo[7] + sum(is.na(centralDepartures3))
      sumo[8] <- sumo[8] + sum(is.na(centralDepartures4))
      
    }
    mean_time_output[1,i] <- sumo[1] /times
    mean_time_output[2,i] <- sumo[2] /times
    mean_time_output[3,i] <- sumo[3] /times
    mean_time_output[4,i] <- sumo[4] /times
    lost_rate_output[1,i] <- sumo[5] /times
    lost_rate_output[2,i] <- sumo[6] /times
    lost_rate_output[3,i] <- sumo[7] /times
    lost_rate_output[4,i] <- sumo[8] /times
  }
  return(list(mean_time_output,lost_rate_output))
}

