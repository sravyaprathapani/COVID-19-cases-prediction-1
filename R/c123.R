C1 <- 
  function(data,
           t) {
    if ((t-14)<=0) {
      return (list("Not possible", "Not possible", "Not possible"));
    }
    mean <- mean(data[(t-7):(t-1)]);
    var <- 0;
    for (i in (t-7):(t-1)) {
      m <- mean(data[(i-7):(i-1)]);
      var <- var+(data[i]-m)**2;
    }
    var <- var/6;
    sd <- sqrt(var);
    res <- (data[t]-mean)/sd;
    return (list(mean, sd, res));
  }
C2 <- 
  function(data,
           t) {
    if(t-18<=0) {
      return (list("Not possible", "Not possible", "Not possible"));
    }
    mean <- mean(data[(t-9):(t-3)]);
    var <- 0;
    var <- 0;
    for (i in (t-9):(t-3)) {
      m <- mean(data[(i-9):(i-3)]);
      var <- var+(data[i]-m)**2;
    }
    var <- var/6;
    sd <- sqrt(var);
    res <- (data[t]-mean)/sd;
    return (list(mean, sd, res));
  }

C3 <- 
  function(data,
           t) {
    res <- 0;
    for (i in (t-2):t) {
      if(C2(data,i)[1] == "Not possible") {
        return (list("Not possible", "Not possible", "Not possible"));
      }
      else
      {
        mean = as.numeric(C2(data,i)[1]);
        sd = as.numeric(C2(data,i)[2]);
        c2 = as.numeric(C2(data,i)[3]);
        res <- res+ max(0,c2-1);
      }
    }    
    return (list(mean, sd, res));
  }

C <- 
  function(bound_metric,
           num_adj,
           i){
    if (bound_metric == 'C1') {
      limit <- 3
      c = C1(num_adj,i)
    }
    
    if (bound_metric == 'C2') {
      limit <- 3
      c = C2(num_adj,i)
    }
    
    if (bound_metric == 'C3') {
      limit <- 2
      c = C3(num_adj,i)
    }
    if (c[1] == 'Not possible') {
      return (list("Not possible", "Not possible", "Not possible", "Not possible"));
    }
    else {
      mean <- as.numeric(c[1]);
      sd <- as.numeric(c[2]);
      res <- as.numeric(c[3]);
    }
    return(list(mean,sd,limit,res));
  }