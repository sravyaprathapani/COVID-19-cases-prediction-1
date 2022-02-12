#' Calculates the mean, standard deviation and C1 metric value
#' @description The C1 uses moving sample average and sample standard deviation
#' to standardize each observation. It calculates the sample average and
#' sample standard deviation using the data of 7 days prior to the current day.
#' @note This function is called by Cmetric function
#' @param df Dataframe with two columns which contains dates and corresponding number of daily confirmed cases
#' @param t Day number
#' @return Returns the mean, standard deviation and C1 metric value
#' @export

C1 <-
  function(df,
           t) {
    if ((t-14)<=0) {
      return (list("Not possible", "Not possible", "Not possible"));
    }
    data <- df[,2];
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

#' Calculates the mean, standard deviation and C2 metric value taking a 2 day lag
#' @description The C2 uses moving sample average and sample standard deviation
#' to standardize each observation. It calculates the sample average and
#' sample standard deviation using the data of seven days and a 2-day lag.
#' @note This function is called by Cmetric function
#' @param df Dataframe with two columns which contains dates and corresponding number of daily confirmed cases
#' @param t Day number
#' @return Returns the mean, standard deviation and C2 metric value
#' @export

C2 <-
  function(df,
           t) {
    if(t-18<=0) {
      return (list("Not possible", "Not possible", "Not possible"));
    }
    data <- df[,2];
    mean <- mean(data[(t-9):(t-3)]);
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

#' Calculates the mean, standard deviation and C2 metric value taking a 1 day lag
#' @description This is similar to the function C2, but it uses the data of seven days and a 1-day lag.
#' @note This function is called by Cmetric function
#' @param df Dataframe with two columns which contains dates and corresponding number of daily confirmed cases
#' @param t Day number
#' @return Returns the mean, standard deviation and C2 metric value
#' @export

C2_1day <-
  function(df,
           t) {
    if(t-16<=0) {
      return (list("Not possible", "Not possible", "Not possible"));
    }
    data <- df[,2];
    mean <- mean(data[(t-8):(t-2)]);
    var <- 0;
    for (i in (t-8):(t-2)) {
      m <- mean(data[(i-8):(i-2)]);
      var <- var+(data[i]-m)**2;
    }
    var <- var/6;
    sd <- sqrt(var);
    res <- (data[t]-mean)/sd;
    return (list(mean, sd, res));
  }

#' Calculates the mean, standard deviation and C3 metric value taking a 2 day lag
#' @description The C3 uses information from the C2 statistic of the past two days.
#' @note This function is called by Cmetric function
#' @param df Dataframe with two columns which contains dates and correspoding number of daily confirmed cases
#' @param t Day number
#' @return Returns the mean, standard deviation and C3 metric value
#' @export

C3 <-
  function(df,
           t) {
    res <- 0;
    for (i in (t-2):t) {
      if(C2(df,i)[1] == "Not possible") {
        return (list("Not possible", "Not possible", "Not possible"));
      }
      else
      {
        c2val = C2(df,i);
        mean = as.numeric(c2val[1]);
        sd = as.numeric(c2val[2]);
        c2 = as.numeric(c2val[3]);
        res <- res+ max(0,c2-1);
      }
    }
    return (list(mean, sd, res));
  }
#' Calculates the mean, standard deviation and C3 metric value taking a 1 day lag
#' @description This is similar to the function C3, but it uses the data of seven days and a 1-day lag.
#' @note This function is called by Cmetric function
#' @param df Dataframe with two columns which contains dates and corresponding number of daily confirmed cases
#' @param t Day number
#' @return Returns the mean, standard deviation and C3 metric value
#' @export

C3_1day <-
  function(df,
           t) {
    res <- 0;
    for (i in (t-2):t) {
      if(C2_1day(df,i)[1] == "Not possible") {
        return (list("Not possible", "Not possible", "Not possible"));
      }
      else
      {
        c2val = C2_1day(df,i);
        mean = as.numeric(c2val[1]);
        sd = as.numeric(c2val[2]);
        c2 = as.numeric(c2val[3]);
        res <- res+ max(0,c2-1);
      }
    }
    return (list(mean, sd, res));
  }

#' Returns the value of C1, C2 or C3 metric and the corresponding limit
#' @description Calls C1, C2, C3 functions and returns the respective metric value and the threshold limit.
#' @note This function is called by threshold function
#' @param bound_metric C1, C2, or C3 metric can be selected
#' @param df Dataframe with two columns which contains dates and corresponding number of daily confirmed cases
#' @param i Day number
#' @return Returns the mean, standard deviation, threshold value for that metric and the metric value
#' @export

Cmetric<-
  function(bound_metric,
           df,
           i){
    if (bound_metric == 'C1') {
      limit <- 3
      c = C1(df,i)
    }
    
    if (bound_metric == 'C2') {
      limit <- 3
      c = C2(df,i)
    }
    
    if (bound_metric == 'C2_1day') {
      limit <- 3
      c = C2_1day(df,i)
    }
    
    if (bound_metric == 'C3') {
      limit <- 2
      c = C3(df,i)
    }
    
    if (bound_metric == 'C3_1day') {
      limit <- 2
      c = C3_1day(df,i)
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

#' Identifies and adjusts jumps and drops
#' @description Identifies jumps and drops using given metric and adjusts if the length of the jump or drop is less than the upper bound.
#' @note This function is called by sisd_cummulative function
#' @param ub_for_adjustment upper bound for the duration of a jump or drop
#' @param bound_metric C1, C2, or C3 metric can be selected
#' @param df_confirmed_values Dataframe of dates and observed number of daily confirmed cases
#' @param df_adjusted_confirmed_values Copy of df_confirmed_values for adjustment
#' @return Dataframe of dates and adjusted data using given metric
#' @export

threshold <-
  function(ub_for_adjustment,
           bound_metric,
           df_confirmed_values,
           df_adjusted_confirmed_values
  ) {
    
    n <- nrow(df_adjusted_confirmed_values);
    
    for (i in 1:n) {
      if(Cmetric(bound_metric,df_adjusted_confirmed_values,i)[1] == "Not possible"){
        next;
      }
      else{
        cval = Cmetric(bound_metric,df_adjusted_confirmed_values,i);
        mean = as.numeric(cval[1]);
        sd = as.numeric(cval[2]);
        limit = as.numeric(cval[3]);
        res = as.numeric(cval[4]); #C value
      }
      #check if C value lies in limit
      if(res <= limit && res >= (-1*limit)) {
        next;
      }
      else{
        #calculate number of consecutive days outside CI
        t = i+1
        res = C(bound_metric,df_adjusted_confirmed_values,t)[4];
        while((t <= n) && (res > limit || res < (-1*limit))){
          t <- t+1
          res = C(bound_metric,df_adjusted_confirmed_values,t)[4]
        }
        # length of interval outside of CI
        length_of_jd = t - i
        
        #if such number of days are more than these many days, then do not adjust
        #else adjust data with averages
        if(length_of_jd <= ub_for_adjustment){
          if( t <= n  ){
            for ( a in 1:length_of_jd )
              df_adjusted_confirmed_values[i+a-1,2] = mean/2 + df_confirmed_values[t,2]/2
          }
          else{
            for ( a in 1:length_of_jd )
              df_adjusted_confirmed_values[i+a-1,2] = mean
          }
        }
        
      }
    }
    
    return( df_adjusted_confirmed_values );
  }

#' Predict with or without adjusting the data
#' @description Prediction of Cumulative number of cases using data driven modified SIS model
#' @param population number of people in the state
#' @param gamma recovery rate
#' @param cur_day current day number for start of prediction phase
#' @param start_date start date in the considered dataset
#' @param last_n_day number of days in training phase
#' @param last_limit maximum number of days in the validation period
#' @param next_n_days number of days in the prediction phase
#' @param data state wise daily cases adjusted data for the given state
#' @param adjusted predictions can be made with and without adjustment in data
#' @param df_adjusted_confirmed_values array of adjusted number of daily confirmed cases
#' @param mu mortality rate of the infection
#' @return graph showing the training phase and prediction of cumulative number of cases, data frame of cumulative cases in training and prediction phase, mean square error of validation and prediction period
#' @importFrom pracma strcmp
#' @importFrom utils tail
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme
#' @export

sisd_cummulative<-
  function(population,
           gamma,
           cur_day,
           start_date,
           last_n_day,
           last_limit,
           next_n_days,
           data,
           adjusted,
           df_adjusted_confirmed_values,
           mu) {
    
    min_mu = 0.001
    max_mu = 0.1
    mu_step = 0.001
    validation_period <- last_n_day
    Count <- Type <- Date <- NULL;
    
    #Adjusting values of confirmed cases (for last_limit days before cur_day)
    if(adjusted){
      #create copy of data without adjustments
      original_data <- data
      j <- cur_day-last_limit;
      ran <- 3*cur_day
      ran_limit <- 3*(cur_day-last_limit-1);
      for (i in ran_limit:ran){
        if (data[i,1] == 'Confirmed' && data[i,2] == df_adjusted_confirmed_values[j,1]){
          data[i,3] <- df_adjusted_confirmed_values[j,2]
          j <- j+1
        }
      }
    }
    
    if (!missing(mu)) {
      min_mu = mu
      max_mu = mu
    }
    
    dt <- vector()
    c <- vector()
    r <- vector()
    d <- vector()
    
    for (i in 1:nrow(data)) {
      row <- data[i,]
      status <- row$Status
      date <- toString(row$Date)
      num <- row$Count
      if(date %in% dt == FALSE)
        dt <- append(dt, date)
      status <- toString(status)
      if (strcmp(status, "Confirmed"))
        c <- append(c, num)
      else if (strcmp(status, "Recovered"))
        r <- append(r, num)
      else if (strcmp(status, "Deceased"))
        d <- append(d, num)
    }
    
    #extract original data
    if(adjusted){
      original_dt <- vector()
      original_c <- vector()
      original_r <- vector()
      original_d <- vector()
      for (i in 1:nrow(original_data)) {
        row <- original_data[i,]
        status <- row$Status
        date <- toString(row$Date)
        num <- row$Count
        if(date %in% dt == FALSE)
          original_dt <- append(original_dt, date)
        status <- toString(status)
        if (strcmp(status, "Confirmed"))
          original_c <- append(original_c, num)
        else if (strcmp(status, "Recovered"))
          original_r <- append(original_r, num)
        else if (strcmp(status, "Deceased"))
          original_d <- append(original_d, num)
      }
    }
    
    #Calculating the Cumulative cases using the data
    get_data <- function(dt, c, r, d, N) {
      sus <- vector()
      cum_inf <- vector()
      inf_active <- vector()
      deceased <- vector()
      date <- vector()
      day <- vector()
      sus <- append(sus, N - c[1])
      cum_inf <- append(cum_inf, c[1])
      inf_active <- append(inf_active, c[1] - r[1])
      deceased <- append(deceased, d[1])
      day <- append(day, 0)
      date <- append(date, dt[1])
      for (i in 2:length(c)) {
        cum_inf <- append(cum_inf, c[i] + tail(cum_inf, n = 1))
        inf_active <-
          append(inf_active, c[i] - r[i] - d[i] + tail(inf_active, n = 1))
        sus <- append(sus, N - tail(inf_active, n = 1))
        date <- append(date, dt[i])
        day <- append(day, i)
        deceased <- append(deceased, d[i] + tail(deceased, n = 1))
      }
      ret <-
        list(
          "S" = sus,
          "I" = inf_active,
          "C" = cum_inf,
          "D" = deceased,
          "date" = date,
          "day" = day
        )
      return(ret)
    }
    
    odata <- get_data(dt, c, r, d, population)
    if(adjusted)
      original_odata <- get_data(original_dt, original_c, original_r, original_d, population)
    else
      original_odata <- odata
    
    #Used for estimating beta and mu
    sisd <-
      function(N,
               beta,
               gamma,
               mu,
               cur_day,
               last_n,
               So,
               Io,
               Co,
               Do) {
        start <- cur_day - last_n + 1
        S_P <- So[start]
        I_P <- Io[start]
        C_P <- Co[start]
        D_P <- Do[start]
        S <- vector()
        I <- vector()
        C <- vector()
        D <- vector()
        S <- append(S, S_P)
        I <- append(I, I_P)
        C <- append(C, C_P)
        D <- append(D, D_P)
        while (start <= cur_day) {
          start <- start + 1
          S_N <- S_P - S_P * I_P * beta / N + gamma * I_P
          I_N <- I_P + S_P * I_P * beta / N - gamma * I_P - mu * I_P
          C_N <- C_P + S_P * I_P * beta / N
          D_N <- D_P + mu * I_P
          S_P <- S_N
          I_P <- I_N
          C_P <- C_N
          D_P <- D_N
          S <- append(S, S_N)
          I <- append(I, I_N)
          C <- append(C, C_N)
          D <- append(D, D_N)
        }
        ret <- list(
          "S" = S,
          "I" = I,
          "C" = C,
          "D" = D
        )
        return(ret)
      }
    
    #Used for prediction
    sisd_pred <-
      function(N,
               beta,
               gamma,
               mu,
               cur_day,
               next_n,
               So,
               Io,
               Co,
               Do) {
        start <- cur_day
        S_P <- So[start]
        I_P <- Io[start]
        C_P <- Co[start]
        D_P <- Do[start]
        S <- vector()
        I <- vector()
        C <- vector()
        D <- vector()
        start <- start + 1
        while (start <= cur_day + next_n) {
          S_N <- S_P - S_P * I_P * beta / N + gamma * I_P
          I_N <- I_P + S_P * I_P * beta / N - gamma * I_P - mu * I_P
          C_N <- C_P + S_P * I_P * beta / N
          D_N <- D_P + mu * I_P
          S_P <- S_N
          I_P <- I_N
          C_P <- C_N
          D_P <- D_N
          S <- append(S, S_N)
          I <- append(I, I_N)
          C <- append(C, C_N)
          D <- append(D, D_N)
          start <- start + 1
        }
        ret <- list(
          "S" = S,
          "I" = I,
          "C" = C,
          "D" = D
        )
        return(ret)
      }
    
    best_last_n_days <- last_n_day
    best_beta <- -1
    best_mu <- -1
    avg_error <- Inf
    loss_limit <- last_n_day
    
    itr <- 1
    while (itr <= last_limit) {
      itr <- itr+ 1
      mu1 = min_mu
      while (mu1 <= max_mu) {
        beta1 = 0.01
        
        while (beta1 < 0.3) {
          ret <-
            sisd(
              population,
              beta1,
              gamma,
              mu1,
              cur_day,
              last_n_day,
              odata$S,
              odata$I,
              odata$C,
              odata$D
            )
          nerr <- 0
          start <- cur_day - last_n_day + 1
          idx <- 1
          while (idx <= length(ret$I)) {
            if (idx >= (length(ret$I) - loss_limit)) {
              nerr <- nerr + abs(ret$C[idx] - odata$C[start]) ** 2
            }
            idx <- idx + 1
            start <- start + 1
          }
          nerr <- nerr / idx
          nerr <- sqrt(nerr)
          if (avg_error > nerr) {
            avg_error <- nerr
            best_beta <- beta1
            best_mu <- mu1
            best_last_n_days <- last_n_day
          }
          beta1 <- beta1 + 0.01
        }
        mu1 <- mu1 + mu_step
      }
      last_n_day <- last_n_day + 1
    }
    
    print(paste("Optimal mu = ", best_mu))
    print(paste("Optimal beta = ", best_beta))
    print(paste("Optimal training period = ", best_last_n_days))
    
    train <-
      sisd(
        population,
        best_beta,
        gamma,
        best_mu,
        cur_day,
        best_last_n_days,
        odata$S,
        odata$I,
        odata$C,
        odata$D
      )
    kk <-
      sisd_pred(
        population,
        best_beta,
        gamma,
        best_mu,
        cur_day,
        next_n_days + 1,
        odata$S,
        odata$I,
        odata$C,
        odata$D
      )
    
    
    df = data.frame(
      Day = integer(),
      Count = double(),
      Type = character(),
      Date = as.Date(character()),
      stringsAsFactors = FALSE
    )
    
    idx <- cur_day - best_last_n_days + 1
    while (idx <= cur_day+next_n_days ) {
      df[nrow(df) + 1, ] = list(odata$day[idx],
                                original_odata$C[idx],
                                "Observed",
                                as.Date((odata$date[idx]), "%d-%b-%y"))
      idx <- idx + 1
    }
    
    idx <- cur_day - best_last_n_days + 1
    idx1 <- 1
    while (idx <= cur_day) {
      df[nrow(df) + 1,] = list(odata$day[idx],
                               formatC( train$C[idx1] , digits = 2, format = "f"),
                               "Optimaly Trained",
                               as.Date(odata$date[idx], "%d-%b-%y"))
      idx <- idx + 1
      idx1 <- idx1 + 1
    }
    
    idx <- cur_day + 1
    idx1 <- 1
    nerr <- 0
    while (idx <= cur_day + next_n_days) {
      df[nrow(df) + 1,] = list(cur_day+idx1,
                               formatC(kk$C[idx1], digits = 2, format = "f"),
                               "Predicted",
                               as.Date(as.Date(start_date)+cur_day+idx1, "%d-%b-%y"))
      
      idx <- idx + 1
      idx1 <- idx1 + 1
    }
    df = transform(df, Count = as.numeric(Count))
    
    #mse and r2 of prediction period
    observed_pred  <- vector()
    idx <- cur_day+1
    idx1 <- 1
    while(idx1<=next_n_days){
      observed_pred <- append( observed_pred , original_odata$C[idx])
      idx1 <- idx1+1
      idx <- idx+1
    }
    mean_sq = mean((observed_pred-kk$C[1:next_n_days])**2)
    stot = sum((observed_pred - mean(observed_pred))**2)
    sres = sum((observed_pred-kk$C)**2)
    rsq = 1 - sres/stot
    prediction_mse <- format(round(sqrt(mean_sq), 2))
    print(paste("Root Mean Square error in predictions= ", format(round(sqrt(mean_sq), 2), nsmall = 2)))
    
    #mse of validation period
    observed_val  <- vector()
    idx <- cur_day-validation_period+1
    while(idx<=cur_day)
    {
      observed_val <- append( observed_val , odata$C[idx])
      idx <- idx+1
    }
    train_len = length(train$C)
    pred_train = train$C[(train_len-validation_period+1):train_len]
    val_mean_sq = mean((observed_val-pred_train)**2)
    validation_mse <- format(round(sqrt(val_mean_sq), 2))
    print(paste("Root Mean Square error of validation period = ", format(round(sqrt(val_mean_sq), 2), nsmall = 2)))
    
    if(adjusted){
      opt_col <- '#cc33ff'
      col <- '#0033cc'
    }
    else{
      opt_col <- '#006600'
      col <- '#ff3300'
    }
    
    p <-
      ggplot(df, aes(
        x = Date,
        y = Count,
        shape = Type,
        color = Type
      )) + geom_point(size = 2) + scale_shape_manual(values = c(3, 16, 17))+scale_color_manual(values = c('#000000', opt_col, col ))
    
    p <-
      p + scale_x_date(date_breaks = "10 day") + labs(y = "Cumulative Number of Cases", x = "Date") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))
    
    return(list(p,df,prediction_mse,validation_mse))
  }


#' plots graph of daily cases with and without adjustment
#' @description Visual representation of the adjustments made using a given metric.
#' @param df_confirmed_values dataframe with two columns which contains dates and corresponding observed number of daily confirmed cases
#' @param df_adjusted_confirmed_values dataframe with two columns which contains dates and corresponding adjusted number of daily confirmed cases
#' @return graph showing number of confirmed daily cases with and without adjustment
#' @export

plot_adjustment <- function(df_confirmed_values,df_adjusted_confirmed_values){
  date <- df_confirmed_values[,1]
  num <- df_confirmed_values[,2]
  num_adj <- df_adjusted_confirmed_values[,2]
  # plot(x=seq(date), y=num, type="l", lty=1,col = 'red', xlab="days", ylab="daily cases", xaxt="n")
  # lines(x=seq(date), y=num_adj, lty=2, col= 'blue')
  # axis(side=1, at<-seq(date)[seq(1, length(seq(date)), 30)] , labels<-date[seq(1, length(date), 30)])
  # par(xpd=FALSE)
  # legend("topleft", legend=c("observed", "adjusted"), lty=1:2,ncol=1)
  
  df = data.frame(
    Count = double(),
    Type = character(),
    Date = as.Date(character()),
    stringsAsFactors = FALSE
  )
  
  idx <- 1
  while (idx <= length(num) ) {
    df[nrow(df) + 1, ] = list(num[idx],
                              "Observed",
                              as.Date(date[idx], "%d-%b-%y"))
    idx <- idx + 1
  }
  
  idx <- 1
  while (idx <= length(num) ) {
    df[nrow(df) + 1, ] = list(num_adj[idx],
                              "adjusted",
                              as.Date(date[idx], "%d-%b-%y"))
    idx <- idx + 1
  }
  
  df = transform(df, Count = as.numeric(Count))
  p <-
    ggplot(df, aes(
      x = Date,
      y = Count,
      group = Type
    )) + geom_line(aes(color=Type))
  p <-
    p + scale_x_date(date_breaks = "60 day") + labs(y = "Daily Cases", x = "Date") +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
  
  return (p)
}

#' Plots cumulative cases graph for original and adjusted predictions
#' @description Provides a visual comparison between predictions made with and without the adjustments and the observed number of cases.
#' @param output_original output of sisd_cummulative for original state-wise data
#' @param output_adjusted output of sisd_cummulative for adjusted state-wise data
#' @return graph showing observed, trained and predicted values for both original and adjusted data
#' @export

plot_cumulative <- function(output_original, output_adjusted){
  df_org = output_original[2][[1]]
  obs = df_org[df_org[3] == "Observed"]
  obs = obs[(length(obs)/4 +1 ): (length(obs)/2)]
  ot = df_org[df_org[3] == "Optimaly Trained"]
  ot_dates = ot[(3*length(ot)/4+1):length(ot)]
  ot = ot[(length(ot)/4 +1 ): (length(ot)/2)]
  pred = df_org[df_org[3] == "Predicted"]
  pred_dates = pred[(3*length(pred)/4+1):length(pred)]
  pred = pred[(length(pred)/4 +1 ): (length(pred)/2)]
  pred_original = c(ot, pred)
  pred_original_dates = c(ot_dates,pred_dates)
  
  df = output_adjusted[2][[1]]
  ot = df[df[3] == "Optimaly Trained"]
  ot_dates = ot[(3*length(ot)/4+1):length(ot)]
  ot = ot[(length(ot)/4 +1 ): (length(ot)/2)]
  pred = df[df[3] == "Predicted"]
  pred_dates = pred[(3*length(pred)/4+1):length(pred)]
  pred = pred[(length(pred)/4 +1 ): (length(pred)/2)]
  pred_adjusted = c(ot, pred)
  pred_adjusted_dates = c(ot_dates,pred_dates)
  
  pred_len = min(length(pred_adjusted),length(pred_original))
  pred_original = tail(pred_original,pred_len)
  pred_original_dates = tail(pred_original_dates,pred_len)
  pred_adjusted = tail(pred_adjusted,pred_len)
  pred_adjusted_dates = tail(pred_adjusted_dates,pred_len)
  obs = tail(obs,pred_len)
  optimaly_trained_period = pred_len-length(pred)
  
  Count <- Type <- Date <- NULL;
  df = data.frame(
    Count = double(),
    Type = character(),
    Date = as.Date(character()),
    stringsAsFactors = FALSE
  )
  idx <- 1
  while (idx <= length(obs) ) {
    df[nrow(df) + 1, ] = list(obs[idx],
                              "Observed",
                              pred_original_dates[idx])
    idx <- idx + 1
  }
  idx<-1
  while (idx <= optimaly_trained_period) {
    df[nrow(df) + 1,] = list(pred_adjusted[idx],
                             "Optimaly Trained Adjusted",
                             pred_original_dates[idx])
    idx <- idx + 1
  }
  idx <- 1
  while (idx <= optimaly_trained_period) {
    df[nrow(df) + 1,] = list(pred_original[idx],
                             "Optimaly Trained Original",
                             pred_original_dates[idx])
    idx <- idx + 1
  }
  
  while (idx <= length(pred_adjusted)) {
    df[nrow(df) + 1,] = list(pred_adjusted[idx],
                             "Predicted Adjusted",
                             pred_original_dates[idx])
    idx <- idx + 1
  }
  idx <- optimaly_trained_period+1
  while (idx <= length(pred_original)) {
    df[nrow(df) + 1,] = list(pred_original[idx],
                             "Predicted Original",
                             pred_original_dates[idx])
    idx <- idx + 1
  }
  
  df = transform(df, Count = as.numeric(Count))
  p <-
    ggplot(df, aes(
      x = Date,
      y = Count,
      shape = Type,
      color = Type
    )) + geom_point(size = 2) + scale_shape_manual(values = c(3, 16, 16, 17,17)) + scale_color_manual(values = c('#000000', '#cc33ff', '#006600', '#0033cc','#ff3300'))
  p <-
    p + scale_x_date(date_breaks = "10 day") + labs(y = "Cumulative Number of Cases", x = "Date") + ggtitle("Daily cases with/without adjustment") +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
  return (p)
}

#' Identifies method with least error
#' @description Compares the validation rmse using original and adjusted data and returns the one with lesser error.
#' @param output_original output of sisd_cummulative for original state-wise data
#' @param output_adjusted output of sisd_cummulative for adjusted state-wise data
#' @return graph showing observed and predicted cummulative cases using the best method
#' @export

comparing_results <- function(output_original,output_adjusted){
  val_original <- output_original[4][[1]]
  val_adjusted <- output_adjusted[4][[1]]
  
  if(as.double(val_adjusted) < as.double(val_original))
  {
    writeLines("Consider Adjusted Data")
    return (list(output_adjusted[1],output_adjusted[3][[1]],output_adjusted[4][[1]]))
  }
  else
  {
    writeLines("Consider Original Data")
    return (list(output_original[1],output_original[3][[1]],output_original[4][[1]]))
  }
}
