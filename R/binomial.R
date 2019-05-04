#1.1

#Title - Checks probability
#Description checks if probability is valid
#param prob = the probability
#return - True if valid or error if not
check_prob = function(prob){
  if(prob>0 && prob <1){
    return(TRUE)
  }else{
    stop("Prob is inavlid")
  }
}

#Title - Checks trials
#Description checks if trials is valid
#param trials = the trials
#return - True if valid or error if not
check_trials = function(trials){
  
  if(trials>=0){
    return(TRUE)
  }else{
    stop("invalid trials number")
  }
  
}
#Title - Checks success
#Description checks if success is valid
#param trials = the trials
#param success = vector
#return - True if valid or error if not
check_success = function(success, trials){
  if(sum(success >= 0)<length(success)){
    stop("Invalid")
  }
  if(length(success)>trials){
    stop("Invalid")
  }
  return(TRUE)
}

#1.2
aux_mean = function(trials, prob){
  return(trials*prob)
}

aux_variance = function(trials,prob){
  re = trials*prob*(1-prob)
  return(re)
}

aux_mode = function(trials,prob){
  return(as.integer((trials*prob)+prob))
}

aux_skewness = function(trials,prob){
  return((1-(2*prob))/sqrt((trials*prob)*(1-prob)))
}

aux_kurtosis = function(trials,prob){
  return((1-6*prob*(1-prob))/(trials*prob*(1-prob)))
}


#1.3
#' @title bin_choose
#' @description Creates an integer or a vector of integers
#' @param success number of successes
#' @param trials number of trials
#' @param prob probability between 0-1
#' @return an integer or a vector of integers
#' @export
#' @examples
#' # standard example
#' bin1 <- bin_choose(5,2)
#' 
#' # Another standard example
#' bin1 <- bin_choose(5,1:3)
bin_choose = function(n,k){
  if(k>n){
    stop("Invalid")
  }
  return(factorial(n)/(factorial(k)*factorial(n-k)))
  
}

#1.4 
#' @title bin_probability
#' @description Creates an integer or a vector of integers
#' @param success number of successes
#' @param trials number of trials
#' @param prob probability between 0-1
#' @return an integer or a vector of integers
#' @export
#' @examples
#' # standard example
#' bin1 <- bin_probability(2,5,0.5)
#' 
#' # Another standard example
#' bin1 <- bin_probability(0:2,5,0.5)
bin_probability = function(success,trials,prob){
  if(check_trials(trials) != TRUE |   check_success(success,trials) != TRUE | check_prob(prob)!=TRUE){
    stop("Something is invalid")
  }
  return(bin_choose(trials,success)*(prob^success)*((1-prob)^(trials-success))) 
  
}

#1.5
#' @title bin_distribution
#' @description Creates an object of class bindis
#' @param trials number of trials
#' @param prob probability between 0-1
#' @return an object of class bindis
#' @export
#' @examples
#' # standard example
#' bin1 <- bin_distribution(5,0.5)
bin_distribution = function(trials,prob){
  re = data.frame(1,1)
  names(re) = c("success","probability")
  for(success in 0:trials){
    re[success+1,"success"] = success
    re[success+1,"probability"]=bin_probability(success,trials,prob)
  }
  class(re) = c("bindis","data.frame") 
  return(re)
}

#' @export
plot.bindis = function(x){
  plot(x$probability,type="b")
}

#1.6
#' @title bin_cumulitive
#' @description Creates an object of class bincum
#' @param trials number of trials
#' @param prob probability between 0-1
#' @return an object of class bincum
#' @export
#' @examples
#' # standard example
#' bin1 <- bin_cumulative(5,0.5)
bin_cumulitive = function(trials,prob){
  re = data.frame(1,1,1)
  names(re) = c("success","probability","cumulitive")
  cumulitive = 0; 
  for(success in 0:trials){
    re[success+1,"success"] = success
    re[success+1,"probability"]=bin_probability(success,trials,prob)
    cumulitive = cumulitive +re[success+1,"probability"]
    re[success+1,"cumulitive"] = cumulitive
  }
  class(re) = c("bincum","data.frame") 
  return(re)
  
}

#' @export
plot.bincum = function(x){
  plot(x$probability,type="b")
}

#1.7 
#' @title bin_variable
#' @description Creates an object of class binvar
#' @param trials number of trials
#' @param prob probability between 0-1
#' @return an object of class binvar
#' @export
#' @examples
#' # standard example
#' bin1 <- bin_variable(10,0.3)
bin_variable = function(trials,prob){
  if(check_trials(trials) != TRUE | check_prob(prob)!=TRUE){
    stop("Something is invalid")
  }
  re = list(trials,prob)
  names(re) = c("trials","prob")
  class(re) = "binvar"
  return(re)
}

#' @export
print.binvar = function(x){
  cat("Bionomial Variable \n Parameters \n  - number of trials: ",x$trials,"\n  - prob of success: ",x$prob)
}

#' @export
summary.binvar = function(x){
  #return(cat("Summary Binomial \nParameters\n- number of trials: ",x$trials,"\n- prob of success: ",x$list,"\nMeasures\n- mean: ",aux_mean(x$trials,x$prob),"\n- variance: ",aux_variance(x$trials,x$prob),"\n- mode: ",aux_mode(x$trials,x$prob),"\n- skewness: ",aux_skewness(x$trials,x$prob),"\n- kurtosis: ",aux_kurtosis(x$trials,x$prob)))
  freqs <- data.frame(
    trials = x$trials,
    prob = x$prob,
    mean = aux_mean(x$trials,x$prob),
    variance = aux_variance(x$trials,x$prob),
    mode = aux_mode(x$trials,x$prob),
    skewness = aux_skewness(x$trials,x$prob),
    kurtosis = aux_kurtosis(x$trials,x$prob))
  obj <- list(freqs = freqs)
  class(obj) <- "summary.binvar"
  obj
  
}

#' @export
print.summary.binvar <- function(x) {
  cat("Summary Binomial \nParameters\n- number of trials: ",x$freqs$trials,"\n- prob of success: ",x$freqs$prob,"\nMeasures\n- mean: ",aux_mean(x$freqs$trials,x$freqs$prob),"\n- variance: ",aux_variance(x$freqs$trials,x$freqs$prob),"\n- mode: ",aux_mode(x$freqs$trials,x$freqs$prob),"\n- skewness: ",aux_skewness(x$freqs$trials,x$freqs$prob),"\n- kurtosis: ",aux_kurtosis(x$freqs$trials,x$freqs$prob))
  invisible(x)
}
#' @export
bin_mean = function(trials,prob){
  if(check_trials(trials) != TRUE | check_prob(prob)!=TRUE){
    stop("Something is invalid")
  }
  return(aux_mean(trials,prob))
}
#' @export
bin_variance = function(trials,prob){
  if(check_trials(trials) != TRUE | check_prob(prob)!=TRUE){
    stop("Something is invalid")
  }
  return(aux_variance(trials,prob))
}
#' @export
bin_mode = function(trials,prob){
  if(check_trials(trials) != TRUE | check_prob(prob)!=TRUE){
    stop("Something is invalid")
  }
  return(aux_mode(trials,prob))
}
#' @export
bin_skewness = function(trials,prob){
  if(check_trials(trials) != TRUE | check_prob(prob)!=TRUE){
    stop("Something is invalid")
  }
  return(aux_skewness(trials,prob))
}
#' @export
bin_kurtosis = function(trials,prob){
  if(check_trials(trials) != TRUE | check_prob(prob)!=TRUE){
    stop("Something is invalid")
  }
  return(aux_kurtosis(trials,prob))
}
