# Functions Script
  # Joseph V. Hernandez
  # 5/3/19
# ------------------------------
# Private Check Functions
# ------------------------------

# 1. check_trials()
# Name: Check Trials
# Desc: checks if a given numebr of trials is valid (non-negative, whole number)
# Parameter: trials dev(a number of trials)
# Returns: True / Error
# ----------
check_trials <- function(trials){
  if(trials%%1 !=0 | trials < 0){
    stop("Error: Invalid trials, must be a non-negative, non-decimal integer")
  } else if (!is.numeric(trials)) {
    stop("error")
  } else {
    TRUE
  }
}

# 2. check_success()
# Name: Check Success
# Desc: checks if a given numebr of successes is valid (non-negative, whole, less than trials)
# Parameter: trials (a number of trials), success (a numebr of successes)
# Returns: True / Error
# ----------
check_success <- function(trials, success){
  if ((check_trials(trials)==FALSE) | trials < success | success %%1!=0 | success < 0) {
    stop("Error: Invalid successes, must be a non-negative, non-decimal integer")
    } else {TRUE}
}

# 3. check_prob()
# Name: Check Probability
# Desc: checks if a given probability is between 0 and 1
# Parameter: prob (a numeric probability between 0 and 1 in decimal form)
# Returns: True / Invalid
# ----------
check_prob <- function(prob){
  if(prob>=0&prob<=1){
    return(TRUE)
  } else {
    stop("Error: Invalid probability, p must be between 1 and 0")
  }
}

# ------------------------------
# Private Auxiliary Functions
# ------------------------------

# 1. aux_mean()
# Name: Auxiliary Average (Mean)
# Desc: gives the probable mean number of successes in a given number of trials
# Parameter: trials (a number of trials), prob (probability of successes)
# Returns: mean
# ----------
aux_mean <- function(trials, prob){
  mean <- trials*prob
  return(mean)
}

# 2. aux_variance()
# Name: Auxiliary Variance
# Desc: gives the probable variance of successes in a given number of trials
# Parameter: trials (a number of trials), prob (probability of successes)
# Returns: mean
# ----------
aux_variance <- function(trials, prob){
  var <- trials*prob*(1-prob)
  return(var)
}

# 3. aux_mode()
# Name: Auxiliary Mode
# Desc: gives the probable mode of successes in a given number of trials
# Parameter: trials (a number of trials), prob (probability of successes)
# Returns: mode
# ----------
aux_mode <- function(trials, prob){
  mode <- round((trials*prob)+prob)
  return(mode)
}

# 4. aux_skewness()
# Name: Auxiliary Skewness
# Desc: gives the probable skewness (asymmetry) of results in a given number of trials
# Parameter: trials (a number of trials), prob (probability of successes)
# Returns: skew
# ----------
aux_skewness <- function(trials, prob){
  numer <- (1-2*prob)
  denom <- sqrt((trials*prob)*(1-prob))
  skew <- numer/denom
  return(skew)
}

# 5. aux_kurtosis()
# Name: Auxiliary Kurtosis
# Desc: gives the probable kurtosis (tailedness) of results in a given number of trials
# Parameter: trials (a number of trials), prob (probability of successes)
# Returns: kurt
# ----------
aux_kurtosis <- function(trials, prob){
  numer <- 1-((6*prob)*(1-prob))
  denom <- (trials*prob)*(1-prob)
  kurt <- numer/denom
  return(kurt)
}

# ------------------------------
# Main Functions and Methods
# ------------------------------

#' @title Binomial Combinations
#' @description Calculates number of combinations in which k successes can occur in n trials
#' @param n trials
#' @param k successes
#' @return combo
#' @export
#' @examples
#' bin_choose(5, 2)

bin_choose <- function(n,k) {
  if(check_trials(trials=n)==TRUE&check_success(trials=n,success=k)==TRUE) {
    numer <- factorial(n)
    denom <- (factorial(k))*(factorial((n-k)))
    combo <- numer/denom
    return(combo)
  }
}

#' @title Binomial Probability
#' @description Calculates probability of given number of successes in given number of trials
#' @param trials trials
#' @param success successes
#' @param prob probability
#' @return binprob
#' @export
#' @examples
#' bin_probability(trials=5, success=2, prob=0.5)

bin_probability <- function(success,trials,prob) {
    if(check_success(trials,success)==TRUE){
      if(check_prob(prob)==TRUE){
        combo <- bin_choose(n=trials, k=success)
        binprob <- combo*(prob^success)*((1-prob)^(trials-success))
        return(binprob)
      }
    }
  }

#' @title Binomial Distribution
#' @description Calculates a distribution given a probability of success and given number of trials
#' @param trials trials
#' @param prob probability
#' @return bindis
#' @export
#' @examples
#' bin_distribution(5, 0.5)

bin_distribution <- function(trials, prob) {
  success <- 0:trials
  probability <- rep(0, (trials + 1))
  for (i in 0:trials) {
    probability[i+1] <- bin_probability(success = success[i+1], trials, prob)
    }
  bindis <- data.frame(success, probability)
  class(bindis)=c("bindis","data.frame")
  return(bindis)
}

#' @export
#' @examples
#' df <- bin_distribution(5, 0.5)
#' plot.bincum(df)

plot.bindis <- function(bindis){
  ggplot(data = bindis, aes(x=success,y=probability)) +
    geom_bar(stat='identity') +
    xlab("success") +
    ylab("probability")+
    theme_classic()
}

#' @title Binomial Cumulative
#' @description Calculates a distribution given a probability of success and given number of trials
#' @param trials trials
#' @param prob probability
#' @return bincum
#' @export
#' @examples
#' bin_cumulative(5, 0.5)

bin_cumulative <- function(trials,prob) {
  success <- 0:trials
  probability <- rep(0, (trials + 1))
  cumulative <- rep(0, (trials + 1))
  for (i in 0:trials) {
    probability[i+1] <- bin_probability(success = success[i+1], trials, prob)
  }
  for (i in 1:(trials + 1)) {
    cumulative[i] <- sum(probability[0:i])
    }
  bincum <- data.frame(success, probability, cumulative)
  class(bincum)=c("bincum","data.frame")
  return(bincum)
}

#' @export
#' @examples
#' df <- bin_cumulative(5, 0.5)
#' plot.bincum(df)

plot.bincum <- function(bincum){
  ggplot(data = bincum, aes(x=success,y=cumulative)) +
    geom_line(stat='identity') +
    geom_point(size = 3, shape = 21) +
    xlab("Successes") +
    ylab("Probability") +
    theme_classic()
}

#' @title Binomial Variable
#' @description Generates a binomial random variable with given trials and probability of success
#' @param trials trials
#' @param prob probability
#' @return output
#' @export
#' @examples
#' bin_variable(5, 0.5)

bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  output <- list(trials = trials, prob = prob)
  class(output) = "binvar"
  return(output)
}
#' @export
#' @examples
#' var <- bin_variable(5, 0.5)
#' plot.binvar(var)

print.binvar <- function(output){
  cat("\"Binomial variable\"")
  cat("\n")
  cat("\n")
  cat("Parameters")
  cat("\n")
  cat("- number of traials:", output$trials)
  cat("\n")
  cat("- prob of success:", output$prob)
}
#' @export
#' @examples
#' sum_var <- bin_variable(5, 0.5)
#' summary.binvar(sum_var)

summary.binvar <- function(output){
  summ <- list(trials = output$trials,
            prob = output$prob,
            mean = aux_mean(output$trials, output$prob),
            variance = aux_variance(output$trials, output$prob),
            mode = aux_mode(output$trials, output$prob),
            skewness = aux_skewness(output$trials, output$prob),
            kurtosis = aux_kurtosis(output$trials, output$prob))
  class(summ) <- "summary.binvar"
  return(summ)
}
#' @export
#' @examples
#' sum_var <- bin_variable(5, 0.5)
#' print.summary.binvar(sum_var)

print.summary.binvar <- function(summ){
  cat("\"Summary Binomial\"")
  cat("\n")
  cat("\n")
  cat("Parameters")
  cat("\n")
  cat("- Number of trials:", summ$trials)
  cat("\n")
  cat("- Probability of success:", summ$prob)
  cat("\n")
  cat("\n")
  cat("Measures")
  cat("\n")
  cat("-mean:", summ$mean)
  cat("\n")
  cat("-variance:", summ$variance)
  cat("\n")
  cat("-mode:", summ$mode)
  cat("\n")
  cat("-skewness:", summ$skewness)
  cat("\n")
  cat("-kurtosis:", summ$kurtosis)
}

#' @title Binomial Mean
#' @description Calculates mean of a binomial distribution of a given probability in n trials
#' @param trials trials
#' @param prob probability
#' @return binmean
#' @export
#' @examples
#' bin_mean(5, 0.5)

bin_mean <- function(trials,prob) {
  if(check_trials(trials)==TRUE) {
    if(check_prob(prob)==TRUE){
      binmean <- aux_mean(trials=trials, prob = prob)
      return(binmean)
    }
  }
}

#' @title Binomial Variance
#' @description Calculates varaince of a binomial distribution of a given probability in n trials
#' @param trials trials
#' @param prob probability
#' @return binvariance
#' @export
#' @examples
#' bin_variance(5, 0.5)

bin_variance <- function(trials,prob) {
  if(check_trials(trials)==TRUE) {
    if(check_prob(prob)==TRUE){
      binvariance <- aux_variance(trials=trials, prob = prob)
      return(binvariance)
    }
  }
}

#' @title Binomial Mode
#' @description Calculates mode of a binomial distribution of a given probability in n trials
#' @param trials trials
#' @param prob probability
#' @return binmode
#' @export
#' @examples
#' bin_mode(5, 0.5)

bin_mode <- function(trials,prob) {
  if(check_trials(trials)==TRUE) {
    if(check_prob(prob)==TRUE){
      binmode <- aux_mode(trials=trials, prob = prob)
      return(binmode)
    }
  }
}

#' @title Binomial Skewness
#' @description Calculates skewness of a binomial distribution of a given probability in n trials
#' @param trials trials
#' @param prob probability
#' @return binskew
#' @export
#' @examples
#' bin_skewness(5, 0.5)

bin_skewness <- function(trials,prob) {
  if(check_trials(trials)==TRUE) {
    if(check_prob(prob)==TRUE){
      binskew <- aux_skewness(trials=trials, prob = prob)
      return(binskew)
    }
  }
}

#' @title Binomial Kurtosis
#' @description Calculates kurtosis of a binomial distribution of a given probability in n trials
#' @param trials trials
#' @param prob probability
#' @return binkurt
#' @export
#' @examples
#' bin_mean(5, 0.5)

bin_kurtosis <- function(trials,prob) {
  if(check_trials(trials)==TRUE) {
    if(check_prob(prob)==TRUE){
      binkurt <- aux_kurtosis(trials=trials, prob = prob)
      return(binkurt)
    }
  }
}

