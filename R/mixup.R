
check_params <- function(x1, y1, alpha, concat, batch_size) {

  is_data_vec_df_mat(x1, 'x1')
  is_data_vec_df_mat(y1, 'y1')

  x1.len <- get_data_length(x1)
  y1.len <- get_data_length(y1)

  if ( x1.len != y1.len ) {
    errmsg <- paste0("'x1' and 'y1' must have compatible lengths:\n",
                     "  'x1' has length: ", x1.len, "\n",
                     "  'y1' has length: ", y1.len, "\n")
    stop(errmsg, call. = FALSE)
  }

  is_data_all_numeric(x1, 'x1')
  is_data_all_numeric(y1, 'y1')

  is_data_finite(x1, 'x1')
  is_data_finite(y1, 'y1')

  if ( alpha < 0 ) {
    errmsg <- paste0("'alpha' must be greater than or equal to 0.\n",
                     "  'alpha' is ", alpha, "\n")
    stop(errmsg, call. = FALSE)
  }

  if ( !is.null(batch_size) && batch_size <= 0 ) {
    errmsg <- paste0("'batch_size' must be greater than 0.\n",
                     "  'batch_size' is ", batch_size, "\n")
    stop(errmsg, call. = FALSE)
  }

  if ( concat != TRUE & concat != FALSE ) {
    errmsg <- paste0("'concat' must be TRUE or FALSE:\n",
                     "  'concat' is ", concat, "\n")
    stop(errmsg, call. = FALSE)
  }

  return(0)
}


is_data_vec_df_mat <- function(data, name) {
  if ( !is.vector(data) & !is.data.frame(data) & !is.matrix(data) ) {
    errmsg <- paste0("'", name, "' must be vector, data.frame or matrix:\n",
                     "  '", name, "' is ", class(data), "\n")
    stop(errmsg, call. = FALSE)
  }

  return(0)
}


is_data_finite <- function(data, name) {

  sum_nas  <- sum(is.na(data))

  if ( is.vector(data) ) {
    sum_nans <- sum(is.nan(data))
    sum_infs <- sum(is.infinite(data))
  }
  else if ( is.data.frame(data) | is.matrix(data) ) {
    sum_nans <- sum(is.nan(as.matrix(data)))
    sum_infs <- sum(is.infinite(as.matrix(data)))
  }

  #if ( any(!is.finite(data)) ) { # Does not work with data.frames :-(
  #if ( any(is.finite(as.matrix(data))) ) {
  if ( sum_nas != 0 | sum_nans != 0 | sum_infs != 0 ) {
    errmsg <- paste0("Can't have non-finite values in '", name, "':\n")

    if ( sum_nas != 0 ) {
      errmsg <- paste0(errmsg, "  NAs found at ", sum_nas, " locations.\n")
    }
    if ( sum_infs != 0 ) {
      errmsg <- paste0(errmsg, "  Infs found at ", sum_infs, " locations.\n")
    }
    if ( sum_nans != 0 ) {
      errmsg <- paste0(errmsg, "  NaNs found at ", sum_nans, " locations.\n")
    }

    stop(errmsg, call. = FALSE)
  }

  return(0)
}


is_data_all_numeric <- function(data, name) {
  if ( is.vector(data) ) {
    num_or_not <- is.numeric(data)
  } else if ( is.data.frame(data) | is.matrix(data) ) {
    num_or_not <- all(sapply(data, function(x) is.numeric(x)))
  }

  if ( num_or_not == FALSE ) {
    errmsg <- paste0("'", name, "' must be entirely numeric:\n")

    if ( is.vector(data) ) {
      errmsg <- paste0(errmsg, "  '", name, "' is ", class(data), "\n")
    }
    else {
      errmsg <- paste0(errmsg, 
                       "  '", name, "' has ", 
                       sum(sapply(x1, function(x) is.numeric(x)) == FALSE), 
                       " non-numeric columns\n") 
    }

    stop(errmsg, call. = FALSE)
  }

  return(0)
}


get_data_length <- function(data) {
  if ( is.vector(data) ) {
    len <- length(data)
  } else if ( is.data.frame(data) | is.matrix(data) ) {
    len <- nrow(data)
  }

  return(len)
}


#' mixup Function
#'
#' This function enlarges training sets using linear interpolations 
#' of features and associated labels as described in 
#' https://arxiv.org/abs/1710.09412.  
#'
#' The x1 and y1 parameters must be numeric and must have equal 
#' numbers of examples.  Non-finite values are not permitted.
#' Factors should be one-hot encoded.
#' 
#' For now, only binary classification is supported.  Meaning y1 must contain 
#' only numeric 0 and 1 values.
#' 
#' Alpha values must be greater than or equal to zero.  Alpha equal to zero
#' specifies no interpolation.
#' 
#' The mixup function returns a two-element list containing interpolated x 
#' and y values.  Optionally, the original values can be concatenated with the
#' new values.
#'
#' @param x1 Original features
#' @param y1 Original labels
#' @param alpha Hyperparameter specifying strength of interpolation
#' @param concat Concatenate mixup data with original
#' @param batch_size How many mixup values to produce
#' @return A list containing interpolated x and y values and optionally the original values
#' @export
#' @examples
#' # Use builtin mtcars dataset with mtcars$am (automatic/manual) as binary target
#' data(mtcars)
#' mtcars.mix <- mixup(mtcars[, -9], mtcars$am)
mixup <- function(x1, y1, alpha=1, concat=FALSE, batch_size=NULL) {

  check_params(x1, y1, alpha, concat, batch_size)

  x1.len <- get_data_length(x1)
  y1.len <- get_data_length(y1)

  if (is.null(batch_size)) {
    batch_size <- x1.len
  }

  # used to shuffle x1 and y1
  #index <- sample(batch_size, batch_size) # shuffle
  if (x1.len < batch_size) {
    index <- sample(x1.len, batch_size, replace=TRUE)
  } else {
    index <- sample(x1.len, batch_size)
  }
  
  # Make x1 same size as x2 by repeating rows and similarly for y1
  x1.orig <- x1
  y1.orig <- y1
  if (x1.len < batch_size) {
      rep.times <- ceiling(batch_size/x1.len)

      if (is.vector(x1)) {
        x1 <- rep(x1, rep.times)
        x1 <- x1[1:batch_size]
      } else {
        x1 <- x1[rep(seq_len(nrow(x1)), rep.times),]

        # Fixes errors with single dimenion data frames & matrices
        if (is.matrix(x1.orig)) {
            x1 <- as.matrix(x1)
        } else if (is.data.frame(x1.orig)) {
            x1 <- as.data.frame(x1)
        }

	    colnames(x1) <- colnames(x1.orig)
        x1 <- x1[1:batch_size,]
      }
  }

  if (y1.len < batch_size) {
      rep.times <- ceiling(batch_size/y1.len)

      if (is.vector(y1)) {
        y1 <- rep(y1, rep.times)
        y1 <- y1[1:batch_size]
      } else {
        y1 <- y1[rep(seq_len(nrow(y1)), rep.times),]

        # Fixes errors with single dimenion data frames & matrices
        if (is.matrix(y1.orig)) {
            y1 <- as.matrix(y1)
        } else if (is.data.frame(y1.orig)) {
            y1 <- as.data.frame(y1)
        }

	    colnames(y1) <- colnames(y1.orig)
        y1 <- y1[1:batch_size,]
      }
  }

  if (is.vector(x1)) {
    x2 <- x1[index]
  } else if (is.matrix(x1)) {
    x2 <- x1[index,]
	colnames(x2) <- colnames(x1)
  } else if (is.data.frame(x1)) {
    x2 <- as.data.frame(x1[index,])
	colnames(x2) <- colnames(x1)
  } 
  
  if (is.vector(y1)) {
    y2 <- y1[index]
  } else if (is.matrix(y1)) {
    y2 <- y1[index,]
	colnames(y2) <- colnames(y1)
  } else if (is.data.frame(y1)) {
    y2 <- as.data.frame(y1[index,])
	colnames(y2) <- colnames(y1)
  }   
  
  lam <- rbeta(batch_size, alpha, alpha)
  x <- lam * x1 + (1. - lam) * x2
  y <- lam * y1 + (1. - lam) * y2
  
  if (concat==TRUE) {
    if (is.vector(y1.orig)) {
      y <- c(y1.orig, y)
    } else {
      if (is.matrix(y1.orig)) {
        y <- as.matrix(y)
      } else if (is.data.frame(y1.orig)) {
        y <- as.data.frame(y)
      }   

	  colnames(y) <- colnames(y1.orig)
      y <- rbind(y1.orig, y)
    }

    if (is.vector(x1.orig)) {
      x <- c(x1.orig, x)
    } else {
      if (is.matrix(x1.orig)) {
        x <- as.matrix(x)
      } else if (is.data.frame(x1.orig)) {
        x <- as.data.frame(x)
      }   

	  colnames(x) <- colnames(x1.orig)
      x <- rbind(x1.orig, x)
    }
  }

  return(list(x=x, y=y))
}


#' mixup: Create Convex Combinations of Pairs of Examples and their Labels for Data Augmentation
#'
#' An R package inspired by mixup: Beyond Empirical Risk Minimization
#'
#' @section Interpolation:
#' This package enlarges training sets using linear interpolations 
#' of features and associated labels: 
#' 
#' \eqn{x' = \lambda * x_i + (1 - \lambda) * x_j}, where \eqn{x_i, x_j} are raw input vectors
#' 
#' \eqn{y' = \lambda * y_i + (1 - \lambda) * y_j}, where \eqn{y_i, y_j} are one-hot label encodings
#' 
#' \eqn{(x_i, y_i)} and \eqn{(x_j ,y_j)} are two examples drawn at random from the training 
#' data, and \eqn{\lambda \in [0, 1]} with \eqn{\lambda \sim Beta(\alpha, \alpha)} for 
#' \eqn{\alpha \in (0, \infty)}.
#' The mixup hyper-parameter \eqn{\alpha} controls the strength of interpolation between 
#' feature-target pairs.
#'
#' @docType package
#' @name mixup
NULL
