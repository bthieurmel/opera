#' Create a complete ggplot for opera's objects
#'
#'
#' @param object an object, whose class will determine the behaviour of ggOpera
#' @param ... other arguments passed to specific methods
#' @return a ggplot object
#' @export
#' 
ggopera <- function(object, ...) {
  UseMethod("ggopera", object)
}



#' @title Plot an object of class mixture with ggplot2
#' 
#' @description provides different diagnostic plots for an aggregation procedure.
#'
#' @param x an object of class mixture. If awake is provided (i.e., some experts are unactive), 
#' their residuals and cumulative losses are computed by using the predictions of the mixture.
#' @param out \code{"mixture"} to plot mixture results or \code{"prevision"} to plot mixtures's prevesion.
#' @param type if \code{out = "mixture"}, which graphic to plot, a value between 1 and 6.
#' @param prevision.experts if \code{out = "prevision"}, plot prevision from the experts ?
#' @param dates a vector of dates to represent the x axis labels, 
#' @param col the color to use to represent each experts, if set to NULL (default) use R\code{RColorBrewer::brewer.pal(...,"Spectral"}
#'
#' @return A ggplot object
#' @export
#'
ggopera.mixture <- function(x, out = "mixture", type = 1, prevision.experts = FALSE, dates = NULL, col = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) 
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  
  out <- match.arg(arg = out, choices = c("mixture", "prevision"))
  
  x$experts <- data.frame(x$experts)
  K <- length(x$experts)
  w.order <- order(apply(x$weights,2,mean),decreasing = TRUE)
  
  if (is.null(col)) col <- rev(RColorBrewer::brewer.pal(n = max(min(K, 11), 4), name = "Spectral"))[1:min(K, 11)]
  my.colors <- col
  
  col <- numeric(K)
  if (K <= length(my.colors)) {
    col[w.order] <- my.colors[1:K]
  } else {
    col[w.order] <- c(my.colors, rep(my.colors[length(my.colors)],K-length(my.colors)))
  }
  
  x$experts <- data.frame(x$experts)
  x$Y <- c(t(x$Y))
  x$prediction <- c(t(x$prediction))
  x$weights <- data.frame(x$weights)
  T <- x$T
  d <- x$d
  
  if (!is.null(x$names.experts)) {
    names(x$weights) <- names(x$experts) <- x$names.experts
  } else {
    if (is.null(names(x$experts))) {
      names(x$weights) <- names(x$experts) <- x$names.experts <- paste("X", 1:K,sep="")
    }
  }
  l.names <- max(nchar(names(x$experts))) / 3 + 1.7
  names.experts <- names(x$experts)
  
  colors <- c("black", col)
  names(colors) <- c(x$model, names.experts)
  
  if (out == "mixture") {
    if (type == 1) { # area chart
      
      weights <- x$weights
      o <- order(colSums(weights), decreasing = FALSE)
      weights <- weights[, o]
      weights.long <- reshape.opera(x$weights, names.experts)
      weights.long$experts <- factor(weights.long$experts, levels = names(weights), ordered = TRUE)
      if (!is.null(dates) && length(dates) == nrow(x$weights)) {
        weights.long$time <- dates
      }
      p <- ggplot(data = weights.long)
      if (x$model == "Ridge") {
        p <- p + geom_line(mapping = aes(x = time, y = value, color = experts, linetype = experts), size = 1.2)
        p <- p + scale_color_manual(name = "Experts", values = colors) + guides(linetype = "none")
      } else {
        p <- p + geom_area(mapping = aes(x = time, y = value, fill = experts))
        p <- p + scale_fill_manual(name = "Experts", values = colors)
      }
      
      p <- p + labs(title = "Weights associated with the experts", y = "Weights", x = NULL)
      
      
    } else if (type == 2) { # boxplots
      
      if (!is.null(x$awake)) {
        pond <- apply(x$awake[d*(1:T),],1,sum)
        normalized.weights <- x$weights * pond / (K*x$awake[d*(1:T),])
        normalized.weights[x$awake[d*(1:T),] == pond] <- NaN
      } else {
        normalized.weights <- x$weights 
      }
      
      i.order <- w.order[1:min(K, 20)]
      normalized.weights <- normalized.weights[, i.order]
      normalized.weights.long <- reshape.opera(normalized.weights, names.experts)
      normalized.weights.long$experts <- factor(normalized.weights.long$experts, levels = names(normalized.weights), ordered = TRUE)
      
      p <- ggplot(data = normalized.weights.long)
      p <- p + geom_boxplot(mapping = aes(x = experts, y = value, fill = experts))
      p <- p + labs(title = "Weights associated with the experts", y = "Weights", x = NULL)
      p <- p + scale_fill_manual(name = "Experts", values = colors)
      p <- p + guides(fill = guide_legend(reverse = TRUE))
      
      colx <- colors[names.experts]
      colx <- colx[i.order]
      
    } else if (type == 3) {# Cumulative loss
      
      pred.experts <- (x$experts * x$awake + x$prediction * (1-x$awake))
      
      cumul.losses <- apply(loss(pred.experts, x$Y, x$loss.type), 2, cumsum)[seq(d,T*d,by=d),]
      cumul.exploss <- cumsum(loss(x$prediction, x$Y, x$loss.type))[seq(d,T*d,by=d)]
      
      cumul.losses <- as.data.frame(cumul.losses)
      cumul.losses.long <- reshape.opera(cumul.losses, names.experts)
      cumul.exploss.df <- data.frame(cumul.exploss)
      names(cumul.exploss.df) <- "value"
      cumul.exploss.df$time <- seq_len(nrow(cumul.exploss.df))
      cumul.exploss.df$experts <- x$model
      
      # lines size
      cumul.losses.long$size <- 1
      cumul.exploss.df$size <- 1.2
      cumul.df <- rbind(cumul.losses.long, cumul.exploss.df)
      
      if (!is.null(dates) && length(dates) == nrow(x$weights)) {
        cumul.df$time <- dates
      }
      
      p <- ggplot(data = cumul.df)
      p <- p + geom_line(mapping = aes(x = time, y = value, color = experts, size = size))
      p <- p + labs(title = "Cumulative square loss", y = "Cumulative loss", x = NULL)
      p <- p + scale_color_manual(name = "Experts", values = colors)
      p <- p + scale_size_identity()
      
    } else if (type == 4) { # Cumulative residuals
      
      pred.experts <- (x$experts * x$awake + x$prediction * (1-x$awake))
      
      cumul.residuals <- apply(x$Y - pred.experts, 2, cumsum)[seq(d,T*d,by=d),]
      cumul.expres <- cumsum(x$Y - x$prediction)[seq(d,T*d,by=d)]
      
      cumul.residuals <- as.data.frame(cumul.residuals)
      cumul.residuals.long <- reshape.opera(cumul.residuals, names.experts)
      cumul.expres.df <- data.frame(cumul.expres)
      names(cumul.expres.df) <- "value"
      cumul.expres.df$time <- seq_len(nrow(cumul.expres.df))
      cumul.expres.df$experts <- x$model
      
      # lines size
      cumul.residuals.long$size <- 1
      cumul.expres.df$size <- 1.2
      cumul.residuals.df <- rbind(cumul.residuals.long, cumul.expres.df)
      
      if (!is.null(dates) && length(dates) == nrow(x$weights)) {
        cumul.residuals.df$time <- dates
      }
      
      p <- ggplot(data = cumul.residuals.df)
      p <- p + geom_line(mapping = aes(x = time, y = value, color = experts, size = size))
      p <- p + labs(title = "Cumulative residuals", y = "Cumulative residuals", x = NULL)
      p <- p + scale_color_manual(name = "Experts", values = colors)
      p <- p + scale_size_identity()
      
    } else if (type == 5) { # losses
      
      pred.experts <- (x$experts * x$awake + x$prediction * (1-x$awake))
      
      x$loss.experts <- apply(loss(x = pred.experts, y = x$Y, loss.type = x$loss.type), 2, mean)
      err.unif <- lossConv(rep(1/K, K), x$Y, x$experts, awake = x$awake, loss.type = x$loss.type)
      err.mixt <- x$loss
      
      losses <- data.frame(
        value = c(x$loss.experts, err.unif, err.mixt),
        experts = c(names(x$experts), "Uniform", x$model),
        cols = c(col, "#000000", "#000000"),
        shape = c(rep(16, K), 8, 8),
        stringsAsFactors = FALSE
      )
      losses <- losses[!duplicated(losses$experts, fromLast = TRUE), ]
      losses <- losses[order(losses$value, decreasing = FALSE), ]
      losses$index <- seq_along(losses$value)
      
      
      p <- ggplot(data = losses)
      p <- p + geom_path(mapping = aes(x = index, y = value))
      p <- p + geom_point(mapping = aes(x = index, y = value, shape = shape, color = cols), size = 4)
      p <- p + labs(title = "Average loss suffered by the experts", y = "Square loss", x = NULL)
      p <- p + scale_shape_identity()
      p <- p + scale_color_identity()
      p <- p + scale_x_continuous(labels = losses$experts)
      
      colx <- losses$cols
      
    } else if (type == 6) { # cumulative plot of the series
      
      if (x$d ==1) {
        p <- ggCumulative(W = x$weights, X = x$experts, Y = x$Y, smooth = TRUE, alpha = 0.01, plot.Y = TRUE, dates = dates)
      } else {
        X <- apply(seriesToBlock(X = x$experts,d = x$d),c(1,3),mean)
        Y <- apply(seriesToBlock(x$Y,d = x$d),1,mean)
        colnames(X) <- x$names.experts
        cumulativePlot(W = x$weights,X = X, Y = Y,smooth = TRUE,alpha = 0.01,plot.Y = TRUE, col.pal = col)    
        p <- ggCumulative(W = x$weights, X = X, Y = Y, smooth = TRUE, alpha = 0.01, plot.Y = TRUE, dates = dates)
      }
      
      p <- p + scale_fill_manual(name = "Experts", values = colors)
      p <- p + guides(fill = guide_legend(reverse = TRUE))
      
    }
  } else if (out == "prevision") {
    
    prev.experts <- reshape.opera(data = as.data.frame(x$experts), experts = names.experts)
    
    if (!is.null(dates) && length(dates) == nrow(x$weights)) {
      prev.experts$time <- dates
    }
    
    prev.mixture <- data.frame(value = x$prediction)
    prev.mixture$experts <- x$model
    if (!is.null(dates) && length(dates) == nrow(x$weights)) {
      prev.mixture$time <- dates
    } else {
      prev.mixture$time <- seq_len(nrow(prev.mixture))
    }
    
    obs <- data.frame(value = x$Y)
    obs$experts <- "Y"
    if (!is.null(dates) && length(dates) == nrow(x$weights)) {
      obs$time <- dates
    } else {
      obs$time <- seq_len(nrow(obs))
    }
    
    prev.opera <- rbind(prev.experts, prev.mixture, obs)
    
    name.model <- x$model
    
    p <- ggplot(data = prev.opera)
    p <- p + geom_line(data = function(x) x[x$experts == "Y", ], 
                       mapping = aes(x = time, y = value, alpha = experts), color = "firebrick")
    p <- p + geom_line(data = function(x) x[x$experts == name.model, ], 
                       mapping = aes(x = time, y = value, linetype = experts), color = "black")
    if (prevision.experts) {
      p <- p + geom_line(data = function(x) x[! x$experts %in% c("Y", name.model), ],
                         mapping = aes(x = time, y = value, color = experts))
    }
    p <- p + scale_alpha_manual(name = "Observed", values = 1,
                                guide = guide_legend(override.aes = list(colour = "firebrick", size = 1.05)), labels = "Y")
    p <- p + scale_linetype_manual(name = x$model, values = 1,
                                   guide = guide_legend(override.aes = list(colour = "black", size = 1.05)), labels = x$model)
    if (prevision.experts) {
      p <- p + scale_color_manual(name = "Experts", values = colors)
    }
    
    p <- p + labs(title = "Previsions", x = NULL, y = NULL)
    
  }
  
  
  if (out == "mixture" & type %in% c(2, 5)) {
    p <- p + theme_minimal() %+replace% theme(axis.text.x = element_text(colour = colx))
  } else {
    p <- p + theme_minimal()
  }
  return(p)
}



reshape.opera <- function(data, experts) {
  res <- reshape(
    data = data,
    varying = list(experts),
    direction = "long", 
    v.names = "value", 
    times = experts, 
    timevar = "experts"
  )
  names(res)[names(res) == "id"] <- "time"
  rownames(res) <- NULL
  return(res)
}


ggCumulative <- function(W, X, Y, smooth = FALSE, plot.Y = FALSE, alpha = 0.1, dates = NULL) {
  time <- c(1:nrow(X))
  active.experts <- which(colMeans(W) > 0)
  W <- W[, active.experts]  
  X <- X[, active.experts]
  
  K <- ncol(X)
  
  o <- order(colSums(W), decreasing = FALSE)
  mat <- W[, o] * X[, o]
  colnames(mat) <- colnames(X)[o]
  
  mat.long <- reshape.opera(mat, names(mat))
  mat.long$experts <- factor(mat.long$experts, levels = rev(names(mat)), ordered = TRUE)
  
  if (!is.null(dates)) {
    mat.long$time <- dates
  }
  
  Y.df <- data.frame(time = time)
  if (smooth) {
    Y.df$value <- lowess(x = time, y = Y, f = alpha)$y
  } else {
    Y.df$value <- Y
  }
  
  if (!is.null(dates)) {
    Y.df$time <- dates
  }
  
  p <- ggplot(data = mat.long)
  p <- p + geom_area(mapping = aes(x = time, y = value, fill = experts))
  if (plot.Y) {
    p <- p + geom_line(data = Y.df, mapping = aes(x = time, y = value), linetype = "longdash", size = 1.05)
  }
  p <- p + labs(title = "Contribution of each expert to prediction", y = NULL, x = NULL)
  p
}







#' @title Plot an aggregation procedure
#'
#' @describeIn oracle \code{plot}. It has one optional arguments. 
#' @param x An object of class \code{oracle}. 
#' @param sort if set to TRUE (default), it sorts the experts by performance before the plots.
#' @param col colors
#'
#' @return  A ggplot object
#' @export
#'
ggopera.oracle <- function(x, sort = TRUE, col = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) 
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", call. = FALSE)
  
  if (x$d > 1) {
    x$experts <- blockToSeries(x$experts)
    x$Y <- blockToSeries(x$Y)
  }
  x$experts <- data.frame(x$experts)
  
  T <- nrow(x$experts)
  K <- ncol(x$experts)
  
  # col.palette <- RColorBrewer::brewer.pal(n = 3, name = "Set1")
  # if (is.null(col)) {
  #   col <- col.palette
  # }
  if (is.null(col)) 
    col <- rev(RColorBrewer::brewer.pal(n = max(min(K, 11), 4),name = "Spectral"))[1:min(K, 11)]
  my.colors <- col
  
  col <- numeric(K)
  if (K <= length(my.colors)) {
    col <- my.colors[1:K]
  } else {
    col <- c(my.colors, rep(my.colors[length(my.colors)], K-length(my.colors)))
  }
  
  
  if (!is.null(x$names.experts)) {
    names(x$experts) <- x$names.experts
  } else {
    if (is.null(names(x$experts))) {
      names(x$experts) <- x$names.experts <- paste("X", 1:K,sep="")
    }
  }
  
  
  if (x$model == "expert") {
    err.unif <- lossConv(rep(1/K, K), x$Y, x$experts, awake = x$awake, loss.type = x$loss.type)
    if (sort) {
      idx.sorted <- order(c(x$loss.experts, err.unif))
      i.min <- 1
    } else {
      idx.sorted = c(K+1,1:K)
      i.min <- order(x$loss.experts)[1]
    }
    # my.col <- rep("#000000", K + 1)
    # my.col[which(idx.sorted == K + 1)] <- col[1]
    # my.col[which(idx.sorted != K + 1)[i.min]] <- col[2]
    
    
    df <- data.frame(
      value = c(x$loss.experts, err.unif)[idx.sorted],
      experts = c(names(x$experts), "Uniform")[idx.sorted],
      cols = c(col, "#000000")[idx.sorted],#my.col,
      stringsAsFactors = FALSE
    )
    df$index <- seq_along(df$value)
    
    main <- "Average loss suffered by the experts"
    ylab <- paste(x$loss.type$name, "loss")
    xlab <- NULL
    shape <- 3
    size <- 5
  }
  
  if (x$model == "convex" || x$model == "linear") {
    err.unif <- lossConv(rep(1/K, K), x$Y, x$experts, awake = x$awake, loss.type = x$loss.type)
    idx.sorted <- order(c(x$loss.experts, err.unif, x$loss))
    # my.col <- rep("#000000", K + 2)
    # my.col[which(idx.sorted == K + 1)] <- col[1]
    # my.col[which(idx.sorted == K + 2)] <- col[2]
    # my.col[which(!(idx.sorted %in% c(K + 1, K + 2)))[1]] <- col[3]
    
    df <- data.frame(
      value = c(x$loss.experts, err.unif, x$loss)[idx.sorted],
      experts = c(names(x$experts), "Uniform", x$model)[idx.sorted],
      cols = c(col, "#000000", "#000000")[idx.sorted], #my.col,
      stringsAsFactors = FALSE
    )
    df$index <- seq_along(df$value)
    
    
    main <- "Average loss suffered by the experts"
    ylab <- paste(x$loss.type$name, "loss")
    xlab <- NULL
    shape <- 3
    size <- 5
  }
  
  if (x$model == "shifting") {
    L <- x$loss
    if (x$loss.type == "square") {
      L <- sqrt(x$loss)
      y.lab <- "rmse"
    } else if (x$loss.type == "absolute") {
      y.lab <- "mean absolute error"
    } else if (x$loss.type == "percentage") {
      y.lab <- "mape"
    }
    
    df <- data.frame(
      value = L,
      experts = 0:(length(L) - 1),
      cols = rep("#000000", length(L)),
      stringsAsFactors = FALSE
    )
    df$index <- seq_along(df$value)
    
    main <- "Error suffered by the shifting oracle"
    ylab <- y.lab
    xlab <- "Number of shifts"
    shape <- 1
    size <- 1
  }
  
  
  
  p <- ggplot(data = df)
  p <- p + labs(title = main, y = ylab, x = xlab)
  p <- p + scale_color_identity()
  if (x$model == "shifting") {
    p <- p + geom_line(mapping = aes(x = index, y = value))
  } else {
    p <- p + scale_x_continuous(labels = df$experts)
  }
  p <- p + geom_point(mapping = aes(x = index, y = value, color = cols), shape = shape, size = size)
  # if (x$model != "shifting") {
  #   p <- p + theme(axis.text.x = element_text(colour = df$cols))
  # }
  p <- p + theme_minimal() %+replace% theme(axis.text.x = element_text(colour = df$cols))
  p
}




