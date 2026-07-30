### gg_plots.R: ggplot-based score plots, loading plots, biplots and autoplot for mvr models

#' @title ggplot-based Score Plot for mvr Models
#'
#' @description
#' Create a score plot from an \code{mvr} model using \pkg{ggplot2}. When
#' \code{select = TRUE}, a local Shiny app is launched with a selectable

#' @noRd
.resolve_gg_point_labels <- function(labels, default_names) {
  if (isTRUE(labels))
    labels <- "names"
  if (identical(labels, FALSE) || is.null(labels))
    return(NULL)
  if (length(labels) == 1) {
    labels <- switch(match.arg(labels, c("names", "numbers")),
                     names = default_names,
                     numbers = as.character(seq_along(default_names)))
  }
  as.character(labels)
}
.data = NULL

#' @noRd
.resolve_gg_axis_labels <- function(labels, default_names, pretty.xlabels = TRUE) {
  # Define the default numerical axis (1, 2, 3...)
  xnum <- seq_along(default_names)

  # Determine what text vector we should draw from based on the 'labels' argument
  if (identical(labels, FALSE) || is.null(labels)) {
    # Default behavior: Use the indices as character labels
    actual_labels <- as.character(xnum)
  } else if (length(labels) == 1) {
    # Match against the specified keywords
    chosen <- match.arg(labels, c("names", "numbers"))

    if (chosen == "names") {
      actual_labels <- default_names
    } else if (chosen == "numbers") {
      parsed <- as.numeric(gsub("[^0-9.]", "", default_names))
      if (any(is.na(parsed))) {
        stop("Could not convert variable names to clean numbers.")
      }
      # Crucial: If we successfully parse clean numbers (e.g., wavelengths),
      # the continuous axis itself should use these actual values as coordinates.
      xnum <- parsed
      actual_labels <- as.character(xnum)
    }
  } else {
    # User supplied a custom vector of labels
    actual_labels <- if (length(labels) == length(default_names)) labels else default_names
  }

  # Generate scale specification
  if (isTRUE(pretty.xlabels)) {
    # 1. Let R calculate a clean set of numeric breaks based on our axis range
    my_breaks <- pretty(xnum)

    # 2. Filter out breaks that fall outside our actual data range
    my_breaks <- my_breaks[my_breaks >= min(xnum) & my_breaks <= max(xnum)]

    # 3. Find the closest actual indices in xnum for these pretty breaks
    idx <- sapply(my_breaks, function(b) which.min(abs(xnum - b)))

    # 4. Extract the corresponding text labels
    my_labels <- actual_labels[idx]

    # 5. Supply BOTH explicit breaks and matching labels to ggplot2
    scale_spec <- ggplot2::scale_x_continuous(breaks = my_breaks, labels = my_labels)

  } else {
    # If pretty is FALSE, map every single variable
    scale_spec <- ggplot2::scale_x_continuous(breaks = xnum, labels = actual_labels)
  }

  list(xnum = xnum, scale_spec = scale_spec)
}

#' \pkg{ggiraph} plot, and the selected sample IDs are returned.
#'
#' @param object model object.
#' @param ... further arguments passed to methods.
#' @return For \code{select = FALSE}, a \code{ggplot} object. For
#' \code{select = TRUE}, a character vector with selected sample IDs.
#' @export
gg_scoreplot <- function(object, ...) UseMethod("gg_scoreplot")


#' @rdname gg_scoreplot
#' @param comps integer vector of length two, giving the score components to
#' plot.
#' @param estimate optional character vector passed to \code{scores()}.
#' @param newdata optional data frame passed to \code{scores()} when
#' \code{estimate = "test"}.
#' @param select logical. If \code{TRUE}, launch an interactive selection app
#' and return selected IDs.
#' @examples
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- gg_scoreplot(mod)
#'   print(p)
#' }
#'
#' \dontrun{ # Requires interactive session
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("ggiraph", quietly = TRUE) &&
#'     requireNamespace("shiny", quietly = TRUE)) {
#'   selected_ids <- gg_scoreplot(mod, select = TRUE)
#'   refit_mod <- refit(mod, selection = selected_ids, exclude = TRUE)
#' }
#' }
#' @export
gg_scoreplot.mvr <- function(object, comps = 1:2, estimate, newdata,
                             select = FALSE, ...) {
  if (length(comps) != 2)
    stop("`comps` must contain exactly two components.")

  score_args <- list(object)
  if (!missing(estimate)) score_args$estimate <- estimate
  if (!missing(newdata)) score_args$newdata <- newdata
  score_mat <- do.call(scores, score_args)

  score_df <- data.frame(
    sample_id = rownames(score_mat),
    x = score_mat[, comps[1]],
    y = score_mat[, comps[2]],
    stringsAsFactors = FALSE
  )
  if (is.null(score_df$sample_id))
    score_df$sample_id <- as.character(seq_len(nrow(score_df)))

  xlab <- compnames(score_mat, comps = comps[1], explvar = TRUE)
  ylab <- compnames(score_mat, comps = comps[2], explvar = TRUE)

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for gg_scoreplot().")
  if (!requireNamespace("rlang", quietly = TRUE))
    stop("Package 'rlang' is required for gg_scoreplot().")

  if (!isTRUE(select)) {
    return(
      ggplot2::ggplot(score_df, ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::geom_point(...) +
        ggplot2::labs(x = xlab, y = ylab)
    )
  }

  if (!requireNamespace("ggiraph", quietly = TRUE))
    stop("Package 'ggiraph' is required when select = TRUE.")
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("Package 'shiny' is required when select = TRUE.")

  interactive_plot <-
    ggplot2::ggplot(
      score_df,
      ggplot2::aes(x = .data$x, y = .data$y, data_id = .data$sample_id, tooltip = .data$sample_id)
    ) +
    ggiraph::geom_point_interactive(...) +
    ggplot2::labs(x = xlab, y = ylab)

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::h4("Select samples and click Done"),
      shiny::actionButton("done", "Done"),
      ggiraph::girafeOutput("score_plot", width = "100%", height = "700px"),
      shiny::p("Closing the app window also returns the current selection.")
    ),
    server = function(input, output, session) {
      selected_ids <- character(0)
      app_done <- FALSE

      output$score_plot <- ggiraph::renderGirafe({
        ggiraph::girafe(
          ggobj = interactive_plot,
          options = list(ggiraph::opts_selection(type = "multiple"))
        )
      })

      shiny::observeEvent(input$score_plot_selected, {
        if (is.null(input$score_plot_selected)) {
          selected_ids <<- character(0)
        } else {
          selected_ids <<- as.character(input$score_plot_selected)
        }
      }, ignoreNULL = FALSE)

      shiny::observeEvent(input$done, {
        app_done <<- TRUE
        shiny::stopApp(selected_ids)
      })

      session$onSessionEnded(function() {
        if (!isTRUE(app_done)) {
          shiny::stopApp(selected_ids)
        }
      })
    }
  )

  shiny::runApp(app)
}


#' @title ggplot-based Loading Plot for mvr Models
#'
#' @description
#' Create a loading plot from an \code{mvr} model using \pkg{ggplot2}.
#'
#' @param object model object.
#' @param ... further arguments passed to methods.
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   gg_loadingplot(mod, scatter = TRUE)
#'   gg_loadingplot(mod)
#' }
gg_loadingplot <- function(object, ...) UseMethod("gg_loadingplot")


#' @rdname gg_loadingplot
#' @param comps integer vector of length two, giving the loading components to
#' plot.
#' @param scatter logical. If \code{TRUE}, make a scatter plot of the selected
#'   loading components. If \code{FALSE}, draw loading profiles across
#'   variables.
#' @param labels optional labels. In scatter mode, labels are used as point
#'   labels. In line mode, labels are used as x-axis labels.
#' @param pretty.xlabels logical. If \code{TRUE}, line-plot x-axis labels are
#'   thinned when possible.
#' @export
gg_loadingplot.mvr <- function(object, comps = 1:2, scatter = FALSE,
                               labels = FALSE, pretty.xlabels = TRUE, ...) {
  if (length(comps) != 2)
    stop("`comps` must contain exactly two components.")

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for gg_loadingplot().")
  if (!requireNamespace("rlang", quietly = TRUE))
    stop("Package 'rlang' is required for gg_loadingplot().")

  L <- loadings(object)[, comps, drop = FALSE]
  vars <- rownames(L)
  if (is.null(vars)) vars <- as.character(seq_len(nrow(L)))

  load_df <- data.frame(
    variable = vars,
    x = L[, 1],
    y = L[, 2],
    stringsAsFactors = FALSE
  )

  if (isTRUE(scatter)) {
    p <- ggplot2::ggplot(load_df, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_point(...) +
      ggplot2::labs(
        x = compnames(object, comps = comps[1], explvar = TRUE),
        y = compnames(object, comps = comps[2], explvar = TRUE)
      )

    point_labels <- .resolve_gg_point_labels(labels, vars)
    if (!is.null(point_labels)) {
      load_df$label <- point_labels
      p <- ggplot2::ggplot(load_df, ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::geom_point(...) +
        ggplot2::geom_text(ggplot2::aes(label = .data$label), vjust = -0.6) +
        ggplot2::labs(
          x = compnames(object, comps = comps[1], explvar = TRUE),
          y = compnames(object, comps = comps[2], explvar = TRUE)
        )
    }
  } else {
    axis_labels <- .resolve_gg_axis_labels(labels, vars,
                                           pretty.xlabels = pretty.xlabels)
    line_df <- data.frame(
      variable = rep(vars, 2L),
      x = rep(axis_labels$xnum, 2L),
      component = factor(rep(colnames(L), each = nrow(L)), levels = colnames(L)),
      value = c(L),
      stringsAsFactors = FALSE
    )

    p <- ggplot2::ggplot(line_df, ggplot2::aes(x = .data$x, y = .data$value,
                                               colour = .data$component,
                                               group = .data$component)) +
      ggplot2::geom_line(...) +
      ggplot2::labs(
        x = compnames(object, comps = comps[1], explvar = TRUE),
        y = compnames(object, comps = comps[2], explvar = TRUE),
        colour = "Component"
      ) +
      axis_labels$scale_spec
  }

  p
}


#' @title ggplot-based Biplot for mvr Models
#'
#' @description
#' Create a biplot from an \code{mvr} model using \pkg{ggplot2}. Scores are
#' shown as points, while loadings are shown either as points (default) or
#' arrows.
#'
#' @param object model object.
#' @param ... further arguments passed to methods.
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   gg_biplot(mod)
#' }
gg_biplot <- function(object, ...) UseMethod("gg_biplot")


#' @rdname gg_biplot
#' @param comps integer vector of length two, giving the components to plot.
#' @param labels logical. If \code{TRUE}, loading labels are added.
#' @param arrows logical. If \code{TRUE}, draw loadings as arrows. Defaults to
#' \code{FALSE}.
#' @param loading.scale numeric scaling factor for loadings.
#' @export
gg_biplot.mvr <- function(object, comps = 1:2, labels = FALSE,
                          arrows = FALSE, loading.scale = 0.8, ...) {
  if (length(comps) != 2)
    stop("`comps` must contain exactly two components.")

  if (!is.logical(arrows) || length(arrows) != 1 || is.na(arrows))
    stop("`arrows` must be a single non-missing logical value.")

  if (!is.numeric(loading.scale) || length(loading.scale) != 1 ||
      is.na(loading.scale) || loading.scale <= 0)
    stop("`loading.scale` must be a single positive number.")

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for gg_biplot().")
  if (!requireNamespace("rlang", quietly = TRUE))
    stop("Package 'rlang' is required for gg_biplot().")

  S <- scores(object)[, comps, drop = FALSE]
  L <- loadings(object)[, comps, drop = FALSE]

  obs_df <- data.frame(x = S[, 1], y = S[, 2])
  load_df <- data.frame(
    variable = if (is.null(rownames(L))) as.character(seq_len(nrow(L))) else rownames(L),
    x = L[, 1],
    y = L[, 2],
    stringsAsFactors = FALSE
  )

  max_obs <- max(abs(c(obs_df$x, obs_df$y)), na.rm = TRUE)
  max_load <- max(abs(c(load_df$x, load_df$y)), na.rm = TRUE)
  if (!is.finite(max_load) || max_load == 0) {
    scale_factor <- 1
  } else {
    scale_factor <- loading.scale * max_obs / max_load
  }
  load_df$x <- load_df$x * scale_factor
  load_df$y <- load_df$y * scale_factor

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = obs_df, ggplot2::aes(x = .data$x, y = .data$y), ...) +
    ggplot2::labs(
      x = compnames(object, comps = comps[1], explvar = TRUE),
      y = compnames(object, comps = comps[2], explvar = TRUE)
    )

  if (isTRUE(arrows)) {
    p <- p + ggplot2::geom_segment(
      data = load_df,
      ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y),
      arrow = ggplot2::arrow(length = grid::unit(0.18, "cm")),
      colour = "firebrick"
    )
  } else {
    p <- p + ggplot2::geom_point(
      data = load_df,
      ggplot2::aes(x = .data$x, y = .data$y),
      colour = "firebrick",
      shape = 4,
      size = 2.2
    )
  }

  if (isTRUE(labels)) {
    p <- p + ggplot2::geom_text(
      data = load_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$variable),
      colour = "firebrick",
      vjust = -0.6
    )
  }

  p
}


#' @title ggplot-based Prediction Plot for mvr Models
#'
#' @description
#' Create a prediction plot from an \code{mvr} model using \pkg{ggplot2}.
#'
#' @param object model object.
#' @param ... further arguments passed to methods.
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   gg_predplot(mod, ncomp = 1:2)
#' }
gg_predplot <- function(object, ...) UseMethod("gg_predplot")


#' @rdname gg_predplot
#' @param ncomp integer vector. The model sizes to use for prediction.
#' @param which character vector. One or more of \code{"train"},
#'   \code{"validation"} and \code{"test"}.
#' @param newdata optional data frame for test predictions.
#' @param labels optional labels for points. If \code{TRUE} or \code{"names"},
#'   row names are used; if \code{"numbers"}, row numbers are used.
#' @param type character. Plot symbol type passed through to \code{geom_point}.
#' @param line logical. If \code{TRUE}, draw a \eqn{y = x} reference line.
#' @param line.col,line.lty,line.lwd graphical settings for the reference line.
#' @param xlab,ylab axis labels.
#' @param main optional plot title.
#' @export
gg_predplot.mvr <- function(object, ncomp = 1:object$ncomp, which, newdata,
                            labels = FALSE, type = "p", line = TRUE,
                            line.col = "grey50", line.lty = 2,
                            line.lwd = 0.5, xlab = "measured",
                            ylab = "predicted", main = NULL, ...) {
  if (missing(which)) {
    if (!missing(newdata)) {
      which <- "test"
    } else if (!is.null(object$validation)) {
      which <- "validation"
    } else {
      which <- "train"
    }
  }

  all_types <- c("train", "validation", "test")
  which <- all_types[pmatch(which, all_types)]
  if (length(which) == 0 || any(is.na(which)))
    stop("`which` should be a subset of train, validation, test")

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for gg_predplot().")
  if (!requireNamespace("rlang", quietly = TRUE))
    stop("Package 'rlang' is required for gg_predplot().")

  ncomp <- as.integer(ncomp)
  nResp <- dim(object$fitted.values)[2]
  panel_df <- list()
  panel_i <- 0L

  train_measured <- as.matrix(model.response(model.frame(object)))
  train_labels <- rownames(train_measured)
  if (is.null(train_labels)) train_labels <- as.character(seq_len(nrow(train_measured)))

  if ("train" %in% which) {
    train_pred <- object$fitted.values[, , ncomp, drop = FALSE]
    for (resp in seq_len(nResp)) for (size in seq_along(ncomp)) {
      panel_i <- panel_i + 1L
      panel_df[[panel_i]] <- data.frame(
        panel = sprintf("%s, %d comps, train", respnames(object)[resp], ncomp[size]),
        measured = train_measured[, resp],
        predicted = train_pred[, resp, size],
        label = train_labels,
        stringsAsFactors = FALSE
      )
    }
  }

  if ("validation" %in% which) {
    if (is.null(object$validation))
      stop("`object' has no `validation' component.")
    val_pred <- object$validation$pred[, , ncomp, drop = FALSE]
    for (resp in seq_len(nResp)) for (size in seq_along(ncomp)) {
      panel_i <- panel_i + 1L
      panel_df[[panel_i]] <- data.frame(
        panel = sprintf("%s, %d comps, validation", respnames(object)[resp], ncomp[size]),
        measured = train_measured[, resp],
        predicted = val_pred[, resp, size],
        label = train_labels,
        stringsAsFactors = FALSE
      )
    }
  }

  if ("test" %in% which) {
    if (missing(newdata))
      stop("Missing `newdata'.")
    test_measured <- as.matrix(model.response(model.frame(formula(object),
                                                          data = newdata)))
    test_pred <- predict(object, ncomp = ncomp, newdata = newdata)
    test_labels <- rownames(test_measured)
    if (is.null(test_labels)) test_labels <- as.character(seq_len(nrow(test_measured)))
    for (resp in seq_len(nResp)) for (size in seq_along(ncomp)) {
      panel_i <- panel_i + 1L
      panel_df[[panel_i]] <- data.frame(
        panel = sprintf("%s, %d comps, test", respnames(object)[resp], ncomp[size]),
        measured = test_measured[, resp],
        predicted = test_pred[, resp, size],
        label = test_labels,
        stringsAsFactors = FALSE
      )
    }
  }

  plot_df <- do.call(rbind, panel_df)
  point_labels <- .resolve_gg_point_labels(labels, plot_df$label)
  if (!is.null(point_labels))
    plot_df$label <- point_labels

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$measured, y = .data$predicted)) +
    ggplot2::geom_point(...) +
    ggplot2::labs(x = xlab, y = ylab, title = main) +
    ggplot2::facet_wrap(~ panel)

  if (!is.null(point_labels))
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$label), vjust = -0.6)

  if (isTRUE(line)) {
    p <- p + ggplot2::geom_abline(intercept = 0, slope = 1,
                                  colour = line.col, linetype = line.lty,
                                  linewidth = line.lwd)
  }
  p
}


#' @title ggplot-based Correlation Loadings Plot for mvr Models
#'
#' @description
#' Create a correlation loadings plot from an \code{mvr} model using
#' \pkg{ggplot2}.
#'
#' @param object model object.
#' @param ... further arguments passed to methods.
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   gg_corrplot(mod)
#' }
gg_corrplot <- function(object, ...) UseMethod("gg_corrplot")


#' @rdname gg_corrplot
#' @param comps integer vector of length at least two, giving the components to
#'   plot.
#' @param labels optional labels for points. If \code{TRUE} or \code{"names"},
#'   row names are used; if \code{"numbers"}, row numbers are used.
#' @param plotx logical. Whether to plot X-variable correlation loadings.
#' @param ploty logical. Whether to plot Y-variable correlation loadings.
#' @param radii numeric vector of circle radii.
#' @param xlab,ylab axis labels.
#' @param main optional title.
#' @importFrom utils combn
#' @importFrom stats setNames
#' @export
gg_corrplot.mvr <- function(object, comps = 1:2, labels = FALSE,
                            plotx = TRUE, ploty = FALSE,
                            radii = c(sqrt(1 / 2), 1),
                            xlab, ylab, main = NULL, ...) {
  if (length(comps) < 2)
    stop("At least two components must be selected.")

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for gg_corrplot().")
  if (!requireNamespace("rlang", quietly = TRUE))
    stop("Package 'rlang' is required for gg_corrplot().")

  score_mat <- scores(object)[, comps, drop = FALSE]
  if (is.null(score_mat))
    stop("`", deparse(substitute(object)), "' has no scores.")

  corr_list <- list()
  if (isTRUE(plotx)) {
    clX <- cor(model.matrix(object), score_mat)
    colnames(clX) <- as.character(comps)
    corr_list[[length(corr_list) + 1L]] <- data.frame(
      variable = rownames(clX),
      type = "X",
      clX,
      stringsAsFactors = FALSE
    )
  }
  if (isTRUE(ploty)) {
    clY <- cor(model.response(model.frame(object)), score_mat)
    if (nrow(clY) == 1L)
      rownames(clY) <- all.vars(formula(object))[1]
    colnames(clY) <- as.character(comps)
    corr_list[[length(corr_list) + 1L]] <- data.frame(
      variable = rownames(clY),
      type = "Y",
      clY,
      stringsAsFactors = FALSE
    )
  }
  if (!length(corr_list))
    stop("At least one of `plotx` or `ploty` must be TRUE.")

  corr_mat <- do.call(rbind, corr_list)
  if (ncol(corr_mat) < max(comps))
    stop("Requested components are not available.")

  panel_pairs <- combn(comps, 2, simplify = FALSE)
  panel_df <- list()
  for (i in seq_along(panel_pairs)) {
    pair <- panel_pairs[[i]]
    panel_df[[i]] <- data.frame(
      panel = sprintf("%s vs %s",
                      compnames(object, comps = pair[1], explvar = TRUE),
                      compnames(object, comps = pair[2], explvar = TRUE)),
      variable = corr_mat$variable,
      type = corr_mat$type,
      x = corr_mat[[pair[1] + 2L]],
      y = corr_mat[[pair[2] + 2L]],
      stringsAsFactors = FALSE
    )
  }
  plot_df <- do.call(rbind, panel_df)
  plot_df$label <- .resolve_gg_point_labels(labels, plot_df$variable)

  circ_df <- data.frame()
  if (length(radii)) {
    theta <- seq(0, 2 * pi, length.out = 200)
    circle_base <- do.call(rbind, lapply(radii, function(r) {
      data.frame(r = r, x = r * cos(theta), y = r * sin(theta), stringsAsFactors = FALSE)
    }))
    circ_df <- do.call(rbind, lapply(unique(plot_df$panel), function(panel_name) {
      tmp <- circle_base
      tmp$panel <- panel_name
      tmp
    }))
  }

  if (missing(xlab)) xlab <- compnames(object, comps = comps[1], explvar = TRUE)
  if (missing(ylab)) ylab <- compnames(object, comps = comps[2], explvar = TRUE)

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$type)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
    ggplot2::geom_vline(xintercept = 0, colour = "grey70") +
    ggplot2::geom_point(...) +
    ggplot2::facet_wrap(~ panel) +
    ggplot2::coord_equal(xlim = c(-1, 1), ylim = c(-1, 1)) +
    ggplot2::labs(x = xlab, y = ylab, title = main, colour = NULL)

  if (!is.null(plot_df$label))
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$label), vjust = -0.6,
                                show.legend = FALSE)

  if (nrow(circ_df)) {
    p <- p + ggplot2::geom_path(data = circ_df,
                                ggplot2::aes(x = .data$x, y = .data$y, group = .data$r),
                                inherit.aes = FALSE, colour = "grey50",
                                linewidth = 0.3)
  }

  p + ggplot2::scale_colour_manual(values = c(X = "steelblue", Y = "firebrick"))
}


#' @title ggplot-based Coefficient Plot for mvr Models
#'
#' @description
#' Create a coefficient plot from an \code{mvr} model using \pkg{ggplot2}.
#'
#' @param object model object.
#' @param ... further arguments passed to methods.
#' @return A \code{ggplot} object.
#' @export
#'
#' @examples
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   gg_coefplot(mod, ncomp = 1:3)
#' }
gg_coefplot <- function(object, ...) UseMethod("gg_coefplot")


#' @rdname gg_coefplot
#' @param ncomp positive integer vector. Number of components used for
#'   coefficient output.
#' @param comps integer vector. Component numbers to plot instead of ncomp.
#' @param intercept logical. Include the intercept in the coefficient output.
#' @param separate logical. If \code{TRUE}, facet by component size.
#' @param se.whiskers logical. If \code{TRUE}, add jackknife-based standard
#'   error whiskers. Only available for models fitted with \code{jackknife =
#'   TRUE}.
#' @param labels optional labels for the x axis.
#' @param pretty.xlabels logical. If \code{TRUE}, x-axis labels are thinned for
#'   dense spectral plots.
#' @param xlab,ylab axis labels.
#' @param main optional title.
#' @export
gg_coefplot.mvr <- function(object, ncomp = object$ncomp, comps,
                            intercept = FALSE, separate = FALSE,
                            se.whiskers = FALSE, labels = FALSE,
                            pretty.xlabels = TRUE, xlab = "variable",
                            ylab = "regression coefficient", main = NULL, ...) {
  if (missing(comps)) comps <- NULL
  separate <- isTRUE(separate)
  se.whiskers <- isTRUE(se.whiskers)

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for gg_coefplot().")
  if (!requireNamespace("rlang", quietly = TRUE))
    stop("Package 'rlang' is required for gg_coefplot().")

  coefs <- coef(object, ncomp = ncomp, comps = comps, intercept = intercept)
  complabs <- dimnames(coefs)[[3]]
  predlab <- dimnames(coefs)[[1]]
  respnames_vec <- dimnames(coefs)[[2]]

  if (se.whiskers) {
    if (isTRUE(intercept))
      stop(sQuote("se.whiskers"), " not supported when ",
           sQuote("intercept"), " is TRUE")
    if (!is.null(comps))
      stop(sQuote("se.whiskers"), " not supported when ",
           sQuote("comps"), " is specified")
    if (dim(coefs)[3] > 1 && !separate)
      stop(sQuote("se.whiskers"), " not supported when ",
           sQuote("separate"), " is FALSE and length(ncomp) > 1")
    SEs <- sqrt(var.jack(object, ncomp = ncomp))
  }

  labels_info <- .resolve_gg_axis_labels(labels, predlab,
                                         pretty.xlabels = pretty.xlabels)

  coef_df <- list()
  idx <- 0L
  for (resp in seq_along(respnames_vec)) for (size in seq_along(complabs)) {
    idx <- idx + 1L
    coef_df[[idx]] <- data.frame(
      predictor = predlab,
      x = labels_info$xnum,
      response = respnames_vec[resp],
      component = complabs[size],
      value = coefs[, resp, size],
      stringsAsFactors = FALSE
    )
    if (se.whiskers)
      coef_df[[idx]]$se <- SEs[, resp, size]
  }
  plot_df <- do.call(rbind, coef_df)

  if (separate) {
    plot_df$panel <- paste(plot_df$response, plot_df$component, sep = ", ")
  } else {
    plot_df$panel <- plot_df$response
  }

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$x, y = .data$value,
                                             colour = .data$component,
                                             group = .data$component)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
    ggplot2::geom_line(...) +
    ggplot2::labs(x = xlab, y = ylab, title = main, colour = "Component")

  if (se.whiskers) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$value - .data$se, ymax = .data$value + .data$se),
      width = 0.12, linewidth = 0.35, alpha = 0.65, show.legend = FALSE
    )
  }

  if (separate) {
    p <- p + ggplot2::facet_grid(response ~ component)
  } else {
    p <- p + ggplot2::facet_wrap(~ response)
  }

  p <- p + labels_info$scale_spec
  p
}


#' @title Autoplot for mvr Models
#'
#' @description
#' \code{autoplot} is a convenience wrapper that calls
#' \code{ggplot2::autoplot()}. This makes \code{autoplot(mod, ...)} available
#' directly from \pkg{pls} when \pkg{ggplot2} is installed.
#'
#' @param object an object to autoplot.
#' @param ... further arguments passed to \code{ggplot2::autoplot()}.
#' @return A plot object returned by \code{ggplot2::autoplot()}.
#' @export
#'
#' @examples
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   autoplot(mod, type = "scores")
#' }
autoplot <- function(object, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for autoplot(). ",
      "Install it with install.packages('ggplot2'), or use scoreplot()/loadingplot()/biplot().",
      call. = FALSE
    )
  }
  ggplot2::autoplot(object, ...)
}


#' @title Autoplot Method for mvr Models
#'
#' @description
#' \code{autoplot.mvr} provides a ggplot2-based plotting entrypoint for
#' \code{mvr} objects.
#'
#' @param object an \code{mvr} object.
#' @param type plot type. One of \code{"scores"}, \code{"loadings"},
#'   \code{"biplot"}, \code{"prediction"}, \code{"correlation"} or
#'   \code{"coefficients"}.
#' @param ... further arguments passed to the selected plotting function.
#' @return A \code{ggplot} object.
#' @exportS3Method ggplot2::autoplot
#'
#' @examples
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   autoplot(mod, type = "scores")
#'   autoplot(mod, type = "loadings")
#'   autoplot(mod, type = "biplot")
#'   autoplot(mod, type = "prediction")
#'   autoplot(mod, type = "correlation")
#'   autoplot(mod, type = "coefficients")
#' }
autoplot.mvr <- function(object, type = c("scores", "loadings", "biplot",
                                          "prediction", "correlation",
                                          "coefficients"), ...) {
  type <- match.arg(type)
  switch(type,
         scores = gg_scoreplot(object, ...),
         loadings = gg_loadingplot(object, ...),
         biplot = gg_biplot(object, ...),
         prediction = gg_predplot(object, ...),
         correlation = gg_corrplot(object, ...),
         coefficients = gg_coefplot(object, ...))
}
