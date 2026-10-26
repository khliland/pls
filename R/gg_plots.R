### gg_plots.R: ggplot-/plotly-based score plots, loading plots, biplots and autoplot for mvr models

#' ggplot- and Plotly-based Score Plot for mvr Models or Extracted Scores
#'
#' @description Create a score plot from an \code{mvr} model or extracted scores
#' using \code{ggplot2}. For general information on scores and plotting,
#' see \code{\link{scores}} and \code{\link{scoreplot}}. \code{gg_scoreplot}
#' is also available as part of \code{\link{gg_dashboard}}.
#'
#' @param object an \code{mvr} model or a matrix of extracted scores.
#' @param ... further arguments passed to methods.
#' @return A \code{ggplot} object (or a \code{plotly} object when
#'   \code{plotly = TRUE}). For \code{mvr} models with
#'   \code{select = TRUE}, a character vector with selected sample IDs.
#' @export
gg_scoreplot <- function(object, ...) UseMethod("gg_scoreplot")


#' @rdname gg_scoreplot
#' @param comps integer vector of length one or two, giving the score
#' components to plot. A single component is plotted against sample index.
#' @param estimate optional character vector passed to \code{scores()}.
#' @param newdata optional data frame passed to \code{scores()} when
#' \code{estimate = "test"}.
#' @param select logical. For \code{mvr} models, if \code{TRUE}, launch an
#' interactive selection app and return selected IDs. Not available for
#' extracted scores.
#' @param main optional plot title.
#' @param plotly logical. If \code{TRUE}, use \pkg{plotly} output. For
#' \code{mvr} models with \code{select = TRUE}, a Shiny app is launched and
#' selected IDs are returned from Plotly lasso/box selection.
#' @details
#' \code{gg_scoreplot} supports interactive sample selection that can be
#' used in conjunction with the \code{refit} method to update models with
#' or without selected samples.
#'
#' @examples
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- gg_scoreplot(mod)
#'   print(p)
#'   gg_scoreplot(mod, main = "Score plot")
#'   gg_scoreplot(scores(mod))
#' }
#'
#' \dontrun{ # Requires an interactive session
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("ggiraph", quietly = TRUE) &&
#'     requireNamespace("shiny", quietly = TRUE)) {
#'   selected_ids <- gg_scoreplot(mod, select = TRUE)
#'   refit_mod <- refit(mod, selection = selected_ids, exclude = TRUE)
#' }}
#' @export
gg_scoreplot.mvr <- function(object, comps = 1:2, estimate, newdata,
                             select = FALSE, plotly = FALSE, main = NULL, ...) {
  if (!is.logical(plotly) || length(plotly) != 1 || is.na(plotly))
    stop("`plotly` must be a single non-missing logical value.")

  score_args <- list(object)
  if (!missing(estimate)) score_args$estimate <- estimate
  if (!missing(newdata)) score_args$newdata <- newdata
  score_mat <- do.call(scores, score_args)

  if (!is.numeric(comps) || length(comps) < 1L || length(comps) > 2L ||
      anyNA(comps) || any(!is.finite(comps)) || any(comps != as.integer(comps)) ||
      any(comps < 1L))
    stop("`comps` must contain one or two positive component indices.")
  comps <- as.integer(comps)
  if (ncol(score_mat) == 1L && identical(comps, 1:2))
    comps <- 1L
  if (any(comps > ncol(score_mat)))
    stop("`comps` selects a component not available in the scores.")

  single_component <- length(comps) == 1L
  sample_id <- rownames(score_mat)
  if (is.null(sample_id))
    sample_id <- as.character(seq_len(nrow(score_mat)))
  score_df <- data.frame(
    sample_id = sample_id,
    x = if (single_component) seq_len(nrow(score_mat)) else score_mat[, comps[1]],
    y = score_mat[, comps[if (single_component) 1L else 2L]],
    stringsAsFactors = FALSE
  )

  xlab <- if (single_component) "Sample" else
    compnames(score_mat, comps = comps[1], explvar = TRUE)
  ylab <- compnames(score_mat,
                    comps = comps[if (single_component) 1L else 2L],
                    explvar = TRUE)

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for gg_scoreplot().")
  if (!requireNamespace("rlang", quietly = TRUE))
    stop("Package 'rlang' is required for gg_scoreplot().")

  base_plot <-
    ggplot2::ggplot(score_df, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(...) +
    ggplot2::labs(x = xlab, y = ylab, title = main)

  if (isTRUE(plotly)) {
    if (!requireNamespace("plotly", quietly = TRUE))
      stop("Package 'plotly' is required when plotly = TRUE.")

    if (!isTRUE(select)) {
      return(plotly::ggplotly(base_plot))
    }

    if (!requireNamespace("shiny", quietly = TRUE))
      stop("Package 'shiny' is required when select = TRUE.")

    app <- shiny::shinyApp(
      ui = shiny::fluidPage(
        shiny::h4("Select samples and click Done"),
        shiny::actionButton("done", "Done"),
        plotly::plotlyOutput("score_plot", width = "100%", height = "700px"),
        shiny::p("Use box or lasso select. Closing the app window also returns the current selection.")
      ),
      server = function(input, output, session) {
        selected_ids <- character(0)
        app_done <- FALSE

        output$score_plot <- plotly::renderPlotly({
          plotly::ggplotly(base_plot, source = "score_plot")
        })

        shiny::observeEvent(plotly::event_data("plotly_selected", source = "score_plot"), {
          sel <- plotly::event_data("plotly_selected", source = "score_plot")
          if (is.null(sel) || !NROW(sel) || is.null(sel$pointNumber)) {
            selected_ids <<- character(0)
          } else {
            idx <- unique(as.integer(sel$pointNumber) + 1L)
            idx <- idx[is.finite(idx) & idx >= 1L & idx <= nrow(score_df)]
            selected_ids <<- as.character(score_df$sample_id[idx])
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

    return(shiny::runApp(app))
  }

  if (!isTRUE(select)) {
    return(base_plot)
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
    ggplot2::labs(x = xlab, y = ylab, title = main)

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

#' @rdname gg_scoreplot
#' @param comps integer vector of length one or two, giving the score
#' components to plot. A single component is plotted against sample index.
#' @export
gg_scoreplot.scores <- function(object, comps = 1:2, plotly = FALSE,
                                main = NULL, ...) {
  if (!is.logical(plotly) || length(plotly) != 1 || is.na(plotly))
    stop("`plotly` must be a single non-missing logical value.")

  if (!is.numeric(comps) || length(comps) < 1L || length(comps) > 2L ||
      anyNA(comps) || any(!is.finite(comps)) || any(comps != as.integer(comps)) ||
      any(comps < 1L))
    stop("`comps` must contain one or two positive component indices.")
  comps <- as.integer(comps)
  if (ncol(object) == 1L && identical(comps, 1:2))
    comps <- 1L
  if (any(comps > ncol(object)))
    stop("`comps` selects a component not available in the scores.")
  single_component <- length(comps) == 1L

  sample_id <- rownames(object)
  if (is.null(sample_id))
    sample_id <- as.character(seq_len(nrow(object)))

  score_df <- data.frame(
    sample_id = sample_id,
    x = if (single_component) seq_len(nrow(object)) else object[, comps[1]],
    y = object[, comps[if (single_component) 1L else 2L]],
    stringsAsFactors = FALSE
  )

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for gg_scoreplot().")
  if (!requireNamespace("rlang", quietly = TRUE))
    stop("Package 'rlang' is required for gg_scoreplot().")

  p <-
    ggplot2::ggplot(score_df, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(...) +
    ggplot2::labs(
      x = if (single_component) "Sample" else
        compnames(object, comps = comps[1], explvar = TRUE),
      y = compnames(object, comps = comps[if (single_component) 1L else 2L],
                    explvar = TRUE),
      title = main
    )

  if (isTRUE(plotly)) {
    if (!requireNamespace("plotly", quietly = TRUE))
      stop("Package 'plotly' is required when plotly = TRUE.")
    p <- plotly::ggplotly(p)
  }
  p
}


#' @title ggplot-based Loading Plot for mvr Models or Extracted Loadings
#'
#' @description
#' Create a loading plot from an \code{mvr} model or extracted loadings using
#' \pkg{ggplot2}.
#'
#' @param object an \code{mvr} model or a matrix of extracted loadings.
#' @param ... further arguments passed to methods.
#' @return A \code{ggplot} object or a \code{plotly} object when
#'   \code{plotly = TRUE}.
#' @export
#'
#' @examples
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   gg_loadingplot(mod, scatter = TRUE)
#'   gg_loadingplot(mod, main = "Loading plot")
#'   gg_loadingplot(loadings(mod))
#' }
gg_loadingplot <- function(object, ...) UseMethod("gg_loadingplot")


#' @rdname gg_loadingplot
#' @param comps integer vector of length one or two, giving the loading
#' components to plot. A single component is plotted against variable index.
#' @param scatter logical. If \code{TRUE}, make a scatter plot of the selected
#'   loading components. If \code{FALSE}, draw loading profiles across
#'   variables.
#' @param labels optional labels. In scatter mode, labels are used as point
#'   labels. In line mode, labels are used as x-axis labels.
#' @param pretty.xlabels logical. If \code{TRUE}, line-plot x-axis labels are
#'   thinned when possible.
#' @param main optional plot title.
#' @param plotly logical. If \code{TRUE}, use \pkg{plotly} output.
#' @export
gg_loadingplot.mvr <- function(object, comps = 1:2, scatter = FALSE,
                               labels = FALSE, pretty.xlabels = TRUE,
                               plotly = FALSE, main = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for gg_loadingplot().")
  if (!requireNamespace("rlang", quietly = TRUE))
    stop("Package 'rlang' is required for gg_loadingplot().")

  L <- loadings(object)
  if (!is.numeric(comps) || length(comps) < 1L || length(comps) > 2L ||
      anyNA(comps) || any(!is.finite(comps)) || any(comps != as.integer(comps)) ||
      any(comps < 1L))
    stop("`comps` must contain one or two positive component indices.")
  comps <- as.integer(comps)
  if (ncol(L) == 1L && identical(comps, 1:2))
    comps <- 1L
  if (any(comps > ncol(L)))
    stop("`comps` selects a component not available in the loadings.")
  single_component <- length(comps) == 1L
  L <- L[, comps, drop = FALSE]
  vars <- rownames(L)
  if (is.null(vars)) vars <- as.character(seq_len(nrow(L)))

  if (single_component) {
    if (isTRUE(scatter)) {
      load_df <- data.frame(
        x = seq_along(vars),
        y = L[, 1],
        stringsAsFactors = FALSE
      )
      point_labels <- .resolve_gg_point_labels(labels, vars)
      if (!is.null(point_labels)) load_df$label <- point_labels

      p <- ggplot2::ggplot(load_df, ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::labs(
          x = "Variable index",
          y = compnames(object, comps = comps[1], explvar = TRUE),
          title = main
        )

      if (!is.null(point_labels)) {
        p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$label), vjust = -0.6)
      } else {
        p <- p + ggplot2::geom_point(...)
      }
    } else {
      axis_labels <- .resolve_gg_axis_labels(labels, vars,
                                             pretty.xlabels = pretty.xlabels)
      line_df <- data.frame(
        variable = vars,
        x = axis_labels$xnum,
        component = colnames(L)[1],
        value = L[, 1],
        stringsAsFactors = FALSE
      )

      p <- ggplot2::ggplot(line_df, ggplot2::aes(x = .data$x, y = .data$value,
                                                 group = 1)) +
        ggplot2::geom_line(...) +
        ggplot2::labs(
          x = "Variable index",
          y = compnames(object, comps = comps[1], explvar = TRUE),
          title = main
        ) +
        axis_labels$scale_spec
    }
  } else {
    load_df <- data.frame(
      variable = vars,
      x = L[, 1],
      y = L[, 2],
      stringsAsFactors = FALSE
    )

    if (isTRUE(scatter)) {
      point_labels <- .resolve_gg_point_labels(labels, vars)
      if (!is.null(point_labels)) load_df$label <- point_labels

      p <- ggplot2::ggplot(load_df, ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::labs(
          x = compnames(object, comps = comps[1], explvar = TRUE),
          y = compnames(object, comps = comps[2], explvar = TRUE),
          title = main
        )

      if (!is.null(point_labels)) {
        p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$label), vjust = -0.6)
      } else {
        p <- p + ggplot2::geom_point(...)
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
          colour = "Component",
          title = main
        ) +
        axis_labels$scale_spec
    }
  }

  if(isTRUE(plotly)) {
    if (!requireNamespace("plotly", quietly = TRUE))
      stop("Package 'plotly' is required when plotly = TRUE.")
    p <- plotly::ggplotly(p)
  }
  p
}

#' @rdname gg_loadingplot
#' @param comps integer vector of length one or two, giving the loading
#' components to plot. A single component is plotted against variable index.
#' @param scatter logical. If \code{TRUE}, make a scatter plot of the selected
#' loading components. If \code{FALSE}, draw loading profiles across
#' variables.
#' @param labels optional labels. In scatter mode, labels are used as point
#' labels. In line mode, labels are used as x-axis labels.
#' @param pretty.xlabels logical. If \code{TRUE}, line-plot x-axis labels are
#' thinned when possible.
#' @export
gg_loadingplot.loadings <- function(object, comps = 1:2, scatter = FALSE,
                                    labels = FALSE, pretty.xlabels = TRUE,
                                    plotly = FALSE, main = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for gg_loadingplot().")
  if (!requireNamespace("rlang", quietly = TRUE))
    stop("Package 'rlang' is required for gg_loadingplot().")

  if (!is.numeric(comps) || length(comps) < 1L || length(comps) > 2L ||
      anyNA(comps) || any(!is.finite(comps)) || any(comps != as.integer(comps)) ||
      any(comps < 1L))
    stop("`comps` must contain one or two positive component indices.")
  comps <- as.integer(comps)
  if (ncol(object) == 1L && identical(comps, 1:2))
    comps <- 1L
  if (any(comps > ncol(object)))
    stop("`comps` selects a component not available in the loadings.")
  single_component <- length(comps) == 1L
  L <- object[, comps, drop = FALSE]
  vars <- rownames(L)
  if (is.null(vars)) vars <- as.character(seq_len(nrow(L)))

  if (single_component) {
    if (isTRUE(scatter)) {
      load_df <- data.frame(
        x = seq_along(vars),
        y = L[, 1],
        stringsAsFactors = FALSE
      )
      point_labels <- .resolve_gg_point_labels(labels, vars)
      if (!is.null(point_labels)) load_df$label <- point_labels

      p <- ggplot2::ggplot(load_df, ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::labs(
          x = "Variable index",
          y = compnames(object, comps = comps[1], explvar = TRUE),
          title = main
        )

      if (!is.null(point_labels)) {
        p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$label), vjust = -0.6)
      } else {
        p <- p + ggplot2::geom_point(...)
      }
    }
    else {
      axis_labels <- .resolve_gg_axis_labels(labels, vars,
                                             pretty.xlabels = pretty.xlabels)
      line_df <- data.frame(
        variable = vars,
        x = axis_labels$xnum,
        component = colnames(L)[1],
        value = L[, 1],
        stringsAsFactors = FALSE
      )

      p <- ggplot2::ggplot(line_df, ggplot2::aes(x = .data$x, y = .data$value,
                                                 group = 1)) +
        ggplot2::geom_line(...) +
        ggplot2::labs(
          x = "Variable index",
          y = compnames(object, comps = comps[1], explvar = TRUE),
          title = main
        ) +
        axis_labels$scale_spec
    }
  } else {
    load_df <- data.frame(
      variable = vars,
      x = L[, 1],
      y = L[, 2],
      stringsAsFactors = FALSE
    )

    if (isTRUE(scatter)) {
      point_labels <- .resolve_gg_point_labels(labels, vars)
      if (!is.null(point_labels)) load_df$label <- point_labels

      p <- ggplot2::ggplot(load_df, ggplot2::aes(x = .data$x, y = .data$y)) +
        ggplot2::labs(
          x = compnames(object, comps = comps[1], explvar = TRUE),
          y = compnames(object, comps = comps[2], explvar = TRUE),
          title = main
        )

      if (!is.null(point_labels)) {
        p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$label), vjust = -0.6)
      } else {
        p <- p + ggplot2::geom_point(...)
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
          colour = "Component",
          title = main
        ) +
        axis_labels$scale_spec
    }
  }

  if (isTRUE(plotly)) {
    if (!requireNamespace("plotly", quietly = TRUE))
      stop("Package 'plotly' is required when plotly = TRUE.")
    p <- plotly::ggplotly(p)
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
#'   gg_biplot(mod, main = "Biplot")
#' }
gg_biplot <- function(object, ...) UseMethod("gg_biplot")


#' @rdname gg_biplot
#' @param comps integer vector of length two, giving the components to plot.
#' @param labels logical. If \code{TRUE}, loading labels are added.
#' @param arrows logical. If \code{TRUE}, draw loadings as arrows. Defaults to
#' \code{FALSE}.
#' @param loading.scale numeric scaling factor for loadings.
#' @param main optional plot title.
#' @param plotly logical. If \code{TRUE}, use \pkg{plotly} output.
#' @export
gg_biplot.mvr <- function(object, comps = 1:2, labels = FALSE,
                          arrows = FALSE, loading.scale = 0.8,
                          plotly = FALSE, main = NULL, ...) {
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

  sample_id <- rownames(S)
  if (is.null(sample_id)) sample_id <- as.character(seq_len(nrow(S)))
  obs_df <- data.frame(sample_id = sample_id, x = S[, 1], y = S[, 2],
                       stringsAsFactors = FALSE)
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
      y = compnames(object, comps = comps[2], explvar = TRUE),
      title = main
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

  if(isTRUE(plotly)) {
    if (!requireNamespace("plotly", quietly = TRUE))
      stop("Package 'plotly' is required when plotly = TRUE.")
    p <- plotly::ggplotly(p)
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
#' @param plotly logical. If \code{TRUE}, use \pkg{plotly} output.
#' @export
gg_predplot.mvr <- function(object, ncomp = 1:object$ncomp, which, newdata,
                            labels = FALSE, type = "p", line = TRUE,
                            line.col = "grey50", line.lty = 2,
                            line.lwd = 0.5, xlab = "measured",
                            ylab = "predicted", main = NULL, plotly = FALSE, ...) {
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
        sample_id = train_labels,
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
        sample_id = train_labels,
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
        sample_id = test_labels,
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
  if(isTRUE(plotly)){
    if (!requireNamespace("plotly", quietly = TRUE))
      stop("Package 'plotly' is required when plotly = TRUE.")
    p <- plotly::ggplotly(p)
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
#' @param plotly logical. If \code{TRUE}, use \pkg{plotly} output.
#' @export
gg_corrplot.mvr <- function(object, comps = 1:2, labels = FALSE,
                            plotx = TRUE, ploty = FALSE,
                            radii = c(sqrt(1 / 2), 1),
                            xlab, ylab, main = NULL, plotly = FALSE, ...) {
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
    ggplot2::coord_fixed(ratio = 1, xlim = c(-1, 1), ylim = c(-1, 1)) +
    ggplot2::labs(x = xlab, y = ylab, title = main, colour = NULL)
  if (length(unique(plot_df$type)) < 2)
    p <- p + ggplot2::guides(colour = "none")
  if (!is.null(plot_df$label))
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$label), vjust = -0.6,
                                show.legend = FALSE)

  if (nrow(circ_df)) {
    p <- p + ggplot2::geom_path(data = circ_df,
                                ggplot2::aes(x = .data$x, y = .data$y, group = .data$r),
                                inherit.aes = FALSE, colour = "grey50",
                                linewidth = 0.3)
  }

  p <- p + ggplot2::scale_colour_manual(values = c(X = "steelblue", Y = "firebrick"))

  if (isTRUE(plotly)) {
    if (!requireNamespace("plotly", quietly = TRUE))
      stop("Package 'plotly' is required when plotly = TRUE.")
    p <- plotly::ggplotly(p)
    # Facets scale x and y to their pixel sizes independently, so pin each y-axis
    # to its x-axis to keep the unit circles round.
    p_built <- plotly::plotly_build(p)
    for (y_id in grep("^yaxis", names(p_built$x$layout), value = TRUE)) {
      p_built$x$layout[[y_id]]$scaleanchor <- sub("^yaxis", "x", y_id)
      p_built$x$layout[[y_id]]$scaleratio <- 1
    }
    p <- p_built
  }
  p
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
#' @param response optional response to plot. If \code{NULL}, all responses
#'   are shown. If specified, only that response is plotted.
#' @param plotly logical. If \code{TRUE}, use \pkg{plotly} output.
#' @export
gg_coefplot.mvr <- function(object, ncomp = object$ncomp, comps,
                            intercept = FALSE, separate = FALSE,
                            se.whiskers = FALSE, labels = FALSE,
                            pretty.xlabels = TRUE, xlab = "variable",
                            ylab = "regression coefficient", main = NULL,
                            response = NULL, plotly = FALSE, ...) {
  if (missing(comps)) comps <- NULL
  separate <- isTRUE(separate)
  se.whiskers <- isTRUE(se.whiskers)

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for gg_coefplot().")
  if (!requireNamespace("rlang", quietly = TRUE))
    stop("Package 'rlang' is required for gg_coefplot().")

  coefs <- coef(object, ncomp = ncomp, comps = comps, intercept = intercept)
  if (!is.null(response)) {
    response_names <- dimnames(coefs)[[2]]
    if (length(response) != 1L)
      stop("`response` must be a single response name or index.")
    if (is.numeric(response)) {
      response <- as.integer(response)
      if (is.na(response) || response < 1L || response > length(response_names))
        stop("`response` is out of range.")
    } else {
      response <- match(response, response_names)
      if (is.na(response))
        stop("`response` was not found in the model response names.")
    }
    coefs <- coefs[, response, , drop = FALSE]
  }
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
  if(isTRUE(plotly)){
    if (!requireNamespace("plotly", quietly = TRUE))
      stop("Package 'plotly' is required when plotly = TRUE.")
    p <- plotly::ggplotly(p)
  }
  p
}


#' @title ggplot-based Validation Plot for mvr Models
#'
#' @description
#' Create a validation-statistic plot from an \code{mvr} model or an
#' \code{mvrVal} object using \pkg{ggplot2}.
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
#'   gg_valplot(mod, val.type = "RMSEP", estimate = c("train", "CV"))
#' }
gg_valplot <- function(object, ...) UseMethod("gg_valplot")


#' @rdname gg_valplot
#' @param val.type character. Validation metric to plot. One of
#'   \code{"RMSEP"}, \code{"MSEP"} or \code{"R2"}.
#' @param estimate optional character vector of estimators passed to
#'   \code{RMSEP()}, \code{MSEP()} or \code{R2()}.
#' @param newdata optional data frame used for test-set validation estimates.
#' @param ncomp,comps model size specification passed to validation metrics.
#' @param intercept logical. Whether to include zero-component model in
#'   validation metrics.
#' @param xlab,ylab axis labels.
#' @param main optional title.
#' @export
gg_valplot.mvr <- function(object, val.type = c("RMSEP", "MSEP", "R2"),
                           estimate, newdata, ncomp, comps, intercept,
                           xlab = "number of components", ylab, main = NULL,
                           ...) {
  val.type <- match.arg(val.type)

  metric_call <- list(as.name(val.type), object = object)
  if (!missing(estimate)) metric_call$estimate <- estimate
  if (!missing(newdata)) metric_call$newdata <- newdata
  if (!missing(ncomp)) metric_call$ncomp <- ncomp
  if (!missing(comps)) metric_call$comps <- comps
  if (!missing(intercept)) metric_call$intercept <- intercept

  val_obj <- eval(as.call(metric_call), parent.frame())

  gg_valplot(val_obj, xlab = xlab, ylab = ylab, main = main, ...)
}


#' @rdname gg_valplot
#' @export
gg_valplot.mvrVal <- function(object, xlab = "number of components", ylab,
                              main = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for gg_valplot().")
  if (!requireNamespace("rlang", quietly = TRUE))
    stop("Package 'rlang' is required for gg_valplot().")

  if (missing(ylab)) ylab <- object$type

  plot_df <- tidy(object)
  plot_df$estimate <- factor(plot_df$estimate, levels = unique(plot_df$estimate))

  p <- ggplot2::ggplot(plot_df,
                       ggplot2::aes(x = .data$component, y = .data$value,
                                    colour = .data$estimate,
                                    group = .data$estimate)) +
    ggplot2::geom_line(...) +
    ggplot2::labs(x = xlab, y = ylab, title = main, colour = "Estimate")

  if (length(unique(plot_df$response)) > 1L) {
    p <- p + ggplot2::facet_wrap(~ response, scales = "free_y")
  }

  p
}

#' Raw Data Plot Panel for mvr Models
#'
#' @param object An mvr model object, matrix, or data.frame.
#' @param plottype Character. One of "boxplot", "line", or "violin".
#' @param jitter_width Numeric width for point jitter in boxplot/violin modes.
#' @param alpha Numeric transparency for observations.
#' @param angle Numeric. Angle (0 to 90) to rotate x-axis labels. Default is 0.
#' @param check_overlap Logical. If TRUE, automatically suppresses overlapping x-axis labels
#'   for categorical axes (boxplot and violin).
#' @param max_ticks Integer. Maximum number of tick labels to show when
#'   \code{plotly = TRUE}. Applies to numeric and line axes; categorical axes thin their labels to fit.
#' @param main optional plot title.
#' @param plotly Logical. If TRUE, returns a Plotly widget.
#' @examples
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   gg_rawdataplot(mod, plottype = "line", main = "Raw data")
#' }
#' @export
gg_rawdataplot <- function(object,
                           plottype = c("boxplot", "line", "violin"),
                           jitter_width = 0.2,
                           alpha = 0.5,
                           angle = 0,
                           check_overlap = TRUE,
                           max_ticks = 12L,
                           plotly = FALSE,
                           main = NULL) {
  plottype <- match.arg(plottype)

  # Extract predictor matrix X
  X <- if (is.matrix(object) || is.data.frame(object)) {
    if (!all(vapply(as.data.frame(object), is.numeric, logical(1))))
      stop("`object` must be a numeric matrix or a numeric data.frame.")
    as.matrix(object)
  } else if (!is.null(object$X)) {
    object$X
  } else if (!is.null(object$model)) {
    stats::model.matrix(object)[, -1, drop = FALSE] # drop intercept if present
  } else {
    stop("Raw data X matrix could not be extracted from the model object.")
  }

  if(is.null(colnames(X))) {
    colnames(X) <- seq_len(ncol(X))
  }
  df_X <- as.data.frame(X)
  var_names <- colnames(df_X)

  n_obs <- nrow(df_X)
  df_X$.obs_id <- paste0("Obs_", seq_len(n_obs))

  # Check if variable names are convertible to continuous numeric values (e.g. wavelengths)
  num_vars <- suppressWarnings(as.numeric(var_names))
  is_numeric_axis <- !any(is.na(num_vars)) && length(num_vars) > 0

  # Pivot to long format for ggplot
  df_long <- tidyr::pivot_longer(
    df_X,
    cols = -.obs_id,
    names_to = "Variable",
    values_to = "Value"
  )

  if (is_numeric_axis) {
    # Continuous numeric axis (e.g., wavelength 400nm .. 2500nm)
    df_long$Variable <- num_vars[match(df_long$Variable, var_names)]
    p <- ggplot2::ggplot(df_long, ggplot2::aes(x = Variable, y = Value))
  } else if (plottype == "line") {
    # Continuous index axis with thinned variable names as tick labels
    df_long$Variable <- match(df_long$Variable, var_names)
    axis_labels <- .resolve_gg_axis_labels("names", var_names)
    p <- ggplot2::ggplot(df_long, ggplot2::aes(x = Variable, y = Value)) +
      axis_labels$scale_spec
  } else {
    # Categorical discrete axis, keeping about `max_ticks` labels
    df_long$Variable <- factor(df_long$Variable, levels = var_names)
    step <- max(1L, ceiling(length(var_names) / max_ticks))
    p <- ggplot2::ggplot(df_long, ggplot2::aes(x = Variable, y = Value)) +
      ggplot2::scale_x_discrete(
        breaks = var_names[seq(1L, length(var_names), by = step)],
        guide = ggplot2::guide_axis(check.overlap = check_overlap)
      )
  }

  # Aesthetic mapping for interactive Plotly tooltips
  jitter_aes <- if (isTRUE(plotly)) {
    ggplot2::aes(text = paste("Obs:", .obs_id), key = .obs_id)
  } else {
    ggplot2::aes()
  }

  withCallingHandlers({
    if (plottype == "line") {
      line_aes <- if (isTRUE(plotly)) {
        ggplot2::aes(group = .obs_id, key = .obs_id)
      } else {
        ggplot2::aes(group = .obs_id)
      }
      p <- p +
        ggplot2::geom_line(line_aes, alpha = alpha, color = "#2b5c8f") +
        ggplot2::labs(x = if (is_numeric_axis) "Wavelength" else "Variable", y = "Value")

    } else if (plottype == "boxplot") {
      p <- p +
        ggplot2::geom_boxplot(outlier.shape = NA, fill = "gray95", color = "darkgray") +
        ggplot2::geom_jitter(jitter_aes, width = jitter_width, alpha = alpha, color = "#2b5c8f") +
        ggplot2::labs(x = "Variable", y = "Value")

    } else if (plottype == "violin") {
      p <- p +
        ggplot2::geom_violin(trim = FALSE, fill = "gray95", color = "darkgray") +
        ggplot2::geom_jitter(jitter_aes, width = jitter_width, alpha = alpha, color = "#2b5c8f") +
        ggplot2::labs(x = "Variable", y = "Value")
    }
  }, warning = function(w) {
    if (isTRUE(plotly) && grepl("unknown aesthetics", conditionMessage(w), fixed = TRUE))
      invokeRestart("muffleWarning")
  })

  p <- p + ggplot2::labs(title = main)

  # Handle label angle rotation if specified
  if (angle != 0) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle, hjust = 1))
  }

  # Plotly conversion and axis overrides
  if (isTRUE(plotly)) {
    if (!requireNamespace("plotly", quietly = TRUE))
      stop("Package 'plotly' is required when plotly = TRUE.")

    p <- plotly::ggplotly(p)

    # Configure Plotly.js layout options to prevent x-tick label collision
    is_continuous <- is_numeric_axis || plottype == "line"
    plotly_xaxis <- list(automargin = TRUE)

    if (is_continuous) {
      plotly_xaxis$type <- "linear"
      plotly_xaxis$nticks <- max_ticks
      if (angle != 0) plotly_xaxis$tickangle <- angle
    } else {
      plotly_xaxis$tickmode <- "auto"
      plotly_xaxis$tickangle <- if (angle != 0) angle else -45
    }

    p <- plotly::layout(p, xaxis = plotly_xaxis)
  }

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
#'   \code{"coefficients"}, \code{"validation"}, \code{"rawdata"}.
#' @param ... further arguments passed to the selected plotting function.
#' @return A \code{ggplot} object.
#' @exportS3Method ggplot2::autoplot
#'
#' @examples
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   autoplot(mod, type = "scores")
#'   autoplot(mod, type = "loadings")
#'   autoplot(mod, type = "biplot")
#'   autoplot(mod, type = "prediction")
#'   autoplot(mod, type = "correlation")
#'   autoplot(mod, type = "coefficients")
#'   autoplot(mod, type = "validation", val.type = "RMSEP", estimate = "CV")
#'   autoplot(mod, type = "rawdata", plottype = "line")
#' }
autoplot.mvr <- function(object, type = c("scores", "loadings", "biplot",
                                          "prediction", "correlation",
            "coefficients", "validation", "rawdata"), ...) {
  type <- match.arg(type)
  switch(type,
         scores = gg_scoreplot(object, ...),
         loadings = gg_loadingplot(object, ...),
         biplot = gg_biplot(object, ...),
         prediction = gg_predplot(object, ...),
         correlation = gg_corrplot(object, ...),
    coefficients = gg_coefplot(object, ...),
    validation = gg_valplot(object, ...),
    rawdata = gg_rawdataplot(object, ...))
}

#' @title ggplot-based Dashboard for mvr Models
#'
#' @description
#' Create a multi-panel dashboard from existing ggplot-based model views.
#' The default dashboard uses score, loading, validation (R2), and prediction
#' panels. Alternative panels can be selected with \code{panels}.
#'
#' @param object an \code{mvr} object.
#' @param panels character vector of panel names. Supported panels are
#'   \code{"score"}, \code{"loading"}, \code{"validation"},
#'   \code{"prediction"}, \code{"biplot"}, \code{"corr"},
#'   \code{"coef"} and \code{"rawdata"}.
#' @param plotly logical. If \code{TRUE}, return an HTML dashboard containing
#'   independent Plotly widgets. If \code{FALSE}, return a static dashboard
#'   composed with gridExtra.
#' @param panel_args named list of named lists. Each element provides
#'   arguments passed to the corresponding panel function.
#' @param height positive numeric height in pixels for each Plotly panel.
#' @param link logical. If \code{TRUE} and \code{plotly = TRUE}, the Plotly
#'   panels are linked with \pkg{crosstalk}: selecting samples in the scores,
#'   prediction or rawdata panels highlights the same samples in the other
#'   panels, and selecting variables in the loadings, coefficient or
#'   correlation panels highlights the same variables. The biplot has no
#'   selection of its own and shows both the selected samples and variables.
#'   Spectral line panels select a single item by clicking a line; other
#'   panels use box selection. Double-clicking a panel clears the selection in
#'   all panels.
#'   Ignored when \code{plotly = FALSE}. Requires the \pkg{crosstalk} package.
#' @importFrom utils modifyList
#' @return A composed dashboard object. In static mode, a \code{gtable}
#'   (drawn automatically in interactive sessions, returned invisibly); in
#'   plotly mode, an \code{htmltools} HTML container.
#' @examples
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
#'
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("gridExtra", quietly = TRUE)) {
#'   gg_dashboard(mod, panel_args = list(prediction = list(ncomp = 3)))
#' }
#'
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("plotly", quietly = TRUE)) {
#'   gg_dashboard(mod, plotly = TRUE)
#' }
#' @export
gg_dashboard <- function(object,
                         panels = c("scores", "loadings", "prediction", "validation"),
                         plotly = FALSE,
                         panel_args = list(),
                         height = 300,
                         link = TRUE) {
  if (!inherits(object, "mvr"))
    stop("`object` must be an mvr model.")

  if (!is.logical(plotly) || length(plotly) != 1L || is.na(plotly))
    stop("`plotly` must be a single non-missing logical value.")

  if (!is.numeric(height) || length(height) != 1L ||
      !is.finite(height) || height <= 0)
    stop("`height` must be a single positive finite number.")

  if (!is.logical(link) || length(link) != 1L || is.na(link))
    stop("`link` must be a single non-missing logical value.")

  panels <- as.character(panels)
  if (!length(panels))
    stop("`panels` must contain at least one panel name.")
  if (anyDuplicated(panels))
    stop("`panels` must not contain duplicates.")

  supported_panels <- c("scores", "loadings", "validation", "prediction",
                        "biplot", "corr", "coef", "rawdata")
  unknown_panels <- setdiff(panels, supported_panels)
  if (length(unknown_panels))
    stop("Unknown dashboard panel(s): ", paste(unknown_panels, collapse = ", "))

  if (!is.list(panel_args))
    stop("`panel_args` must be a named list.")
  if (length(panel_args) && is.null(names(panel_args)))
    stop("`panel_args` must be a named list.")

  unknown_args <- setdiff(names(panel_args), supported_panels)
  if (length(unknown_args))
    stop("Unknown panel_args entry(ies): ", paste(unknown_args, collapse = ", "))

  scores_args <- panel_args[["scores"]]
  if (is.list(scores_args) && isTRUE(scores_args$select))
    stop("`select = TRUE` is not supported for the scores panel in gg_dashboard(); ",
         "use `plotly = TRUE` with `link = TRUE` for linked selection.")

  prediction_ncomp <- if (is.null(object$validation)) {
    warning("No validation data available; using 1 component for the prediction panel.")
    1L
  } else {
    rmsep <- RMSEP(object, estimate = "CV")
    estimate_index <- match("CV", dimnames(rmsep$val)[[1]])
    if (is.na(estimate_index)) {
      warning("Could not determine a CV RMSEP minimum; using 1 component for the prediction panel.")
      1L
    } else {
      mean_rmsep <- apply(rmsep$val[estimate_index, , , drop = FALSE], 3, mean)
      comps <- rmsep$comps
      positive <- comps > 0
      if (!any(positive)) {
        warning("Could not determine a positive CV RMSEP component; using 1 component for the prediction panel.")
        1L
      } else {
        as.integer(comps[positive][which.min(mean_rmsep[positive])][1])
      }
    }
  }

  response_names <- respnames(object)
  coef_response <- if (length(response_names) <= 1L) NULL else response_names[1]
  validation_estimate <- if (is.null(object$validation)) "train" else c("train", "CV")

  panel_plots <- lapply(panels, function(panel) {
    defaults <- switch(
      panel,
      scores = list(comps = 1:2),
      loadings = list(comps = 1:2),
      validation = list(val.type = "R2", estimate = validation_estimate),
      prediction = list(ncomp = prediction_ncomp),
      biplot = list(comps = 1:2),
      corr = list(comps = 1:2),
      coef = list(response = coef_response),
      rawdata = list(plottype = "line"),
      stop("Unknown dashboard panel: ", panel)
    )

    panel_override <- panel_args[[panel]]
    if (is.null(panel_override)) {
      panel_override <- list()
    }

    args <- modifyList(defaults, panel_override, keep.null = TRUE)
    if (panel == "scores") {
      args$select <- FALSE
    }

    call <- switch(
      panel,
      scores = gg_scoreplot,
      loadings = gg_loadingplot,
      validation = gg_valplot,
      prediction = gg_predplot,
      biplot = gg_biplot,
      corr = gg_corrplot,
      rawdata = gg_rawdataplot,
      coef = gg_coefplot
    )

    plot <- do.call(call, c(list(object = object), args))
    plot + ggplot2::labs(title = panel)
  })

  if (isTRUE(plotly)) {
    if (!requireNamespace("plotly", quietly = TRUE))
      stop("Package 'plotly' is required when plotly = TRUE.")
    if (!requireNamespace("htmltools", quietly = TRUE))
      stop("Package 'htmltools' is required when plotly = TRUE.")

    if (isTRUE(link) && !requireNamespace("crosstalk", quietly = TRUE))
      stop("Package 'crosstalk' is required when link = TRUE.")

    sample_ids <- NULL
    if (isTRUE(link)) {
      score_mat <- scores(object)
      sample_ids <- rownames(score_mat)
      if (is.null(sample_ids))
        sample_ids <- as.character(seq_len(nrow(score_mat)))
    }

    # plotly's highlight() only clears the last group when a widget has several,
    # so clear every linked group explicitly. plotly keeps the box selection on
    # the traces (selectedpoints), so clear that too or unselected points stay grey.
    reset_js <- "function(el, x, data) {
      ['plotly_doubleclick', 'plotly_deselect'].forEach(function(evt) {
        el.on(evt, function() {
          data.groups.forEach(function(g) {
            crosstalk.group(g).var('selection').set(null);
          });
          // plotly re-applies the box selection after this event, so clear on the next tick.
          setTimeout(function() {
            Plotly.restyle(el, 'selectedpoints', el.data.map(function() { return null; }));
          }, 0);
        });
      });
    }"

    # The biplot has no selection of its own and follows both groups. plotly's
    # highlight() routes every group of a widget to the last one, so the biplot
    # applies each trace's selection itself. The group and keys move to the trace's
    # meta, since plotly's own crosstalk handling would otherwise dim the traces
    # and add subset traces on top.
    biplot_js <- "function(el, x, data) {
      var apply = function() {
        var sel = {};
        data.groups.forEach(function(g) {
          sel[g] = crosstalk.group(g).var('selection').get();
        });
        var selectedpoints = el.data.map(function(t) {
          if (!t.meta) return null;
          var s = sel[t.meta.set];
          if (!s) return null;
          var idx = [];
          t.meta.key.forEach(function(k, i) { if (s.indexOf(k) !== -1) idx.push(i); });
          return idx.length === t.meta.key.length ? null : idx;
        });
        Plotly.restyle(el, 'selectedpoints', selectedpoints);
      };
      data.groups.forEach(function(g) {
        crosstalk.group(g).var('selection').on('change', apply);
      });
    }"

    # Spectra are selected by clicking a line. highlight() would draw the selected
    # part as a polyline through the selected points, which bridges the gaps. Instead,
    # selected traces get a copy with null y values at unselected positions, so
    # plotly draws gaps there, and the original traces are dimmed.
    line_js <- "function(el, x, data) {
      var base = [];
      el.data.forEach(function(t, i) {
        if (t.key && t.set) base.push({ index: i, opacity: t.opacity === undefined ? null : t.opacity });
      });
      var pending = Promise.resolve();
      var apply = function() {
        pending = pending.then(function() {
          var sel = {};
          data.groups.forEach(function(g) {
            sel[g] = crosstalk.group(g).var('selection').get() || [];
          });
          var dimIdx = [];
          var dimOpacity = [];
          var masks = [];
          base.forEach(function(b) {
            var t = el.data[b.index];
            var s = sel[t.set] || [];
            dimIdx.push(b.index);
            dimOpacity.push(s.length ? 0.2 : b.opacity);
            if (!s.length) return;
            var n = t.y.length;
            var runs = [];
            var start = -1;
            t.key.forEach(function(k, j) {
              var kept = s.indexOf(k) !== -1;
              if (kept && start < 0) start = j;
              if (!kept && start >= 0) { runs.push([start, j]); start = -1; }
            });
            if (start >= 0) runs.push([start, n]);
            runs.forEach(function(r) {
              var mask = JSON.parse(JSON.stringify(t));
              Object.keys(mask).forEach(function(p) {
                if (Array.isArray(mask[p]) && mask[p].length === n) mask[p] = mask[p].slice(r[0], r[1]);
              });
              if (b.opacity === null) delete mask.opacity; else mask.opacity = b.opacity;
              mask._plsMask = true;
              mask.showlegend = false;
              mask.hoverinfo = 'skip';
              masks.push(mask);
            });
          });
          var stale = [];
          // plotly's crosstalk adds subset traces that would join separate selected runs
          el.data.forEach(function(t, i) { if (t._plsMask || t._isCrosstalkTrace) stale.push(i); });
          return Plotly.restyle(el, { opacity: dimOpacity }, dimIdx)
            .then(function() {
              return stale.length ? Plotly.deleteTraces(el, stale.reverse()) : null;
            })
            .then(function() {
              return masks.length ? Plotly.addTraces(el, masks) : null;
            });
        });
      };
      data.groups.forEach(function(g) {
        crosstalk.group(g).var('selection').on('change', apply);
      });
      el.on('plotly_click', function(ev) {
        var keys = {};
        ev.points.forEach(function(p) {
          var t = p.data;
          if (!t.set || !t.key) return;
          keys[t.set] = keys[t.set] || [];
          var k = t.key[p.pointNumber];
          if (keys[t.set].indexOf(k) === -1) keys[t.set].push(k);
        });
        Object.keys(keys).forEach(function(g) {
          crosstalk.group(g).var('selection').set(keys[g]);
        });
      });
    }"

    panel_groups <- lapply(panels, function(panel) if (isTRUE(link)) switch(
      panel,
      scores = list(samples = "sample_id"),
      prediction = list(samples = "sample_id"),
      rawdata = list(samples = ".obs_id"),
      biplot = list(samples = "sample_id", variables = "variable"),
      loadings = list(variables = "variable"),
      coef = list(variables = "predictor"),
      corr = list(variables = "variable"),
      list()
    ) else list())
    all_groups <- unique(unlist(lapply(panel_groups, names)))

    widgets <- lapply(seq_along(panel_plots), function(i) {
      panel_plot <- panel_plots[[i]]
      groups <- panel_groups[[i]]
      if (!length(groups))
        return(plotly::ggplotly(panel_plot, height = height))

      key_map <- if (identical(panels[[i]], "rawdata"))
        stats::setNames(sample_ids, paste0("Obs_", seq_along(sample_ids)))
      to_linked <- function(df, column, group) {
        keys <- df[[column]]
        if (!is.null(key_map))
          keys <- unname(key_map[as.character(keys)])
        ggplot2::fortify(crosstalk::SharedData$new(df, key = keys, group = group))
      }
      for (group in names(groups)) {
        column <- groups[[group]]
        if (is.data.frame(panel_plot$data) && column %in% names(panel_plot$data))
          panel_plot$data <- to_linked(panel_plot$data, column, group)
        panel_plot$layers <- lapply(panel_plot$layers, function(layer) {
          if (is.data.frame(layer$data) && column %in% names(layer$data))
            layer$data <- to_linked(layer$data, column, group)
          layer
        })
      }
      # Spectra are drawn as lines with no selectable points, so use click there.
      is_line <- any(vapply(panel_plot$layers, function(layer)
        inherits(layer$geom, "GeomLine"), logical(1)))
      reset_data <- list(groups = I(unname(all_groups)))
      if (identical(panels[[i]], "biplot")) {
        linked <- plotly::ggplotly(panel_plot, height = height)
        linked$x$data <- lapply(linked$x$data, function(trace) {
          if (!is.null(trace$set)) {
            trace$meta <- list(set = trace$set, key = trace$key)
            trace$set <- NULL
            trace$key <- NULL
          }
          trace
        })
        linked <- htmlwidgets::onRender(linked, reset_js, data = reset_data)
        return(htmlwidgets::onRender(linked, biplot_js,
                                     data = list(groups = I(unname(names(groups))))))
      }
      if (is_line) {
        linked <- plotly::ggplotly(panel_plot, height = height)
        linked <- htmlwidgets::onRender(linked, line_js,
                                        data = list(groups = I(unname(names(groups)))))
        return(htmlwidgets::onRender(linked, reset_js, data = reset_data))
      }
      linked <- plotly::highlight(plotly::ggplotly(panel_plot, height = height),
                                  on = "plotly_selected",
                                  off = "plotly_doubleclick")
      htmlwidgets::onRender(linked, reset_js, data = reset_data)
    })
    cells <- lapply(widgets, function(widget) {
      htmltools::tags$div(class = "pls-dashboard-panel", widget)
    })
    return(htmltools::browsable(htmltools::tags$div(
      class = "pls-dashboard",
      style = paste(
        "display: grid;",
        "grid-template-columns: repeat(2, minmax(0, 1fr));",
        "gap: 1rem;",
        "width: 100%;"
      ),
      do.call(htmltools::tagList, cells)
    )))

  }

  if (!requireNamespace("gridExtra", quietly = TRUE))
    stop("Package 'gridExtra' is required for the static dashboard.")

  dashboard <- gridExtra::arrangeGrob(
    grobs = panel_plots,
    ncol = min(2L, length(panel_plots))
  )

  if (interactive()) {
    grid::grid.newpage()
    grid::grid.draw(dashboard)
  }

  invisible(dashboard)
}

# Axis label resolution helper function for ggplot-based plots.
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

# Helper function to resolve point labels across several plot functions.
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
