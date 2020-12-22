#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
mychordplot <- function(message, data, width = NULL, height = NULL, elementId = NULL) {
  jsonk <- jsonlite::toJSON(data)
  namesColors <- data$from
  namesColors <- jsonlite::toJSON( namesColors)
  # forward options using x
  x = list(
    message = message,
    data = jsonk,
    colors = namesColors
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'mychordplot',
    x,
    width = width,
    height = height,
    package = 'mychordplot',
    elementId = elementId
  )
}

#' Shiny bindings for mychordplot
#'
#' Output and render functions for using mychordplot within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a mychordplot
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name mychordplot-shiny
#'
#' @export
mychordplotOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'mychordplot', width, height, package = 'mychordplot')
}

#' @rdname mychordplot-shiny
#' @export
renderMychordplot <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, mychordplotOutput, env, quoted = TRUE)
}
