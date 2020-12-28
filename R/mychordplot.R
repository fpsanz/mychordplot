#' <Add Title>
#' data are columns pathway and genes from enrichment object
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
mychordplot <- function(data, width = NULL, height = NULL, elementId = NULL, div="chartdiv" ) {
  require("tidyverse")
  names(data) <- c("Pathway","Genes")
  data2 <- data %>% separate_rows(Genes, convert = TRUE)
  data2 <- tidyr::pivot_wider(data2, names_from = Pathway, values_from = Genes)
  k <- data.frame( gtools::combinations(length(names(data2)), v = names(data2), r=2) , stringsAsFactors = F)
  values <- apply(k, MARGIN = 1, function(x){ length(intersect( unlist(data2[1, x[1]]), unlist(data2[1, x[2]])   )) } )
  k$value = values
  k <- k %>% filter(value!=0)
  names(k)<- c("from","to","value")
  
  colours <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(length(data$Pathway))
  col <- data.frame(from = data$Pathway, nodeColor = colours)
  col <- jsonlite::toJSON(col)

  jsonk <- jsonlite::toJSON(k)
  div <- div
  # forward options using x
  x = list(
    data = jsonk,
    color = col,
    div = div
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
