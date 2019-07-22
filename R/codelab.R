#' Convert to a codelab
#'
#' Format for converting from R Markdown to a codelab-like HTML document.
#'
#' @import rmarkdown
#' @inheritParams rmarkdown::html_document
#'
#' @param template Pandoc template to use for rendering. Pass "default" to use
#'   the package default template; pass \code{NULL} to use pandoc's built-in
#'   template; pass a path to use custom template that you've created. Note that
#'   if you don't use the "default" template then some features of
#'   \code{codelab} won't be available.
#' @param extra_dependencies Additional function arguments to pass to the base R
#'   Markdown HTML output formatter [rmarkdown::html_document_base()].
#' @param ... Ignored
#'
#' @return R Markdown output format to pass to \code{\link{render}}
#'
#' @examples
#' \dontrun{
#'
#' library(rmarkdown)
#' library(codelabr)
#'
#' render("codelab.Rmd", codelab())
#' }
#'
#' @export
codelab <- function(incremental = FALSE,
                    toc = TRUE,
                    toc_depth = 2,
                    self_contained = TRUE,
                    fig_width = 6,
                    fig_height = 6,
                    fig_retina = if (!fig_caption) 2,
                    fig_caption = TRUE,
                    smart = TRUE,
                    highlight = "default",
                    mathjax = "default",
                    template = "default",
                    css = NULL,
                    includes = NULL,
                    keep_md = FALSE,
                    lib_dir = NULL,
                    pandoc_args = NULL,
                    extra_dependencies = NULL,
                    ...) {

  # base pandoc options
  args <- c()

  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc_depth))

  # template path and assets
  if (identical(template, "default")) {
    default_template <- system.file(
      "rmarkdown/templates/codelab/resources/default.html",
      package = "codelabr"
    )
    args <- c(args, "--template", pandoc_path_arg(default_template))
  } else if (!is.null(template)) {
    args <- c(args, "--template", pandoc_path_arg(template))
  }

  # incremental
  if (incremental) {
    args <- c(args, "--incremental")
  }

  # use history
  args <- c(args, pandoc_variable_arg("history", "true"))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # additional css
  for (css_file in css) {
    args <- c(args, "--css", pandoc_path_arg(css_file))
  }

  # pre-processor for arguments that may depend on the name of the input file
  # (e.g ones that need to copy supporting files)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {

    # we don't work with runtime shiny
    if (identical(runtime, "shiny")) {
      stop("codelab is not compatible with runtime 'shiny'", call. = FALSE)
    }

    # use files_dir as lib_dir
    if (is.null(lib_dir)) {
      lib_dir <- files_dir
    }

    # extra args
    args <- c()

    # codelab
    codelab_path <- system.file("codelab", package = "codelabr")
    if (!self_contained || identical(.Platform$OS.type, "windows")) {
      codelab_path <- relative_to(
        output_dir, render_supporting_files(codelab_path, lib_dir))
    } else {
      codelab_path <- pandoc_path_arg(codelab_path)
    }
    args <- c(args, "--variable", paste0("codelab-url=", codelab_path))

    # highlight
    args <- c(args, pandoc_highlight_args(highlight, default = "pygments"))

    # return additional args
    args
  }

  # return format
  output_format(
    knitr = knitr_options_html(fig_width, fig_height, fig_retina, keep_md),
    pandoc = pandoc_options(to = "html5",
                            from = rmarkdown_format(ifelse(fig_caption,
                                                           "",
                                                           "-implicit_figures")),
                            args = args),
    keep_md = keep_md,
    clean_supporting = self_contained,
    pre_processor = pre_processor,
    base_format = html_document_base(smart = smart, lib_dir = lib_dir,
                                     self_contained = self_contained,
                                     mathjax = mathjax,
                                     pandoc_args = pandoc_args,
                                     extra_dependencies = extra_dependencies,
                                     ...))
}
