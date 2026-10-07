#' Knit the dashboard reader's guide
#'
#' Renders \code{inst/guide/immuta_accounts_guide.Rmd} — the reader's guide
#' for \code{immuta_accounts.html} — to HTML and/or PDF. The HTML version is
#' what \code{\link{report_all_accounts_html}} embeds behind the dashboard's
#' "User Guide and Definitions" button, so render it first in a build (see
#' \code{scripts/main.R}). The PDF is for sharing outside the dashboard.
#'
#' Scoring weights, thresholds and rating labels in the guide are read from
#' the package constants at knit time (\code{.PBAC_COMPLEXITY_WEIGHTS},
#' \code{.REQUEST_ADOPTION_POINTS}, \code{.PBAC_ADOPTION_RATINGS}, ...), so
#' re-knitting after a scoring change keeps the guide in step. Edit the prose
#' in the .Rmd, never the rendered files.
#'
#' @param output_dir Character. Where to write the rendered files. Default
#'   \code{"output"}.
#' @param formats Character vector, any of \code{"html"} and \code{"pdf"}.
#'   Default \code{"html"}. PDF needs a LaTeX install with xelatex (e.g.
#'   \code{tinytex::install_tinytex()}).
#'
#' @return Invisibly, the paths of the rendered files, named by format.
#' @examples
#' \dontrun{
#'   report_user_guide()                     # output/immuta_accounts_guide.html
#'   report_user_guide(formats = "pdf")      # output/immuta_accounts_guide.pdf
#' }
#' @export
report_user_guide <- function(output_dir = "output", formats = "html") {
  src <- system.file("guide", "immuta_accounts_guide.Rmd", package = "ImmuteR")
  if (!nzchar(src)) stop("Guide source inst/guide/immuta_accounts_guide.Rmd not found.")

  formats <- match.arg(formats, c("html", "pdf"), several.ok = TRUE)
  fmt_name <- c(html = "html_document", pdf = "pdf_document")

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_dir <- normalizePath(output_dir)

  out <- vapply(formats, function(f) {
    path <- rmarkdown::render(
      input             = src,
      output_format     = fmt_name[[f]],
      output_file       = "immuta_accounts_guide",
      output_dir        = output_dir,
      clean             = TRUE,   # drop the knitted .md / LaTeX aux files
      envir             = new.env(),
      quiet             = TRUE
    )
    message(".. wrote reader's guide to ", path)
    path
  }, character(1))

  invisible(out)
}
