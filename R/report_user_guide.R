#' Knit the dashboard reader's guide
#'
#' Renders \code{inst/guide/immuta_accounts_guide.Rmd} — the reader's guide
#' for \code{immuta_accounts.html} — to HTML and/or PDF. The HTML version is
#' what \code{\link{report_all_accounts_html}} embeds in the dashboard's
#' Documentation tab, so render it first in a build (see
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

#' Knit the Customer Success Doctrine page
#'
#' Renders \code{inst/guide/cs_doctrine.Rmd} — the Immuta CS Doctrine 2.0, a
#' static copy of the Google Doc "Customer_Success_Doctrine_v2.0" — to
#' \code{cs_doctrine.html}. \code{\link{report_all_accounts_html}} embeds it,
#' with the reader's guide, in the dashboard's Documentation tab, so render it
#' first in a build (see \code{scripts/main.R}). The text is not synced with the
#' Google Doc: edit the .Rmd when the doctrine changes. Styling is shared with
#' the guide (\code{inst/guide/c360_doc.css}).
#'
#' @param output_dir Character. Where to write the rendered file. Default
#'   \code{"output"}.
#'
#' @return Invisibly, the path of the rendered file.
#' @examples
#' \dontrun{
#'   report_cs_doctrine()                    # output/cs_doctrine.html
#' }
#' @export
report_cs_doctrine <- function(output_dir = "output") {
  .render_guide_page("cs_doctrine", output_dir, "CS doctrine")
}

#' Knit the CSM Reporting Requirements page
#'
#' Renders \code{inst/guide/csm_reporting.Rmd} — the "CSM Consolidated
#' Reporting Requirements" part of the CS Doctrine Google Doc, kept as its own
#' page — to \code{csm_reporting.html}. \code{\link{report_all_accounts_html}}
#' embeds it as the CSM Reporting sub-tab of the dashboard's Documentation tab,
#' so render it first in a build (see \code{scripts/main.R}). Not synced with
#' the Google Doc: edit the .Rmd.
#'
#' @inheritParams report_cs_doctrine
#' @return Invisibly, the path of the rendered file.
#' @examples
#' \dontrun{
#'   report_csm_reporting()                  # output/csm_reporting.html
#' }
#' @export
report_csm_reporting <- function(output_dir = "output") {
  .render_guide_page("csm_reporting", output_dir, "CSM reporting requirements")
}

# Knit inst/guide/<name>.Rmd to <output_dir>/<name>.html. Shared by the
# Documentation-tab pages (the user guide has its own PDF option, so it doesn't).
.render_guide_page <- function(name, output_dir, what) {
  src <- system.file("guide", paste0(name, ".Rmd"), package = "ImmuteR")
  if (!nzchar(src)) stop("Source inst/guide/", name, ".Rmd not found.")

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  output_dir <- normalizePath(output_dir)

  path <- rmarkdown::render(
    input       = src,
    output_file = name,
    output_dir  = output_dir,
    clean       = TRUE,
    envir       = new.env(),
    quiet       = TRUE
  )
  message(".. wrote ", what, " to ", path)
  invisible(path)
}
