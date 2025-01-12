#' Run the Shiny Application
#'
#' @param dataset A metadata sheet with sample information
#' @param vcf_path A path to the vcf in question
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  dataset = NULL,
  vcf_path = NULL,
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui(dataset = dataset, vcf_path = vcf_path),
      server = function(input, output, session) {
        app_server(input, output, session, dataset = dataset, vcf_path = vcf_path, ...)
      },
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(dataset = dataset, vcf_path = vcf_path, ...)
  )
}
