#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import here
#' @importFrom golem get_golem_options
#' @noRd

app_ui <- function(request, dataset = NULL, vcf_path = NULL) {
  norm_dataset <<- normalizePath(dataset)
  norm_vcf_path <<- normalizePath(vcf_path)
  print(getwd())
  setwd(system.file(package = "herder"))
  print(getwd())
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    # we can run this here, but I'm not sure what the consequences of it are really. I think the documentation recommends putting this in the server object, but it doesn't work there as the server object runs after this, and the UI has dependencies in here
    process_csv(norm_dataset, norm_vcf_path),
    ui <- navbarPage(
      # first argument is a title for the navbar at the top of the page
      "Herder",
      # make a navbar tab that drops down to offer both manual selection and uploading a list of sample names
      navbarMenu("Select Subset",
                 tabPanel("Manual Selection",
                          fluidRow(
                            column(
                              # just max the width parameter here to make it fill as much as possible, give it a width of 6 for the column so it's half (6/12)
                              # makes a multi-input option using the breeds pulled from the metadata file
                              multiInput("selected_breeds", "Breeds:", choices = unique_breeds, width = 1000, selected = unique_breeds), width = 6, align = "center"
                            ),
                            column(
                              uiOutput("sample_selector"), width = 6, align = "center"
                            )
                          ),
                          fluidRow(
                            column(
                              actionButton("clear_breeds", "Deselect all breeds"), width = 3, align = "center"
                            ),
                            column(
                              actionButton("reselect_breeds", "Reselect all breeds"), width = 3, align = "center"
                            ),
                            column(
                              actionButton("clear_samples", "Deselect all samples"), width = 3, align = "center"
                            ),
                            column(
                              actionButton("reselect_samples", "Reselect all samples"), width = 3, align = "center"
                            )
                          ),
                          fluidRow(
                            column(
                              checkboxInput("unknown_ages", "Include unknown ages", value = TRUE), width = 4
                            ),
                            column(
                              checkboxInput("infer_ages", "Infer unknown ages by sex category?", value = FALSE), width = 8
                            )
                          ),
                          fluidRow(
                            column(
                              sliderInput("age_range", "Age Range:", value = c(as.numeric(unique_ages[which.min(unique_ages)]), as.numeric(unique_ages[which.max(unique_ages)])), min = as.numeric(unique_ages[which.min(unique_ages)]), max = as.numeric(unique_ages[which.max(unique_ages)]), step = 0.5), width = 4
                            ),
                            column(
                              # only these categories should be needed, as age is handled by the slider to the left
                              checkboxGroupInput("selected_sexes", "Included Sexes:", choices = c("Female", "Ambiguous Male", "Intact Male", "Gelding", "Unknown"), selected = c("Female", "Ambiguous Male", "Intact Male", "Gelding", "Unknown"), inline = TRUE), width = 8
                            )
                          ),
                          fluidRow(
                            column(
                              textInput("save_name", label = NULL, placeholder = "Subset name"), actionButton("save_button", "Save Subset"), actionButton("vcf_generate_button", "Generate VCF"), align = "center", width = 12
                            )
                          )
                 ),
                 tabPanel("Upload Sample List",
                          fileInput("list_upload", "Upload a list of sample names")
                 )
      ),
      # a tab to display all of the currently saved panels
      tabPanel("Saved Subsets",
               uiOutput("saved_subsets_ui")
      ),
      tabPanel("Allele Frequencies",
               fluidRow(
                 column(
                   textInput("roi_text", label = "Enter your region of interest:", placeholder = "chr3:1-10000000"), actionButton("calculate_roi", "Calculate Allele Frequencies"), align = "right", width = 4
                 ),
                 column(
                   verbatimTextOutput("calc_display"), fileInput("af_upload", "Upload a region af.tsv"), width = 4, align = "center"
                 ),
                 column(
                   textInput("roi_view_text", label = "Enter your region to view:", placeholder = "1-10000"), actionButton("view_roi", "View Allele Frequencies"), align = "left", width = 4
                 )
               ),
               fluidRow(
                 column(
                   plotOutput("af_plot", brush = brushOpts("af_brush"), height = "1.25in"), width = 12
                 ),
                 column(
                   plotOutput("af_zoom_plot", click = "af_zoom_click", height = "1.25in"), width = 12
                 )
               ),
               fluidRow(
                 column(
                   sliderInput("af_cutoff_slider", label = "Frequency Cutoff Percent", value = c(0, 100), min = 0, max = 100,step = 0.5), width = 4
                 ),
                 column(
                   verbatimTextOutput("af_info_text"), width = 8
                 )
               ),
               fluidRow(
                 column(
                   downloadButton("entire_roi_download", label = "Download entire calculated region"), width = 6, align = "right"
                 ),
                 column(
                   downloadButton("little_roi_download", label = "Download zoomed/filtered region"), width = 6, align = "left"
                 )
               )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Herder"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
