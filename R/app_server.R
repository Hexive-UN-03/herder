#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyWidgets
#' @import ggplot2
#' @importFrom golem get_golem_options
#' @noRd

app_server <- function(input, output, session, dataset = NULL, vcf_path = NULL) {
  # make upload sizes infinitely large (the server will stop being responsive long before that, but hey if you don't mind waiting who am I to stop you)
  options(shiny.maxRequestSize=Inf)
  # function to reset all category sliders to default
  # absolutely brutal abuse of global variables here, and everywhere in shiny
  reset_all_categories <- function(session){
    # set the global flag to not reset the samples here, only categories (differs from category selection which resets samples)
    reset_samples <<- FALSE
    updateMultiInput(session = session,
                     inputId = "selected_breeds",
                     selected = unique_breeds)
    updateSliderInput(session = session,
                      inputId = "age_range",
                      value = c(as.numeric(unique_ages[which.min(unique_ages)]), as.numeric(unique_ages[which.max(unique_ages)])),
                      min = as.numeric(unique_ages[which.min(unique_ages)]),
                      max = as.numeric(unique_ages[which.max(unique_ages)]))
    updateCheckboxGroupInput(session = session,
                             inputId = "selected_sexes",
                             selected = c("Female", "Ambiguous Male", "Intact Male", "Gelding", "Unknown"))
    updateCheckboxInput(session = session,
                        inputId = "unknown_ages",
                        value = TRUE)
    updateCheckboxInput(session = session,
                        inputId = "infer_ages",
                        value = FALSE)
    updateTextInput(session = session,
                    inputId = "save_name",
                    value = "")
  }

  # general af_plot function
  generate_af_plot <- function(df){
    ggplot(df, aes(x = POS, y = 0, color = AF)) +
      geom_point(size = 5) +
      scale_color_gradient(low = "red", high = "green", name = "Frequency") +
      geom_hline(yintercept = 0, color = "black", size = 0.5) +
      theme_minimal() +
      labs(
        title = "Allele Frequencies",
        x = "Genomic Position",
        y = ""
      ) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  }

  # enforce concordance with the samples actually in the provided vcf
  # make a reactive container to hold the currently available values for samples, as well as current samples for when we need to reset
  allowed_samples <- reactiveValues("samples" = processed_csv$Horse_ID)
  current_samples <- reactiveValues("samples" = processed_csv$Horse_ID)
  # make a vector to hold saved subset vectors
  saved_subsets <- reactiveValues(subsets = list())
  # make a list to track dynamically-created observers
  obs_list <- list()
  reset_samples <- TRUE

  # reactive UI element that allows our sample list to change as we start exclusion criteria for horses we care about
  output$sample_selector <- renderUI({
    multiInput("selected_samples", "Samples:", choices = allowed_samples$samples, width = 1000, selected = current_samples$samples)
  })

  # bind all of the category selection to the category selection process
  bindEvent(observe({
    # make a snipped CSV of just the breeds we like, filtering specifically by whether or not it's in the selected_breeds
    snipped_csv <- processed_csv |> dplyr::filter(Breed %in% input$selected_breeds)
    # same for sexes
    snipped_csv <- snipped_csv |> dplyr::filter(Sex %in% input$selected_sexes)
    # if using inferred ages, merge the inferred ages column into the standard age column wherever a value is missing from the original age column
    if (input$infer_ages == TRUE){
      # this actually works! it's so pretty too! :D
      snipped_csv$Age[is.na(snipped_csv$Age)] <- snipped_csv$inferred_age[is.na(snipped_csv$Age)]
    }
    # now that we have our inferred ages, we'd like to update our slider input to accurately reflect what ages are still available under our current categories
    updateSliderInput(session = session,
                      inputId = "age_range",
                      min = snipped_csv$Age[which.min(snipped_csv$Age)],
                      max = snipped_csv$Age[which.max(snipped_csv$Age)])
    # find any samples that are NAs (should be all of our unknown ages, as all of the known ones are numeric now, doing this before we mess with age categories)
    unknown_age_samples <- snipped_csv |> dplyr::filter(is.na(Age))
    # filter our ages by the currently selected range
    snipped_csv <- snipped_csv |> dplyr::filter(as.numeric(Age) >= input$age_range[1], as.numeric(Age) <= input$age_range[2])
    # check if we want to include unknown ages at all, and include them if so
    if (input$unknown_ages == TRUE){
      snipped_csv <- rbind(unknown_age_samples, snipped_csv)
    }
    # update the allowed samples reactive variable that's used to provide the possible samples within the categories that are selected
    allowed_samples$samples <- snipped_csv$Horse_ID
    if (reset_samples){
      current_samples$samples <- allowed_samples$samples
    }
    reset_samples <<- TRUE
    # sample selector will automatically return to all selected samples every time it re-renders, so we don't need to specifically prompt it
  }), list(input$selected_breeds, input$selected_sexes, input$age_range, input$unknown_ages, input$reselect_samples))

  # clear breeds button
  bindEvent(observe({
    updateMultiInput(
      session = session,
      inputId = "selected_breeds",
      # feed an empty char vector because it can't handle just being fed NULL
      selected = character(0)
    )
  }), input$clear_breeds)
  # clear samples button
  bindEvent(observe({
    updateMultiInput(
      session = session,
      inputId = "selected_samples",
      selected = character(0)
    )
  }), input$clear_samples)
  # reselect breeds button
  bindEvent(observe({
    updateMultiInput(
      session = session,
      inputId = "selected_breeds",
      selected = unique_breeds
    )
  }), input$reselect_breeds)
  # reselect samples button
  bindEvent(observe({
    updateMultiInput(
      session = session,
      inputId = "selected_samples",
      selected = allowed_samples$samples
    )
  }), input$reselect_samples)

  # make a list to store observers for our buttons
  obs_list <- list()
  # needs to be a reactive list when constructing this in this way
  subsets_ui_object <- reactiveValues(ui_list = list())
  # observe when the save button is pressed, and save the currently selected subset into the saved vector
  bindEvent(observe({
    # make sure that the name isn't empty, else do nothing
    if (input$save_name != ""){
      # add the list of selected samples to the saved_subsets object referencing its name as the index
      saved_subsets$subsets[[input$save_name]] <- input$selected_samples
      # make a ui object for the new subset
      subsets_ui_object$ui_list[[input$save_name]] <-
        panel(
          column(
            # not sure why we need [[2]] here to reference the list itself
            h4(input$save_name), h5(paste(length(input$selected_samples), " samples")), width = 6
          ),
          column(
            actionButton(paste("focus", input$save_name, sep = "_"),"Focus Subset"),
            downloadButton(paste("download", input$save_name, sep = "_"),"Download Names"),
            actionButton(paste("delete", input$save_name, sep = "_"),"Delete Subset"), width = 6, align = "right"
          )
          , width = 12)

      current_name <- input$save_name
      # make observers to accompany our new ui buttons with logic
      # HOW DOES THIS WORKKKKKK WTFFFFFF
      # apparently when passing the current_name var it INTERPRETS IT LITERALLY WHEN CREATING THE OBSERVER
      # THIS BREAKS LIKE EVERY UNDERSTANDING I HAVE OF HOW VARIABLES ARE INTERPRETED AT RUNTIME IN LIKE, EVERY LANGUAGE, BUT WHATEVER :)))))))))
      if (is.null(obs_list[[paste("delete", current_name, sep="_")]])){
        obs_list[[paste("delete", current_name, sep="_")]] <<- bindEvent(observe({
          saved_subsets$subsets[[current_name]] <- NULL
          subsets_ui_object$ui_list[[current_name]] <- NULL
          # it is not possible to suicide button the observers, so we just leave them in case they want another panel named the same (MEMORY LEAK BTW :D)
        }), input[[paste("delete", current_name, sep="_")]])
      }
      # make logic for the download button
      output[[paste("download", current_name, sep="_")]] <- downloadHandler(
        filename = function(){paste(current_name, ".txt", sep = "")},
        content = function(file){writeLines(saved_subsets$subsets[[current_name]], con = file)}
      )
      # make logic for the focus button
      if (is.null(obs_list[[paste("focus", current_name, sep="_")]])){
        obs_list[[paste("focus", current_name, sep="_")]] <<- bindEvent(observe({
          # update our multiinput sample selector to be what the focus samples were after resetting all of the categories
          reset_all_categories(session)
          current_samples$samples <- saved_subsets$subsets[[current_name]]
        }), input[[paste("focus", current_name, sep="_")]])
      }
      # reset the text input to be empty
      updateTextInput(session = session, inputId = "save_name", value = "")
      # render the list of objects we have using taglist to reconstruct everything together
      output$saved_subsets_ui <- renderUI({
        do.call(tagList, subsets_ui_object$ui_list)
      })
    }
  }), input$save_button)

  # observer for list upload
  # file upload returns this structure where datapath is a temporary filepath:
  # name       size    type    datapath
  # myfile.txt 1024    text    /tmp/Rtmp1234/myfile.txt
  # extremely sloppy, should really make the new panel ui/logic creation a function, but there's not a very clean way to do that so it's copied from above instead
  bindEvent(observe({
    # someone decided that . was a good character to mean wildcard in R c:
    name <- strsplit(input$list_upload$name, "\\.")[[1]][1]
    uploaded_samples <- readLines(input$list_upload$datapath)
    saved_subsets$subsets[[name]] <- uploaded_samples
    # make a ui object for the new subset
    subsets_ui_object$ui_list[[name]] <-
      panel(
        column(
          # not sure why we need [[2]] here to reference the list itself
          h4(name), h5(paste(length(uploaded_samples), " samples")), width = 6
        ),
        column(
          actionButton(paste("focus", name, sep = "_"),"Focus Subset"),
          downloadButton(paste("download", name, sep = "_"),"Download Names"),
          actionButton(paste("delete", name, sep = "_"),"Delete Subset"), width = 6, align = "right"
        )
        , width = 12)

    current_name <- name
    if (is.null(obs_list[[paste("delete", current_name, sep="_")]])){
      obs_list[[paste("delete", current_name, sep="_")]] <<- bindEvent(observe({
        saved_subsets$subsets[[current_name]] <- NULL
        subsets_ui_object$ui_list[[current_name]] <- NULL
      }), input[[paste("delete", current_name, sep="_")]])
    }
    # make logic for the download button
    output[[paste("download", current_name, sep="_")]] <- downloadHandler(
      filename = function(){paste(current_name, ".txt", sep = "")},
      content = function(file){writeLines(saved_subsets$subsets[[current_name]], con = file)}
    )
    # make logic for the focus button
    if (is.null(obs_list[[paste("focus", current_name, sep="_")]])){
      obs_list[[paste("focus", current_name, sep="_")]] <<- bindEvent(observe({
        # update our multiinput sample selector to be what the focus samples were after resetting all of the categories
        reset_all_categories(session)
        current_samples$samples <- saved_subsets$subsets[[current_name]]
      }), input[[paste("focus", current_name, sep="_")]])
    }
    output$saved_subsets_ui <- renderUI({
      do.call(tagList, subsets_ui_object$ui_list)
    })
  }), input$list_upload)

  allele_freq_df <- NULL
  snipped_af_df <- NULL
  filtered_af_df <- NULL
  # CALCULATE ALLELE FREQUENCIES IN A REGION
  # !!!!!!!!!!!! Handle making the user comfy and safe later! !!!!!!!!!!!!!!!
  bindEvent(observe({
    if (input$roi_text != ""){
      roi <- input$roi_text
      updateTextInput(session = session, inputId = "roi_text", value = "")
      writeLines(as.character(input$selected_samples), "./output/samples.txt")
      # run a call to fast.af
        # fast_af.sh vcf_path region samplefile_path output_path
      system(paste("./scripts/fast_af", norm_vcf_path, roi, 8, "./output/samples.txt", "./output/roi_af.tsv", sep = " "))
      # withProgress(message = "Calculating Allele Frequencies...", value = 0, {
      #   incProgress(1/n, detail = paste("Finished Subregion ", i))
      # })
      allele_freq_df <<- read.table("./output/roi_af.tsv", sep = "\t", header = TRUE)
      output$calc_display <- renderText(paste("Current Calculated Region:\n", roi, sep = ""))
    }
  }), input$calculate_roi)

  # VIEWING ALLELE FREQUENCIES IN A VIEW REGION
  bindEvent(observe({
    if (input$roi_view_text != "" & !is.null(allele_freq_df)){
      session$resetBrush("af_brush")
      roi <<- strsplit(input$roi_view_text,  "-")
      snipped_af_df <<- allele_freq_df |> dplyr::filter((POS >= as.numeric(roi[[1]][1])), (POS <= as.numeric(roi[[1]][2])), as.numeric(AF) >= (input$af_cutoff_slider[1] / 100), as.numeric(AF) <= (input$af_cutoff_slider[2] / 100))
      output$af_plot <- renderPlot({
        generate_af_plot(snipped_af_df) +
          xlim(as.numeric(roi[[1]][1]), as.numeric(roi[[1]][2]))
      })
      output$af_zoom_plot <- renderPlot({
        # render nothing at all, since we just reset the view
      })
      updateTextInput(session = session, inputId = "roi_view_text", value = "")
    }else if (is.null(allele_freq_df)){
      sendSweetAlert(
        session = session,
        title = "Error: no region",
        text = "No region has been calculated yet. Make sure to calculate or upload a region before attempting to view it.",
        type = "error"
      )
    }
  }), input$view_roi)

  # cutoff allele frequency slider logic
  bindEvent(observe({
    output$af_zoom_plot <- renderPlot({
      if (!is.null(input$af_brush$xmin)){
        generate_af_plot(snipped_af_df) +
          xlim(input$af_brush$xmin, input$af_brush$xmax) +
          labs(title = "Zoomed Allele Frequencies")
      }
      # silent else here to pass nothing to the plot output if we don't have a brush, which will vanish it
    })
  }), input$af_brush)

  # display specific af click info
  bindEvent(observe({
    # we need to make a spoof Y var here otherwise nearpoints will have an aneurysm trying to figure out what Y value we want
    snipped_af_df$Y <- 0
    output$af_info_text <- renderPrint({
      near_df <- nearPoints(snipped_af_df, input$af_zoom_click, xvar = "POS", yvar = "Y", threshold = 10)
      if (length(near_df[[1]]) > 0){
        # make the names more readable and output to user with verbatim text output
        colnames(near_df)[colnames(near_df) == "POS"] <- "Position"
        colnames(near_df)[colnames(near_df) == "AF"] <- "Allele_Frequency"
        colnames(near_df)[colnames(near_df) == "REF"] <- "Ref_Base"
        colnames(near_df)[colnames(near_df) == "ALT"] <- "Variant"
        print(near_df[c("Position", "Ref_Base", "Variant", "Allele_Frequency")], row.names = FALSE)
      }
    })
  }), input$af_zoom_click)
  
  bindEvent(observe({
    if (input$save_name != ""){
      writeLines(as.character(input$selected_samples), "./output/samples.txt")
      system(paste("./scripts/vcf_trimmer", norm_vcf_path, "./output/samples.txt", paste(dirname(norm_vcf_path), "/", input$save_name, ".vcf.gz", sep = ""), sep = " "))
      updateTextInput(session = session, inputId = "save_name", value = "")
    }
  }), input$vcf_generate_button)

  # slider cutoff logic, using a filtered df to pass to the other plots
  bindEvent(observe({
    if (!is.null(allele_freq_df)){
      snipped_af_df <<- allele_freq_df |> dplyr::filter(as.numeric(AF) >= (input$af_cutoff_slider[1] / 100), as.numeric(AF) <= (input$af_cutoff_slider[2] / 100), (POS >= as.numeric(roi[[1]][1])), (POS <= as.numeric(roi[[1]][2])))
      output$af_plot <- renderPlot({
        generate_af_plot(snipped_af_df) +
          xlim(as.numeric(roi[[1]][1]), as.numeric(roi[[1]][2]))
      })
      output$af_zoom_plot <- renderPlot({
        if (!is.null(input$af_brush$xmin)){
          generate_af_plot(snipped_af_df) +
            xlim(input$af_brush$xmin, input$af_brush$xmax) +
            labs(title = "Zoomed Allele Frequencies")
        }
      })
    }
  }), input$af_cutoff_slider)

  output$entire_roi_download <- downloadHandler(
    filename = "whole_region_of_interest.tsv",
    content = function(file){write.table(allele_freq_df, sep = "\t", file = file, row.names = F, quote = FALSE)}
  )

  output$little_roi_download <- downloadHandler(
    filename = "zoomed_filtered_region.tsv",
    content = function(file){
      if (!is.null(input$af_brush$xmin)){
        download_df <- snipped_af_df |> dplyr::filter(as.numeric(POS) >= input$af_brush$xmin, as.numeric(POS) <= input$af_brush$xmax)
      }else{
        download_df <- snipped_af_df
      }
      write.table(download_df, sep = "\t", file = file, row.names = F, quote = FALSE)}
  )

  bindEvent(observe({
    allele_freq_df <<- read.table(input$af_upload$datapath, sep = "\t", header = TRUE)
    output$calc_display <- renderText(paste("Current Calculated Region:\n", "upload:", min(allele_freq_df$POS), "-", max(allele_freq_df$POS), sep = ""))
  }), input$af_upload)
}
