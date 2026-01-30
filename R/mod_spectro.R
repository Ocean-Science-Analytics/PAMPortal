#' spectro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinycssloaders
#' @import seewave
#' @import tuneR
#' @import ggplot2
#' @import tidyr
mod_spectro_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      # tags$script(HTML("
      #   Shiny.addCustomMessageHandler('stopAudio', function(message) {
      #     var audioEl = document.getElementById(message.id);
      #     if (audioEl && !audioEl.paused) {
      #       audioEl.pause();
      #       audioEl.currentTime = 0;
      #     }
      #   });
      # ")),
      
      tags$style(HTML("
        .full-height {
          height: 100vh;
          display: flex;
          flex-direction: column;
        }
        .custom-btn {
          background-color: #00688B !important;
          color: white !important;
          border-color: black !important;
        }
        .custom-btn:hover {
          background-color: lightskyblue !important;
          transform: scale(1.05);
          cursor: pointer;
        }
      "))
    ),
    div(id = ns("spectro_card"),
      card_spectro(ns, "spectro1", 1)
    ),
    card_spectro(ns, "spectro2", 2)
  )
}
    
#' spectro Server Functions
#'
#' @noRd 
mod_spectro_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    library(uuid)
    base_path <- reactive({
      req(data$selected_dir())
    })
    tree <- reactive(data$acoustic_file_tree())
    
    audio_dir <- file.path(tempdir(), "audio")
    dir.create(audio_dir, showWarnings = FALSE, recursive = TRUE)
    shiny::addResourcePath("temp_audio", audio_dir)
    
    # AUDIO FILE DELETION LOGIC
    # session$onSessionEnded(function() {
    #   unlink(audio_dir, recursive = TRUE, force = TRUE)
    # })
    
    for (i in 1:2) {
      local({
        index <- i
        
        spectro_cache <- reactiveVal(NULL)
        
        locationInput <- paste0("location_", index)
        speciesInput <- paste0("species_", index)
        folderInput <- paste0("folder_", index)
        fileInput <- paste0("file_", index)
        plotOutput <- paste0("plot_", index)
        
        observeEvent(tree(), {
          updateSelectInput(session, locationInput, choices = names(tree()))
        })
        
        observeEvent(input[[locationInput]], {
          loc <- input[[locationInput]]
          updateSelectInput(session, speciesInput, choices = names(tree()[[loc]]))
        }, ignoreInit = TRUE)
        
        observeEvent(input[[speciesInput]], {
          loc <- input[[locationInput]]
          sp <- input[[speciesInput]]
          updateSelectInput(session, folderInput, choices = names(tree()[[loc]][[sp]]))
        }, ignoreInit = TRUE)
        
        observeEvent(input[[folderInput]], {
          loc <- input[[locationInput]]
          sp <- input[[speciesInput]]
          fldr <- input[[folderInput]]
          wavs <- tree()[[loc]][[sp]][[fldr]]
          
          # Display just filenames, keep full paths as values
          named_wavs <- setNames(wavs, basename(wavs))
          
          updateSelectInput(session, fileInput, choices = named_wavs)
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0("render_", index)]], {
          req(input[[fileInput]])
          
          # Window length
          wav_path_val <- input[[fileInput]]
          
          wl_val <- input[[paste0("wl_", index)]]
          if (is.null(wl_val) || is.na(wl_val)) wl_val <- 1024
          
          # Overlap Percentage
          overlap_val <- input[[paste0("overlap_", index)]]
          if (is.null(overlap_val) || is.na(overlap_val)) {
            overlap_val <- 70
          }
          
          # Dynamic Range
          dyn_range_val <- input[[paste0("dyn_range_", index)]]
          if (is.null(dyn_range_val) || is.na(dyn_range_val)) {
            dyn_range_val <- 60
          }
          
          # === Find event name from folder selection ===
          selected_loc <- input[[locationInput]]
          selected_name <- input[[folderInput]]
          
          # Build comments path
          comments_path <- file.path(base_path(), "Audio", selected_loc, paste0(selected_loc, "_species_list.csv"))
          
          if (file.exists(comments_path)) {
            comments_df <- read.csv(comments_path, stringsAsFactors = FALSE)
            comments_df <- comments_df[, c("Event", "Description", "Analyst_Comments")]
          } else {
            comments_df <- data.frame(
              Event = character(0), 
              Description = character(0), 
              Analyst_Comments = character(0)
            )
          }
          
          # Match current Event row (if exists)
          current_desc <- ""
          current_analysis <- ""
          
          if (nrow(comments_df) > 0) {
            row_match <- comments_df[comments_df$Event == selected_name, ]
            if (nrow(row_match) > 0) {
              current_desc <- row_match$Description[1]
              current_analysis <- row_match$Analyst_Comments[1]
            }
          }
          
          # Render description + analysis to UI
          output[[paste0("description_", index)]] <- renderText({
            current_desc
          })
          output[[paste0("analysis_", index)]] <- renderText({
            current_analysis
          })
          
          # This collects the audio file
          unique_tag <- gsub("-", "", uuid::UUIDgenerate())
          audio_name <- paste0("audio_", index, "_", unique_tag, ".wav")
          temp_audio_path <- file.path(audio_dir, audio_name)
          
          ####!!!!!!!!!!!!!!!!!!!
          # Stop any playing audio first
          session$sendCustomMessage("stopAudio", list(id = ns(paste0("audio_element_", index))))
          
          later::later(function() {
            
            if (!dir.exists(audio_dir)) {
              dir.create(audio_dir, showWarnings = FALSE, recursive = TRUE)
            }
            
            # remove old file for this index (optional) - removes any previous audio_N_*.wav for this index
            old_files <- list.files(audio_dir, pattern = paste0("^audio_", index, "_.*\\.wav$"), full.names = TRUE)
            if (length(old_files) > 0) file.remove(old_files)
            
            # copy into unique file
            file.copy(from = wav_path_val, to = temp_audio_path, overwrite = TRUE)
            
            # Now render the audio player and spectrogram; add a cache-busting query param
            output[[paste0("audio_", index)]] <- renderUI({
              tagList(
                tags$audio(
                  id = ns(paste0("audio_element_", index)),
                  controls = TRUE,
                  style = "width: 50%; margin-top: 5px;",
                  # append timestamp query so browser always re-fetches the correct file
                  tags$source(src = paste0(file.path("temp_audio", audio_name), "?v=", unique_tag), type = "audio/wav"),
                  "Your browser does not support the audio element."
                )
              #   tags$script(HTML(sprintf("
              #   setTimeout(function() {
              #     const audio = document.getElementById('%s');
              #     const plotDiv = document.getElementById('%s');
              # 
              #     if (audio && plotDiv) {
              #       audio.addEventListener('timeupdate', function () {
              #         const currentTime = audio.currentTime;
              #         Plotly.relayout(plotDiv, {
              #           'shapes[0].x0': currentTime,
              #           'shapes[0].x1': currentTime
              #         });
              #       });
              #     }
              #   }, 500);
              # ", ns(paste0("audio_element_", index)), ns(paste0("plot_", index))))
              #   )
              )
            })
          }, delay = 0.1)
          
          
          # Show the spinner immediately by rendering plot_ui_ before starting the processing
          output[[paste0("plot_ui_", index)]] <- renderUI({
              shinycssloaders::withSpinner(
              plotlyOutput(ns(plotOutput), height = "100%"),
              type = 4, color = "#001f3f", size = 1
            )
          })
          
          later::later(function() {

            wave <- tuneR::readWave(wav_path_val)

            if (wave@samp.rate < 8000) {
              showNotification(
                "Sampling rate of the recorded audio file is too low.",
                type = "warning",
                duration = 8,
                session = session
              )
            }

            # ---- COMPUTE SPECTROGRAM ONCE ----
            spect <- seewave::spectro(
              wave,
              wl = wl_val,
              ovlp = overlap_val,
              zp = 2,
              plot = FALSE
            )

            colnames(spect$amp) <- spect$time
            rownames(spect$amp) <- spect$freq

            spect_df <- spect$amp |>
              tibble::as_tibble(rownames = "freq") |>
              tidyr::pivot_longer(-freq, names_to = "time", values_to = "amp") |>
              dplyr::mutate(
                freq = as.numeric(freq),
                time = as.numeric(time)
              )

            # Cache Spectrogram
            spectro_cache(spect_df)

            # ---- INITIAL PLOT ----
            zmax <- max(spect_df$amp, na.rm = TRUE)
            zmin <- zmax - dyn_range_val

            output[[plotOutput]] <- renderPlotly({
              
              plot_ly(
                data = spect_df,
                x = ~time,
                y = ~freq,
                z = ~amp,
                type = "heatmap",
                colorscale = "Jet",
                zmin = zmin,
                zmax = zmax,
                colorbar = list(
                  title = "Amplitude (dB)",
                  titleside = "right",
                  tickfont  = list(color = "white"),
                  titlefont = list(color = "white")
                ),
                hovertemplate = paste(
                  "Time: %{x:.3f} s<br>",
                  "Freq: %{y:.1f} kHz<br>",
                  "Amp: %{z:.1f} dB<extra></extra>"
                ),
                source = paste0("spectro_", index)
              ) |>
                layout(
                  xaxis = list(
                    title = "Time (s)",
                    titlefont = list(size = 14, color = "white"),
                    tickfont  = list(size = 12, color = "white"),
                    tickcolor = "white",
                    linecolor = "white",
                    mirror    = TRUE
                  ),
                  yaxis = list(
                    title = "Frequency (kHz)",
                    titlefont = list(size = 14, color = "white"),
                    tickfont  = list(size = 12, color = "white"),
                    tickcolor = "white",
                    linecolor = "white",
                    mirror    = TRUE
                  ),
                  paper_bgcolor = "#001f3f",
                  plot_bgcolor  = "#001f3f",
                  margin = list(t = 25, r = 25, b = 55, l = 35),
                  showlegend = FALSE
                ) |>
                style(
                  hoverlabel = list(
                    bgcolor = "white",
                    font = list(color = "black")
                  )
                )
            })

          }, delay = 0.1)
        })
        
        observeEvent(input[[paste0("dyn_range_", index)]], {

          spect_df <- spectro_cache()
          req(spect_df)

          zmax <- max(spect_df$amp, na.rm = TRUE)
          zmin <- zmax - input[[paste0("dyn_range_", index)]]

          plotlyProxy(
            outputId = plotOutput,
            session = session
          ) |>
            plotlyProxyInvoke(
              "restyle",
              list(zmin = zmin, zmax = zmax),
              0
            )
        })
      })
    }
  })
}
    
# seewave - https://rug.mnhn.fr/seewave/
# spectro - https://www.rdocumentation.org/packages/seewave/versions/2.2.3/topics/spectro
# ggspectro - https://www.rdocumentation.org/packages/seewave/versions/2.2.3/topics/ggspectro

## To be copied in the UI
# mod_spectro_ui("spectro_1")
    
## To be copied in the server
# mod_spectro_server("spectro_1")
