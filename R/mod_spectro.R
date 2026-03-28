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
          width: 100%;
          transition: background-color 0.2s ease, box-shadow 0.2s ease;
        }
        .custom-btn:hover {
          background-color: lightskyblue !important;
          box-shadow: 0 0 0 3px rgba(135, 206, 250, 0.6);
          cursor: pointer;
        }
        .download-audio-btn:hover {
          background-color: #66bb6a !important;
          box-shadow: 0 0 0 3px rgba(102, 187, 106, 0.6);
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
    session$onSessionEnded(function() {
      unlink(audio_dir, recursive = TRUE, force = TRUE)
    })
    
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
          req(loc)
          species_choices <- names(tree()[[loc]])
          updateSelectInput(session, speciesInput, choices = species_choices)
          
          # Also immediately update folder based on first species of new location
          first_species <- species_choices[1]
          if (!is.null(first_species)) {
            folder_choices <- names(tree()[[loc]][[first_species]])
            updateSelectInput(session, folderInput, choices = folder_choices)
            
            # And update files based on first folder
            first_folder <- folder_choices[1]
            if (!is.null(first_folder)) {
              wavs <- tree()[[loc]][[first_species]][[first_folder]]
              updateSelectInput(session, fileInput, choices = setNames(wavs, basename(wavs)))
            }
          }
        }, ignoreInit = TRUE)
        
        observeEvent(input[[speciesInput]], {
          loc <- input[[locationInput]]
          sp  <- input[[speciesInput]]
          req(loc, sp)
          folder_choices <- names(tree()[[loc]][[sp]])
          updateSelectInput(session, folderInput, choices = folder_choices)
          
          # Also immediately update files based on first folder
          first_folder <- folder_choices[1]
          if (!is.null(first_folder)) {
            wavs <- tree()[[loc]][[sp]][[first_folder]]
            updateSelectInput(session, fileInput, choices = setNames(wavs, basename(wavs)))
          }
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
        
        observeEvent(input[[folderInput]], {
          loc  <- input[[locationInput]]
          sp   <- input[[speciesInput]]
          fldr <- input[[folderInput]]
          req(loc, sp, fldr)
          wavs <- tree()[[loc]][[sp]][[fldr]]
          updateSelectInput(session, fileInput, choices = setNames(wavs, basename(wavs)))
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
        
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
            
            # Use fixed filename per index — overwrites previous file
            audio_name      <- paste0("audio_", index, ".wav")   # <-- fixed name, no UUID
            temp_audio_path <- file.path(audio_dir, audio_name)
            
            # Remove old file and copy new one
            if (file.exists(temp_audio_path)) file.remove(temp_audio_path)
            file.copy(from = wav_path_val, to = temp_audio_path, overwrite = TRUE)
            
            # Download handler
            output[[paste0("download_audio_", index)]] <- downloadHandler(
              filename    = function() paste0(tools::file_path_sans_ext(basename(wav_path_val)), ".wav"),
              content     = function(file) file.copy(temp_audio_path, file),
              contentType = "audio/wav"
            )
            
            # Use cache-busting query param on the resource path URL instead of base64
            cache_bust <- as.integer(Sys.time())
            
            output[[paste0("audio_", index)]] <- renderUI({
              audio_id  <- ns(paste0("audio_element_", index))
              plot_id   <- ns(paste0("plot_", index))
              canvas_id <- ns(paste0("canvas_", index))
              
              tagList(
                div(
                  style = "display: flex; align-items: center; gap: 10px;",
                  tags$audio(
                    id      = audio_id,
                    controls = TRUE,
                    preload  = "auto",
                    style    = "width: 50%; margin-top: 5px;",
                    tags$source(
                      # serve from resource path with cache-bust — no base64 in memory
                      src  = paste0("temp_audio/", audio_name, "?v=", cache_bust),
                      type = "audio/wav"
                    ),
                    "Your browser does not support the audio element."
                  ),
                  downloadButton(
                    ns(paste0("download_audio_", index)),
                    label = "Download WAV",
                    icon  = shiny::icon("download"),
                    class = "download-audio-btn",
                    style = "background-color: #2e7d32; color: white; border: none;
                   border-radius: 5px; white-space: nowrap; height: 35px;
                   display: flex; align-items: center; justify-content: center;
                   padding: 0 12px; line-height: 1;"
                  )
                ),
                
                # Canvas overlaid on top of the plot
                tags$script(HTML(sprintf("
                  (function() {
                    // Wait for both the audio and plot to be ready
                    function initOverlay() {
                      var audio    = document.getElementById('%s');
                      var plotEl   = document.getElementById('%s');
                      
                      if (!audio || !plotEl) {
                        setTimeout(initOverlay, 200);
                        return;
                      }
                      
                      // Create canvas overlay
                      var canvas = document.createElement('canvas');
                      canvas.id  = '%s';
                      canvas.style.position = 'absolute';
                      canvas.style.top      = '0';
                      canvas.style.left     = '0';
                      canvas.style.pointerEvents = 'none'; // clicks pass through to plot
                      
                      // Position canvas over the plot
                      var wrapper = plotEl.parentElement;
                      wrapper.style.position = 'relative';
                      wrapper.appendChild(canvas);
                      
                      function resizeCanvas() {
                        canvas.width  = plotEl.offsetWidth;
                        canvas.height = plotEl.offsetHeight;
                      }
                      resizeCanvas();
                      window.addEventListener('resize', resizeCanvas);
                      
                      // Plot margins as fractions of total plot size (must match par(mar=...) in R)
                      // mar = c(4, 4.5, 2, 5) with default cex
                      // These are approximate - adjust if line is slightly off
                      var marginL = 0.09;   // left margin fraction
                      var marginR = 0.10;   // right margin fraction  
                      var marginT = 0.06;   // top margin fraction
                      var marginB = 0.10;   // bottom margin fraction
                      
                      function drawLine() {
                        var ctx      = canvas.getContext('2d');
                        var duration = audio.duration;
                        var current  = audio.currentTime;
                        
                        ctx.clearRect(0, 0, canvas.width, canvas.height);
                        
                        if (!duration || duration === 0) return;
                        
                        // Plot area bounds in pixels
                        var plotLeft   = canvas.width  * marginL;
                        var plotRight  = canvas.width  * (1 - marginR);
                        var plotTop    = canvas.height * marginT;
                        var plotBottom = canvas.height * (1 - marginB);
                        var plotWidth  = plotRight - plotLeft;
                        
                        // X position of playhead
                        var xPos = plotLeft + (current / duration) * plotWidth;
                        
                        // Draw line
                        ctx.beginPath();
                        ctx.moveTo(xPos, plotTop);
                        ctx.lineTo(xPos, plotBottom);
                        ctx.strokeStyle = 'rgba(255, 0, 0, 0.9)';  // red
                        ctx.lineWidth   = 2;
                        ctx.setLineDash([]);
                        ctx.stroke();
                      }
                      
                      // Update line on timeupdate
                      audio.addEventListener('timeupdate', drawLine);
                      
                      // Clear line when audio ends or is reset
                      audio.addEventListener('ended', function() {
                        var ctx = canvas.getContext('2d');
                        ctx.clearRect(0, 0, canvas.width, canvas.height);
                      });
                    }
                    
                    setTimeout(initOverlay, 300);
                  })();", 
                  audio_id, plot_id, canvas_id)))
                )
              })
            
          }, delay = 0.1)
          
          
          # Show the spinner immediately by rendering plot_ui_ before starting the processing
          output[[paste0("plot_ui_", index)]] <- renderUI({
            shinycssloaders::withSpinner(
              plotOutput(ns(plotOutput), height = "500px"),
              type    = 4,
              color   = "#ffffff",       # <-- white spinner
              color.background = "#001f3f",  # <-- match dark background so spinner is visible
              size    = 1.5              # <-- slightly larger
            )
          })
          
          later::later(function() {
            
            wave <- tuneR::readWave(wav_path_val)
            
            # Explicitly clear old cache before computing new one
            spectro_cache(NULL)          # <-- free previous matrix
            gc()                         # <-- hint to R to collect garbage
            
            spect <- seewave::spectro(
              wave,
              wl   = wl_val,
              ovlp = overlap_val,
              zp   = 2,
              plot = FALSE
            )
            
            amp_matrix <- spect$amp
            amp_matrix[!is.finite(amp_matrix)] <- -120
            
            spectro_cache(list(
              amp  = amp_matrix,
              time = spect$time,
              freq = spect$freq
            ))
            
            # ---- RENDER WITH BASE R image() ----
            output[[plotOutput]] <- renderPlot({
              cache <- spectro_cache()
              req(cache)
              
              dyn_range_val <- input[[paste0("dyn_range_", index)]]
              if (is.null(dyn_range_val) || is.na(dyn_range_val)) dyn_range_val <- 60
              
              zmax <- max(cache$amp, na.rm = TRUE)
              zmin <- zmax - dyn_range_val
              
              amp_disp <- pmax(pmin(cache$amp, zmax), zmin)
              
              jet_colors <- colorRampPalette(
                c("#00007F", "#0000FF", "#007FFF", "#00FFFF",
                  "#7FFF7F", "#FFFF00", "#FF7F00", "#FF0000", "#7F0000")
              )(512)
              
              par(
                bg       = "#001f3f",
                col.axis = "white",
                col.lab  = "white",
                fg       = "white",
                mar      = c(4, 4.5, 2, 5),
                xaxs     = "i",    # <-- forces x axis to exact data range, no padding
                yaxs     = "i",     # <-- forces y axis to exact data range, no padding
                cex.lab  = 1.3
              )
              
              image(
                x         = cache$time,
                y         = cache$freq,
                z         = t(amp_disp),
                col       = jet_colors,
                zlim      = c(zmin, zmax),
                xlab      = "Time (s)",
                ylab      = "Frequency (kHz)",
                axes      = FALSE,
                useRaster = TRUE
              )
              
              # Generate ticks that stay within the actual data range
              x_ticks <- pretty(range(cache$time), n = 8)
              x_ticks <- x_ticks[x_ticks >= min(cache$time) & x_ticks <= max(cache$time)]
              
              y_ticks <- pretty(range(cache$freq), n = 8)
              y_ticks <- y_ticks[y_ticks >= min(cache$freq) & y_ticks <= max(cache$freq)]
              
              axis(1, at = x_ticks, col = "white", col.ticks = "white",
                   col.axis = "white", cex.axis = 1.1)   # <-- increased text size
              axis(2, at = y_ticks, col = "white", col.ticks = "white",
                   col.axis = "white", cex.axis = 1.1,   # <-- increased text size
                   las = 1)
              
              # Larger axis labels
              # title(xlab = "Time (s)",        col.lab = "white", cex.lab = 1.3)
              # title(ylab = "Frequency (kHz)", col.lab = "white", cex.lab = 1.3)
              
              box(col = adjustcolor("white", alpha.f = 0.3))
              
              # Colorbar
              usr <- par("usr")
              cx1 <- usr[2] + diff(usr[1:2]) * 0.015
              cx2 <- usr[2] + diff(usr[1:2]) * 0.045
              n   <- length(jet_colors)
              ys  <- seq(usr[3], usr[4], length.out = n + 1)
              for (k in seq_len(n))
                rect(cx1, ys[k], cx2, ys[k+1],
                     col = jet_colors[k], border = NA, xpd = TRUE)
              rect(cx1, usr[3], cx2, usr[4],
                   col = NA, border = adjustcolor("white", 0.4), xpd = TRUE)
              tks <- pretty(c(zmin, zmax), n = 5)
              tks <- tks[tks >= zmin & tks <= zmax]
              for (tk in tks) {
                yp <- usr[3] + (tk - zmin) / (zmax - zmin) * diff(usr[3:4])
                text(cx2 + diff(usr[1:2]) * 0.012, yp,
                     paste0(tk, " dB"), col = "white", cex = 1.1,  # <-- increased colorbar text
                     adj = c(0, 0.5), xpd = TRUE)
              }
              
            }, bg = "#001f3f")
            
          }, delay = 0.1)
        })
        
        observeEvent(input[[paste0("dyn_range_", index)]], {
          req(spectro_cache())
          # renderPlot above already reads dyn_range input reactively — no proxy needed
        }, ignoreInit = TRUE)
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
