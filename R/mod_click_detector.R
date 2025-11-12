#' click_detector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import pixture
mod_click_detector_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Top control card
    bslib::card(
      style = "min-height: 150px; display: flex; flex-direction: column; justify-content: center;",
      bslib::card_header("Click Detector Controls"),
      bslib::card_body(
        # Flex layout for controls
        div(
          style = "display: flex; align-items: center; justify-content: space-between;",
          
          # Left side: Deployment + conditional Species input
          div(
            style = "display: flex; align-items: center; gap: 15px;",
            
            # Deployment input
            selectInput(
              ns("deployment"),
              "Deployment",
              choices = NULL,
              width = "200px"
            ),
            
            # Conditional species selector
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("filter_species")),
              selectInput(
                ns("species"),
                "Species",
                choices = NULL,
                width = "200px"
              )
            )
          ),
          
          # Divider
          div(
            style = "border-left: 2px solid black; height: 60px; margin: 0 20px;"
          ),
          
          # Right side: single checkbox
          div(
            style = "display: flex; flex-direction: column; justify-content: center;",
            checkboxInput(ns("filter_species"), "Filter by Species", value = FALSE)
          )
        )
      )
    ),
    
    # Output
    uiOutput(ns("gallery_title")),
    pixture::pixgalleryOutput(ns("gallery"))
  )
}
    
#' click_detector Server Functions
#'
#' @noRd 
mod_click_detector_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Populate Deployment choices ---
    observeEvent(data$click_detector_data(), {
      sites <- data$click_detector_data()
      if (!is.null(sites) && length(sites) > 0) {
        site_names <- basename(sites)
        updateSelectInput(session, "deployment", choices = site_names)
      }
    })
    
    # --- Base directory (root of acoustic data) ---
    base_path <- reactive({
      req(data$selected_dir())
    })
    
    # --- Update Species list when checkbox is checked or deployment changes ---
    observeEvent({
      input$filter_species
      input$deployment
    }, {
      req(base_path())
      
      if (isTRUE(input$filter_species) && !is.null(input$deployment)) {
        all_sites <- data$click_detector_data()
        site_names <- basename(all_sites)
        deployment_index <- match(input$deployment, site_names)
        deployment_path <- all_sites[deployment_index]
        
        if (dir.exists(deployment_path)) {
          species_folders <- list.dirs(deployment_path, full.names = FALSE, recursive = FALSE)
          updateSelectInput(session, "species", choices = species_folders)
        } else {
          updateSelectInput(session, "species", choices = character(0))
        }
      } else {
        updateSelectInput(session, "species", choices = character(0))
      }
    })
    
    # --- Reactive: gather image files ---
    images_to_display <- reactive({
      req(input$deployment)
      
      # Base path including Click_Detector_Screenshots
      deployment_root <- file.path(data$selected_dir(), "Click_Detector_Screenshots")
      deployment_path <- file.path(deployment_root, input$deployment)
      if (!dir.exists(deployment_path))
        return(character(0))
      
      # Determine folder to search
      if (isTRUE(input$filter_species) && !is.null(input$species) && input$species != "") {
        search_path <- file.path(deployment_path, input$species)
      } else {
        search_path <- deployment_path
      }
      
      # Get all image files
      imgs <- list.files(
        search_path,
        pattern = "\\.(png|jpg|jpeg)$",
        recursive = !isTRUE(input$filter_species),
        full.names = TRUE,
        ignore.case = TRUE
      )
      if (length(imgs) == 0) return(character(0))
      
      # Register one stable alias for the entire Click_Detector_Screenshots folder
      alias <- "click_detector_gallery"
      shiny::removeResourcePath(alias)
      shiny::addResourcePath(alias, deployment_root)
      
      # Normalize paths for string replacement (convert backslashes to forward slashes)
      root_norm <- gsub("\\\\", "/", normalizePath(deployment_root))
      imgs_norm <- gsub("\\\\", "/", normalizePath(imgs))
      
      # Strip the root part and keep substructure
      relative_paths <- sub(paste0("^", root_norm, "/?"), "", imgs_norm)
      
      # Build Shiny-accessible URLs
      imgs_web <- file.path(alias, relative_paths)
      
      imgs_web
    })
    
    # --- Gallery title ---
    output$gallery_title <- renderUI({
      req(input$deployment)
      title_text <- paste("Images for", input$deployment)
      if (isTRUE(input$filter_species) && !is.null(input$species) && input$species != "") {
        title_text <- paste(title_text, "-", input$species)
      }
      h4(title_text, style = "margin-top: 20px; margin-bottom: 10px;")
    })
    
    # --- Render gallery ---
    output$gallery <- pixture::renderPixgallery({
      req(input$deployment)
      
      imgs <- images_to_display()
      if (length(imgs) == 0) return(tags$p("No images found for the selected deployment/species."))
      
      # Extract clean filenames as captions
      captions <- tools::file_path_sans_ext(basename(imgs))
      
      pixture::pixgallery(
        path = imgs,
        caption = captions,
        layout = "grid"
      )
    })
  })
}
    
## To be copied in the UI
# mod_click_detector_ui("click_detector_1")
    
## To be copied in the server
# mod_click_detector_server("click_detector_1")
