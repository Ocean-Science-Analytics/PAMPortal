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
    uiOutput(ns("click_detector_content"))
  )
}
    
#' click_detector Server Functions
#'
#' @noRd 
mod_click_detector_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    species_cache <- reactiveVal(list())
    images_cache  <- reactiveVal(list())
    
    has_click_detector <- reactive({
      !is.null(data$click_detector_data()) && length(data$click_detector_data()) > 0
    })
    
    output$click_detector_content <- renderUI({
      if (!has_click_detector()) {
        div(
          style = "
        margin-top: 30px;
        padding: 20px;
        border: 1px solid #ddd;
        border-radius: 8px;
        background-color: #f8f8f8;
        text-align: center;
        color: #888;
        font-size: 1.3rem;
      ",
          shiny::icon("circle-info", style = "margin-right: 8px;"),
          "No click detector screenshots are available for this dataset."
        )
      } else {
        
        # Get site names here so selectInput is pre-populated on first render
        sites      <- data$click_detector_data()
        site_names <- basename(sites)
        
        bslib::layout_sidebar(
          sidebar = bslib::card(
            style = "background-color: #f8f9fa; border-radius: 12px; box-shadow: 0 2px 6px rgba(0,0.08,0.2); padding: 10px;",
            selectInput(ns("deployment"), "Deployment", 
                        choices  = site_names,    # <-- pre-populated directly
                        selected = site_names[1]),
            conditionalPanel(
              condition = sprintf("input['%s']", ns("filter_species")),
              selectInput(ns("species"), "Species", choices = NULL)
            ),
            checkboxInput(ns("filter_species"), "Filter by Species")
          ),
          bslib::card(
            style = "
          margin-top: 20px;
          background-color: #ffffff;
          border-radius: 12px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        ",
            bslib::card_header(
              style = "
            padding: 16px 20px;
            border-bottom: 1px solid #e6e6e6;
            background-color: #fafafa;
            font-weight: 600;
            font-size: 18px;
            color: #333;
          ",
              uiOutput(ns("gallery_title"))
            ),
            bslib::card_body(pixture::pixgalleryOutput(ns("gallery")))
          )
        )
      }
    })
    
    # Reset the species and image caches when new data is loaded
    observeEvent(data$click_detector_data(), {
      species_cache(list())
      images_cache(list())
    })
    
    current_page <- reactiveVal(1)
    observeEvent(list(input$deployment, input$species, input$filter_species), {
      current_page(1)
    })
    
    PAGE_SIZE <- 30
    
    # --- Populate Deployment choices — keep as fallback for subsequent updates ---
    observeEvent(data$click_detector_data(), {
      req(has_click_detector())
      sites      <- data$click_detector_data()
      site_names <- basename(sites)
      updateSelectInput(session, "deployment", choices = site_names, selected = site_names[1])
    })
    
    # --- Base directory ---
    base_path <- reactive({
      req(data$selected_dir())
    })
    
    # --- Update Species list when checkbox or deployment changes ---
    observeEvent({
      input$filter_species
      input$deployment
    }, {
      req(base_path(), has_click_detector())
      
      if (isTRUE(input$filter_species) && !is.null(input$deployment)) {
        all_sites        <- data$click_detector_data()
        site_names       <- basename(all_sites)
        deployment_index <- match(input$deployment, site_names)
        deployment_path  <- all_sites[deployment_index]
        
        # Check cache first
        cache_key <- input$deployment
        cache      <- species_cache()
        
        if (!cache_key %in% names(cache)) {
          # Not cached — fetch from filesystem (slow on AWS, but only once)
          if (dir.exists(deployment_path)) {
            species_folders <- list.dirs(deployment_path, 
                                         full.names = FALSE, 
                                         recursive  = FALSE)
            species_folders <- species_folders[species_folders != ""]
          } else {
            species_folders <- character(0)
          }
          cache[[cache_key]] <- species_folders
          species_cache(cache)
        }
        
        updateSelectInput(session, "species", choices = cache[[cache_key]])
        
      } else {
        updateSelectInput(session, "species", choices = character(0))
      }
    })
    
    # --- Reactive: gather image files ---
    images_to_display <- reactive({
      req(input$deployment, has_click_detector())
      
      all_sites        <- data$click_detector_data()
      site_names       <- basename(all_sites)
      deployment_index <- match(input$deployment, site_names)
      deployment_path  <- all_sites[deployment_index]
      deployment_root  <- dirname(deployment_path)
      
      if (!dir.exists(deployment_path)) return(character(0))
      
      search_path <- if (isTRUE(input$filter_species) && 
                         !is.null(input$species) && 
                         input$species != "") {
        file.path(deployment_path, input$species)
      } else {
        deployment_path
      }
      
      if (!dir.exists(search_path)) return(character(0))
      
      # Check image cache
      cache_key <- paste0(input$deployment, "/", 
                          if (isTRUE(input$filter_species)) input$species else "__all__")
      cache <- images_cache()
      
      if (!cache_key %in% names(cache)) {
        # Not cached — fetch from filesystem (slow on AWS, only once per key)
        imgs_raw <- list.files(
          search_path,
          pattern     = "\\.(png|jpg|jpeg)$",
          recursive   = TRUE,
          full.names  = TRUE,
          ignore.case = TRUE
        )
        
        if (length(imgs_raw) == 0) {
          cache[[cache_key]] <- character(0)
          images_cache(cache)
          return(character(0))
        }
        
        alias <- "click_detector_gallery"
        shiny::removeResourcePath(alias)
        shiny::addResourcePath(alias, deployment_root)
        
        root_norm      <- normalizePath(deployment_root, winslash = "/", mustWork = FALSE)
        imgs_norm      <- normalizePath(imgs_raw,        winslash = "/", mustWork = FALSE)
        relative_paths <- sub(paste0("^", root_norm, "/?"), "", imgs_norm)
        web_paths      <- file.path(alias, relative_paths)
        
        cache[[cache_key]] <- web_paths
        images_cache(cache)
      }
      
      cache[[cache_key]]
    })
    
    # Paginated subset of images
    images_paginated <- reactive({
      imgs  <- images_to_display()
      if (length(imgs) == 0) return(character(0))
      
      total_pages <- ceiling(length(imgs) / PAGE_SIZE)
      page        <- min(current_page(), total_pages)
      
      start <- (page - 1) * PAGE_SIZE + 1
      end   <- min(page * PAGE_SIZE, length(imgs))
      
      imgs[start:end]
    })
    
    total_pages <- reactive({
      imgs <- images_to_display()
      if (length(imgs) == 0) return(1)
      ceiling(length(imgs) / PAGE_SIZE)
    })
    
    # Page navigation observers
    observeEvent(input$page_prev, {
      current_page(max(1, current_page() - 1))
    })
    
    observeEvent(input$page_next, {
      current_page(min(total_pages(), current_page() + 1))
    })
    
    # --- Gallery title ---
    output$gallery_title <- renderUI({
      req(input$deployment)
      
      title_text <- paste("Images for", input$deployment)
      if (isTRUE(input$filter_species) && !is.null(input$species) && input$species != "") {
        title_text <- paste(title_text, "-", input$species)
      }
      
      n_imgs    <- length(images_to_display())
      n_pages   <- total_pages()
      page      <- current_page()
      start_img <- (page - 1) * PAGE_SIZE + 1
      end_img   <- min(page * PAGE_SIZE, n_imgs)
      
      tagList(
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          
          # Title and image count
          div(
            h4(title_text, style = "margin: 0;"),
            tags$p(
              style = "font-size: 0.82rem; color: #888; margin: 2px 0 0 0;",
              sprintf("Showing %d–%d of %d images", start_img, end_img, n_imgs)
            )
          ),
          
          # Page controls
          div(
            style = "display: flex; align-items: center; gap: 10px;",
            actionButton(
              ns("page_prev"), "",
              icon  = shiny::icon("chevron-left"),
              style = "background-color: #00688B; color: white; border: none;
                   border-radius: 6px; padding: 6px 12px;",
              disabled = if (page <= 1) "disabled" else NULL
            ),
            tags$span(
              style = "font-size: 0.9rem; color: #555;",
              sprintf("Page %d of %d", page, n_pages)
            ),
            actionButton(
              ns("page_next"), "",
              icon  = shiny::icon("chevron-right"),
              style = "background-color: #00688B; color: white; border: none;
                   border-radius: 6px; padding: 6px 12px;",
              disabled = if (page >= n_pages) "disabled" else NULL
            )
          )
        )
      )
    })
    
    # --- Render gallery ---
    output$gallery <- pixture::renderPixgallery({
      req(input$deployment, has_click_detector())
      imgs <- images_paginated()
      if (length(imgs) == 0) return(tags$p("No images found for the selected deployment/species."))
      captions <- tools::file_path_sans_ext(basename(imgs))
      pixture::pixgallery(path = imgs, caption = captions, caption_valign = "below", layout = "grid")
    })
    
  })
}
    
## To be copied in the UI
# mod_click_detector_ui("click_detector_1")
    
## To be copied in the server
# mod_click_detector_server("click_detector_1")
