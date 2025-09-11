# Minimal Cohorts Shiny App (cleaned)
# - Removes tag cloud, per-card tags, dropdowns, cohort views, boxplot, and sample filtering
# - Keeps: file upload (with auto-load), NJ tree gallery cards, drag-to-cohort with cloning,
#          cohort boxes list names with removable (x) controls

# ---- Packages ----
suppressPackageStartupMessages({
    library(shiny)
    library(sortable)   # drag & drop with cloning via SortableJS
    library(ape)        # NJ trees & plotting
})

# ---- Helpers ----
# Coerce any of: 'phylo', 'dist', or numeric matrix to a phylo tree via NJ (for dist/matrix)
as_phylo <- function(x) {
    if (inherits(x, "phylo")) return(x)
    if (inherits(x, "dist"))  return(nj(x))
    if (is.matrix(x) || is.data.frame(x)) return(nj(as.dist(x)))
    stop("Unsupported object in data.rds: expected phylo, dist, or numeric matrix.")
}

# Attempt to load defaults from working dir if present
load_default_files <- function() {
    list(
        data_rds   = if (file.exists("./data.rds")) "./data.rds" else NULL,
        sample_info = if (file.exists("./sample_info_annotates.txt")) "./sample_info_annotates.txt" else NULL
    )
}

# ---- UI ----
ui <- fluidPage(
    tags$head(
        tags$style(HTML("
      .app-title { font-size:22px; font-weight:700; margin:10px 0; }
      .thin-hr { margin-top:8px; margin-bottom:8px; }
      .tree-card { background:#fafafa; border:1px solid #e5e7eb; border-radius:12px; padding:10px; margin-bottom:12px; box-shadow:0 1px 2px rgba(0,0,0,0.04); }
      .tree-title { font-weight:600; margin-bottom:6px; font-size:14px; }
      .tree-plot { margin-top:4px; }
      .rank-list { min-height:60px; }
      #cohort1 .tree-plot, #cohort2 .tree-plot { display:none; }
      #cohort1 .tree-card, #cohort2 .tree-card { padding:6px 10px; }
      .badge { display:inline-flex; align-items:center; gap:6px; background:#eef2ff; color:#1f2937; border:1px solid #c7d2fe; border-radius:9999px; padding:4px 10px; margin:4px 6px 0 0; font-size:12px; }
      .badge .x { cursor:pointer; font-weight:700; padding-left:6px; }
      .cohort-box { background:#ffffff; border:1px dashed #cbd5e1; border-radius:12px; padding:10px; min-height:100px; }
      .section-title { font-weight:700; font-size:16px; margin-bottom:8px; }
    "))
    ),
    
    fluidRow(
        column(8,
               div(class = "app-title", "NJ Tree Gallery"),
               fluidRow(
                   column(6,
                          fileInput("data_rds", "Upload data.rds", accept = ".rds", buttonLabel = "Browse...", placeholder = "data.rds")
                   ),
                   column(6,
                          fileInput("sample_info", "Upload sample_info_annotates.txt", accept = c(".txt", ".tsv"), buttonLabel = "Browse...", placeholder = "sample_info_annotates.txt")
                   )
               ),
               div("Tip: If present, ./data.rds and ./sample_info_annotates.txt will auto-load on start.", style = "font-size:12px;color:#6b7280;"),
               tags$hr(class = "thin-hr"),
               uiOutput("gallery_list")
        ),
        
        column(4,
               div(class = "section-title", "Cohorts"),
               
               div(class = "cohort-box",
                   h5("Cohort 1"),
                   uiOutput("cohort1_list"),
                   uiOutput("cohort1_names")
               ),
               tags$br(),
               
               div(class = "cohort-box",
                   h5("Cohort 2"),
                   uiOutput("cohort2_list"),
                   uiOutput("cohort2_names")
               ),
               tags$br(),
               
               actionButton("clear1", "Clear Cohort 1"),
               actionButton("clear2", "Clear Cohort 2")
        )
    )
)

# ---- Server ----
server <- function(input, output, session) {
    defaults <- load_default_files()
    
    trees <- reactiveVal(NULL)          # named list of phylo
    sample_info <- reactiveVal(NULL)    # data.frame (unused, but loaded per requirement)
    cohorts <- reactiveValues(cohort1 = character(), cohort2 = character())
    
    # ---- Load defaults on start (if present) ----
    observe({
        if (is.null(input$data_rds) && !is.null(defaults$data_rds)) {
            try({
                dat <- readRDS(defaults$data_rds)
                if (!is.list(dat)) stop("data.rds must be a list.")
                phy <- lapply(dat, as_phylo)
                if (is.null(names(phy)) || any(!nzchar(names(phy)))) names(phy) <- paste0("Tree_", seq_along(phy))
                trees(phy)
            }, silent = TRUE)
        }
        if (is.null(input$sample_info) && !is.null(defaults$sample_info)) {
            try({
                si <- tryCatch(read.delim(defaults$sample_info, stringsAsFactors = FALSE, check.names = FALSE),
                               error = function(e) read.table(defaults$sample_info, header = TRUE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE))
                sample_info(si)
            }, silent = TRUE)
        }
    })
    
    # ---- Load uploaded files ----
    observeEvent(input$data_rds, {
        file <- input$data_rds$datapath
        validate(need(!is.null(file) && nzchar(file), "Please upload data.rds"))
        dat <- readRDS(file)
        validate(need(is.list(dat), "data.rds must be a list of trees or distance matrices."))
        phy <- lapply(dat, as_phylo)
        if (is.null(names(phy)) || any(!nzchar(names(phy)))) names(phy) <- paste0("Tree_", seq_along(phy))
        trees(phy)
    })
    
    observeEvent(input$sample_info, {
        file <- input$sample_info$datapath
        validate(need(!is.null(file) && nzchar(file), "Please upload sample_info_annotates.txt"))
        si <- tryCatch(read.delim(file, stringsAsFactors = FALSE, check.names = FALSE),
                       error = function(e) read.table(file, header = TRUE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE))
        sample_info(si)
    })
    
    # ---- Render Gallery: a rank_list with clone-enabled items ----
    output$gallery_list <- renderUI({
        phy <- trees()
        validate(need(!is.null(phy) && length(phy) > 0, "Waiting for data.rds..."))
        
        # Build HTML items for each tree: title + plotOutput
        items <- lapply(names(phy), function(nm) {
            div(class = "tree-card", `data-value` = nm,
                div(class = "tree-title", nm),
                div(class = "tree-plot", plotOutput(outputId = paste0("plot_", nm), height = 200))
            )
        })
        
        # Hook up plots
        lapply(names(phy), function(nm) {
            local_nm <- nm
            tree <- phy[[local_nm]]
            output[[paste0("plot_", local_nm)]] <- renderPlot({
                plot.phylo(tree, cex = 0.6, no.margin = TRUE)
                title(local_nm, line = -1, cex.main = 0.8)
            })
        })
        
        rank_list(
            text = "Drag a tree card from here into a cohort (it will create a copy).",
            labels = items,
            input_id = "gallery",
            options = sortable_options(group = list(name = "trees", pull = "clone", put = FALSE))
        )
    })
    
    # ---- Cohort Rank Lists (targets) ----
    output$cohort1_list <- renderUI({
        rank_list(
            text = NULL,
            labels = cohorts$cohort1,
            input_id = "cohort1",
            options = sortable_options(group = "trees")
        )
    })
    output$cohort2_list <- renderUI({
        rank_list(
            text = NULL,
            labels = cohorts$cohort2,
            input_id = "cohort2",
            options = sortable_options(group = "trees")
        )
    })
    
    # Keep reactive state in sync with drag/drop
    observeEvent(input$cohort1, {
        if (is.null(input$cohort1)) {
            cohorts$cohort1 <- character()
        } else {
            cohorts$cohort1 <- as.character(input$cohort1)
        }
    })
    observeEvent(input$cohort2, {
        if (is.null(input$cohort2)) {
            cohorts$cohort2 <- character()
        } else {
            cohorts$cohort2 <- as.character(input$cohort2)
        }
    })
    
    # ---- Cohort name badges with remove (x) ----
    make_badges <- function(vec, remove_input_id) {
        if (length(vec) == 0) return(NULL)
        tags$div(
            lapply(seq_along(vec), function(i) {
                nm <- vec[[i]]
                key <- paste0(nm, "|||", i)
                tags$span(class = "badge",
                          span(nm),
                          tags$span(class = "x", "\u00D7", onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", remove_input_id, key))
                )
            })
        )
    }
    
    output$cohort1_names <- renderUI({ make_badges(cohorts$cohort1, "remove1") })
    output$cohort2_names <- renderUI({ make_badges(cohorts$cohort2, "remove2") })
    
    # ---- Removal handlers (remove a single occurrence by index) ----
    observeEvent(input$remove1, {
        vec <- cohorts$cohort1
        key <- input$remove1
        parts <- strsplit(key, "|||", fixed = TRUE)[[1]]
        if (length(parts) == 2) {
            idx <- suppressWarnings(as.integer(parts[2]))
            if (!is.na(idx) && idx >= 1 && idx <= length(vec)) {
                cohorts$cohort1 <- vec[-idx]
            }
        }
    })
    observeEvent(input$remove2, {
        vec <- cohorts$cohort2
        key <- input$remove2
        parts <- strsplit(key, "|||", fixed = TRUE)[[1]]
        if (length(parts) == 2) {
            idx <- suppressWarnings(as.integer(parts[2]))
            if (!is.na(idx) && idx >= 1 && idx <= length(vec)) {
                cohorts$cohort2 <- vec[-idx]
            }
        }
    })
    
    # ---- Clear buttons ----
    observeEvent(input$clear1, { cohorts$cohort1 <- character() })
    observeEvent(input$clear2, { cohorts$cohort2 <- character() })
}

shinyApp(ui, server)
