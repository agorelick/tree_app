# app.R — per-tree tags now computed by subsetting tag TSV on Patient_ID == tree label,
# then using that specific tree’s samples to derive met_type/met_timing/met_treated tags.
# Tag cloud counts use the same logic. Color palette template retained.

library(shiny)
library(jsonlite)
library(ggplot2)
library(ggpubr)
library(phytools)

if (!requireNamespace("ape", quietly = TRUE)) {
    stop("This app requires the 'ape' package. Install it with install.packages('ape').")
}

# ================================
# EDITABLE TAG COLOR PALETTE (TEMPLATE)
# Define colors for all tag values used in the app.
# Feel free to change these hex values.
# Any tag missing here will be assigned a fallback color automatically.
# ================================
TAG_COLORS <- c(
    # Metastasis type tags
    "Peritoneum"               = "#7B61FF",
    "Liver"                    = "#2BB673",
    "Locoregional"             = "#FF8C42",
    "Distant (any)"            = "#E83E8C",
    
    # Metastasis timing tags
    "Synchronous"              = "#17A2B8",
    "Metachronous"             = "#6F42C1",
    "Meta after sync"          = "#20C997",   # we normalize any variants to this label
    
    # Metastasis treatment tags
    "Untreated"                = "#6C757D",
    "Systemic chemo"           = "#007BFF",
    "Sys-chemo after untreated"= "#6610F2",
    "HIPEC"                    = "#28A745"
)

# Helper to make a color mapping for a set of tag names using TAG_COLORS, with fallbacks
make_color_map <- function(tag_names) {
    if (!length(tag_names)) return(setNames(character(), character()))
    pal <- TAG_COLORS
    missing <- setdiff(tag_names, names(pal))
    if (length(missing)) {
        # fallback palette for any unexpected tags
        extra_cols <- grDevices::hcl(h = seq(15, 375, length.out = length(missing) + 1L)[1:length(missing)],
                                     c = 80, l = 55)
        names(extra_cols) <- missing
        pal <- c(pal, extra_cols)
    }
    pal[tag_names]
}

# Fixed vocabulary used for tag-cloud list and color mapping
default_tag_vocab <- function() {
    c(
        # type
        "Peritoneum","Liver","Locoregional","Distant (any)",
        # timing
        "Synchronous","Metachronous","Meta after sync",
        # treatment
        "Untreated","Systemic chemo","Sys-chemo after untreated","HIPEC"
    )
}

ui <- fluidPage(
    tags$head(
        tags$style(HTML("
      html, body, .container-fluid { height: 100%; }
      .page-root { display: flex; flex-direction: column; height: 100vh; }
      .page-root > .title-panel { padding: 15px; padding-bottom: 0; }
      .app-wrap { display: flex; gap: 16px; padding: 12px; flex: 1 1 auto; min-height: 0; }
      .left-col { flex: 0 0 320px; overflow-y: auto; padding-right: 8px; border-right: 1px solid #eee; }
      .right-col { flex: 1 1 auto; overflow-y: auto; padding-left: 8px; min-width: 0; }
      .help-text { color:#555; }
      .note { margin-top: 10px; font-size: 12px; color:#666; }
      .gallery { display: grid !important; grid-template-columns: repeat(auto-fill, minmax(190px, 1fr)); gap: 16px; width: 100%; }
      .card { position: relative; font-family: system-ui,-apple-system,Segoe UI,Roboto,Ubuntu,Cantarell,'Helvetica Neue',Arial,'Noto Sans','Apple Color Emoji','Segoe UI Emoji'; transition: opacity .15s ease, filter .15s ease; }
      .card.disabled { opacity: 0.48; filter: grayscale(100%); }
      .filename { font-size: 12px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; margin-bottom: 6px; color:#333; }
      .thumb { position: relative; display: inline-block; cursor: pointer; width: 100%; }
      .thumbnail { width: 100%; aspect-ratio: 1 / 1; object-fit: contain; background:#fff; border:1px solid #d0d0d0; border-radius:10px; box-shadow: 0 1px 3px rgba(0,0,0,.06); }
      /* Tags now live BELOW the image, not overlayed */
      .tag-strip { position: static; display: flex; flex-wrap: wrap; gap: 6px; margin-top: 6px; justify-content: flex-start; max-width: 100%; }
      .tag-chip { font-size: 10px; line-height: 1; color: #fff; border-radius: 999px; padding: 4px 7px; box-shadow: 0 1px 2px rgba(0,0,0,.18); white-space: nowrap; max-width: 100%; overflow: hidden; text-overflow: ellipsis; }
      .popup { position: fixed; display: none; border:1px solid #bdbdbd; background:#fff; border-radius:12px; box-shadow:0 8px 26px rgba(0,0,0,.20); width: 520px; height: auto; z-index: 9999; max-width: 90vw; max-height: 90vh; overflow: hidden; }
      .popup-title { font-size: 14px; font-weight: 600; background:#f5f5f5; padding:6px 10px; border-bottom:1px solid #ddd; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
      .popup img { display:block; max-width:100%; height:auto; max-height:calc(90vh - 32px); object-fit:contain; }
      @media (max-width: 900px) { .popup { width: min(520px, 90vw); } }
      .cohort-section h4 { margin-top: 0; }
      .cohort-actions a { display: inline-block; margin-right: 8px; }
      .cohort-zone { margin-top: 8px; border: 2px dashed #bbb; border-radius: 10px; padding: 10px; min-height: 70px; background: #fafafa; transition: border-color .15s ease, background-color .15s ease; }
      .cohort-zone.over { border-color: #4a90e2; background: #eef6ff; }
      .cohort-badges { display: flex; flex-wrap: wrap; gap: 6px; margin-top: 8px; }
      .badge { font-size: 11px; background: #eef2ff; color: #334; border: 1px solid #c8d1ff; border-radius: 999px; padding: 3px 8px; max-width: 100%; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; position: relative; padding-right: 18px; }
      .badge-close { position: absolute; right: 4px; top: 50%; transform: translateY(-50%); display: inline-block; font-weight: 700; line-height: 1; width: 14px; height: 14px; text-align: center; border-radius: 50%; background: #d9534f; color: #fff; font-size: 10px; cursor: pointer; box-shadow: 0 1px 1px rgba(0,0,0,.15); }
      .badge-close:hover { filter: brightness(0.95); }
      .filter-banner { margin: 8px 0 12px 0; padding: 8px 12px; background: #f4f7ff; border: 1px solid #cfe0ff; border-radius: 10px; font-size: 12px; color: #244; }
      .cohort-select .form-group { margin-top: 6px; margin-bottom: 0; }
      .cohort-select label { font-size: 12px; margin-bottom: 4px; color:#334; }
      .cohort-select select.form-control { padding: 2px 6px; height: 30px; }
    ")),
        tags$script(HTML("
      (function() {
        function placePopup(thumb) {
          var popup = thumb.querySelector('.popup'); if (!popup) return;
          popup.style.display = 'block'; popup.style.left = '0px'; popup.style.top = '0px';
          var rect = thumb.getBoundingClientRect();
          var pw = Math.min(popup.offsetWidth || 520, window.innerWidth * 0.9);
          var ph = Math.min(popup.offsetHeight || 520, window.innerHeight * 0.9);
          var margin = 12, left = rect.right + margin, top = rect.top;
          if (left + pw > window.innerWidth - margin) left = rect.left - pw - margin;
          if (left < margin) left = margin;
          if (left + pw > window.innerWidth - margin) left = window.innerWidth - margin - pw;
          if (top + ph > window.innerHeight - margin) top = window.innerHeight - margin - ph;
          if (top < margin) top = margin;
          if (window.innerWidth < 480) left = Math.max((window.innerWidth - pw) / 2, margin);
          popup.style.left = left + 'px'; popup.style.top = top + 'px';
        }
        document.addEventListener('mouseenter', function(e){ var a = e.target.closest('.thumb'); if (!a) return; var p = a.querySelector('.popup'); if (!p) return; placePopup(a); }, true);
        document.addEventListener('mousemove', function(e){ var a = e.target.closest('.thumb'); if (!a) return; var p = a.querySelector('.popup'); if (!p || p.style.display !== 'block') return; placePopup(a); }, true);
        document.addEventListener('mouseleave', function(e){ var a = e.target.closest('.thumb'); if (!a) return; var p = a.querySelector('.popup'); if (p) p.style.display = 'none'; }, true);
        window.addEventListener('scroll', function(){ document.querySelectorAll('.popup').forEach(function(p){ p.style.display='none'; }); }, { passive:true });
        window.addEventListener('resize', function(){ document.querySelectorAll('.popup').forEach(function(p){ p.style.display='none'; }); });

        document.addEventListener('dragstart', function(e){
          var card = e.target.closest('.card'); if (!card) return;
          e.dataTransfer.effectAllowed = 'copy';
          var id = card.getAttribute('data-id') || '';
          if (id) e.dataTransfer.setData('text/plain', id);
        }, true);

        function setupDropZone() {
          ['cohortDrop1','cohortDrop2'].forEach(function(zoneId, idx){
            var zone = document.getElementById(zoneId); if (!zone) return;
            zone.addEventListener('dragover', function(e){ e.preventDefault(); e.dataTransfer.dropEffect = 'copy'; zone.classList.add('over'); });
            zone.addEventListener('dragleave', function(){ zone.classList.remove('over'); });
            zone.addEventListener('drop', function(e){
              e.preventDefault(); zone.classList.remove('over');
              var id = e.dataTransfer.getData('text/plain');
              if (id) Shiny.setInputValue('cohort_add' + (idx+1), { id: id, nonce: Date.now() }, { priority: 'event' });
            });
          });
        }
        document.addEventListener('DOMContentLoaded', setupDropZone);
        if (window.Shiny && Shiny.addCustomMessageHandler) {
          Shiny.addCustomMessageHandler('setupDropZone', setupDropZone);
        }
      })();
    "))
    ),
    
    div(class = "page-root",
        div(class = "title-panel",
            titlePanel("Phylo Tree Gallery (auto-loads ./data.rds)")
        ),
        div(class = "app-wrap",
            div(class = "left-col",
                tags$p(
                    class = "help-text",
                    strong("Expected input: "), code("data.rds"),
                    " should contain a ", strong("named list of distance matrices (class ", code("matrix"), ")"),
                    ". Each matrix will be converted to a tree with ", code("ape::nj()"), "."
                ),
                fileInput("rds", "Upload .rds (named list of distance matrices)", accept = ".rds"),
                actionButton("reload_default", "Reload ./data.rds"),
                br(), br(),
                
                tags$p(class = "help-text",
                       strong("Optional tags TSV:"),
                       " include columns ", code("sample"), ", ", code("met_type"), ", ", code("met_treated"), ", ", code("met_timing"),
                       ", optional ", code("in_collapsed"),
                       " (tips with ", code("FALSE"), " are dropped in Collapsed view). ",
                       "Optionally include ", code("Patient_ID"), " for the tag cloud."
                ),
                fileInput("tag_csv", "Upload tags (TSV)", accept = c(".tsv", ".txt")),
                
                div(class = "tag-cloud-head",
                    tags$h4("Tags"),
                    actionLink("clear_tag", "Show all")
                ),
                uiOutput("tag_cloud"),
                uiOutput("tag_filter_status"),
                tags$hr(),
                
                radioButtons("layout", "Tree layout",
                             choices = c("Unrooted", "Rooted"),
                             selected = "Unrooted", inline = TRUE),
                radioButtons("tree_size", "Tree size",
                             choices = c("Full", "Collapsed"),
                             selected = "Full", inline = TRUE),
                
                div(class = "cohort-section",
                    tags$h4("Cohorts"),
                    div(class = "cohort-actions",
                        actionLink("view_all", label = "Full cohort"),
                        HTML("&nbsp;|;&nbsp;"),
                        actionLink("view_c1", label = "Cohort 1"),
                        HTML("&nbsp;|;&nbsp;"),
                        actionLink("view_c2", label = "Cohort 2")
                    ),
                    
                    div(class = "cohort-zone", id = "cohortDrop1",
                        div(class="cohort-header",
                            tags$h5(class="cohort-title","Cohort 1"),
                            actionButton("clear_c1", "Clear", class="btn btn-xs")
                        ),
                        div(class = "cohort-badges", uiOutput("cohort_badges1")),
                        div(class = "cohort-select",
                            selectInput(
                                "cohort1_type", "Metastasis type",
                                choices = c("Peritoneum", "Liver", "Distant (any)", "Locoregional"),
                                selected = NULL, width = "100%", selectize = FALSE
                            ),
                            selectInput(
                                "cohort1_timing", "Metastasis timing",
                                choices = c("No preference", "Synchronous", "Metachronous", "Meta after sync"),
                                selected = "No preference", width = "100%", selectize = FALSE
                            ),
                            selectInput(
                                "cohort1_treatment", "Metastasis treatment",
                                choices = c("No preference", "Untreated", "Systemic chemo", "Sys-chemo after untreated", "HIPEC"),
                                selected = "No preference", width = "100%", selectize = FALSE
                            )
                        )
                    ),
                    div(class = "cohort-zone", id = "cohortDrop2",
                        div(class="cohort-header",
                            tags$h5(class="cohort-title","Cohort 2"),
                            actionButton("clear_c2", "Clear", class="btn btn-xs")
                        ),
                        div(class = "cohort-badges", uiOutput("cohort_badges2")),
                        div(class = "cohort-select",
                            selectInput(
                                "cohort2_type", "Metastasis type",
                                choices = c("Peritoneum", "Liver", "Distant (any)", "Locoregional"),
                                selected = NULL, width = "100%", selectize = FALSE
                            ),
                            selectInput(
                                "cohort2_timing", "Metastasis timing",
                                choices = c("No preference", "Synchronous", "Metachronous", "Meta after sync"),
                                selected = "No preference", width = "100%", selectize = FALSE
                            ),
                            selectInput(
                                "cohort2_treatment", "Metastasis treatment",
                                choices = c("No preference", "Untreated", "Systemic chemo", "Sys-chemo after untreated", "HIPEC"),
                                selected = "No preference", width = "100%", selectize = FALSE
                            )
                        )
                    ),
                    uiOutput("cohort_counts"),
                    div(class = "cohort-actions",
                        actionButton("test_boxplot", "Calculate RDS/QDS")
                    )
                ),
                
                br(), tags$hr(),
                tags$h4("Plot"),
                plotOutput("cohort_plot", height = "260px"),
                uiOutput("status"),
                
                br(),
                downloadButton("download_cohorts", "Download cohorts JSON")
            ),
            
            div(class = "right-col",
                uiOutput("filter_banner"),
                uiOutput("gallery", container = div, class = "gallery")
            )
        )
    )
)

server <- function(input, output, session) {
    prefix <- paste0("trees-", substr(session$token, 1, 8))
    trees_dir <- file.path(tempdir(), prefix)
    dir.create(trees_dir, showWarnings = FALSE, recursive = TRUE)
    shiny::addResourcePath(prefix, trees_dir)
    session$onSessionEnded(function() {
        try(shiny::removeResourcePath(prefix), silent = TRUE)
        unlink(trees_dir, recursive = TRUE, force = TRUE)
    })
    
    `%||%` <- function(a, b) if (is.null(a) || (is.character(a) && !nzchar(a))) b else a
    sanitize_name <- function(x) { if (is.null(x) || !nzchar(x)) x <- "unnamed"; gsub("[^A-Za-z0-9_-]+", "_", x) }
    label_cex <- function(n, scale_min = 0.25, scale_max = 1.2, ref = 40) { cex <- ref / max(10, n); max(scale_min, min(scale_max, cex)) }
    
    save_tree_plot <- function(tree, file, type = "unrooted", device = c("png","pdf"),
                               width = 800, height = 800, cex = 0.8) {
        device <- match.arg(device)
        op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
        par(mar = c(0.2, 0.2, 0.2, 0.2), xaxs = "i", yaxs = "i")
        if (device == "png") { png(file, width = width, height = height, res = 96, bg = "white"); on.exit(dev.off(), add = TRUE) }
        else { pdf(file, width = width/96, height = height/96, onefile = FALSE, paper = "special"); on.exit(dev.off(), add = TRUE) }
        ape::plot.phylo(tree, type = type, no.margin = TRUE, cex = cex)
        invisible(TRUE)
    }
    
    save_placeholder_plot <- function(file, message = "Insufficient tips", device = c("png","pdf"),
                                      width = 520, height = 520) {
        device <- match.arg(device)
        if (device == "png") { png(file, width = width, height = height, res = 96, bg = "white"); on.exit(dev.off(), add = TRUE) }
        else { pdf(file, width = width/96, height = height/96, onefile = FALSE, paper = "special"); on.exit(dev.off(), add = TRUE) }
        op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
        par(mar = c(0,0,0,0))
        plot.new(); rect(0,0,1,1,col="#ffffff",border=NA)
        text(0.5, 0.6, "No tree preview", cex = 1.2)
        text(0.5, 0.45, message, cex = 0.95)
        invisible(TRUE)
    }
    
    # --- Reactive state ---
    dist_list  <- reactiveVal(NULL)
    raw_list   <- reactiveVal(NULL)
    items      <- reactiveVal(list())
    notes      <- reactiveVal(NULL)
    
    # Holds the full, normalized tag data frame (TSV) --------------- NEW
    tag_df     <- reactiveVal(NULL)
    
    # Patient tag-cloud (left) - objects exist; counts will be computed on-the-fly now
    tags_mat       <- reactiveVal(NULL)                               # kept for compatibility (not used for counts)
    tag_colors     <- reactiveVal(setNames(character(), character())) # color map for left tag cloud
    selected_tags  <- reactiveVal(character())
    
    # Sample-level attributes (per-tip)
    sample_attrs <- reactiveVal(list(
        type        = setNames(character(), character()),
        treated     = setNames(character(), character()),
        timing      = setNames(character(), character()),
        in_collapsed= setNames(logical(),   character())
    ))
    
    # Two cohorts
    empty_cohorts <- list(`Cohort 1` = character(), `Cohort 2` = character())
    cohort_labels <- reactiveVal(empty_cohorts)
    
    view_mode     <- reactiveVal("all")
    layout_mode   <- reactiveVal("Unrooted")
    
    # --- Card tag vocabulary (from dropdowns, excluding 'No preference') ---
    card_tag_vocab <- reactive({
        type_opts  <- c("Peritoneum","Liver","Locoregional","Distant (any)")
        time_opts  <- c("Synchronous","Metachronous","Meta after sync")
        treat_opts <- c("Untreated","Systemic chemo","Sys-chemo after untreated","HIPEC")
        unique(c(type_opts, time_opts, treat_opts))
    })
    card_tag_colors <- reactive({
        make_color_map(card_tag_vocab())
    })
    
    # --- Cohort counters/badges ---
    output$cohort_counts <- renderUI({
        its <- items(); n_all <- length(its)
        cls <- cohort_labels()
        n1 <- length(cls[[1]]); n2 <- length(cls[[2]])
        tags$div(class = "note",
                 tags$strong("Counts: "),
                 sprintf("Full cohort = %d | C1 = %d | C2 = %d", n_all, n1, n2)
        )
    })
    
    output$cohort_badges1 <- renderUI({
        labs <- cohort_labels()[[1]]
        if (!length(labs)) return(NULL)
        lapply(labs, function(l) {
            l_js <- htmltools::htmlEscape(l, attribute = TRUE)
            tags$span(
                class = "badge", title = l, l,
                tags$span(
                    class = "badge-close",
                    title = sprintf("Remove “%s” from Cohort 1", l),
                    onclick = sprintf("Shiny.setInputValue('cohort_remove', { idx: 1, label: '%s', nonce: Date.now() }, {priority: 'event'});", l_js),
                    "\u00D7"
                )
            )
        })
    })
    output$cohort_badges2 <- renderUI({
        labs <- cohort_labels()[[2]]
        if (!length(labs)) return(NULL)
        lapply(labs, function(l) {
            l_js <- htmltools::htmlEscape(l, attribute = TRUE)
            tags$span(
                class = "badge", title = l, l,
                tags$span(
                    class = "badge-close",
                    title = sprintf("Remove “%s” from Cohort 2", l),
                    onclick = sprintf("Shiny.setInputValue('cohort_remove', { idx: 2, label: '%s', nonce: Date.now() }, {priority: 'event'});", l_js),
                    "\u00D7"
                )
            )
        })
    })
    
    # View toggles
    observeEvent(input$view_all, { view_mode("all") })
    observeEvent(input$view_c1,  { view_mode("c1") })
    observeEvent(input$view_c2,  { view_mode("c2") })
    
    # Clear cohorts
    observeEvent(input$clear_c1, { x <- cohort_labels(); x[[1]] <- character(); cohort_labels(x); if (view_mode()=="c1") view_mode("all") })
    observeEvent(input$clear_c2, { x <- cohort_labels(); x[[2]] <- character(); cohort_labels(x); if (view_mode()=="c2") view_mode("all") })
    
    # Helper to add to cohort WITHOUT switching views
    add_to_cohort <- function(idx, lab) {
        x <- cohort_labels()
        cur <- x[[idx]]
        if (!(lab %in% cur)) {
            showNotification(sprintf("Added “%s” to Cohort %d", lab, idx), type = "message", duration = 2)
            x[[idx]] <- c(cur, lab)
            cohort_labels(x)
        }
    }
    
    # Map card id -> label
    id_to_label <- function(card_id) {
        its <- items(); if (!length(its)) return(NULL)
        id2lab <- setNames(vapply(its, `[[`, "", "label"), vapply(its, `[[`, "", "id"))
        unname(id2lab[[card_id]])
    }
    
    observeEvent(input$cohort_add1, { add_id <- input$cohort_add1$id; if (is.null(add_id) || !nzchar(add_id)) return(); lab <- id_to_label(add_id); if (!is.null(lab) && nzchar(lab)) add_to_cohort(1, lab) })
    observeEvent(input$cohort_add2, { add_id <- input$cohort_add2$id; if (is.null(add_id) || !nzchar(add_id)) return(); lab <- id_to_label(add_id); if (!is.null(lab) && nzchar(lab)) add_to_cohort(2, lab) })
    observeEvent(input$cohort_remove, {
        info <- input$cohort_remove
        req(info$idx, info$label)
        x <- cohort_labels()
        idx <- as.integer(info$idx)
        if (idx >= 1 && idx <= length(x)) {
            x[[idx]] <- setdiff(x[[idx]], info$label)
            cohort_labels(x)
        }
    }, ignoreInit = TRUE)
    
    # -------- Loaders ----------
    as_dist_safe <- function(x) {
        if (inherits(x, "dist")) return(x)
        if (is.matrix(x)) {
            if (!is.numeric(x)) return(NULL)
            if (nrow(x) != ncol(x)) return(NULL)
            if (!isTRUE(all.equal(x, t(x), tolerance = 1e-8))) return(NULL)
            diag(x) <- 0
            return(as.dist(x))
        }
        NULL
    }
    
    load_from_path <- function(path, source_label = NULL) {
        obj <- try(readRDS(path), silent = TRUE)
        if (inherits(obj, "try-error")) {
            items(list()); cohort_labels(empty_cohorts); view_mode("all"); dist_list(NULL); raw_list(NULL)
            notes(paste0("Failed to read RDS (", if (!is.null(source_label)) source_label else path, "): ", attr(obj, "condition")$message))
        } else if (!is.list(obj)) {
            items(list()); cohort_labels(empty_cohorts); view_mode("all"); dist_list(NULL); raw_list(NULL)
            notes("The RDS must contain a named list of distance matrices (class 'matrix' or 'dist').")
        } else {
            if (is.null(names(obj))) names(obj) <- paste0("item_", seq_along(obj))
            dist_list(obj)
            cohort_labels(empty_cohorts)
            notes(paste0("Loaded ", length(obj), " matrices", if (!is.null(source_label)) paste0(" from ", source_label) else "", "."))
            view_mode("all")
        }
        invisible(TRUE)
    }
    
    # ---- NEW: load tags TSV from a path (used for auto-load + uploads) ----
    to_logical_safe <- function(x) {
        y <- tolower(trimws(as.character(x)))
        out <- ifelse(y %in% c("true","t","1","yes","y"), TRUE,
                      ifelse(y %in% c("false","f","0","no","n"), FALSE, NA))
        as.logical(out)
    }
    
    # Builds patient-level tag matrix (kept for reference; counts now computed on the fly)
    build_tag_matrix_from_tsv <- function(df) {
        if ("Patient_ID" %in% names(df)) df$Patient_ID <- trimws(as.character(df$Patient_ID))
        if ("met_treated" %in% names(df)) df$met_treated <- trimws(as.character(df$met_treated))
        if ("met_timing"  %in% names(df)) df$met_timing  <- gsub("(?i)^meta[- ]?after[- ]?sync$", "Meta after sync",
                                                                 trimws(as.character(df$met_timing)), perl = TRUE)
        if ("met_type"    %in% names(df)) df$met_type    <- trimws(as.character(df$met_type))
        
        type_levels  <- c("Peritoneum","Liver","Locoregional","Distant (any)")
        time_levels  <- c("Synchronous","Metachronous","Meta after sync")
        treat_levels <- c("Untreated","Systemic chemo","Sys-chemo after untreated","HIPEC")
        
        if (!("Patient_ID" %in% names(df))) {
            return(matrix(0, nrow = 0, ncol = 0, dimnames = list(character(0), character(0))))
        }
        patients <- sort(unique(df$Patient_ID[nzchar(df$Patient_ID)]))
        if (!length(patients)) {
            return(matrix(0, nrow = 0, ncol = 0, dimnames = list(character(0), character(0))))
        }
        
        tag_levels <- unique(c(type_levels, time_levels, treat_levels))
        mat <- matrix(0, nrow = length(tag_levels), ncol = length(patients),
                      dimnames = list(tag_levels, patients))
        
        spl <- split(df, df$Patient_ID)
        for (pid in names(spl)) {
            sub <- spl[[pid]]
            
            type_vals_raw <- unique(sub$met_type[!is.na(sub$met_type) & nzchar(sub$met_type)])
            type_tags <- intersect(type_vals_raw, c("Peritoneum","Liver","Locoregional"))
            if (length(type_vals_raw) &&
                any(!(type_vals_raw %in% c("Normal","Primary","Locoregional","Peritoneum")))) {
                type_tags <- union(type_tags, "Distant (any)")
            }
            
            time_vals  <- unique(sub$met_timing[!is.na(sub$met_timing) & nzchar(sub$met_timing)])
            time_tags  <- intersect(time_vals, time_levels)
            
            treat_vals <- unique(sub$met_treated[!is.na(sub$met_treated) & nzchar(sub$met_treated)])
            treat_tags <- intersect(treat_vals, treat_levels)
            
            tset <- unique(c(type_tags, time_tags, treat_tags))
            tset <- intersect(tset, rownames(mat))
            if (length(tset) && pid %in% colnames(mat)) mat[tset, pid] <- 1
        }
        
        mat
    }
    
    build_sample_attrs_from_tsv <- function(df) {
        need <- c("sample","met_type","met_treated","met_timing")
        if (!all(need %in% colnames(df))) {
            return(list(
                type        = setNames(character(), character()),
                treated     = setNames(character(), character()),
                timing      = setNames(character(), character()),
                in_collapsed= setNames(logical(),   character())
            ))
        }
        s  <- trimws(as.character(df$sample))
        mt <- trimws(as.character(df$met_type))
        tr <- trimws(as.character(df$met_treated))
        tm <- gsub("(?i)^meta[- ]?after[- ]?sync$", "Meta after sync", trimws(as.character(df$met_timing)), perl = TRUE)
        ic <- if ("in_collapsed" %in% colnames(df)) to_logical_safe(df$in_collapsed) else rep(NA, length(s))
        
        keep <- nzchar(s)
        s <- s[keep]; mt <- mt[keep]; tr <- tr[keep]; tm <- tm[keep]; ic <- ic[keep]
        if (!length(s)) {
            return(list(
                type        = setNames(character(), character()),
                treated     = setNames(character(), character()),
                timing      = setNames(character(), character()),
                in_collapsed= setNames(logical(),   character())
            ))
        }
        idx <- !duplicated(s, fromLast = TRUE)
        list(
            type        = setNames(mt[which(idx)], s[which(idx)]),
            treated     = setNames(tr[which(idx)], s[which(idx)]),
            timing      = setNames(tm[which(idx)], s[which(idx)]),
            in_collapsed= setNames(ic[which(idx)], s[which(idx)])
        )
    }
    
    load_tags_from_path <- function(path, source_label = NULL) {
        df <- try(utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE), silent = TRUE)
        if (inherits(df, "try-error")) {
            tags_mat(NULL); tag_colors(setNames(character(), character())); selected_tags(character())
            sample_attrs(list(type=setNames(character(), character()),
                              treated=setNames(character(), character()),
                              timing=setNames(character(), character()),
                              in_collapsed=setNames(logical(), character())))
            tag_df(NULL)
            notes(paste0("Failed to read tags TSV (", if (!is.null(source_label)) source_label else path, "): ", attr(df, "condition")$message))
            return(invisible(FALSE))
        }
        
        # normalize fields once and store full DF ---------------------- NEW
        if ("met_timing" %in% names(df)) {
            df$met_timing <- gsub("(?i)^meta[- ]?after[- ]?sync$", "Meta after sync",
                                  trimws(as.character(df$met_timing)), perl = TRUE)
        }
        if ("Patient_ID" %in% names(df)) df$Patient_ID <- trimws(as.character(df$Patient_ID))
        if ("met_treated" %in% names(df)) df$met_treated <- trimws(as.character(df$met_treated))
        if ("met_type" %in% names(df))    df$met_type    <- trimws(as.character(df$met_type))
        if ("sample" %in% names(df))      df$sample      <- trimws(as.character(df$sample))
        tag_df(df)
        
        # left-panel patient tag matrix (kept for compatibility; counts now computed per-tree)
        mat <- try(build_tag_matrix_from_tsv(df), silent = TRUE)
        if (inherits(mat, "try-error") || !nrow(mat) || !ncol(mat)) {
            tags_mat(NULL)
        } else {
            tags_mat(mat)
        }
        # set a stable color map for the full vocabulary
        tag_colors(make_color_map(default_tag_vocab()))
        selected_tags(character())
        
        # per-tip attributes (used for subsetting trees; left in place)
        sa <- build_sample_attrs_from_tsv(df)
        sample_attrs(sa)
        
        notes(paste0("Loaded tags TSV (", if (!is.null(source_label)) source_label else path, "). samples: ", length(sa$type), "."))
        invisible(TRUE)
    }
    
    # Auto-load data.rds if present
    if (file.exists("data.rds")) {
        load_from_path("data.rds", source_label = "./data.rds")
    } else {
        notes("Tip: place data.rds (named list of distance matrices) in the working directory to auto-load on startup.")
    }
    # Auto-load sample_info_annotated.txt if present
    if (file.exists("sample_info_annotated.txt")) {
        load_tags_from_path("sample_info_annotated.txt", source_label = "./sample_info_annotated.txt")
    }
    
    observeEvent(input$reload_default, {
        if (file.exists("data.rds")) load_from_path("data.rds", source_label = "./data.rds")
        else { items(list()); cohort_labels(empty_cohorts); view_mode("all"); dist_list(NULL); raw_list(NULL); notes("No ./data.rds found.") }
    })
    observeEvent(input$rds, { req(input$rds$datapath); load_from_path(input$rds$datapath, source_label = "uploaded file") }, ignoreInit = TRUE)
    
    # Upload tags TSV (overrides auto-load)
    observeEvent(input$tag_csv, {
        req(input$tag_csv$datapath)
        load_tags_from_path(input$tag_csv$datapath, source_label = "uploaded file")
    }, ignoreInit = TRUE)
    
    observeEvent(input$layout,    { layout_mode(input$layout)    }, ignoreInit = TRUE)
    observeEvent(input$tree_size, { NULL }, ignoreInit = TRUE)
    
    # --- Helpers for per-tree tags (subset DF by Patient_ID, then limit to tree samples) ---- NEW
    tree_tip_labels <- function(label) {
        # Prefer current built tree (respects Full/Collapsed and cohort subsetting); fallback to raw dist matrix
        lst <- raw_list()
        if (!is.null(lst) && label %in% names(lst)) {
            tr <- lst[[label]]
            if (!is.null(tr) && !is.null(tr$tip.label)) return(as.character(tr$tip.label))
        }
        obj <- dist_list()
        if (!is.null(obj) && label %in% names(obj)) {
            dmat <- obj[[label]]
            return(rownames(if (inherits(dmat, "dist")) as.matrix(dmat) else as.matrix(dmat)))
        }
        character(0)
    }
    
    # Compute tags for a given tree using Patient_ID subset + the tree's tip samples
    tags_for_tree <- function(label) {
        df <- tag_df()
        if (is.null(df) || !("Patient_ID" %in% names(df))) return(character())
        sub <- df[df$Patient_ID == label, , drop = FALSE]
        if (!nrow(sub)) return(character())
        
        # further restrict to samples present in this tree (if sample column exists)
        rn <- tree_tip_labels(label)
        if ("sample" %in% names(sub) && length(rn)) {
            sub <- sub[sub$sample %in% rn, , drop = FALSE]
            if (!nrow(sub)) return(character())
        }
        
        # derive tags from met_type / timing / treated (normalized timing already)
        typ_raw <- unique(sub$met_type[!is.na(sub$met_type) & nzchar(sub$met_type)])
        type_tags <- intersect(typ_raw, c("Peritoneum","Liver","Locoregional"))
        if (length(typ_raw) &&
            any(!(typ_raw %in% c("Normal","Primary","Locoregional","Peritoneum")))) {
            type_tags <- union(type_tags, "Distant (any)")
        }
        
        tim_raw <- unique(sub$met_timing[!is.na(sub$met_timing) & nzchar(sub$met_timing)])
        tim_tags <- intersect(tim_raw, c("Synchronous","Metachronous","Meta after sync"))
        
        trt_raw <- unique(sub$met_treated[!is.na(sub$met_treated) & nzchar(sub$met_treated)])
        trt_tags <- intersect(trt_raw, c("Untreated","Systemic chemo","Sys-chemo after untreated","HIPEC"))
        
        unique(c(type_tags, tim_tags, trt_tags))
    }
    
    # Card tags derived from per-tree data frame subset -------------- UPDATED
    card_tags_for_tree <- function(label) {
        tags_for_tree(label)
    }
    
    # ---- Tag cloud UI (left) — counts now computed per-tree with the same logic ---- UPDATED
    output$tag_cloud <- renderUI({
        df <- tag_df()
        if (is.null(df)) {
            return(tags$div(class = "note", "Upload a tags TSV to enable tag filtering (optional)."))
        }
        
        tg_names <- default_tag_vocab()
        cols_map <- make_color_map(tg_names)
        sel <- selected_tags()
        
        its_all <- items()
        vm <- view_mode()
        tree_subset <- if (identical(vm, "all")) {
            its_all
        } else {
            idx  <- switch(vm, c1 = 1L, c2 = 2L, 0L)
            labs <- if (idx > 0) cohort_labels()[[idx]] else character()
            if (length(labs)) Filter(function(x) x$label %in% labs, its_all) else list()
        }
        tree_labels <- if (length(tree_subset)) vapply(tree_subset, `[[`, "", "label") else character()
        
        # For each tag, count how many trees in the current view would have that tag
        counts <- vapply(tg_names, function(tg) {
            if (!length(tree_labels)) return(0L)
            sum(vapply(tree_labels, function(lbl) tg %in% tags_for_tree(lbl), logical(1)))
        }, integer(1))
        
        tags$div(class = "tag-cloud",
                 lapply(seq_along(tg_names), function(i) {
                     tg <- tg_names[[i]]
                     bg <- cols_map[[tg]] %||% "#666"
                     is_active     <- tg %in% sel
                     has_selection <- length(sel) > 0
                     cls <- paste("tag-pill", if (is_active) "active" else if (has_selection) "inactive")
                     cnt <- counts[i]
                     tags$span(
                         class = cls,
                         style = paste0("background:", bg, ";"),
                         onclick = sprintf("Shiny.setInputValue('tag_clicked', '%s', {priority: 'event'})", htmltools::htmlEscape(tg, attribute = TRUE)),
                         title = sprintf("%s (%d)", tg, cnt),
                         sprintf("%s (%d)", tg, cnt)
                     )
                 })
        )
    })
    
    observeEvent(input$tag_clicked, {
        tg <- input$tag_clicked
        cur <- selected_tags()
        if (tg %in% cur) selected_tags(setdiff(cur, tg)) else selected_tags(c(cur, tg))
    }, ignoreInit = TRUE)
    observeEvent(input$clear_tag, { selected_tags(character()) })
    
    output$tag_filter_status <- renderUI({
        sel <- selected_tags()
        if (!length(sel)) return(NULL)
        tags$div(class = "filter-banner",
                 tags$b("Showing trees with: "),
                 paste(sel, collapse = " AND ")
        )
    })
    
    # --------- Build per-tree specs (unchanged) ----------
    build_current_specs <- reactive({
        obj <- dist_list()
        if (is.null(obj) || !length(obj)) return(list())
        
        vm <- view_mode()
        sa <- sample_attrs()
        layout <- layout_mode()  # dependency
        size  <- input$tree_size   # dependency
        
        get_selection <- function(idx) {
            sel_type    <- if (idx == 1L) input$cohort1_type    else input$cohort2_type
            sel_timing  <- if (idx == 1L) input$cohort1_timing  else input$cohort2_timing
            sel_treated <- if (idx == 1L) input$cohort1_treatment else input$cohort2_treatment
            sel_timing  <- if (is.null(sel_timing)  || sel_timing  == "No preference") NULL else sel_timing
            sel_treated <- if (is.null(sel_treated) || sel_treated == "No preference") NULL else sel_treated
            
            uniq_types <- unique(unname(sa$type)); uniq_types <- uniq_types[nzchar(uniq_types)]
            if (!is.null(sel_type) && nzchar(sel_type) && sel_type == "Distant (any)") {
                sel_met_set <- setdiff(uniq_types, c("Locoregional","Peritoneum","Normal","Primary"))
            } else if (!is.null(sel_type) && nzchar(sel_type)) {
                sel_met_set <- sel_type
            } else {
                sel_met_set <- character(0)
            }
            list(sel_met_set = sel_met_set, sel_timing = sel_timing, sel_treated = sel_treated)
        }
        
        collapsed_on <- identical(size, "Collapsed")
        idx <- switch(vm, c1 = 1L, c2 = 2L, 0L)
        specs <- list()
        
        for (nm in names(obj)) {
            m <- obj[[nm]]
            dmat <- if (inherits(m, "dist")) as.matrix(m) else as.matrix(m)
            rn <- rownames(dmat); if (is.null(rn)) next
            
            # collapse mask: drop only explicit FALSE
            ic_map <- sa$in_collapsed; ic_vals <- ic_map[rn]
            keep_collapse <- if (!collapsed_on) rep(TRUE, length(rn)) else !( !is.na(ic_vals) & ic_vals == FALSE )
            
            if (identical(vm, "all") || !length(sa$type)) {
                keep_mask <- keep_collapse
                keep <- rn[keep_mask]
                kept_n <- length(keep)
                selected_n <- NA_integer_
                
                phy <- NULL; tooltip <- NULL
                if (kept_n >= 3) {
                    sub <- dmat[keep, keep, drop = FALSE]
                    d <- as_dist_safe(sub)
                    if (!is.null(d)) {
                        phy <- try(ape::nj(d), silent = TRUE)
                        phy <- try(phytools::reroot(phy, grep('^N', phy$tip.label)), silent = TRUE)
                        if (inherits(phy, "try-error") || !inherits(phy, "phylo")) { phy <- NULL; tooltip <- "Failed to build tree on subset" }
                    } else tooltip <- "Subset matrix invalid"
                } else tooltip <- sprintf("Too few tips in subset (n=%d)", kept_n)
                
                specs[[nm]] <- list(label = nm, phylo = phy, disabled = FALSE, tooltip = tooltip,
                                    kept_n = kept_n, selected_n = selected_n)
                next
            }
            
            # Cohort view
            choice <- get_selection(idx)
            sel_met_set <- choice$sel_met_set
            sel_timing  <- choice$sel_timing
            sel_treated <- choice$sel_treated
            have_selected_group <- length(sel_met_set) > 0
            
            typ <- sa$type[rn]; trt <- sa$treated[rn]; tim <- sa$timing[rn]
            keep_np  <- !is.na(typ) & typ %in% c("Normal","Primary")
            keep_sel <- rep(FALSE, length(rn))
            if (have_selected_group) {
                match_met <- !is.na(typ) & typ %in% sel_met_set
                match_trt <- if (is.null(sel_treated)) rep(TRUE, length(rn)) else (!is.na(trt) & trt == sel_treated)
                match_tim <- if (is.null(sel_timing))  rep(TRUE, length(rn)) else (!is.na(tim) & tim == sel_timing)
                keep_sel  <- match_met & match_trt & match_tim
            }
            keep_mask <- (keep_np | keep_sel) & keep_collapse
            keep <- rn[keep_mask]
            
            selected_n <- sum(keep_sel & keep_collapse, na.rm = TRUE)
            kept_n     <- length(keep)
            
            phy <- NULL; tooltip <- NULL
            if (kept_n >= 3) {
                sub <- dmat[keep, keep, drop = FALSE]
                d <- as_dist_safe(sub)
                if (!is.null(d)) {
                    phy <- try(ape::nj(d), silent = TRUE)
                    phy <- try(phytools::reroot(phy, grep('^N', phy$tip.label)), silent = TRUE)
                    if (inherits(phy, "try-error") || !inherits(phy, "phylo")) { phy <- NULL; tooltip <- "Failed to build tree on subset" }
                } else tooltip <- "Subset matrix invalid"
            } else tooltip <- sprintf("Too few tips in subset (n=%d)", kept_n)
            
            disabled <- (kept_n < 4) || (have_selected_group && selected_n < 2)
            if (disabled && is.null(tooltip)) {
                if (kept_n < 4) tooltip <- sprintf("Gray: total kept < 4 (n=%d)", kept_n)
                else            tooltip <- sprintf("Gray: < 2 selected-group tips (n=%d)", selected_n)
            }
            
            specs[[nm]] <- list(label = nm, phylo = phy, disabled = disabled, tooltip = tooltip,
                                kept_n = kept_n, selected_n = selected_n)
        }
        specs
    })
    
    # Build images + item metadata from specs
    rebuild_assets <- function(layout, specs, current_cohorts = NULL) {
        old <- list.files(trees_dir, full.names = TRUE)
        if (length(old)) unlink(old, recursive = TRUE, force = TRUE)
        if (is.null(specs) || !length(specs)) {
            items(list()); raw_list(list()); notes("No valid trees to display.")
            session$sendCustomMessage("setupDropZone", NULL)
            return(invisible(FALSE))
        }
        
        ptype <- if (identical(layout, "Rooted")) "phylogram" else "unrooted"
        out <- list(); built <- list(); idx <- 0L
        
        for (nm in names(specs)) {
            sp <- specs[[nm]]
            idx <- idx + 1L
            stamp <- as.integer(Sys.time())
            base <- paste0(sanitize_name(nm), "_", idx, "_", stamp)
            
            thumb_png <- file.path(trees_dir, paste0(base, "_thumb.png"))
            large_png <- file.path(trees_dir, paste0(base, "_large.png"))
            pdf_file  <- file.path(trees_dir, paste0(base, ".pdf"))
            
            if (!is.null(sp$phylo) && inherits(sp$phylo, "phylo")) {
                n_tips <- if (!is.null(sp$phylo$tip.label)) length(sp$phylo$tip.label) else 30L
                cex_thumb <- label_cex(n_tips, scale_min = 0.25, scale_max = 0.9,  ref = 32)
                cex_large <- label_cex(n_tips, scale_min = 0.45, scale_max = 1.2, ref = 42)
                cex_pdf   <- label_cex(n_tips, scale_min = 0.55, scale_max = 1.3, ref = 48)
                save_tree_plot(sp$phylo, thumb_png, type = ptype, device = "png", width = 190, height = 190, cex = cex_thumb)
                save_tree_plot(sp$phylo, large_png, type = ptype, device = "png", width = 520, height = 520, cex = cex_large)
                save_tree_plot(sp$phylo, pdf_file,  type = ptype, device = "pdf", width = 800, height = 800, cex = cex_pdf)
                built[[nm]] <- sp$phylo
            } else {
                msg <- sp$tooltip %||% "Insufficient tips"
                save_placeholder_plot(thumb_png, message = msg, device = "png", width = 190, height = 190)
                save_placeholder_plot(large_png, message = msg, device = "png", width = 520, height = 520)
                save_placeholder_plot(pdf_file,   message = msg, device = "pdf", width = 800, height = 800)
            }
            
            out[[length(out) + 1L]] <- list(
                id    = base, label = nm,
                thumb_url = sprintf("/%s/%s", prefix, basename(thumb_png)),
                large_url = sprintf("/%s/%s", prefix, basename(large_png)),
                pdf_url   = sprintf("/%s/%s", prefix, basename(pdf_file)),
                download_name = paste0(sanitize_name(nm), ".pdf"),
                disabled = isTRUE(sp$disabled),
                tooltip  = sp$tooltip %||% ""
            )
        }
        
        items(out)
        raw_list(built)
        
        if (!is.null(current_cohorts)) {
            valid_labs <- vapply(out, `[[`, "", "label")
            trimmed <- list(
                `Cohort 1` = intersect(current_cohorts[[1]], valid_labs),
                `Cohort 2` = intersect(current_cohorts[[2]], valid_labs)
            )
            cohort_labels(trimmed)
        }
        
        n_disabled <- sum(vapply(out, function(x) isTRUE(x$disabled), logical(1)))
        notes(paste0("Layout: ", layout, ". Generated ", length(out), " item(s). Grayed-out: ", n_disabled, "."))
        session$sendCustomMessage("setupDropZone", NULL)
        invisible(TRUE)
    }
    
    observe({
        specs <- build_current_specs()
        rebuild_assets(layout_mode(), specs, current_cohorts = cohort_labels())
    })
    
    # ---- Gallery (cohort + AND tag filter) ----
    output$gallery <- renderUI({
        its_all <- items()
        shiny::validate(shiny::need(length(its_all) > 0, "No trees to display yet. Load ./data.rds or upload a .rds file (named list of distance matrices)."))
        
        show <- its_all
        vm <- view_mode()
        if (vm != "all") {
            idx <- switch(vm, c1 = 1L, c2 = 2L, 0L)
            if (idx > 0) {
                labs <- cohort_labels()[[idx]]
                if (length(labs)) {
                    labset <- unique(labs)
                    show <- Filter(function(x) x$label %in% labset, its_all)
                } else show <- list()
            }
        }
        
        sel <- selected_tags()
        if (length(sel)) {
            show <- Filter(function(x) {
                # filter uses the same per-tree tag logic
                tgs <- tags_for_tree(x$label)
                all(sel %in% tgs)
            }, show)
        }
        
        if (!length(show)) return(tags$div(class = "note", "No trees in this view."))
        
        cols_cards <- card_tag_colors()
        lapply(show, function(it) {
            lbl <- it$label
            card_tgs <- card_tags_for_tree(lbl)
            chips <- NULL
            if (length(card_tgs)) {
                chips <- tags$div(
                    class = "tag-strip",
                    lapply(card_tgs, function(tag_nm) {
                        bg <- cols_cards[[tag_nm]] %||% "#666"
                        tags$span(class = "tag-chip", style = paste0("background:", bg, ";"), title = tag_nm, tag_nm)
                    })
                )
            }
            tags$div(
                class = paste("card", if (isTRUE(it$disabled)) "disabled" else ""),
                draggable = "true", `data-id` = it$id, `data-label` = lbl, title = it$tooltip %||% "",
                tags$div(class = "filename", lbl),
                tags$a(
                    href = it$pdf_url, download = it$download_name, class = "thumb",
                    tags$img(src = it$thumb_url, class = "thumbnail", alt = paste0(lbl, " (thumbnail)")),
                    tags$div(class = "popup",
                             tags$div(class = "popup-title", lbl),
                             tags$img(src = it$large_url, alt = paste0(lbl, " (preview)"))
                    )
                ),
                chips
            )
        })
    })
    
    # Status panel
    output$status <- renderUI({
        txt <- notes(); if (is.null(txt) || !nzchar(txt)) return(NULL)
        tags$div(
            class = "note",
            tags$strong("Status: "),
            tags$pre(style = "white-space: pre-wrap; font-size: 12px; margin: 6px 0 0 0;", txt)
        )
    })
    
    # ---- Boxplot demo ----
    boxplot_data <- eventReactive(input$test_boxplot, {
        cls <- cohort_labels()
        lst <- raw_list()
        if (is.null(lst) || !length(lst)) {
            return(data.frame(Cohort = character(), Tips = integer()))
        }
        get_counts <- function(labels) {
            labs <- intersect(labels, names(lst))
            if (!length(labs)) return(integer(0))
            vapply(labs, function(l) {
                tr <- lst[[l]]
                if (!is.null(tr) && !is.null(tr$tip.label)) length(tr$tip.label) else NA_integer_
            }, integer(1))
        }
        c1 <- get_counts(cls[[1]])
        c2 <- get_counts(cls[[2]])
        data.frame(
            Cohort = c(rep("Cohort 1", length(c1)), rep("Cohort 2", length(c2))),
            Tips   = c(c1, c2),
            stringsAsFactors = FALSE
        )
    }, ignoreInit = TRUE)
    
    output$cohort_plot <- renderPlot({
        df <- boxplot_data()
        if (is.null(df) || !nrow(df)) {
            ggplot() +
                theme_minimal() +
                theme(axis.text  = element_blank(), axis.title = element_blank(), panel.grid = element_blank())
        } else {
            ggplot(df, aes(x = Cohort, y = Tips)) +
                scale_y_continuous(limits=c(min(df$Tips),max(df$Tips)*1.025)) +
                geom_point(position=position_jitter(width=0.15, height=0, seed=42), pch=21, size=4, color='white', stroke=0.25, aes(fill=Cohort)) +
                stat_compare_means() +
                geom_boxplot(fill=NA,outlier.shape=NA) +
                labs(x = NULL, y = "Number of tips") +
                theme_minimal()
        }
    })
}

shinyApp(ui, server)
