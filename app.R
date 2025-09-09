# app.R (Cohort tip-subset by met_type)
# Modified to subset each tree's distance matrix by sample met_type when Cohort 1/2 is selected,
# then rebuild NJ trees from the subset matrices.

library(shiny)
library(jsonlite)
library(ggplot2)
library(rds)
library(ggpubr)
library(phytools)

if (!requireNamespace("ape", quietly = TRUE)) {
  stop("This app requires the 'ape' package. Install it with install.packages('ape').")
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
      .card { position: relative; font-family: system-ui,-apple-system,Segoe UI,Roboto,Ubuntu,Cantarell,'Helvetica Neue',Arial,'Noto Sans','Apple Color Emoji','Segoe UI Emoji'; }
      .filename { font-size: 12px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; margin-bottom: 6px; color:#333; }
      .thumb { position: relative; display: inline-block; cursor: pointer; width: 100%; }
      .thumbnail { width: 100%; aspect-ratio: 1 / 1; object-fit: contain; background:#fff; border:1px solid #d0d0d0; border-radius:10px; box-shadow: 0 1px 3px rgba(0,0,0,.06); }
      .tag-strip { position: absolute; right: 8px; bottom: 8px; display: flex; flex-wrap: wrap; gap: 6px; justify-content: flex-end; max-width: calc(100% - 16px); pointer-events: none; }
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
          " should contain a ",
          strong("named list of distance matrices (class ", code("matrix"), ")"),
          ". Each matrix will be converted to a tree with ", code("ape::nj()"), "."
        ),
        fileInput("rds", "Upload .rds (named list of distance matrices)", accept = ".rds"),
        actionButton("reload_default", "Reload ./data.rds"),
        br(), br(),

        tags$p(class = "help-text",
               strong("Optional tags TSV:"),
               " include columns ", code("sample"), " and ", code("met_type"),
               " to enable cohort tip subsetting. ",
               "You may also include ", code("Patient_ID"), ", ", code("met_treated"), ", ", code("met_timing"),
               " to drive the tag cloud."
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

        div(class = "cohort-section",
          tags$h4("Cohorts"),
          div(class = "cohort-actions",
            actionLink("view_all", label = "Full cohort"),
            HTML("&nbsp;|&nbsp;"),
            actionLink("view_c1", label = "Cohort 1"),
            HTML("&nbsp;|&nbsp;"),
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
                  selected = NULL, width = "100%", selectize = FALSE
                ),
                selectInput(
                  "cohort1_treatment", "Metastasis treatment",
                  choices = c("No preference", "Untreated", "Systemic chemo", "Sys-chemo after untreated", "HIPEC"),
                  selected = NULL, width = "100%", selectize = FALSE
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
                  choices = c("No preference", "Synchronous", "Metachronous", "Meta-after-sync"),
                  selected = NULL, width = "100%", selectize = FALSE
                ),
                selectInput(
                  "cohort2_treatment", "Metastasis treatment",
                  choices = c("No preference", "Untreated", "Systemic chemo", "Sys-chemo after untreated", "HIPEC"),
                  selected = NULL, width = "100%", selectize = FALSE
                )
            )
          ),
          uiOutput("cohort_counts"),
          div(class = "cohort-actions",
            actionButton("test_boxplot", "Calculate RDS/QDS")
          )
        ),

        br(),
        tags$hr(),
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
  plot_type_for_layout <- function(layout) if (identical(layout, "Rooted")) "phylogram" else "unrooted"

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

  # --- Reactive state ---
  dist_list  <- reactiveVal(NULL)      # named list of distance matrices (original)
  raw_list   <- reactiveVal(NULL)      # last list of phylo objects used to render (for plotting, stats)
  items      <- reactiveVal(list())
  notes      <- reactiveVal(NULL)

  # Tag data (patient-level tag cloud) - optional
  tags_mat       <- reactiveVal(NULL)
  tag_colors     <- reactiveVal(setNames(character(), character()))
  selected_tags  <- reactiveVal(character())

  # Sample-level met_type mapping for cohort subsetting
  sample_met_map <- reactiveVal(setNames(character(), character()))

  # Two cohorts
  empty_cohorts <- list(`Cohort 1` = character(), `Cohort 2` = character())
  cohort_labels <- reactiveVal(empty_cohorts)

  view_mode     <- reactiveVal("all")               # 'all','c1','c2'
  layout_mode   <- reactiveVal("Unrooted")          # reactive instead of global

  # ---- Cohort counters/badges ----
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
        class = "badge",
        title = l,
        l,
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
        class = "badge",
        title = l,
        l,
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

  # Convert named list of (dist or symmetric matrix) -> list of phylo
  convert_list_mats_to_phylo <- function(obj) {
    nms <- names(obj); if (is.null(nms)) nms <- paste0("item_", seq_along(obj))
    phy <- list(); out_names <- character(0)
    for (i in seq_along(obj)) {
      nm <- nms[[i]]; el <- obj[[i]]
      d <- as_dist_safe(el); if (is.null(d)) next
      tr <- try(ape::nj(d), silent = TRUE)
      tr <- try(phytools::reroot(tr, grep('^N', tr$tip.label)), silent = TRUE)
      if (inherits(tr, "try-error") || !inherits(tr, "phylo")) next
      phy[[length(phy)+1L]] <- tr
      out_names <- c(out_names, nm)
    }
    if (length(phy)) names(phy) <- out_names
    phy
  }

  # Subset matrices by allowed met_type tips, then convert
  subset_and_convert_by_met <- function(obj, allowed_types, met_map) {
    nms <- names(obj); if (is.null(nms)) nms <- paste0("item_", seq_along(obj))
    out <- list(); out_names <- character(0)
    for (i in seq_along(obj)) {
      nm <- nms[[i]]; m <- obj[[i]]
      dmat <- if (inherits(m, "dist")) as.matrix(m) else as.matrix(m)
      rn <- rownames(dmat)
      if (is.null(rn)) {
        next
      }
      keep <- rn[rn %in% names(met_map) & met_map[rn] %in% allowed_types]
      if (length(keep) >= 3) {
        sub <- dmat[keep, keep, drop = FALSE]
        d <- as_dist_safe(sub); if (is.null(d)) next
        tr <- try(ape::nj(d), silent = TRUE)
        tr <- try(phytools::reroot(tr, grep('^N', tr$tip.label)), silent = TRUE)
        if (inherits(tr, "try-error") || !inherits(tr, "phylo")) next
        out[[length(out)+1L]] <- tr
        out_names <- c(out_names, nm)
      } else {
        # If fewer than 3 tips match, skip rendering this tree in subset mode
        next
      }
    }
    if (length(out)) names(out) <- out_names
    out
  }

  load_from_path <- function(path, source_label = NULL) {
    obj <- try(readRDS(path), silent = TRUE)
    if (inherits(obj, "try-error")) {
      items(list()); cohort_labels(empty_cohorts); view_mode("all"); dist_list(NULL); raw_list(NULL)
      notes(paste0("Failed to read RDS (", if (!is.null(source_label)) source_label else path, "): ", attr(obj, "condition")$message))
      return(invisible(FALSE))
    }
    if (!is.list(obj)) {
      items(list()); cohort_labels(empty_cohorts); view_mode("all"); dist_list(NULL); raw_list(NULL)
      notes("The RDS must contain a named list of distance matrices (class 'matrix' or 'dist').")
      return(invisible(FALSE))
    }
    if (is.null(names(obj))) names(obj) <- paste0("item_", seq_along(obj))

    # Keep original matrices; rebuilt later depending on view
    dist_list(obj)
    cohort_labels(empty_cohorts)
    notes(paste0("Loaded ", length(obj), " matrices", if (!is.null(source_label)) paste0(" from ", source_label) else "", "."))
    view_mode("all")
    invisible(TRUE)
  }

  if (file.exists("data.rds")) {
    load_from_path("data.rds", source_label = "./data.rds")
  } else {
    notes("Tip: place a file named data.rds (named list of distance matrices) in the app's working directory to auto-load on startup.")
  }

  observeEvent(input$reload_default, {
    if (file.exists("data.rds")) {
      load_from_path("data.rds", source_label = "./data.rds")
    } else {
      items(list()); cohort_labels(empty_cohorts); view_mode("all"); dist_list(NULL); raw_list(NULL)
      notes("No ./data.rds found in the current working directory.")
    }
  })

  observeEvent(input$rds, { req(input$rds$datapath); load_from_path(input$rds$datapath, source_label = "uploaded file") }, ignoreInit = TRUE)

  # Layout toggle
  observeEvent(input$layout, { layout_mode(input$layout) }, ignoreInit = TRUE)

  # ---------- TAG TSV LOADER (patient tags + sample met_type) ----------
  # Patient-level tag matrix (optional, for tag cloud)
  build_tag_matrix_from_tsv <- function(df) {
    need_any <- c("Patient_ID","met_treated","met_timing")
    if (!all(need_any %in% colnames(df))) {
      return(matrix(0, nrow = 0, ncol = 0, dimnames = list(character(0), character(0))))
    }
    df$Patient_ID  <- trimws(as.character(df$Patient_ID))
    df$met_treated <- trimws(as.character(df$met_treated))
    df$met_timing  <- trimws(as.character(df$met_timing))
    vals_treated <- unique(df$met_treated[nzchar(df$met_treated)])
    vals_timing  <- unique(df$met_timing[nzchar(df$met_timing)])
    tag_levels   <- sort(unique(c(vals_treated, vals_timing)))
    patients <- sort(unique(df$Patient_ID[nzchar(df$Patient_ID)]))
    if (!length(tag_levels) || !length(patients)) {
      return(matrix(0, nrow = 0, ncol = 0))
    }
    mat <- matrix(0, nrow = length(tag_levels), ncol = length(patients),
                  dimnames = list(tag_levels, patients))
    split(df, df$Patient_ID) |>
      lapply(function(d) {
        union(
          unique(d$met_treated[nzchar(d$met_treated)]),
          unique(d$met_timing[nzchar(d$met_timing)])
        )
      }) |>
      (\(patient_tags) {
        for (pid in names(patient_tags)) {
          tset <- intersect(patient_tags[[pid]], rownames(mat))
          if (length(tset) && pid %in% colnames(mat)) mat[tset, pid] <- 1
        }
        mat
      })()
  }

  # Sample-level met_type
  build_sample_met_from_tsv <- function(df) {
    need <- c("sample","met_type")
    if (!all(need %in% colnames(df))) return(setNames(character(), character()))
    s <- trimws(as.character(df$sample))
    mt <- trimws(as.character(df$met_type))
    keep <- nzchar(s) & nzchar(mt)
    s <- s[keep]; mt <- mt[keep]
    # collapse duplicates by last occurrence
    if (length(s)) {
      idx <- !duplicated(s, fromLast = TRUE)
      setNames(mt[which(idx)], s[which(idx)])
    } else {
      setNames(character(), character())
    }
  }

  observeEvent(input$tag_csv, {
    req(input$tag_csv$datapath)
    df <- try(utils::read.delim(input$tag_csv$datapath, check.names = FALSE, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(df, "try-error")) {
      notes(paste0("Failed to read tags TSV: ", attr(df, "condition")$message))
      tags_mat(NULL); tag_colors(setNames(character(), character())); selected_tags(character())
      sample_met_map(setNames(character(), character()))
      return(invisible())
    }

    # Patient-level tag cloud (optional)
    mat <- try(build_tag_matrix_from_tsv(df), silent = TRUE)
    if (inherits(mat, "try-error") || !nrow(mat) || !ncol(mat)) {
      tags_mat(NULL); tag_colors(setNames(character(), character())); selected_tags(character())
    } else {
      tg_names <- rownames(mat); n <- length(tg_names)
      if (n) {
        hues <- seq(15, 375, length.out = n + 1L)[1:n]
        cols <- grDevices::hcl(h = hues, c = 80, l = 55)
        names(cols) <- tg_names
        tag_colors(cols)
      } else {
        tag_colors(setNames(character(), character()))
      }
      tags_mat(mat); selected_tags(character())
    }

    # Sample-level met_type mapping (required for cohort tip subsetting)
    sm <- build_sample_met_from_tsv(df)
    sample_met_map(sm)

    # Status
    msg <- sprintf("Loaded tags TSV. sample/met_type pairs: %d.", length(sm))
    if (!is.null(mat) && nrow(mat) && ncol(mat)) {
      tree_labels <- vapply(items(), `[[`, "", "label")
      matched <- intersect(colnames(mat), tree_labels)
      msg <- paste0(msg, " Tag cloud available with ", nrow(mat), " distinct tag(s) across ", ncol(mat), " patient(s).")
      if (length(matched)) msg <- paste0(msg, " Matched ", length(matched), " tree name(s).")
    }
    notes(msg)
  }, ignoreInit = TRUE)
  # ---------- END TSV LOADER ----------

  # Helper: tags for a given tree label (patient-level)
  tags_for_tree <- function(label) {
    mat <- tags_mat()
    if (is.null(mat) || !length(mat)) return(character())
    if (!(label %in% colnames(mat))) return(character())
    inds <- which(mat[, label, drop = TRUE] == 1)
    if (!length(inds)) return(character())
    rownames(mat)[inds]
  }

  output$tag_cloud <- renderUI({
    mat  <- tags_mat()
    cols <- tag_colors()
    if (is.null(mat) || !nrow(mat)) {
      return(tags$div(class = "note", "Upload a tags TSV with patient tags to enable tag filtering (optional)."))
    }
    tg_names <- rownames(mat)
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

    counts <- vapply(tg_names, function(tg) {
      if (!tg %in% rownames(mat)) return(0L)
      have_tg <- colnames(mat)[mat[tg, , drop = TRUE] == 1]
      length(intersect(have_tg, tree_labels))
    }, integer(1))
    names(counts) <- tg_names

    tags$div(class = "tag-cloud",
      lapply(seq_along(tg_names), function(i) {
        tg <- tg_names[[i]]
        bg <- cols[[tg]] %||% "#666"
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
    tags$div(class = "tag-legend", sprintf("Selected tags (AND): %s", paste(sel, collapse = " AND ")))
  })

  output$filter_banner <- renderUI({
    sel <- selected_tags()
    if (!length(sel)) return(NULL)
    tags$div(class = "filter-banner",
      tags$b("Showing trees with: "),
      paste(sel, collapse = " AND ")
    )
  })

  # -------- Central re-render pipeline --------
  # Build the list of phylo objects to render, based on view (all vs c1/c2) and TSV sample met_type
  build_current_phy_list <- reactive({
    obj <- dist_list()
    if (is.null(obj) || !length(obj)) return(list())
    vm <- view_mode()
    sm <- sample_met_map()
    layout <- layout_mode()  # ensure reactive dep

    if (identical(vm, "all") || !length(sm)) {
      # Full tips (either no TSV, or viewing 'all')
      convert_list_mats_to_phylo(obj)
    } else {
      idx <- switch(vm, c1 = 1L, c2 = 2L, 0L)
      sel_type <- if (idx == 1L) input$cohort1_type else if (idx == 2L) input$cohort2_type else NULL
      allowed <- unique(c("Normal", "Primary", sel_type %||% character()))
      subset_and_convert_by_met(obj, allowed, sm)
    }
  })

  # Call rebuild_assets whenever relevant inputs change
  rebuild_assets <- function(layout, lst, current_cohorts = NULL) {
    old <- list.files(trees_dir, full.names = TRUE)
    if (length(old)) unlink(old, recursive = TRUE, force = TRUE)
    if (is.null(lst) || !length(lst)) {
      items(list()); notes("No valid trees to render.")
      session$sendCustomMessage("setupDropZone", NULL)
      return(invisible(FALSE))
    }

    nms <- names(lst); if (is.null(nms)) nms <- paste0("item_", seq_along(lst))
    out <- list(); idx <- 0L
    ptype <- if (identical(layout, "Rooted")) "phylogram" else "unrooted"

    for (i in seq_along(lst)) {
      nm <- nms[[i]]; el <- lst[[i]]; if (!inherits(el, "phylo")) next
      idx <- idx + 1L
      stamp <- as.integer(Sys.time())
      base <- paste0(sanitize_name(nm), "_", idx, "_", stamp)

      thumb_png <- file.path(trees_dir, paste0(base, "_thumb.png"))
      large_png <- file.path(trees_dir, paste0(base, "_large.png"))
      pdf_file  <- file.path(trees_dir, paste0(base, ".pdf"))

      n_tips <- if (!is.null(el$tip.label)) length(el$tip.label) else 30L
      cex_thumb <- label_cex(n_tips, scale_min = 0.25, scale_max = 0.9,  ref = 32)
      cex_large <- label_cex(n_tips, scale_min = 0.45, scale_max = 1.2, ref = 42)
      cex_pdf   <- label_cex(n_tips, scale_min = 0.55, scale_max = 1.3, ref = 48)

      save_tree_plot(el, thumb_png, type = ptype, device = "png", width = 190, height = 190, cex = cex_thumb)
      save_tree_plot(el, large_png, type = ptype, device = "png", width = 520, height = 520, cex = cex_large)
      save_tree_plot(el, pdf_file,  type = ptype, device = "pdf", width = 800, height = 800, cex = cex_pdf)

      out[[length(out) + 1L]] <- list(
        id    = base, label = nm,
        thumb_url = sprintf("/%s/%s", prefix, basename(thumb_png)),
        large_url = sprintf("/%s/%s", prefix, basename(large_png)),
        pdf_url   = sprintf("/%s/%s", prefix, basename(pdf_file)),
        download_name = paste0(sanitize_name(nm), ".pdf")
      )
    }

    items(out)

    if (!is.null(current_cohorts)) {
      valid_labs <- vapply(out, `[[`, "", "label")
      trimmed <- list(
        `Cohort 1` = intersect(current_cohorts[[1]], valid_labs),
        `Cohort 2` = intersect(current_cohorts[[2]], valid_labs)
      )
      cohort_labels(trimmed)
    }

    notes(paste0("Layout: ", layout, ". Generated ", length(out), " tree", if (length(out) == 1) "" else "s", "."))
    session$sendCustomMessage("setupDropZone", NULL)
    invisible(TRUE)
  }

  observe({
    lst <- build_current_phy_list()
    raw_list(lst)
    rebuild_assets(layout_mode(), lst, current_cohorts = cohort_labels())
  })

  # ---- Gallery render (cohort view + AND tag filter) ----
  output$gallery <- renderUI({
    its_all <- items()
    shiny::validate(shiny::need(length(its_all) > 0, "No trees to display yet. Load ./data.rds or upload a .rds file (named list of distance matrices)."))

    # Cohort filtering first: only show cards for the chosen cohort
    show <- its_all
    vm <- view_mode()
    if (vm != "all") {
      idx <- switch(vm, c1 = 1L, c2 = 2L, 0L)
      if (idx > 0) {
        labs <- cohort_labels()[[idx]]
        if (length(labs)) {
          labset <- unique(labs)
          show <- Filter(function(x) x$label %in% labset, its_all)
        } else {
          show <- list()
        }
      }
    }

    # AND tag filtering (patient-level)
    sel <- selected_tags()
    if (length(sel)) {
      show <- Filter(function(x) {
        tgs <- tags_for_tree(x$label)
        all(sel %in% tgs)
      }, show)
    }

    if (!length(show)) return(tags$div(class = "note", "No trees in this view."))

    cols <- tag_colors()
    lapply(show, function(it) {
      lbl <- it$label
      tgs <- tags_for_tree(lbl)
      chips <- NULL
      if (length(tgs)) {
        chips <- tags$div(
          class = "tag-strip",
          lapply(tgs, function(tag_nm) {
            bg <- cols[[tag_nm]] %||% "#666"
            tags$span(class = "tag-chip", style = paste0("background:", bg, ";"), title = tag_nm, tag_nm)
          })
        )
      }
      tags$div(
        class = "card", draggable = "true", `data-id` = it$id, `data-label` = lbl,
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

