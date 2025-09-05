# app.R (4 cohorts + JSON + CSV tag overlays + Tag cloud AND filter + banner)
library(shiny)
library(jsonlite)
library(ggplot2)


if (!requireNamespace("ape", quietly = TRUE)) {
  stop("This app requires the 'ape' package. Install it with install.packages('ape').")
}


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* --- Layout to enable independent vertical scroll --- */
      html, body, .container-fluid { height: 100%; }
      .page-root { display: flex; flex-direction: column; height: 100vh; }

      /* Title spacing tweaks (keeps original look) */
      .page-root > .title-panel { padding: 15px; padding-bottom: 0; }

      /* Two-column app area that fills the rest of the viewport */
      .app-wrap {
        display: flex;
        gap: 16px;
        padding: 12px 12px 12px 12px;
        flex: 1 1 auto;       /* fill available height */
        min-height: 0;        /* critical for flex overflow scrolling */
      }

      .left-col {
        flex: 0 0 320px;      /* fixed-ish sidebar width */
        overflow-y: auto;     /* independent vertical scroll */
        padding-right: 8px;
        border-right: 1px solid #eee;
      }

      .right-col {
        flex: 1 1 auto;       /* main area takes remaining width */
        overflow-y: auto;     /* independent vertical scroll */
        padding-left: 8px;
        min-width: 0;         /* prevent overflow due to wide content */
      }

      /* --- Your existing styles (kept intact) --- */
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
      .badge { font-size: 11px; background: #eef2ff; color: #334; border: 1px solid #c8d1ff; border-radius: 999px; padding: 3px 8px; max-width: 100%; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
      .cohort-header { display:flex; align-items:center; justify-content:space-between; gap:8px; }
      .cohort-title { margin:0; font-size:14px; }

      .tag-cloud-head { display:flex; align-items:center; justify-content:space-between; gap:8px; }
      .tag-cloud { display:flex; flex-wrap:wrap; gap:8px; margin-top:6px; }
      .tag-pill { font-size: 11px; color:#fff; border-radius: 999px; padding: 6px 10px; cursor: pointer; user-select:none; box-shadow: 0 1px 2px rgba(0,0,0,.18); transition: opacity .12s ease, filter .12s ease, transform .04s ease; }
      .tag-pill.active { outline: 2px solid rgba(0,0,0,.25); transform: translateY(-1px); }
      .tag-pill.inactive { opacity: 0.38; filter: grayscale(0.2); }
      .tag-legend { font-size:11px; color:#444; margin-top:6px; }

      /* small close button inside cohort badges */
      .badge { position: relative; padding-right: 18px; }
      .badge-close {
        position: absolute; right: 4px; top: 50%; transform: translateY(-50%);
        display: inline-block; font-weight: 700; line-height: 1;
        width: 14px; height: 14px; text-align: center; border-radius: 50%;
        background: #d9534f; color: #fff; font-size: 10px; cursor: pointer;
        box-shadow: 0 1px 1px rgba(0,0,0,.15);
      }
      .badge-close:hover { filter: brightness(0.95); }

      .filter-banner { margin: 8px 0 12px 0; padding: 8px 12px; background: #f4f7ff; border: 1px solid #cfe0ff; border-radius: 10px; font-size: 12px; color: #244; }
    ")),
    # (Your original <script> stays unchanged)
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

  # Root container with title + 2 scrollable columns
  div(class = "page-root",
    div(class = "title-panel",
      titlePanel("Phylo Tree Gallery (auto-loads ./data.rds)")
    ),
    div(class = "app-wrap",
      # LEFT: control panel (was sidebarPanel)
      div(class = "left-col",
        tags$p(
          class = "help-text",
          strong("Expected input: "), code("data.rds"),
          " should contain a ",
          strong("named list of ", code("ape::phylo"), " objects"),
          ". You can also upload another .rds to override."
        ),
        fileInput("rds", "Upload .rds (named list of phylo objects)", accept = ".rds"),
        actionButton("reload_default", "Reload ./data.rds"),
        br(), br(),

        tags$p(class = "help-text",
               strong("Optional tags CSV:"),
               " rows = tags; columns = tree names; cells == 1 indicate the tag applies."
        ),
        fileInput("tag_csv", "Upload tag matrix (CSV)", accept = c(".csv", ".txt")),
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
            actionLink("view_c2", label = "Cohort 2"),
          ),

          div(class = "cohort-zone", id = "cohortDrop1",
            div(class="cohort-header",
              tags$h5(class="cohort-title","Cohort 1"),
              actionButton("clear_c1", "Clear", class="btn btn-xs")
            ),
            div(class = "cohort-badges", uiOutput("cohort_badges1"))
          ),
          div(class = "cohort-zone", id = "cohortDrop2",
            div(class="cohort-header",
              tags$h5(class="cohort-title","Cohort 2"),
              actionButton("clear_c2", "Clear", class="btn btn-xs")
            ),
            div(class = "cohort-badges", uiOutput("cohort_badges2"))
          ),
          uiOutput("cohort_counts"),
          div(class = "cohort-actions",
            actionButton("test_boxplot", "Test")
          ),
          br(),
          downloadButton("download_cohorts", "Download cohorts JSON")
        ),

        br(),
        tags$hr(),
        tags$h4("Plot"),
        plotOutput("cohort_plot", height = "260px"),
        uiOutput("status")
      ),

      # RIGHT: tree view (was mainPanel)
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

  sanitize_name <- function(x) { if (is.null(x) || !nzchar(x)) x <- "unnamed"; gsub("[^A-Za-z0-9_-]+", "_", x) }
  label_cex <- function(n, scale_min = 0.25, scale_max = 1.2, ref = 40) {
    cex <- ref / max(10, n); max(scale_min, min(scale_max, cex))
  }
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
  raw_list  <- reactiveVal(NULL)       # named list of phylo objects
  items     <- reactiveVal(list())     # rendered asset metadata
  notes     <- reactiveVal(NULL)

  # Tag data (matrix + colors + multi-select)
  tags_mat    <- reactiveVal(NULL)                # rows=tags, cols=tree names; 1=present
  tag_colors  <- reactiveVal(setNames(character(), character()))
  selected_tags <- reactiveVal(character())       # multi-select vector

  # Four cohorts
  empty_cohorts <- list(`Cohort 1` = character(), `Cohort 2` = character())
  cohort_labels <- reactiveVal(empty_cohorts)

  view_mode <- reactiveVal("all")     # 'all','c1','c2'
  current_layout <- "Unrooted"

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
    # escape for inclusion in a JS string
    l_js <- htmltools::htmlEscape(l, attribute = TRUE)
    tags$span(
      class = "badge",
      title = l,
      l,
      tags$span(
        class = "badge-close",
        title = sprintf("Remove “%s” from Cohort 1", l),
        onclick = sprintf(
          "Shiny.setInputValue('cohort_remove', { idx: 1, label: '%s', nonce: Date.now() }, {priority: 'event'});",
          l_js
        ),
        "\u00D7"  # ×
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
        onclick = sprintf(
          "Shiny.setInputValue('cohort_remove', { idx: 2, label: '%s', nonce: Date.now() }, {priority: 'event'});",
          l_js
        ),
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
  # IMPORTANT: don't change view_mode here
  # view_mode(paste0("c", idx))  # <- delete this line
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


  # -------- Asset builder ----------
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
    ptype <- plot_type_for_layout(layout)

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

  # -------- Loaders ----------
  load_from_path <- function(path, source_label = NULL) {
    obj <- try(readRDS(path), silent = TRUE)
    if (inherits(obj, "try-error")) {
      items(list()); cohort_labels(empty_cohorts); view_mode("all"); raw_list(NULL)
      notes(paste0("Failed to read RDS (", if (!is.null(source_label)) source_label else path, "): ",
                   attr(obj, "condition")$message))
      return(invisible(FALSE))
    }
    if (!is.list(obj)) {
      items(list()); cohort_labels(empty_cohorts); view_mode("all"); raw_list(NULL)
      notes("The RDS must contain a named list of ape::phylo objects.")
      return(invisible(FALSE))
    }
    if (is.null(names(obj))) names(obj) <- paste0("item_", seq_along(obj))
    raw_list(obj)

    cohort_labels(empty_cohorts)
    notes(paste0("Loaded ", length(obj), " tree", if (length(obj) == 1) "" else "s",
                 if (!is.null(source_label)) paste0(" from ", source_label) else ""))

    rebuild_assets(current_layout, obj, current_cohorts = empty_cohorts)
    view_mode("all")
    invisible(TRUE)
  }

  if (file.exists("data.rds")) {
    load_from_path("data.rds", source_label = "./data.rds")
  } else {
    notes("Tip: place a file named data.rds (named list of ape::phylo) in the app's working directory to auto-load on startup.")
  }

  observeEvent(input$reload_default, {
    if (file.exists("data.rds")) {
      load_from_path("data.rds", source_label = "./data.rds")
    } else {
      items(list()); cohort_labels(empty_cohorts); view_mode("all"); raw_list(NULL)
      notes("No ./data.rds found in the current working directory.")
    }
  })

  observeEvent(input$rds, { req(input$rds$datapath); load_from_path(input$rds$datapath, source_label = "uploaded file") }, ignoreInit = TRUE)

  # Layout toggle
  observeEvent(input$layout, {
    current_layout <<- input$layout
    lst <- raw_list()
    if (!is.null(lst)) {
      rebuild_assets(current_layout, lst, current_cohorts = cohort_labels())
    }
  }, ignoreInit = TRUE)

  # ---- TAG CSV loader ----
  observeEvent(input$tag_csv, {
    req(input$tag_csv$datapath)
    df <- try(utils::read.csv(input$tag_csv$datapath, check.names = FALSE, row.names = 1), silent = TRUE)
    if (inherits(df, "try-error")) {
      notes(paste0("Failed to read tag CSV: ", attr(df, "condition")$message))
      tags_mat(NULL); tag_colors(setNames(character(), character())); selected_tags(character())
      return(invisible())
    }
    if (!nrow(df) || !ncol(df)) {
      notes("Tag CSV has no rows or columns; ignoring.")
      tags_mat(NULL); tag_colors(setNames(character(), character())); selected_tags(character())
      return(invisible())
    }

    mat <- suppressWarnings(as.matrix(df))
    mode(mat) <- "numeric"
    mat[is.na(mat)] <- 0
    mat[mat != 1] <- 0

    tg_names <- rownames(mat)
    n <- length(tg_names)
    if (n) {
      hues <- seq(15, 375, length.out = n + 1L)[1:n]
      cols <- grDevices::hcl(h = hues, c = 80, l = 55)
      names(cols) <- tg_names
      tag_colors(cols)
    } else {
      tag_colors(setNames(character(), character()))
    }
    tags_mat(mat)
    selected_tags(character())  # reset
    # Report overlap
    tree_labels <- vapply(items(), `[[`, "", "label")
    matched <- intersect(colnames(mat), tree_labels)
    notes(paste0(
      "Loaded tag CSV with ", nrow(mat), " tags over ", ncol(mat), " columns. ",
      if (length(matched)) paste0("Matched ", length(matched), " tree name(s).") else "No tree names matched."
    ))
  }, ignoreInit = TRUE)

  # Helper: tags for a given tree label
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
    return(tags$div(class = "note", "Upload a tag CSV to enable tag filtering."))
  }

  tg_names <- rownames(mat)
  sel <- selected_tags()

  # Always make tree_subset a LIST of item records, never a character vector
  its_all <- items()
  vm <- view_mode()
  tree_subset <- if (identical(vm, "all")) {
    its_all
  } else {
    idx  <- switch(vm, c1 = 1L, c2 = 2L, c3 = 3L, c4 = 4L, 0L)
    labs <- if (idx > 0) cohort_labels()[[idx]] else character()
    if (length(labs)) Filter(function(x) x$label %in% labs, its_all) else list()
  }
  tree_labels <- if (length(tree_subset)) vapply(tree_subset, `[[`, "", "label") else character()

  # Count how many trees carry each tag (within current cohort view)
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
      cnt <- counts[i]  # avoid potential name lookup issues
      tags$span(
        class = cls,
        style = paste0("background:", bg, ";"),
        onclick = sprintf(
          "Shiny.setInputValue('tag_clicked', '%s', {priority: 'event'})",
          htmltools::htmlEscape(tg, attribute = TRUE)
        ),
        title = sprintf("%s (%d)", tg, cnt),
        sprintf("%s (%d)", tg, cnt)
      )
    })
  )
})


  observeEvent(input$tag_clicked, {
    tg <- input$tag_clicked
    cur <- selected_tags()
    if (tg %in% cur) {
      selected_tags(setdiff(cur, tg))   # deselect
    } else {
      selected_tags(c(cur, tg))         # add
    }
  }, ignoreInit = TRUE)

  observeEvent(input$clear_tag, {
    selected_tags(character())
  })

  output$tag_filter_status <- renderUI({
    sel <- selected_tags()
    if (!length(sel)) return(NULL)
    tags$div(class = "tag-legend", sprintf("Selected tags (AND): %s", paste(sel, collapse = " AND ")))
  })

  # ---- Banner above gallery ----
  output$filter_banner <- renderUI({
    sel <- selected_tags()
    if (!length(sel)) return(NULL)
    tags$div(class = "filter-banner",
      tags$b("Showing trees with: "),
      paste(sel, collapse = " AND ")
    )
  })

  # ---- Gallery render (cohort view + AND tag filter) ----
  output$gallery <- renderUI({
    its_all <- items()
    shiny::validate(shiny::need(length(its_all) > 0, "No trees to display yet. Load ./data.rds or upload a .rds file (named list of phylo)."))

    # Cohort filtering first
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

    # AND tag filtering
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
    # Empty ggplot “placeholder”
    ggplot() +
      theme_minimal() +
      theme(
        axis.text  = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
      )
  } else {
    ggplot(df, aes(x = Cohort, y = Tips)) +
      geom_boxplot() +
      labs(x = NULL, y = "Number of tips") +
      theme_minimal()
  }
})

}

`%||%` <- function(a, b) if (is.null(a) || (is.character(a) && !nzchar(a))) b else a

shinyApp(ui, server)

