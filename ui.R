# ============================================================
#  🎵  SOUNDSCOPE — Spotify Analytics Dashboard
#  ui.R
#  NOTE: libraries, df, all_genres, top20_genres, audio_features
#        are all defined in global.R — do NOT redefine here.
# ============================================================

# ── 1. Theme ────────────────────────────────────────────────
spotify_theme <- bs_theme(
  version            = 5,
  bg                 = "#0D0D0D",
  fg                 = "#EDEDED",
  primary            = "#1DB954",
  secondary          = "#535353",
  # base_font          = font_google("DM Sans"),
  # heading_font       = font_google("Space Grotesk"),
  # code_font          = font_google("JetBrains Mono"),
  "navbar-bg"        = "#121212",
  "card-bg"          = "#181818",
  "card-border-color"= "#282828"
)

# ── 2. Reusable card wrapper ─────────────────────────────────
card_ <- function(..., title = NULL, height = NULL) {
  style_str <- if (!is.null(height)) paste0("height:", height, "px;") else ""
  div(class = "dash-card",
      style = style_str,
      if (!is.null(title)) div(class = "card-title-bar", title),
      ...
  )
}

# ── 3. Custom CSS ────────────────────────────────────────────
custom_css <- "
/* ── Base ── */
body { background:#0D0D0D; color:#EDEDED; font-family:'DM Sans',sans-serif; }

/* ── bslib Main Layout Override ── */
.bslib-sidebar-layout > .main, .tab-pane {
  /* Shrinks the massive default gap under the top navbar */
  padding-top: 0.5rem !important; 
}

/* ── Sidebar ── */
.bslib-sidebar-layout > .sidebar {
  background: #121212 !important;
  border-right: 1px solid #282828 !important;
}
.sidebar .form-label, .sidebar label { color:#B3B3B3 !important; font-size:.82rem; }
.sidebar .form-select, .sidebar .form-control {
  background:#282828; border:1px solid #383838;
  color:#EDEDED; border-radius:6px;
}
.sidebar .form-select:focus, .sidebar .form-control:focus {
  border-color:#1DB954; box-shadow:0 0 0 2px rgba(29,185,84,.25);
}

/* ── Nav pills ── */
.nav-pills .nav-link { color:#B3B3B3; border-radius:20px; font-size:.88rem; padding:.35rem 1rem; }
.nav-pills .nav-link.active { background:#1DB954 !important; color:#000 !important; font-weight:600; }
.nav-pills .nav-link:hover { background:#282828; color:#EDEDED; }

/* ── Cards ── */
.dash-card {
  background:#181818; border:1px solid #282828; border-radius:12px;
  padding:0.75rem 1rem; margin-bottom:1rem;
}
.card-title-bar {
  font-family:'Space Grotesk',sans-serif; font-weight:600;
  font-size:.95rem; color:#1DB954; margin-bottom:.4rem;
  letter-spacing:.04em; text-transform:uppercase;
}

/* ── KPI tiles ── */
.kpi-row { display:flex; gap:.75rem; flex-wrap:wrap; margin-bottom:1rem; }
.kpi-tile {
  flex:1; min-width:110px; background:#181818; border:1px solid #282828;
  border-radius:10px; padding:.25rem .5rem; text-align:center;
}
.kpi-value { font-size:1.6rem; line-height: 1; font-weight:700; color:#1DB954; font-family:'Space Grotesk',sans-serif; margin-top: 0.2rem;}
.kpi-label { font-size:.72rem; color:#B3B3B3; text-transform:uppercase; letter-spacing:.06em; }

/* ── Logo ── */
.logo-wrap {
  display:flex; align-items:center; gap:10px;
  padding:1rem 1.2rem .5rem;
}
.logo-icon {
  width:36px; height:36px;
  display:flex; align-items:center; justify-content:center;
  font-size:18px;
}
.logo-text { font-family:'Space Grotesk',sans-serif; font-size:1.1rem; font-weight:700; line-height:1.1; }
.logo-sub  { font-size:.7rem; color:#B3B3B3; letter-spacing:.08em; text-transform:uppercase; }

/* ── DT table ── */
.dataTables_wrapper { color:#EDEDED !important; }
table.dataTable thead th { border-bottom:1px solid #1DB954 !important; color:#1DB954 !important; }
table.dataTable tbody tr { background:#181818 !important; color:#EDEDED !important; }
table.dataTable tbody tr:hover { background:#282828 !important; cursor:pointer; }
.dataTables_filter input, .dataTables_length select {
  background:#282828 !important; border:1px solid #383838 !important; color:#EDEDED !important;
}
.dataTables_info, .dataTables_paginate { color:#B3B3B3 !important; }
.paginate_button { color:#B3B3B3 !important; }
.paginate_button.current { background:#1DB954 !important; color:#000 !important; border-radius:4px; }

table.dataTable tbody tr td.dtfc-fixed-left,
table.dataTable thead tr th.dtfc-fixed-left {
  background-color: #181818 !important;
  border-right: 1px solid #282828 !important; 
}
table.dataTable tbody tr:hover td.dtfc-fixed-left {
  background-color: #282828 !important; 
}

.clamp-text {
  max-width: 200px !important;
  white-space: normal !important;
  overflow: hidden !important;
  text-overflow: ellipsis !important;
  display: -webkit-box !important;
  -webkit-line-clamp: 2 !important;
  -webkit-box-orient: vertical !important;
}

table.dataTable thead td .selectize-control {
  min-width: 120px !important;
}

table.dataTable thead td:nth-last-child(1) div[style*='absolute'],
table.dataTable thead td:nth-last-child(2) div[style*='absolute'] {
  left: auto !important;
  right: 0 !important;
}

.dataTables_scrollBody thead div[style*='absolute'] {
  display: none !important;
  visibility: hidden !important;
  opacity: 0 !important;
  pointer-events: none !important;
}

/* ── About section ── */
.about-box { background:#121212; border:1px solid #282828; border-radius:10px; padding:1.5rem; }
.about-box h4 { color:#1DB954; font-family:'Space Grotesk',sans-serif; }
.about-box ul li { margin-bottom:.4rem; color:#B3B3B3; }
.feature-badge {
  display:inline-block; background:#282828; border:1px solid #1DB954;
  color:#1DB954; border-radius:12px; font-size:.75rem; padding:.2rem .65rem; margin:.15rem;
}

/* ── Plotly override ── */
.js-plotly-plot .plotly { background:transparent !important; }

/* ── Range slider ── */
.irs--shiny .irs-bar, .irs--shiny .irs-from, .irs--shiny .irs-to,
.irs--shiny .irs-single { background:#1DB954 !important; }
.irs--shiny .irs-handle { border-color:#1DB954 !important; }
"

# ── Plotly resize + force-redraw for Shinylive ───────────────
# Root cause: bslib's fillable=TRUE layout computes container
# heights as 0px on first paint in Shinylive's WASM environment.
# Plotly.Plots.resize() alone won't help because it re-measures
# a still-zero container.  We also set fillable=FALSE on the
# page_navbar, and call Plotly.relayout({autosize:true}) to force
# a full re-render cycle AFTER the browser has painted the
# container at its real CSS pixel height.
plotly_resize_fix <- tags$script(HTML("
  (function () {

    function resizePlots() {
      if (!window.Plotly) return;
      document.querySelectorAll('.js-plotly-plot').forEach(function (el) {
        try {
          Plotly.Plots.resize(el);
          Plotly.relayout(el, { autosize: true });
        } catch (e) {}
      });
      window.dispatchEvent(new Event('resize'));
    }

    // After every Shiny output lands, wait one animation frame
    // (browser paints first), then force-redraw all plots.
    document.addEventListener('shiny:value', function () {
      requestAnimationFrame(resizePlots);
    });

    // Re-render when user switches bslib / Bootstrap tabs.
    document.addEventListener('shown.bs.tab', function () {
      requestAnimationFrame(resizePlots);
    });

    // Poll every 500 ms for 15 s — catches plots rendered before
    // event listeners were attached (non-deterministic WASM boot).
    var polls = 0;
    var poller = setInterval(function () {
      resizePlots();
      if (++polls >= 30) clearInterval(poller);
    }, 500);

  })();
"))

# ── 4. UI definition ─────────────────────────────────────────
ui <- page_navbar(
  title = div(
    class = "logo-wrap",
    div(class = "logo-icon", style = "color: #0D0D0D;", img(src = "Music_logo.png", style = "width: 100%; height: 100%; object-fit: cover;")),
    div(
      div(class = "logo-text", "SoundScope"),
      div(class = "logo-sub", "Spotify Analytics")
    )
  ),
  theme    = spotify_theme,
  fillable = FALSE,
  # ── inject CSS + the resize script into <head> ─────────
  header = tagList(
    tags$head(tags$style(HTML(custom_css))),
    plotly_resize_fix
  ),
  
  # ── TAB 1: Overview ───────────────────────────────────────
  nav_panel(
    "📊 Overview",
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        # ── FIX: server = FALSE prevents selectize.js from
        #    waiting for a server response that never comes in
        #    shinylive, which left input$ov_genres NULL on load.
        selectizeInput("ov_genres", "Filter Genres",
                       choices  = all_genres,
                       selected = all_genres[1:10],
                       multiple = TRUE,
                       options  = list(placeholder = "All genres…",
                                       plugins     = list("remove_button"))),
        sliderInput("ov_pop", "Popularity Range",
                    min = 0, max = 100, value = c(0,100), step = 1),
        radioButtons("ov_explicit", "Content",
                     choices = c("All","Explicit","Clean"), selected = "All",
                     inline  = TRUE),
        hr(style = "border-color:#282828"),
        p(class = "logo-sub", "Adjust filters to explore the music")
      ),
      div(
        uiOutput("kpi_row"),
        fluidRow(
          column(6, card_(title = "Top Genres by Track Count",
                          plotlyOutput("ov_genre_bar", height = "280px"))),
          column(6, card_(title = "Popularity Distribution",
                          plotlyOutput("ov_pop_hist", height = "280px")))
        ),
        fluidRow(
          column(6, card_(title = "Danceability vs Energy",
                          selectInput("ov_color_by", "Colour by",
                                      choices  = c("track_genre","mode_label",
                                                   "explicit","popularity_bin"),
                                      selected = "mode_label"),
                          plotlyOutput("ov_scatter", height = "280px"))),
          column(6, card_(title = "Average Audio Features by Genre (Radar)",
                          selectInput("ov_radar_genre", "Select Genre",
                                      choices  = all_genres,
                                      selected = "pop"),
                          plotlyOutput("ov_radar", height = "280px")))
        )
      )
    )
  ),
  
  # ── TAB 2: Deep Dive ─────────────────────────────────────
  nav_panel(
    "🔬 Deep Dive",
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        selectInput("dd_feature_x", "X Axis Feature",
                    choices = audio_features, selected = "valence"),
        selectInput("dd_feature_y", "Y Axis Feature",
                    choices = audio_features, selected = "energy"),
        selectizeInput("dd_genres2", "Genres",
                       choices  = all_genres,
                       selected = all_genres[1:5],
                       multiple = TRUE,
                       options  = list(placeholder  = "Pick genres…",
                                       plugins      = list("remove_button"))),
        sliderInput("dd_tempo", "Tempo (BPM)",
                    min = 0, max = 250, value = c(60, 200), step = 5),
        hr(style = "border-color:#282828"),
        checkboxInput("dd_show_smooth", "Show trend line", value = TRUE)
      ),
      div(
        fluidRow(
          column(7, card_(title = textOutput("dd_scatter_title", inline = TRUE),
                          plotlyOutput("dd_scatter", height = "360px"))),
          column(5, card_(title = "Feature Correlation Heatmap",
                          plotlyOutput("dd_heatmap", height = "360px")))
        ),
        fluidRow(
          column(6, card_(title = "Attribute Distribution by Genre",
                          selectInput("dd_violin_attr", "Attribute",
                                      choices  = c(audio_features,
                                                   "tempo","loudness","duration_min","popularity"),
                                      selected = "energy"),
                          plotlyOutput("dd_violin", height = "255px"))),
          column(6, card_(title = "Key Distribution",
                          selectInput("dd_key_genre", "Genre",
                                      choices = all_genres, selected = "pop"),
                          plotlyOutput("dd_key_bar", height = "255px")))
        )
      )
    )
  ),
  
  # ── TAB 3: Track Table ────────────────────────────────────
  nav_panel(
    "🎵 Tracks",
    fluidRow(
      column(8,style = "width: 70%;",
             card_(
               title = div(style = "display: flex; justify-content: space-between; align-items: center;",
                           span("Track Table"),
                           span(id = "custom_length_container", 
                                style = "color: #B3B3B3; text-transform: none; letter-spacing: normal; font-family: 'DM Sans', sans-serif; font-weight: 400; font-size: 0.85rem;")
               ),
               DTOutput("track_table")
             )
      ),
      column(4,style = "width: 30%;",
             card_(title = "Selected Track Details",
                   uiOutput("track_detail")),
             card_(title = "Audio Fingerprint",
                   plotlyOutput("track_radar", height = "355px"))
      )
    )
  ),
  
  # ── TAB 4: About ─────────────────────────────────────────
  nav_panel(
    "ℹ About",
    div(style = "max-width:800px; margin:2rem auto; padding:0 1rem;",
        div(class = "about-box",
            div(style = "display:flex;align-items:center;gap:12px;margin-bottom:1rem;",
                div(class = "logo-icon", style = "color: #0D0D0D;", img(src = "Music_logo.png", style = "width: 100%; height: 100%; object-fit: cover;")),
                div(
                  h3(style = "margin:0;font-family:'Space Grotesk',sans-serif;", "SoundScope"),
                  p(style = "margin:0;color:#B3B3B3;font-size:.85rem;", "Spotify Track Analytics Dashboard")
                )
            ),
            hr(style = "border-color:#282828"),
            h4("📋 About this Dashboard"),
            p("SoundScope is an interactive analytics dashboard built with R Shiny for exploring
          the Spotify Tracks dataset. It lets you uncover patterns in audio features, compare genres, and drill into individual tracks."),
            h4("📁 Dataset"),
            tags$ul(
              tags$li(
                "Source: ", 
                tags$a(href = "https://www.kaggle.com/datasets/maharshipandya/-spotify-tracks-dataset", 
                       target = "_blank", 
                       style = "color: #1DB954; text-decoration: none; border-bottom: 1px dotted #1DB954;", 
                       "Spotify Tracks Dataset (Kaggle)")
              ),
              tags$li("~114,000 tracks across 100+ genres"),
              tags$li("Extracted via the Spotify Web API")
            ),
            h4("📖 Audio Feature Dictionary"),
            tags$ul(style = "font-size: 0.85rem; line-height: 1.6;",
                    tags$li(strong(style = "color: #EDEDED;", "Valence:"), " Measures musical positiveness. High valence (1.0) sounds happy/cheerful, low valence (0.0) sounds sad/angry."),
                    tags$li(strong(style = "color: #EDEDED;", "Energy:"), " Represents intensity and activity. Highly energetic tracks feel fast, loud, and noisy."),
                    tags$li(strong(style = "color: #EDEDED;", "Danceability:"), " How suitable a track is for dancing, based on tempo, rhythm stability, and beat strength."),
                    tags$li(strong(style = "color: #EDEDED;", "Acousticness:"), " A confidence measure from 0.0 to 1.0 of whether the track is acoustic."),
                    tags$li(strong(style = "color: #EDEDED;", "Instrumentalness:"), " Predicts whether a track contains no vocals. (Values above 0.5 suggest instrumental tracks)."),
                    tags$li(strong(style = "color: #EDEDED;", "Speechiness:"), " Detects the presence of spoken words (e.g., podcasts, pure rap)."),
                    tags$li(strong(style = "color: #EDEDED;", "Liveness:"), " Detects the presence of an audience. (Values above 0.8 highly suggest a live recording).")
            ),
            h4("🛠 Dashboard Tabs"),
            tags$ul(
              tags$li(strong("Overview:"), " KPIs, genre bar chart, popularity histogram, scatter plot, radar chart"),
              tags$li(strong("Deep Dive:"), " Custom feature scatter, correlation heatmap, violin plots, key distribution"),
              tags$li(strong("Tracks:"), " Searchable data table with row-click detail panel & audio fingerprint radar")
            ),
            h4("⚡ Interactive Features"),
            div(
              span(class = "feature-badge", "Genre filter"),
              span(class = "feature-badge", "Popularity slider"),
              span(class = "feature-badge", "Scatter axis selector"),
              span(class = "feature-badge", "Row-click → detail"),
              span(class = "feature-badge", "Radar chart"),
              span(class = "feature-badge", "Correlation heatmap"),
              span(class = "feature-badge", "Violin plots"),
              span(class = "feature-badge", "Column filters"),
              span(class = "feature-badge", "Hover tooltips")
            ),
            hr(style = "border-color:#282828"),
            h4("📦 R Packages"),
            p(code("shiny"), " · ", code("bslib"), " · ", code("bsicons"), " · ",
              code("plotly"), " · ", code("DT"), " · ", code("dplyr"), " · ", 
              code("tidyr"), " · ", code("stringr"), " · ", code("forcats"), " · ", code("scales")),
            hr(style = "border-color:#282828"),
            p(style = "color:#535353; font-size:.78rem;",
              "Dashboard built for Data Visualisation assignment · 2025/2026")
        )
    )
  )
)