# ============================================================
#  🎵  SOUNDSCOPE — Spotify Analytics Dashboard
#  ui.R
# ============================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)
library(scales)
library(forcats)
library(stringr)

# ── 0. Load & clean data ────────────────────────────────────
df_raw <- read.csv("data/dataset.csv", stringsAsFactors = FALSE)

df <- df_raw %>%
  rename(idx = 1) %>%
  mutate(
    duration_min   = round(duration_ms / 60000, 2),
    explicit       = ifelse(explicit %in% c("True","TRUE","true",TRUE), "Explicit", "Clean"),
    mode_label     = ifelse(mode == 1, "Major", "Minor"),
    key_label      = factor(key, levels = 0:11,
                            labels = c("C","C#","D","D#","E","F",
                                       "F#","G","G#","A","A#","B")),
    popularity_bin = cut(popularity, breaks = c(-1,20,40,60,80,100),
                         labels = c("0-20","21-40","41-60","61-80","81-100"))
  ) %>%
  filter(!is.na(track_genre), track_genre != "4", track_genre != "")

# Pre-computed lookups (shared with server.R via global scope)
all_genres     <- sort(unique(df$track_genre))
top20_genres   <- df %>% count(track_genre, sort = TRUE) %>% head(20) %>% pull(track_genre)
audio_features <- c("danceability","energy","speechiness",
                    "acousticness","instrumentalness","liveness","valence")

# ── 1. Theme ────────────────────────────────────────────────
spotify_theme <- bs_theme(
  version            = 5,
  bg                 = "#0D0D0D",
  fg                 = "#EDEDED",
  primary            = "#1DB954",
  secondary          = "#535353",
  base_font          = font_google("DM Sans"),
  heading_font       = font_google("Space Grotesk"),
  code_font          = font_google("JetBrains Mono"),
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
  padding:1rem 1.2rem; margin-bottom:1rem;
}
.card-title-bar {
  font-family:'Space Grotesk',sans-serif; font-weight:600;
  font-size:.95rem; color:#1DB954; margin-bottom:.7rem;
  letter-spacing:.04em; text-transform:uppercase;
}

/* ── KPI tiles ── */
.kpi-row { display:flex; gap:.75rem; flex-wrap:wrap; margin-bottom:1rem; }
.kpi-tile {
  flex:1; min-width:110px; background:#181818; border:1px solid #282828;
  border-radius:10px; padding:.75rem 1rem; text-align:center;
}
.kpi-value { font-size:1.6rem; font-weight:700; color:#1DB954; font-family:'Space Grotesk',sans-serif; }
.kpi-label { font-size:.72rem; color:#B3B3B3; text-transform:uppercase; letter-spacing:.06em; }

/* ── Logo ── */
.logo-wrap {
  display:flex; align-items:center; gap:10px;
  padding:1rem 1.2rem .5rem;
}
.logo-icon {
  width:36px; height:36px; background:#1DB954; border-radius:50%;
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

# ── 4. UI definition ─────────────────────────────────────────
ui <- page_navbar(
  title = div(
    class = "logo-wrap",
    img(src   = "PP_logo.png",
        height = "42px",
        style  = "border-radius:50%; object-fit:cover;"),
    div(
      div(class = "logo-text", "SoundScope"),
      div(class = "logo-sub", "Spotify Analytics")
    )
  ),
  theme    = spotify_theme,
  fillable = TRUE,
  header   = tags$head(tags$style(HTML(custom_css))),
  
  # ── TAB 1: Overview ───────────────────────────────────────
  nav_panel(
    "📊 Overview",
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        selectizeInput("ov_genres", "Filter Genres",
                       choices  = all_genres,
                       selected = all_genres[1:10],
                       multiple = TRUE,
                       options  = list(placeholder = "All genres…")),
        sliderInput("ov_pop", "Popularity Range",
                    min = 0, max = 100, value = c(0,100), step = 1),
        radioButtons("ov_explicit", "Content",
                     choices = c("All","Explicit","Clean"), selected = "All",
                     inline  = TRUE),
        hr(style = "border-color:#282828"),
        p(class = "logo-sub", "KPIs update with filters")
      ),
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
                       options  = list(placeholder = "Pick genres…")),
        sliderInput("dd_tempo", "Tempo (BPM)",
                    min = 0, max = 250, value = c(60, 200), step = 5),
        hr(style = "border-color:#282828"),
        checkboxInput("dd_show_smooth", "Show trend line", value = TRUE)
      ),
      fluidRow(
        column(7, card_(title = "Feature Scatter Explorer",
                        plotlyOutput("dd_scatter", height = "360px"))),
        column(5, card_(title = "Feature Correlation Heatmap",
                        plotlyOutput("dd_heatmap", height = "360px")))
      ),
      fluidRow(
        column(6, card_(title = "Attribute Distribution by Genre",
                        selectInput("dd_violin_attr", "Attribute",
                                    choices  = c(audio_features,
                                                 "tempo","loudness","duration_min","popularity"),
                                    selected = "tempo"),
                        plotlyOutput("dd_violin", height = "255px"))),
        column(6, card_(title = "Key Distribution",
                        selectInput("dd_key_genre", "Genre",
                                    choices = all_genres, selected = "pop"),
                        plotlyOutput("dd_key_bar", height = "270px")))
      )
    )
  ),
  
  # ── TAB 3: Track Table ────────────────────────────────────
  nav_panel(
    "🎵 Tracks",
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        selectizeInput("tbl_genres", "Genres",
                       choices  = all_genres,
                       selected = c("pop","hip-hop"),
                       multiple = TRUE),
        sliderInput("tbl_pop2", "Popularity",
                    min = 0, max = 100, value = c(50, 100)),
        selectInput("tbl_explicit", "Content",
                    choices = c("All","Explicit","Clean"), selected = "All"),
        sliderInput("tbl_dur", "Duration (min)",
                    min = 0, max = 15, value = c(1, 8), step = .5),
        hr(style = "border-color:#282828"),
        p(class = "logo-sub", "Click a row to see track details")
      ),
      fluidRow(
        column(8,
               card_(title = "Track Table",
                     DTOutput("track_table"))
        ),
        column(4,
               card_(title = "Selected Track Details",
                     uiOutput("track_detail")),
               card_(title = "Audio Fingerprint",
                     plotlyOutput("track_radar", height = "280px"))
        )
      )
    )
  ),
  
  # ── TAB 5: About ─────────────────────────────────────────
  nav_panel(
    "ℹ About",
    div(style = "max-width:800px; margin:2rem auto; padding:0 1rem;",
        div(class = "about-box",
            div(style = "display:flex;align-items:center;gap:12px;margin-bottom:1rem;",
                img(src   = "PP_logo.png",
                    height = "48px",
                    style  = "border-radius:50%; object-fit:cover;"),
                div(
                  h3(style = "margin:0;font-family:'Space Grotesk',sans-serif;", "SoundScope"),
                  p(style = "margin:0;color:#B3B3B3;font-size:.85rem;", "Spotify Track Analytics Dashboard")
                )
            ),
            hr(style = "border-color:#282828"),
            h4("📋 About this Dashboard"),
            p("SoundScope is an interactive analytics dashboard built with R Shiny for exploring
          the Spotify Tracks dataset (~114 000 songs across 100+ genres). It lets you uncover
          patterns in audio features, compare genres, and drill into individual tracks."),
            h4("📁 Dataset"),
            tags$ul(
              tags$li("Source: Spotify Tracks Dataset (Kaggle)"),
              tags$li("~114 000 tracks across 100+ genres"),
              tags$li("Features: danceability, energy, valence, tempo, acousticness, and more")
            ),
            h4("🛠 Dashboard Tabs"),
            tags$ul(
              tags$li(strong("Overview:"), " KPIs, genre bar chart, popularity histogram, scatter plot, radar chart"),
              tags$li(strong("Deep Dive:"), " Custom feature scatter, correlation heatmap, violin plots, key distribution"),
              tags$li(strong("Tracks:"), " Searchable data table with row-click detail panel & audio fingerprint radar"),
              tags$li(strong("Compare:"), " Side-by-side boxplots, popularity leaderboard, multi-feature heatmap")
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
              span(class = "feature-badge", "Lollipop chart")
            ),
            hr(style = "border-color:#282828"),
            h4("📦 R Packages"),
            p(code("shiny"), " · ", code("bslib"), " · ", code("ggplot2"), " · ",
              code("plotly"), " · ", code("DT"), " · ", code("dplyr"), " · ", code("tidyr")),
            hr(style = "border-color:#282828"),
            p(style = "color:#535353; font-size:.78rem;",
              "Dashboard built for Data Visualisation assignment · 2025/2026")
        )
    )
  )
)
