# ============================================================
#  🎵  SOUNDSCOPE — Spotify Analytics Dashboard
#  server.R
# ============================================================
# NOTE: df, all_genres, top20_genres, audio_features, and
#       card_() are all defined in ui.R and shared via the
#       global environment when Shiny loads both files.
# ============================================================

server <- function(input, output, session) {
  # ── 0. Load & clean data ────────────────────────────────────
  df_raw <- read.csv("data/dataset.csv", stringsAsFactors = FALSE)
  
  df <- df_raw %>%
    rename(idx = 1) %>%
    mutate(
      artists        = str_replace_all(artists, ";", ", "),
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
  
  # ── Overview: filtered reactive ───────────────────────────
  ov_data <- reactive({
    d <- df
    
    # 1. Safe fallback for genres dropdown
    genres <- input$ov_genres
    if (length(genres) > 0) {
      d <- d %>% filter(track_genre %in% genres)
    }
    
    # 2. Safe fallback for popularity slider (defaults to 0-100 if temporarily NULL)
    pop_range <- if (is.null(input$ov_pop)) c(0, 100) else input$ov_pop
    d <- d %>% filter(popularity >= pop_range[1], popularity <= pop_range[2])
    
    # 3. Safe fallback for explicit content radio buttons
    explicit_val <- if (is.null(input$ov_explicit)) "All" else input$ov_explicit
    if (explicit_val != "All") {
      d <- d %>% filter(explicit == explicit_val)
    }
    
    d
  })
  
  # ── KPI tiles ─────────────────────────────────────────────
  output$kpi_row <- renderUI({
    d <- ov_data()
    div(class = "kpi-row",
        div(class = "kpi-tile",
            div(class = "kpi-value", format(nrow(d), big.mark = ",")),
            div(class = "kpi-label", "Tracks")),
        div(class = "kpi-tile",
            div(class = "kpi-value", n_distinct(d$artists)),
            div(class = "kpi-label", "Artists")),
        div(class = "kpi-tile",
            div(class = "kpi-value", n_distinct(d$track_genre)),
            div(class = "kpi-label", "Genres")),
        div(class = "kpi-tile",
            div(class = "kpi-value", round(mean(d$popularity,    na.rm = TRUE), 1)),
            div(class = "kpi-label", "Avg Popularity")),
        div(class = "kpi-tile",
            div(class = "kpi-value", round(mean(d$danceability,  na.rm = TRUE), 2)),
            div(class = "kpi-label", "Avg Danceability")),
        div(class = "kpi-tile",
            div(class = "kpi-value", round(mean(d$energy,        na.rm = TRUE), 2)),
            div(class = "kpi-label", "Avg Energy"))
    )
  })
  
  # ── Overview: genre bar chart ──────────────────────────────
  output$ov_genre_bar <- renderPlotly({
    d <- ov_data() %>%
      count(track_genre, sort = TRUE) %>%
      head(15) %>%
      mutate(track_genre = fct_reorder(track_genre, n))
    
    plot_ly(d, x = ~n, y = ~track_genre, type = "bar",
            orientation = "h",
            marker = list(color = "#1DB954",
                          line = list(color = "#0D0D0D", width = .5))) %>%
      layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        font  = list(color = "#EDEDED", family = "DM Sans"),
        xaxis = list(title = "Tracks", color = "#B3B3B3",
                     gridcolor = "#282828", zeroline = FALSE),
        yaxis = list(title = "", color = "#B3B3B3"),
        margin = list(l = 10, r = 10, t = 10, b = 30),
        hoverlabel = list(bgcolor = "#181818", font = list(color = "#EDEDED"))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── Overview: popularity density curve ────────────────────
  output$ov_pop_hist <- renderPlotly({
    d <- ov_data()
    
    req(nrow(d) > 1) 
    
    pop_min <- input$ov_pop[1]
    pop_max <- input$ov_pop[2]
    
    dens <- density(d$popularity, na.rm = TRUE, from = pop_min, to = pop_max)
    
    plot_ly(x = dens$x, y = dens$y, type = "scatter", mode = "lines",
            fill = "tozeroy",
            fillcolor = "rgba(29,185,84,0.2)",
            line = list(color = "#1DB954", width = 2)) %>%
      layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        font  = list(color = "#EDEDED", family = "DM Sans"),
        xaxis = list(title = "Popularity", color = "#B3B3B3",
                     gridcolor = "#282828", zeroline = FALSE, range = c(pop_min, pop_max)),
        yaxis = list(title = "Density", color = "#B3B3B3",
                     gridcolor = "#282828", zeroline = FALSE),
        margin = list(l = 10, r = 10, t = 10, b = 30),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "#181818", font = list(color = "#EDEDED"))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── Overview: danceability vs energy scatter ───────────────
  output$ov_scatter <- renderPlotly({
    d         <- ov_data() %>% sample_n(min(2000, nrow(.)))
    color_col <- input$ov_color_by
    
    plot_ly(d, x = ~danceability, y = ~energy,
            color     = ~.data[[color_col]],
            type      = "scatter", mode = "markers",
            text      = ~paste0(track_name, "<br>", artists, "<br>", track_genre),
            hoverinfo = "text",
            marker    = list(size = 5, opacity = .7)) %>%
      layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        font  = list(color = "#EDEDED", family = "DM Sans"),
        xaxis = list(title = "Danceability", color = "#B3B3B3",
                     gridcolor = "#282828", zeroline = FALSE, range = c(0,1)),
        yaxis = list(title = "Energy", color = "#B3B3B3",
                     gridcolor = "#282828", zeroline = FALSE, range = c(0,1)),
        legend = list(font = list(size = 12), bgcolor = "transparent"),
        margin = list(l = 10, r = 10, t = 10, b = 30),
        hoverlabel = list(bgcolor = "#181818", font = list(color = "#EDEDED"))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── Overview: audio features radar ────────────────────────
  output$ov_radar <- renderPlotly({
    req(input$ov_radar_genre, input$ov_pop)
    d <- df %>%
      filter(
        track_genre == input$ov_radar_genre,
        popularity >= input$ov_pop[1],
        popularity <= input$ov_pop[2]
      )
    if (input$ov_explicit != "All") {
      d <- d %>% filter(explicit == input$ov_explicit)
    }
    req(nrow(d) > 0)
    means <- d %>%
      summarise(across(all_of(audio_features), mean, na.rm = TRUE)) %>%
      pivot_longer(everything())
    
    theta_vals <- c(means$name,  means$name[1])
    r_vals     <- c(means$value, means$value[1])
    
    plot_ly(type = "scatterpolar", fill = "toself",
            r = r_vals, theta = theta_vals,
            fillcolor = "rgba(29,185,84,0.25)",
            line = list(color = "#1DB954", width = 2)) %>%
      layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        font  = list(color = "#EDEDED", family = "DM Sans", size = 14),
        polar = list(
          bgcolor    = "transparent",
          radialaxis = list(visible = TRUE, range = c(0,1),
                            color = "#535353", gridcolor = "#282828"),
          angularaxis = list(color = "#B3B3B3", gridcolor = "#282828")
        ),
        showlegend = FALSE,
        margin     = list(l = 40, r = 40, t = 25, b = 30),
        hoverlabel = list(bgcolor = "#181818", font = list(color = "#EDEDED"))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── Deep Dive: filtered reactive ──────────────────────────
  dd_data <- reactive({
    req(input$dd_tempo)
    d <- df
    if (length(input$dd_genres2) > 0)
      d <- d %>% filter(track_genre %in% input$dd_genres2)
    d %>% filter(tempo >= input$dd_tempo[1], tempo <= input$dd_tempo[2])
  })
  # ── Deep Dive: feature scatter title ────────────────────────
  output$dd_scatter_title <- renderText({
    req(input$dd_feature_x, input$dd_feature_y)
    paste(
      "Feature exploration:",
      tools::toTitleCase(input$dd_feature_y), 
      "vs.", 
      tools::toTitleCase(input$dd_feature_x)
    )
  })
  # ── Deep Dive: feature scatter ────────────────────────────
  output$dd_scatter <- renderPlotly({
    d   <- dd_data() %>% sample_n(min(3000, nrow(.)))
    req(input$dd_feature_x, input$dd_feature_y)
    xf  <- input$dd_feature_x
    yf  <- input$dd_feature_y
    
    p <- plot_ly(d, x = ~.data[[xf]], y = ~.data[[yf]],
                 color     = ~track_genre,
                 type      = "scatter", mode = "markers",
                 text      = ~paste0(track_name, "<br>", artists),
                 hoverinfo = "text",
                 marker    = list(size = 5, opacity = .65)) %>%
      layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        font  = list(color = "#EDEDED", family = "DM Sans"),
        xaxis = list(title = xf, color = "#B3B3B3",
                     gridcolor = "#282828", zeroline = FALSE),
        yaxis = list(title = yf, color = "#B3B3B3",
                     gridcolor = "#282828", zeroline = FALSE),
        legend = list(font = list(size = 12), bgcolor = "transparent"),
        margin = list(l = 10, r = 10, t = 10, b = 30),
        hoverlabel = list(bgcolor = "#181818", font = list(color = "#EDEDED"))
      ) %>%
      config(displayModeBar = FALSE)
    
    if (isTRUE(input$dd_show_smooth)) {
      lm_fit <- lm(reformulate(xf, yf), data = d)
      xseq   <- seq(min(d[[xf]], na.rm = TRUE), max(d[[xf]], na.rm = TRUE), length.out = 80)
      ypred  <- predict(lm_fit, newdata = setNames(data.frame(xseq), xf))
      p <- p %>%
        add_trace(x = xseq, y = ypred,
                  type = "scatter", mode = "lines",
                  line = list(color = "#FFFFFF", width = 1.5, dash = "dot"),
                  showlegend = FALSE, hoverinfo = "none", inherit = FALSE)
    }
    p
  })
  
  # ── Deep Dive: correlation heatmap ────────────────────────
  output$dd_heatmap <- renderPlotly({
    d  <- dd_data() %>% select(all_of(audio_features))
    cm <- cor(d, use = "pairwise.complete.obs")
    
    plot_ly(z = cm, x = colnames(cm), y = rownames(cm),
            type       = "heatmap",
            colorscale = list(c(0,"#191970"), c(.5,"#282828"), c(1,"#1DB954")),
            zmin = -1, zmax = 1,
            text = round(cm, 2), hoverinfo = "text",
            colorbar = list(
              thickness = 15,
              tickfont = list(color = "#B3B3B3"),
              title    = ""
            )) %>%
      layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        font  = list(color = "#B3B3B3", family = "DM Sans", size = 12),
        xaxis = list(tickangle = -35, color = "#B3B3B3",tickfont = list(size = 13)),
        yaxis = list(color = "#B3B3B3",tickfont = list(size = 13)),
        margin = list(l = 90, r = 10, t = 10, b = 80)
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── Deep Dive: violin — selectable attribute by genre ──────
  output$dd_violin <- renderPlotly({
    req(input$dd_violin_attr)
    attr <- input$dd_violin_attr
    d    <- dd_data()
    
    plot_ly(d, x = ~track_genre, y = ~.data[[attr]],
            type      = "violin",
            box       = list(visible = TRUE),
            meanline  = list(visible = TRUE),
            fillcolor = "rgba(29,185,84,0.3)",
            line      = list(color = "#1DB954")) %>%
      layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        font  = list(color = "#EDEDED", family = "DM Sans"),
        xaxis = list(title = "", color = "#B3B3B3", tickangle = -35,
                     gridcolor = "#282828",tickfont = list(size = 13)),
        yaxis = list(title = attr, color = "#B3B3B3",
                     gridcolor = "#282828",tickfont = list(size = 13)),
        showlegend = FALSE,
        margin     = list(l = 10, r = 10, t = 10, b = 80),
        hoverlabel = list(bgcolor = "#181818", font = list(color = "#EDEDED"))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── Deep Dive: key distribution bar ───────────────────────
  output$dd_key_bar <- renderPlotly({
    req(input$dd_key_genre, input$dd_tempo)
    d <- df %>%
      filter(
        track_genre == input$dd_key_genre,
        tempo >= input$dd_tempo[1],
        tempo <= input$dd_tempo[2]
      )
    req(nrow(d) > 0)
    d <- d %>%
      count(key_label) %>%
      arrange(key_label)
    
    plot_ly(d, x = ~key_label, y = ~n, type = "bar",
            marker = list(color = "#1DB954",
                          line = list(color = "#0D0D0D", width = .5))) %>%
      layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        font  = list(color = "#EDEDED", family = "DM Sans"),
        xaxis = list(title = "Key", color = "#B3B3B3",
                     categoryorder = "array",
                     categoryarray = c("C","C#","D","D#","E","F",
                                       "F#","G","G#","A","A#","B"),
                     tickfont = list(size = 13)),
        yaxis = list(title = "Tracks", color = "#B3B3B3",
                     gridcolor = "#282828",
                     tickfont = list(size = 13)),
        margin = list(l = 10, r = 10, t = 10, b = 40),
        hoverlabel = list(bgcolor = "#181818", font = list(color = "#EDEDED"))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── Tracks: filtered reactive ─────────────────────────────
  tbl_data <- reactive({
    df %>%
      mutate(
        explicit    = as.factor(explicit),
        track_genre = as.factor(track_genre)
      ) %>%
      select(track_name, artists, track_genre, popularity,
             duration_min, explicit, danceability, energy, valence, tempo) 
  })
  
  # ── Tracks: data table ─────────────────────────────────────
  output$track_table <- renderDT({
    datatable(
      tbl_data(),
      selection = "single",
      rownames  = FALSE,
      colnames  = c("Track","Artist","Genre","Popularity","Duration (min)",
                    "Explicit","Danceability","Energy","Valence","Tempo"),
      extensions = 'FixedColumns',
      options   = list(
        autoWidth  = FALSE,
        dom        = "rtip",
        pageLength = 10, scrollX = TRUE,
        fixedColumns = list(leftColumns = 1),
        columnDefs = list(
          list(className = "dt-center", targets = 3:9),
          list(
            targets = 0:1,
            width = "180px",
            render = JS("function(data, type, row) {
              if (type === 'display' && data != null) {
                // Escape quotes so the tooltip doesn't break
                var safeStr = String(data).replace(/\"/g, '&quot;');
                // Returns the text inside our clamped div, with a native hover tooltip!
                return '<div class=\"clamp-text\" title=\"' + safeStr + '\">' + data + '</div>';
              }
              return data;
            }")
          )
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  var api = this.api();",
          "  var lengthContainer = $('#custom_length_container');",
          "  lengthContainer.html('<label>Show <input type=\"number\" min=\"1\" max=\"500\" value=\"10\" class=\"form-control form-control-sm\" style=\"display: inline-block; width: 60px; background-color: #181818; color: #EDEDED; border: 1px solid #282828; margin: 0 5px;\"> entries</label>');",
          "  lengthContainer.find('input').on('change keyup', function() {",
          "    var val = parseInt(this.value, 10);",
          "    if (val > 0 && val <= 500) {",
          "      api.page.len(val).draw();",
          "    }",
          "  });",
          "}"
        )
      ),
      class  = "display compact",
      filter = "top"
    ) %>%
      formatRound(c("danceability","energy","valence"), 3) %>%
      formatRound("tempo", 1)
  })
  
  # ── Tracks: selected row - reactive ───────────────────────
  selected_track <- reactive({
    sel <- input$track_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return(NULL)
    tbl_data()[sel, ]
  })
  
  # ── Tracks: detail panel (row-click interaction) ───────────
  output$track_detail <- renderUI({
    t <- selected_track()
    if (is.null(t)) {
      return(div(style = "color:#535353; font-size:.85rem; padding:.5rem;",
                 "← Click any row to see track details here."))
    }
    full <- df %>%
      filter(track_name == t$track_name[1], artists == t$artists[1]) %>%
      head(1)
    
    tagList(
      tags$p(tags$span(
        style = "font-size:1.1rem;font-weight:700;color:#EDEDED;",
        full$track_name
      )),
      tags$p(style = "color:#B3B3B3;font-size:.85rem;margin-top:-.5rem;",
             full$artists),
      tags$hr(style = "border-color:#282828;margin:.5rem 0;"),
      tags$table(style = "width:100%;font-size:.82rem;",
                 tags$tr(tags$td(style="color:#B3B3B3;padding:3px 6px;","Genre"),
                         tags$td(style="color:#1DB954;font-weight:600;", full$track_genre)),
                 tags$tr(tags$td(style="color:#B3B3B3;padding:3px 6px;","Popularity"),
                         tags$td(full$popularity)),
                 tags$tr(tags$td(style="color:#B3B3B3;padding:3px 6px;","Duration"),
                         tags$td(paste0(full$duration_min, " min"))),
                 tags$tr(tags$td(style="color:#B3B3B3;padding:3px 6px;","Key"),
                         tags$td(paste0(full$key_label, " ", full$mode_label))),
                 tags$tr(tags$td(style="color:#B3B3B3;padding:3px 6px;","Tempo"),
                         tags$td(paste0(round(full$tempo, 1), " BPM"))),
                 tags$tr(tags$td(style="color:#B3B3B3;padding:3px 6px;","Explicit"),
                         tags$td(full$explicit)),
                 tags$tr(tags$td(style="color:#B3B3B3;padding:3px 6px;","Album"),
                         tags$td(style="word-break:break-word;", full$album_name))
      )
    )
  })
  
  # ── Tracks: audio fingerprint radar (linked to row click) ──
  output$track_radar <- renderPlotly({
    t <- selected_track()
    
    if (is.null(t)) {
      means <- df %>%
        summarise(across(all_of(audio_features), mean, na.rm = TRUE)) %>%
        pivot_longer(everything())
      label <- "All Tracks (avg)"
    } else {
      full  <- df %>%
        filter(track_name == t$track_name[1], artists == t$artists[1]) %>%
        head(1)
      means <- full %>%
        select(all_of(audio_features)) %>%
        pivot_longer(everything())
      label <- full$track_name
    }
    
    clean_names <- c(
      "danceability"     = "dance-<br>ability",
      "energy"           = "energy",
      "speechiness"      = "spee-<br>chiness",
      "acousticness"     = "acous-<br>ticness",
      "instrumentalness" = "instru-<br>mental-<br>ness",
      "liveness"         = "liveness",
      "valence"          = "valence"
    )
    # Overwrite the 'name' column with our stacked labels
    means$name <- clean_names[means$name]
    
    theta_vals <- c(means$name,  means$name[1])
    r_vals     <- c(means$value, means$value[1])
    
    plot_ly(type = "scatterpolar", fill = "toself",
            r = r_vals, theta = theta_vals, name = label,
            fillcolor = "rgba(29,185,84,0.25)",
            line      = list(color = "#1DB954", width = 2)) %>%
      layout(
        paper_bgcolor = "transparent", plot_bgcolor = "transparent",
        font  = list(color = "#EDEDED", family = "DM Sans", size = 10),
        polar = list(
          bgcolor     = "transparent",
          radialaxis  = list(visible = TRUE, range = c(0,1),
                             color = "#535353", gridcolor = "#282828"),
          angularaxis = list(color = "#B3B3B3", gridcolor = "#282828", tickfont = list(size = 12))
        ),
        showlegend = FALSE,
        margin     = list(l = 40, r = 40, t = 50, b = 52),
        hoverlabel = list(bgcolor = "#181818", font = list(color = "#EDEDED"))
      ) %>%
      config(displayModeBar = FALSE)
  })
  
 
}
