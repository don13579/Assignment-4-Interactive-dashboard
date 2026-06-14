# ============================================================
#  🎵  SOUNDSCOPE — global.R
#  Loaded ONCE before ui.R and server.R in both regular Shiny
#  and shinylive. Put all data loading + shared objects here
#  so they are never computed twice or inside the server fn.
# ============================================================

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(plotly)
library(DT)
library(tidyr)
library(scales)
library(forcats)
library(stringr)

# ── Load & clean data ────────────────────────────────────────
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

# ── Pre-computed lookups ─────────────────────────────────────
all_genres     <- sort(unique(df$track_genre))
top20_genres   <- df %>% count(track_genre, sort = TRUE) %>% head(20) %>% pull(track_genre)
audio_features <- c("danceability","energy","speechiness",
                    "acousticness","instrumentalness","liveness","valence")