# Streams, Indie Devs and Nostalgia: What's Trending in Gaming on Bluesky?

COMP3020 Social Web Analytics, Group 22: Anamol Gautam (22226775), Dikshant Thapa (22214022), Ricky Melo (22090685)

Social web analytics investigation of English-language gaming posts on Bluesky.
It covers text analysis, trending-term detection, k-means clustering of post themes,
hypothesis tests on engagement, and a reply/mention/quote interaction network.

## Repository contents

| Path | What it is |
|---|---|
| `R/01_collect_bluesky.R` | Collects the data from the Bluesky API (needs an app password) |
| `report.Rmd` | All analysis. Knit it to produce `report.pdf` (takes about 8-10 minutes) |
| `report.pdf` | The submitted report |
| `poster.pdf` | The submitted poster |
| `data/posts.rds` | Collected posts (used by the report) |
| `data/posts.csv` | The same posts as CSV |
| `data/edges.csv` | Interactions (reply / mention / quote) between accounts |
| `data/profiles.csv` | Follower counts for every account in the data |
| `data/collection_log.csv` | Posts returned per search term and 6-hour window |
| `data/collection_info.txt` | Collection date, window and totals |
| `figures/` | Figures saved by the report (used on the poster) |

## How to reproduce

1. Install R (≥ 4.2) and RStudio, then install the packages:
   ```r
   install.packages(c("httr", "jsonlite", "dplyr", "tidyr", "stringr", "lubridate",
                      "ggplot2", "scales", "tm", "slam", "wordcloud", "RColorBrewer",
                      "cluster", "igraph", "knitr", "kableExtra", "rmarkdown", "stringi", "tinytex"))
   tinytex::install_tinytex()   # LaTeX, needed once to knit to PDF
   ```
2. Open `bluesky-gaming.Rproj` in RStudio.
3. **To reproduce the report with our data:** open `report.Rmd` and click **Knit**.
4. **To collect fresh data** (results will differ, because Bluesky changes constantly):
   create an app password in Bluesky (Settings → Privacy and security → App passwords),
   then source `R/01_collect_bluesky.R` and enter your handle and app password when asked.
   Credentials are never written to disk. Do **not** commit a `.Renviron` file.

## Data collection summary

* Endpoint: `app.bsky.feed.searchPosts` (authenticated), plus `app.bsky.actor.getProfiles`
* Search terms: `gaming`, `#gaming`, `videogames`, `#videogames`, `"video games"`, `gamer`
* Language: English. Window: 28 days, split into 6-hour blocks, up to 100 posts per term per block
* See `data/collection_info.txt` for the exact dates and totals.
