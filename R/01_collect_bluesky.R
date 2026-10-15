# =============================================================================
# 01_collect_bluesky.R
# COMP3020 Social Web Analytics - Group Project
# "What's trending in gaming on Bluesky?"
#
# PURPOSE: Collect English-language gaming posts from Bluesky using the official
#          AT Protocol API (app.bsky.feed.searchPosts), then look up the profiles
#          of every account that appears in the data (authors + people they
#          replied to / mentioned / quoted).
#
# HOW TO RUN (in RStudio):
#   1. Open the project folder (bluesky-gaming.Rproj) in RStudio.
#   2. Run:  install.packages(c("httr", "jsonlite", "dplyr", "tidyr"))  (once)
#   3. Click "Source" on this file (or Ctrl+Shift+S / Cmd+Shift+S).
#   4. When asked, type your Bluesky handle and your APP PASSWORD
#      (NOT your normal password - make one at Settings > Privacy and security
#       > App passwords).
#   5. Wait ~10-15 minutes. Files appear in the data/ folder.
#
# OUTPUT FILES (data/):
#   posts.rds      - one row per unique post (all variables, used by report.Rmd)
#   posts.csv      - same posts as a plain CSV (for GitHub / checking in Excel)
#   edges.csv      - interactions: who replied to / mentioned / quoted whom
#   profiles.csv   - follower counts etc. for every account in the data
#   collection_log.csv - how many posts each query/time-window returned
#
# NOTE: Your password is never saved to disk by this script. Do not commit
#       .Renviron or any password to GitHub.
# =============================================================================

library(httr)
library(jsonlite)
library(dplyr)

# ----------------------------- SETTINGS --------------------------------------
# Search terms. Kept deliberately GENERIC (no platform or game names) so that
# the data does not decide in advance which games/platforms "win".
QUERIES <- c("gaming", "#gaming", "videogames", "#videogames",
             "\"video games\"", "gamer")

N_DAYS        <- 28   # length of the collection window (4 weeks)
WINDOW_HOURS  <- 6    # each day is split into 4 x 6-hour windows ...
PAGES_PER_WIN <- 1    # ... and we take up to 100 posts from each window.
                      # This spreads the sample evenly over days AND times of day
                      # instead of only getting the most recent posts.
LANG          <- "en"
PAUSE_SEC     <- 0.4  # polite pause between API calls

# Window ends at the start of *today* (UTC) so every day is a complete day.
END_TIME   <- as.POSIXct(paste(format(Sys.time(), "%Y-%m-%d", tz = "UTC"),
                               "00:00:00"), tz = "UTC")
START_TIME <- END_TIME - N_DAYS * 24 * 3600

dir.create("data", showWarnings = FALSE)

# ----------------------------- LOGIN -----------------------------------------
get_credential <- function(var, prompt) {
  val <- Sys.getenv(var)
  if (val == "") {
    if (!interactive()) stop("Set ", var, " in your .Renviron file.")
    val <- readline(prompt)
  }
  trimws(val)
}

handle   <- get_credential("BSKY_HANDLE",
                           "Bluesky handle (e.g. yourname.bsky.social): ")
app_pass <- get_credential("BSKY_APP_PASSWORD",
                           "Bluesky APP password (xxxx-xxxx-xxxx-xxxx): ")
handle   <- sub("^@", "", handle)

login <- function() {
  r <- POST(paste0(LOGIN_HOST, "/xrpc/com.atproto.server.createSession"),
            body = list(identifier = handle, password = app_pass),
            encode = "json")
  if (status_code(r) != 200) {
    stop("Login failed (HTTP ", status_code(r), "): ",
         content(r, as = "text", encoding = "UTF-8"),
         "\nCheck your handle and that you used an APP password.")
  }
  s <- content(r, as = "parsed", simplifyVector = FALSE)
  # Requests must go to the user's own server (PDS); it forwards them to Bluesky.
  pds <- tryCatch(s$didDoc$service[[1]]$serviceEndpoint,
                  error = function(e) NULL)
  if (is.null(pds)) pds <- LOGIN_HOST
  list(token = s$accessJwt, pds = pds)
}

LOGIN_HOST <- Sys.getenv("BSKY_HOST", "https://bsky.social")  # leave as is
session <- login()
cat("Logged in. Using server:", session$pds, "\n")

# Generic GET with retries for rate limits (429), expired tokens and hiccups.
api_get <- function(method, query) {
  for (attempt in 1:6) {
    r <- tryCatch(
      GET(paste0(session$pds, "/xrpc/", method), query = query,
          add_headers(Authorization = paste("Bearer", session$token)),
          timeout(60)),
      error = function(e) NULL)
    if (is.null(r)) { Sys.sleep(5 * attempt); next }
    code <- status_code(r)
    if (code == 200) {
      return(fromJSON(content(r, as = "text", encoding = "UTF-8"),
                      simplifyVector = FALSE))
    }
    if (code == 401) {                       # token expired -> log in again
      session <<- login(); next
    }
    if (code == 429) {                       # rate limited -> wait
      reset <- suppressWarnings(as.numeric(headers(r)[["ratelimit-reset"]]))
      wait  <- if (length(reset) && !is.na(reset))
        max(5, reset - as.numeric(Sys.time())) else 60
      cat("  Rate limited; waiting", round(wait), "s\n"); Sys.sleep(wait); next
    }
    if (code >= 500) { Sys.sleep(5 * attempt); next }
    warning("HTTP ", code, " for ", method, ": ",
            substr(content(r, as = "text", encoding = "UTF-8"), 1, 200))
    return(NULL)
  }
  NULL
}

# ----------------------------- PARSING ---------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
did_from_uri <- function(uri) {           # at://did:plc:xyz/app.bsky.feed.post/abc
  if (is.null(uri) || is.na(uri)) return(NA_character_)
  sub("^at://([^/]+)/.*$", "\\1", uri)
}

# Turn one postView (a nested list) into a one-row data frame + its edges.
parse_post <- function(p) {
  rec <- p$record
  emb <- p$embed
  emb_type <- sub("#view$", "", emb$`$type` %||% NA_character_)

  # facets = mentions, links and hashtags inside the text
  mentions <- character(0); tags <- character(0); links <- character(0)
  for (f in rec$facets %||% list()) for (ft in f$features %||% list()) {
    ty <- ft$`$type` %||% ""
    if (ty == "app.bsky.richtext.facet#mention") mentions <- c(mentions, ft$did)
    if (ty == "app.bsky.richtext.facet#tag")     tags     <- c(tags, ft$tag)
    if (ty == "app.bsky.richtext.facet#link")    links    <- c(links, ft$uri)
  }
  # old-style hashtags stored in record$tags
  tags <- unique(tolower(c(tags, unlist(rec$tags %||% list()))))

  # quoted post (embed record / recordWithMedia)
  # (only counts if the embedded record is a post, not a feed/list/starter pack)
  quoted_did <- NA_character_
  q_uri <- NULL
  if (!is.na(emb_type)) {
    if (emb_type == "app.bsky.embed.record")          q_uri <- emb$record$uri
    if (emb_type == "app.bsky.embed.recordWithMedia") q_uri <- emb$record$record$uri
  }
  if (!is.null(q_uri) && grepl("/app.bsky.feed.post/", q_uri, fixed = TRUE))
    quoted_did <- did_from_uri(q_uri)

  reply_parent <- rec$reply$parent$uri %||% NA_character_
  has_media <- !is.na(emb_type) && emb_type %in% c(
    "app.bsky.embed.images", "app.bsky.embed.video",
    "app.bsky.embed.recordWithMedia")

  row <- data.frame(
    uri          = p$uri,
    author_did   = p$author$did,
    author_handle= p$author$handle %||% NA_character_,
    author_name  = p$author$displayName %||% NA_character_,
    created_at   = rec$createdAt %||% NA_character_,
    indexed_at   = p$indexedAt %||% NA_character_,
    text         = rec$text %||% "",
    langs        = paste(unlist(rec$langs %||% list()), collapse = ";"),
    like_count   = p$likeCount   %||% 0L,
    repost_count = p$repostCount %||% 0L,
    reply_count  = p$replyCount  %||% 0L,
    quote_count  = p$quoteCount  %||% 0L,
    is_reply     = !is.na(reply_parent),
    reply_parent_uri = reply_parent,
    reply_to_did = did_from_uri(reply_parent),
    embed_type   = emb_type,
    has_media    = has_media,
    has_link     = length(links) > 0 || identical(emb_type, "app.bsky.embed.external"),
    hashtags     = paste(tags, collapse = ";"),
    mention_dids = paste(unique(mentions), collapse = ";"),
    quoted_did   = quoted_did,
    stringsAsFactors = FALSE)
  row
}

# ----------------------------- COLLECTION ------------------------------------
windows <- seq(START_TIME, END_TIME - WINDOW_HOURS * 3600,
               by = WINDOW_HOURS * 3600)
fmt <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

all_rows <- list(); log_rows <- list(); k <- 0
total_calls <- length(QUERIES) * length(windows) * PAGES_PER_WIN
cat("Collecting from", fmt(START_TIME), "to", fmt(END_TIME), "-",
    total_calls, "API calls (about", round(total_calls * 1.0 / 60), "min)\n")

call_no <- 0
for (q in QUERIES) {
  for (i in seq_along(windows)) {
    since <- windows[i]; until <- since + WINDOW_HOURS * 3600
    cursor <- NULL; n_win <- 0; hits <- NA
    for (pg in seq_len(PAGES_PER_WIN)) {
      call_no <- call_no + 1
      res <- api_get("app.bsky.feed.searchPosts",
                     list(q = q, sort = "latest", lang = LANG, limit = 100,
                          since = fmt(since), until = fmt(until),
                          cursor = cursor))
      Sys.sleep(PAUSE_SEC)
      if (is.null(res) || length(res$posts) == 0) break
      hits <- res$hitsTotal %||% NA
      rows <- lapply(res$posts, function(p)
        tryCatch(parse_post(p), error = function(e) NULL))
      rows <- do.call(rbind, rows[!sapply(rows, is.null)])
      if (!is.null(rows)) {
        rows$query <- q
        k <- k + 1; all_rows[[k]] <- rows; n_win <- n_win + nrow(rows)
      }
      cursor <- res$cursor
      if (is.null(cursor)) break
    }
    log_rows[[length(log_rows) + 1]] <- data.frame(
      query = q, since = fmt(since), until = fmt(until),
      n_returned = n_win, hits_total = hits)
    if (call_no %% 20 == 0)
      cat(sprintf("  %d/%d calls done, %d rows so far\n", call_no, total_calls,
                  sum(sapply(all_rows, nrow))))
  }
}

raw <- bind_rows(all_rows)
cat("Rows returned (with duplicates across queries):", nrow(raw), "\n")

# A post found by several queries is kept once; remember which queries hit it.
posts <- raw %>%
  group_by(uri) %>%
  mutate(matched_queries = paste(sort(unique(query)), collapse = "|")) %>%
  slice(1) %>%
  ungroup() %>%
  select(-query)
cat("Unique posts:", nrow(posts), "\n")

# ----------------------------- EDGES -----------------------------------------
# Directed edge  from = author of the post  ->  to = account they interacted with
edge_list <- list()
rp <- posts %>% filter(!is.na(reply_to_did)) %>%
  transmute(from = author_did, to = reply_to_did, type = "reply", uri)
qt <- posts %>% filter(!is.na(quoted_did)) %>%
  transmute(from = author_did, to = quoted_did, type = "quote", uri)
mn <- posts %>% filter(mention_dids != "") %>%
  select(author_did, mention_dids, uri) %>%
  tidyr::separate_rows(mention_dids, sep = ";") %>%
  transmute(from = author_did, to = mention_dids, type = "mention", uri)
edges <- bind_rows(rp, qt, mn)
cat("Interaction edges:", nrow(edges), "\n")

# ----------------------------- PROFILES --------------------------------------
dids <- unique(c(posts$author_did, edges$to))
cat("Looking up", length(dids), "profiles (25 per call)...\n")
prof_rows <- list()
chunks <- split(dids, ceiling(seq_along(dids) / 25))
for (j in seq_along(chunks)) {
  res <- api_get("app.bsky.actor.getProfiles",
                 setNames(as.list(chunks[[j]]), rep("actors", length(chunks[[j]]))))
  Sys.sleep(PAUSE_SEC)
  if (!is.null(res)) for (pr in res$profiles) {
    prof_rows[[length(prof_rows) + 1]] <- data.frame(
      did = pr$did, handle = pr$handle %||% NA_character_,
      display_name = pr$displayName %||% NA_character_,
      followers = pr$followersCount %||% NA_integer_,
      follows   = pr$followsCount   %||% NA_integer_,
      n_posts   = pr$postsCount     %||% NA_integer_,
      stringsAsFactors = FALSE)
  }
  if (j %% 50 == 0) cat("  ", j, "/", length(chunks), "profile calls\n")
}
profiles <- bind_rows(prof_rows) %>% distinct(did, .keep_all = TRUE)

# ----------------------------- SAVE ------------------------------------------
attr(posts, "collected_at") <- Sys.time()
attr(posts, "window") <- c(fmt(START_TIME), fmt(END_TIME))
attr(posts, "queries") <- QUERIES
saveRDS(posts, "data/posts.rds")
write.csv(posts, "data/posts.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(edges, "data/edges.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(profiles, "data/profiles.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(bind_rows(log_rows), "data/collection_log.csv", row.names = FALSE)
writeLines(c(paste("collected_at:", format(Sys.time(), tz = "UTC"), "UTC"),
             paste("window_start:", fmt(START_TIME)),
             paste("window_end:", fmt(END_TIME)),
             paste("queries:", paste(QUERIES, collapse = ", ")),
             paste("unique_posts:", nrow(posts)),
             paste("edges:", nrow(edges)),
             paste("profiles:", nrow(profiles))),
           "data/collection_info.txt")

cat("\nDONE! Files saved in the data/ folder:\n")
print(file.info(list.files("data", full.names = TRUE))[, "size", drop = FALSE])
