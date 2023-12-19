## read RSS with updates
library(atr)
library(dplyr)
library(stringr)
library(xml2)

# get already posted updates
Sys.setenv(BSKY_TOKEN = "cranberries.rds")
auth(user = "cranberriesfeed.bsky.social", password = Sys.getenv("ATR_PW"), overwrite = TRUE)
posts <- get_skeets_authored_by("cranberriesfeed.bsky.social", limit = 5000L)

# collect rss feed
feed <- read_xml("http://dirk.eddelbuettel.com/cranberries/index.rss")
changes <- tibble(
  title = xml_find_all(feed, "//item/title") |>
    xml_text(),
  timestamp = xml_find_all(feed, "//item/pubDate") |>
    xml_text() |>
    anytime::anytime()
) |>
  mutate(
    type = case_when(
      grepl("New package", title) ~ "new_package",
      grepl("updated to version", title) ~ "update",
      grepl("was removed from CRAN", title) ~ "removed"
    ),
    package = str_extract(title, "(?<=Package )\\w+|(?<=New package )\\w+"),
    version = str_extract(
      title,
      "(?<=initial version )\\d+.\\d+.\\d*|(?<=last version )\\d+.\\d+.\\d*|(?<=to version )\\d+.\\d+.\\d*"
    ))

# create posts for new releases
new_pkgs <- changes |>
  filter(type == "new_package") |>
  mutate(post = paste0(
    "New CRAN package {",
    package, "} with initial version", version,
    " #rstats\n\nhttps://cran.r-project.org/package=", package)) |>
  # check which posts were already released
  filter(!post %in% posts$text)


# create posts for updates and removals
updates <- changes |>
  filter(type != "new_package") |>
  group_by(type, timestamp) |>
  summarise(package = paste0("{", package, "}", collapse = ", "), .groups = "drop",
            title = head(title, 1L)) |>
  mutate(post = case_when(
    type ==  "update" ~ paste("CRAN updates:", package, "#rstats"),
    type ==  "removed" ~ paste("CRAN removals:", package, "#rstats")
  )) |>
  # check which posts were already released
  filter(!post %in% posts$text)

for (i in seq_len(nrow(new_pkgs))) {
  post_skeet(text = new_pkgs$post[i], created_at = new_pkgs$timestamp[i])
}

for (i in seq_len(nrow(updates))) {
  post_skeet(text = updates$post[i], created_at = updates$timestamp[i])
}

