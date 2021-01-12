# ==============================================================================
# input: clean campaign spending data
# output: party-year aggregate spending
# ==============================================================================
source(
    here::here("source/aux/globals.R")
)

source(
    here("source/aux/utils.R")
)

# ---------------------------------------------------------------------------- #
message("importing data")
input_filenames <- list.files(
    here("data/clean/"),
    pattern = "campaign_spending",
    full.names = TRUE
)

# read in files
campaign <- input_filenames %>%
    map(fread)

campaign_party <- campaign %>% 
    map_dfr(summarise_party_spending)

