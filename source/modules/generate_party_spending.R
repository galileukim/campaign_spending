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

election <- fread(
    here("data/raw/election_local.csv.gz")
) 

# ---------------------------------------------------------------------------- #
message("generate summary statistics")
campaign_party <- campaign %>% 
    map_dfr(summarise_party_spending)

# filter out na's (72 rows)
campaign_party <- campaign_party %>%
    filter(!is.na(cod_ibge_6))

vote_party <- election %>%
    rename(year = election_year) %>%
    filter(year >= 2008 & position == "prefeito") %>%
    group_by(cod_ibge_6, party, year) %>%
    summarise(
        vote = sum(vote, na.rm = TRUE),
        .groups = "drop"
    )
