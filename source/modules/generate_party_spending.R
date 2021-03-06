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

election_2020 <- fread(
    here("data/raw/election_local_2020.csv"),
    encoding = "Latin-1"
) %>%
    filter(CD_CARGO == 11) %>% # extract only mayors
    transmute(
        cod_tse = SG_UE,
        party = SG_PARTIDO,
        vote = QT_VOTOS_NOMINAIS,
        year = 2020
    ) %>%
    mutate(
        across(where(is.character), str_to_lower)
    )

identifiers <- fread(
    here("data/raw/data_rosettastone.csv")
) %>%
    transmute(
        cod_tse = codetse,
        cod_ibge_6 = codeipea
    )

# ---------------------------------------------------------------------------- #
message("generate summary statistics")

# aggregate total spending by party per year
campaign_party <- campaign %>%
    map_dfr(summarise_party_spending)

# filter out na's (24 rows)
campaign_party <- campaign_party %>%
    filter(!is.na(cod_ibge_6))

# filter out 2012 and 2016
campaign_party_spending <- campaign_party %>% 
    filter(year %in% c(2012, 2016)) %>%
    group_by(year) %>% 
    complete(cod_ibge_6, year, nesting(party)) %>%
    ungroup()

# generate party vote by municipality year
# and number of candidates
vote_party_pre_2020 <- election %>%
    rename(year = election_year) %>%
    filter(year >= 2008 & position == "prefeito") %>%
    group_by(cod_ibge_6, party, year) %>%
    summarise(
        vote = sum(vote, na.rm = TRUE),
        .groups = "drop"
    )

vote_party_2020 <- election_2020 %>%
    left_join(
        identifiers,
        by = "cod_tse"
    ) %>%
    group_by(
        cod_ibge_6, party, year
    ) %>%
    summarise(
        vote = sum(vote, na.rm = TRUE),
        .groups = "drop"
    )

vote_party <- vote_party_pre_2020 %>%
    bind_rows(
        vote_party_2020
    ) %>%
    arrange(
        party, cod_ibge_6, year
    )

# join data
campaign_party_vote <- campaign_party %>%
    left_join(
        vote_party,
        by = c("cod_ibge_6", "party", "year")
    )
# campaign_party_vote by electoral year
campaign_party_per_year <- campaign_party_vote %>%
    group_by(party, year) %>%
    summarise(
        n_mayor = n_distinct(cod_ibge_6),
        mean_campaign = mean(value_expense),
        median_campaign_per_vote = median(value_expense, na.rm = TRUE),
        mean_campaign_per_vote = mean(value_expense/vote, na.rm = TRUE),
        median_campaign_per_vote = median(value_expense/vote, na.rm = TRUE)
    )

# ---------------------------------------------------------------------------- #
message("writing out data")

# write-out data
campaign_party_spending %>% 
    write_csv(
        here("data/clean/campaign_party_spending.csv")
    )
    
campaign_party_vote %>%
    data.table::fwrite(
        here("data/clean/campaign_party_vote.csv")
    )

campaign_party_per_year %>%
    data.table::fwrite(
        here("data/clean/campaign_party_per_year.csv")
    )

vote_party %>%
    data.table::fwrite(
        here("data/clean/party_vote.csv")
    )
