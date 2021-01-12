# ==============================================================================
# input: clean files for campaign spending by party
# output: visualization of campaign spending by party
# ==============================================================================
source(
    here::here("source/aux/globals.R")
)

source(
    here("source/aux/utils.R")
)

theme_set(
    theme_minimal()
)
# ---------------------------------------------------------------------------- #
message("generate visualizations")

campaign_party <- fread(
    here("data/clean/campaign_party_vote.csv")
)

# fix republicanos -> prb
campaign_party <- campaign_party %>%
    mutate(
        party = if_else(party == "republicanos", "prb", party)
    )

campaign_party %>%
    filter(
        party %in% c("prb", "psc") &
        value_expense > 0
    ) %>%
    ggplot() +
    geom_boxplot(
        aes(as.factor(year), value_expense, color = party),
        outlier.shape = NA
    ) +
    coord_cartesian(
        ylim = c(0, 8e4)
    )

campaign_party %>%
    filter(
        party %in% c("prb", "psc") &
        value_expense > 0
    ) %>%
    ggplot() +
    geom_boxplot(
        aes(as.factor(year), value_expense/vote, color = party),
        outlier.shape = NA
    ) +
    coord_cartesian(
        ylim = c(0, 1000)
    )
