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
message("import data")

campaign_party <- fread(
    here("data/clean/campaign_party_vote.csv")
)

vote_party <- fread(
    here("data/clean/party_vote.csv")
) %>%
     mutate(
        party = if_else(party == "republicanos", "prb", party)
    )

# fix republicanos -> prb
campaign_party <- campaign_party %>%
    mutate(
        party = if_else(party == "republicanos", "prb", party)
    )

# ---------------------------------------------------------------------------- #
message("find similar parties with respect to number of candidates")

christian_parties <- c("prb", "psc")
disagg_parties <- c("prb", "psc", "pmdb", "dem", "psdb", "pt", "pp", "pr")

# calculating number of candidates per party-year
candidates_per_party <- vote_party %>%
    group_by(party, year) %>%
    summarise(
        total = n_distinct(cod_ibge_6),
        .groups = "drop"
    )

# extract christian party candidates
christian_candidates <- candidates_per_party %>%
    filter(party %in% christian_parties)

# obtain similar parties  wrt total no. candidates fielded
# range from 200-500 throughout the period
# most similar are sol and pv, that also include records for all years
similar_parties <- candidates_per_party %>%
    filter(
        !(party %in% christian_parties) &
        between(total, 200, 500)
    ) %>%
    group_by(party) %>%
    mutate(
        number_of_races = n_distinct(year)
    ) %>%
    filter(number_of_races == 4) %>%
    distinct(party) %>%
    pull()

# ---------------------------------------------------------------------------- #
message("generate visualizations")

parties_to_plot <- list(
    aggregated = christian_parties,
    disaggregated = disagg_parties,
    comparable = c(christian_parties, similar_parties)
)
plots_output <- list()

for (parties in parties_to_plot) {
    # generate tables for plotting
    campaign_party_plot <- campaign_party %>%
        mutate(
            party = if_else(party %in% parties, party, "other") %>%
                fct_relevel(
                    c("other", parties)
                ),
            year = as.factor(year)
        )

    campaign_party_plot_summary <- campaign_party_plot %>%
        group_by(party, year) %>%
        summarise(
            value_expense_per_vote = sum(value_expense, na.rm = TRUE) /
                sum(vote, na.rm = TRUE),
            value_expense = mean(value_expense, na.rm = TRUE),
            .groups = "drop"
        )

    # boxplots
    boxplot_value <- campaign_party_plot %>%
        gg_boxplot(
            x = party,
            y = value_expense,
            grouping = year,
            lims = c(0, 8e4)
        )

    boxplot_value_per_vote <- campaign_party_plot %>%
        gg_boxplot(
            party,
            value_expense / vote,
            grouping = year,
            lims = c(0, 50)
        )

    # point estimate: average
    point_value <- campaign_party_plot_summary %>%
        gg_point(
            party,
            value_expense,
            grouping = year,
            lims = c(0, 8e4)
        )

    point_value_per_vote <- campaign_party_plot_summary %>%
        gg_point(
            party,
            value_expense_per_vote,
            grouping = year,
            lims = c(0, 20)
        )

    repository <- names(parties_to_plot)[
        map_lgl(parties_to_plot, identical, parties)
    ]

    plots_output[[repository]] <- list(
        boxplot_value,
        boxplot_value_per_vote,
        point_value,
        point_value_per_vote
    )
}

plot_names <- c(
        "boxplot_expense",
        "boxplot_expense_per_vote",
        "mean_expense",
        "mean_expense_per_vote"
    )

filenames <- map(
    list(
        here("figures/pooled_%s.pdf"),
        here("figures/disaggregated_%s.pdf"),
        here("figures/similar_%s.pdf")
    ),
    sprintf,
    plot_names
)

map2(
    plots_output,
    filenames,
    ~ pwalk(
        list(plot = .x, filename = .y),
            ggsave
        )
    )
