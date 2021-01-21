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

number_of_candidates <- 

# ---------------------------------------------------------------------------- #
message("generate visualizations")

parties_to_plot <- list(
    aggregated = christian_parties,
    disaggregated = disagg_parties
)
plots_output <- list()

for (parties in parties_to_plot) {
    # generate tables for plotting
    campaign_party_plot <- campaign_party %>%
        mutate(
            party = if_else(party %in% parties, party, "other") %>%
                fct_relevel(
                    c(parties, "other")
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

    repository <- if(length(parties) == 2) "aggregated" else "disaggregated"

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
        here("figures/%s.pdf"),
        here("figures/disaggregated_%s.pdf")
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