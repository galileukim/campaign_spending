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

# fix republicanos -> prb
campaign_party <- campaign_party %>%
    mutate(
        party = if_else(party == "republicanos", "prb", party)
    )

# ---------------------------------------------------------------------------- #
message("generate visualizations")
christian_parties <- c("prb", "psc")
parties_to_plot <- c("prb", "psc", "pmdb", "dem", "psdb", "pt", "pp", "pr")

# boxplots
campaign_party_plot <-  campaign_party %>%
    mutate(
        party = if_else(party %in% parties_to_plot, party, "other") %>%
            fct_relevel(
                c(parties_to_plot, "other")
            ),
            year = as.factor(year)
    )

boxplot_value <- campaign_party_plot %>%
    gg_boxplot(
        x = party,
        y = value_expense,
        grouping = year,
        lims = c(0, 8e4)
    )

boxplot_value_per_vote <- campaign_party_plot %>%
    gg_boxplot(
        year,
        value_expense/vote,
        grouping = party,
        lims = c(0, 50)
    )

# point estimate: average
campaign_party_plot_summary <- campaign_party_plot %>%
    group_by(party, year) %>%
    summarise(
        value_expense_per_vote = sum(value_expense, na.rm = TRUE)/
            sum(vote, na.rm = TRUE),
        value_expense = mean(value_expense, na.rm = TRUE),
        .groups = "drop"
    ) 

point_value <- campaign_party_plot_summary%>%
    gg_point(
        party,
        value_expense,
        grouping = year,
        lims = c(0, 8e4)
    ) +
    coord_flip()

point_value_per_vote <- campaign_party_plot_summary %>%
    gg_point(
        party,
        value_expense_per_vote,
        grouping = year,
        lims = c(0, 20)
    )

plot <- list(
    boxplot_value,
    boxplot_value_per_vote,
    point_value,
    point_value_per_vote
)

filename <- sprintf(
    here("figures/%s.pdf"),
    c(
        "boxplot_expense", 
        "boxplot_expense_per_vote", 
        "mean_expense", 
        "mean_expense_per_vote"
    )
)

pwalk(
list(
    plot = plot,
    filename = filename
),
    ggsave)
