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

# boxplots
boxplot_value <- campaign_party %>%
    mutate(
        party = if_else(party %in% c("prb", "psc"), party, "other")
    ) %>%
    ggplot() +
    geom_boxplot(
        aes(as.factor(year), value_expense, color = party),
        outlier.shape = NA
    ) +
    coord_cartesian(
        ylim = c(0, 8e4)
    )

boxplot_value_per_vote <- campaign_party %>%
    mutate(
        party = if_else(party %in% c("prb", "psc"), party, "other")
    ) %>%
    ggplot() +
    geom_boxplot(
        aes(as.factor(year), value_expense/vote, color = party),
        outlier.shape = NA
    ) +
    coord_cartesian(
        ylim = c(0, 50)
    )

# point estimate: average
point_value <- campaign_party %>%
    mutate(
        party = if_else(party %in% c("prb", "psc"), party, "other")
    ) %>%
    group_by(party, year) %>%
    summarise(
        value_expense = mean(value_expense, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    ggplot() + 
    geom_point(
        aes(as.factor(year), value_expense, color = party),
        position = position_dodge(0.5),
        size = 3
    ) +
    coord_cartesian(
        ylim = c(0, 3e5)
    )

point_value_per_vote <- campaign_party %>%
    mutate(
        party = if_else(party %in% c("prb", "psc"), party, "other")
    ) %>%
    group_by(party, year) %>%
    summarise(
        value_per_vote = sum(value_expense, na.rm = TRUE)/
            sum(vote, na.rm = TRUE),
        .groups = "drop"
        ) %>%
    ggplot() + 
    geom_point(
        aes(as.factor(year), value_per_vote, color = party),
        position = position_dodge(0.5),
        size = 3
    ) +
    coord_cartesian(
        ylim = c(0, 20)
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
