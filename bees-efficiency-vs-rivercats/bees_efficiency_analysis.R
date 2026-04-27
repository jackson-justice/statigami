library(tidyverse)
library(googlesheets4)
library(scales)

gs4_deauth()
gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")

stat1 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1WbWB1uosqmtusHV1da2vrlxBueBpsYFFoEqLwoL9V18/edit?gid=0#gid=0"
)

colnames(stat1)

rivercats <- stat1 |>
  filter(Team == "RiverCats")

bees <- stat1 |>
  filter(Team == "Bees")

sum(bees$Baserunners)
sum(rivercats$Baserunners)
sum(bees$Runs)
sum(rivercats$Runs)
sum(bees$LOB)
sum(rivercats$LOB)

bees_c_rate <- sum(bees$Runs) / sum(bees$Baserunners)
rc_c_rate <- sum(rivercats$Runs) / sum(rivercats$Baserunners)
bees_s_rate <- sum(bees$LOB) / sum(bees$Baserunners)
rc_s_rate <- sum(rivercats$LOB) / sum(rivercats$Baserunners)

game_summary <- stat1 |>
  filter(Team %in% c("Bees", "RiverCats")) |>
  summarize(
    baserunners = sum(Baserunners, na.rm = TRUE),
    runs = sum(Runs, na.rm = TRUE),
    lob = sum(LOB, na.rm = TRUE),
    conversion_rate = runs / baserunners,
    strand_rate = lob / baserunners,
    .by = Team
  ) |>
  mutate(
    Team = recode(Team, "RiverCats" = "River Cats"),
    Team = factor(Team, levels = c("Bees", "River Cats")),
    label = paste0(
      percent(conversion_rate, accuracy = 0.1),
      "\n",
      runs, " runs / ", baserunners, " baserunners"
    )
  )

game_summary

game1_plot <- ggplot(game_summary, aes(x = Team, y = conversion_rate, fill = Team)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(
    aes(label = label),
    vjust = -0.3,
    size = 5.5,
    fontface = "bold",
    lineheight = 1.1
  ) +
  scale_fill_manual(
    values = c(
      "Bees" = "#FFB81C",
      "River Cats" = "#862633"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.5),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Bees Were More Efficient",
    subtitle = "Same chances. Better execution • 4/3/2026",
    x = NULL,
    y = "Conversion Rate"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

game1_plot

ggsave(
  filename = "slcbees/game1_plot.png",
  plot = game1_plot,
  width = 8,
  height = 10,
  dpi = 300
)

ggsave(
  "slcbees/game1_plot_vert.png",
  plot = game1_plot,
  width = 9,
  height = 16,
  dpi = 300
)
