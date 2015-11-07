pdta <- subset(blueprint,
  CcodeQOG %in% key.ccodeQog & Year %in% c(1990, 2012)
)
pdta <- reshape2::melt(
  data = pdta,
  id.vars = c('Country', 'Year'),
  measure.vars = c('Freedom', 'Control', 'Equality')
)
p <- ggplot(
  data = pdta,
  aes(x = factor(0), y = value, fill = variable)
) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(Year ~ Country) +
  scale_fill_manual(values = wzb.colors) +
  coord_flip() +
  labs(fill = 'Democratic Principle') +
  theme_bw(base_size = 24) +
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.key.size = grid::unit(.8, 'lines'),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank()
  )

ggsave(
  plot = p,
  file = file.path(path.out, '03_contribution_barChart.png'),
  width = plot.size, height = plot.size/1.618, dpi = 300
)

# Housekeeping ---------------------------------------------
rm(p, pdta)

