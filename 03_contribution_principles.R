pdta <- subset(blueprint,
  CcodeQOG %in% key.ccodeQog & Year %in% c(1990, 2012)
)
pdta <- within(pdta,
  cname <- factor( # fix country order to lo -- mean -- hi
    Country, levels = key.names, labels = key.names
  )
)
pdta <- reshape2::melt(
  data = pdta,
  id.vars = c('cname', 'Year'),
  measure.vars = c('Freedom', 'Control', 'Equality')
)
p <- ggplot(
  data = pdta,
  aes(x = variable, y = value, fill = variable)
) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(cname ~ Year) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_fill_manual(values = wzb.colors) +
  coord_flip() +
  theme_bw(base_size = base.size) +
  theme(
    legend.direction = 'horizontal', legend.position = 'bottom',
    legend.key.size = grid::unit(.5, 'lines'),
    legend.key = element_rect(colour = 'transparent'),
    legend.title = element_blank(),
    legend.background = element_rect(fill = '#e9e9e9'),
    axis.title = element_blank(),
    text = element_text(family = 'CMU Sans Serif'),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = grid::unit(c(0,0,0,0)+.1, units = 'lines'),
    plot.background = element_rect(fill = '#e9e9e9', colour = '#e9e9e9'),
    strip.background = element_rect(fill = 'grey65')
  )

ggsave(
  plot = p,
  file = file.path(path.out, '03_contribution_barChart.png'),
  width = plot.size/1.5, height = plot.size, dpi = 300
)

# Housekeeping ---------------------------------------------
rm(p, pdta)

