# Slope charts Democracy Barometer -------------------------

# prepare plotting data ------------------------------------
pdta <- subset(blueprint,
  Year %in% c(1990, 2012) & CcodeQOG %in% key.ccodeQog
)

pdta <- reshape2::melt(
  data = pdta,
  id.vars = c('Country', 'Year'),
  measure.vars = c('DQ')
)

# proceed to plotting --------------------------------------
p <- ggplot(data = pdta, aes(x = factor(Year), y = value)) +
  geom_point(size = 5) +
  geom_line(aes(x = as.numeric(as.factor(Year)))) +
  scale_y_continuous(
    limits = c(40, 85), breaks = seq(40, 80, 10)
  ) +
  facet_grid(~Country) +
  labs(y = 'Democratic Quality') +
  theme_bw(base_size = 32) +
  theme(axis.title.x = element_blank())
ggsave(
  plot = p,
  file = file.path(path.out, '02_contribution_slopeChart.png'),
  width = plot.size, height = plot.size/1.618, dpi = 1200
)

# Housekeeping ---------------------------------------------
rm(p, pdta)
