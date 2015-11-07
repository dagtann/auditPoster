# Slope charts Democracy Barometer -------------------------

# prepare plotting data ------------------------------------
pdta <- subset(blueprint, Year %in% c(1990, 2012))
pdta <- within(pdta, {
  tag <- ifelse(CcodeQOG == key.ccodeQog[1], 1, 0)
  tag <- ifelse(CcodeQOG == key.ccodeQog[2], 2, tag)
  tag <- ifelse(CcodeQOG == key.ccodeQog[3], 3, tag)
  tag <- factor(tag, levels = 0:3, labels = c('Other', key.names))
  }
)
pdta <- reshape2::melt(
  data = pdta,
  id.vars = c('Country', 'Year', 'tag'),
  measure.vars = c('DQ')
)

# proceed to plotting --------------------------------------
p <- ggplot(data = pdta, aes(x = factor(Year), y = value, colour = tag, group = Country)) +
  geom_point(size = 5) +
  geom_line(aes(x = as.numeric(as.factor(Year))), show_guide = FALSE) +
  scale_x_discrete(expand = c(.5, 0)) +
  scale_y_continuous(
    limits = c(40, 85), breaks = seq(40, 80, 10)
  ) +
  scale_colour_manual(values = c('grey85', wzb.colors)) +
  labs(y = 'Democratic Quality', colour = 'Country') +
  theme_bw(base_size = 32) +
  theme(
    legend.position = 'bottom', legend.direction = 'horizontal',
    legend.key = element_blank(),
    axis.title.x = element_blank()
  )
ggsave(
  plot = p,
  file = file.path(path.out, '02_contribution_slopeChart.png'),
  width = plot.size, height = plot.size/1.618, dpi = 300
)

# Housekeeping ---------------------------------------------
rm(p, pdta)
