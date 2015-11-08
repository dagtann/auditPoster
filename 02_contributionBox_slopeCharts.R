# Slope charts Democracy Barometer -------------------------

# prepare plotting data ------------------------------------
pdta <- subset(blueprint, Year %in% c(1990, 2012))
pdta <- within(pdta, {
  tag <- ifelse(CcodeQOG == key.ccodeQog[1], 1, 999)
  tag <- ifelse(CcodeQOG == key.ccodeQog[2], 2, tag)
  tag <- ifelse(CcodeQOG == key.ccodeQog[3], 3, tag)
  tag <- factor(tag, levels = c(1:3, 999), labels = c(key.names, 'Other'))
  }
)
table(pdta$tag)
pdta <- reshape2::melt(
  data = pdta,
  id.vars = c('Country', 'Year', 'tag'),
  measure.vars = c('DQ')
)

# proceed to plotting --------------------------------------
p <- ggplot(                        # plot non-key countries
  data = subset(pdta, Country %in% key.names == FALSE),
  aes(
    x = factor(Year), y = value, group = Country
  ),
) +
geom_line(
  aes(x = as.numeric(as.factor(Year))),
  colour = 'grey75', show_guide = FALSE
) +
geom_point(size = 3, colour = 'grey75', show_guide = FALSE) +
geom_point(                      # plot key countries on top
  data = subset(pdta, Country %in% key.names),
  aes(colour = tag), size = 3
) +
geom_line(
  data = subset(pdta, Country %in% key.names),
  aes(colour = tag), show_guide = FALSE
) +
scale_x_discrete(expand = c(.5, 0)) +
scale_y_continuous(
  limits = c(40, 85), breaks = seq(40, 80, 10)
) +
scale_colour_manual(values = wzb.colors, labels = c('Costa Rica', 'USA', 'Denmark')) +
labs(y = 'Democratic Quality') +
guides(colour = guide_legend(
  title = NULL, nrow = 1, size = .3*base.size
  )
) +
theme_bw(base_size = base.size) +
theme(
  legend.position = 'bottom', legend.direction = 'horizontal',
  legend.key = element_blank(),
  legend.key.size = grid::unit(.1, 'lines'),
  legend.background = element_rect(fill = '#e9e9e9'),
  legend.box = 'vertical',
  text = element_text(family = 'CMU Sans Serif'),
  axis.title.x = element_blank(),
  plot.margin = grid::unit(rep(0, 4)+.1, units = 'lines'),
  plot.background = element_rect(fill = '#e9e9e9', colour = '#e9e9e9'),
  strip.background = element_rect(fill = wzb.colors[3])
)
ggsave(
  plot = p,
  file = file.path(path.out, '02_contribution_slopeChart.png'),
  width = plot.size/1.5, height = plot.size, dpi = 300
)

# Housekeeping ---------------------------------------------
rm(p, pdta)
