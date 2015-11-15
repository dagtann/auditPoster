# Slope charts Freedom house & polity ----------------------

# fetch data -----------------------------------------------
blueprint.qog <- within(blueprint.qog,
  fh_status.numeric <- rowMeans(
    cbind(fh_cl, fh_pr), na.rm = FALSE
  )
)

# prepare plotting data ------------------------------------
pdta <- subset(blueprint.qog,
  year %in% c(1990, 2012) & ccode %in% key.ccodeQog
)
pdta <- reshape2::melt(
  data = pdta,
  id.vars = c('cname', 'year'),
  measure.vars = c('fh_status.numeric') #, 'p_polity2')
)
pdta <- within(pdta, {
  variable <- factor(
    variable,
    levels = c('fh_status.numeric'), #'p_polity2'),
    labels = c('Freedom House')#, 'Polity IV'))
  )
  cname <- factor( # fix country order to lo -- mean -- hi
    cname, levels = key.names, labels = key.names
  )
  }
)

# Proceed to plotting --------------------------------------
p <- ggplot(
  data = pdta, aes(x = factor(year), y = value)
) +
geom_point(size = 2) +
geom_line(aes(x = as.numeric(as.factor(year)))) + # trick into slope chart
  facet_grid(cname ~ variable) +
  scale_y_continuous(
    limits = c(0, 7), breaks = seq(1, 7, 3)
  ) +
  labs(y = 'Index Value') +
  theme_bw(base_size = base.size) +
  theme(
    text = element_text(family = 'CMU Sans Serif'),
    axis.title.x = element_blank(),
    plot.margin = grid::unit(rep(0, 4)+.1, units = 'lines'),
    plot.background = element_rect(fill = '#e9e9e9', colour = '#e9e9e9'),
    strip.background = element_rect(fill = 'grey65')
  )

ggsave(
  plot = p, file = file.path(path.out, '01_issue_slope.png'),
  width = plot.size/1.5, height = plot.size, dpi = 300
)

# House keeping --------------------------------------------
rm(p)
