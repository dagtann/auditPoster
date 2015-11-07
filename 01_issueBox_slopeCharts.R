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
  measure.vars = c('fh_status.numeric', 'p_polity2')
)
pdta <- within(pdta,
  variable <- factor(
    variable,
    levels = c('fh_status.numeric', 'p_polity2'),
    labels = c('Freedom House', 'Polity IV'))
)

# Proceed to plotting --------------------------------------
p <- ggplot(data = pdta, aes(x = factor(year), y = value)) +
  geom_point(size = 5) +
  geom_line(aes(x = as.numeric(as.factor(year)))) +
  facet_grid(cname ~ variable) +
  scale_y_continuous( # free_y gives more non-sensical result
    limits = c(0, 10), breaks = seq(1, 10, 3)
  ) +
  labs(y = 'Index Value') +
  theme_bw(base_size = 32) +
  theme(axis.title.x = element_blank())

ggsave(
  plot = p, file = file.path(path.out, '01_issue_slope.png'),
  width = plot.size, height = plot.size/1.618, dpi = 300
)

# House keeping --------------------------------------------
rm(p)
