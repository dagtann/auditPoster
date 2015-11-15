# Slope charts Democracy Barometer -------------------------

# prepare plotting data ------------------------------------
pdta <- subset(blueprint,
  Year %in% c(1990, 2012) & CcodeQOG %in% c(840)
)
pdta <- within(pdta,
  cname <- factor( # fix country order to lo -- mean -- hi
    Country, levels = key.names, labels = key.names
  )
)

pdta <- reshape2::melt(
  data = pdta,
  id.vars = c('cname', 'Year'),
  measure.vars = c("INDLIB", "RULEOFLAW", "PUBLIC",
    "COMPET", "MUTUCONS", "GOVCAP", "TRANSPAR", "PARTICIP",
    "REPRES"
  )
)
pdta <- within(pdta, {
  order <- factor(variable,        ## factor with democratic
  ## functions ordered by democratic principle for x-scale
    levels = c(
      "INDLIB", "RULEOFLAW", "PUBLIC", "COMPET",
      "MUTUCONS", "GOVCAP", "TRANSPAR", "PARTICIP", "REPRES"
    ),
    labels = c(                    ## define speaking labels
      "Individual Freedom", "Rule of Law",
      "Public Sphere", "Competition",
      "Horizontal Control", "Government Capability",
      "Transparency", "Participation", "Representation"
    )
  )
  fill <- NA                       ## factor for fill colors
  fill <- ifelse(order %in% c("Individual Freedom",
    "Rule of Law", "Public Sphere") == TRUE, 1, NA
  )
  fill <- ifelse(order %in% c("Competition",
    "Horizontal Control", "Government Capability") == TRUE,
    2, fill
  )
  fill <- ifelse(order %in% c("Transparency",
      "Participation", "Representation") == TRUE, 3, fill
  )
  fill <- factor(                 ## Principles of Democracy
    fill,
    levels = 1:3,
    labels = c("Freedom", "Control", "Equality")
  )
  }
)

scale.labs <- data.frame(             ## define scale labels
  x = rep(1, 5),
  y = seq(20, 100, 20),
  labels = as.character(seq(20, 100, 20))
)

scale.grid <- data.frame(            ## define grid for plot
  x = rep(1:9, each = 10),       ## 9 functions of democracy
  y = rep(10, 90)
)

p <- ggplot(data = pdta,
  aes(x = variable, y = value, fill = fill)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = wzb.colors) +
  scale_x_discrete(
    labels = c(                    ## define speaking labels
      "Ind. Freed.", "Rule of Law",
      "Pub. Sphere", "Competition",
      "Horiz. Contr.", "Gov. Cap.",
      "Transpar.", "Participat.", "Represent."
    )
  ) +
#   scale_y_continuous(
#     limits = c(0, 100), breaks = seq(0,100,20)
#   ) +
  labs(x = "", y = "") +
  #coord_polar(start = 340*0.0174532925) +
  facet_grid(cname ~ Year) +
  theme_bw(base_size = base.size) +
  theme(
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks.y = element_blank(),
    legend.direction = 'horizontal', legend.position = 'bottom',
    legend.key.size = grid::unit(.5, 'lines'),
    legend.key = element_rect(colour = 'transparent'),
    legend.title = element_blank(),
    legend.background = element_rect(fill = 'transparent'),
    text = element_text(family = 'Dahrendorf Light'),
    plot.margin = grid::unit(c(0,.1,0,0)+.1, units = 'lines'),
    plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
    strip.background = element_rect(fill = 'grey65')
  ) +
  coord_flip()

ggsave(
  plot = p,
  file = file.path(path.out, '04_contribution_roseDiagram.png'),
  width = plot.size/1.5, height = plot.size, dpi = 300,
  bg = 'transparent'
)

# Housekeeping ---------------------------------------------
rm(p, pdta)
