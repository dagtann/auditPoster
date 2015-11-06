# Slope charts Democracy Barometer -------------------------

# prepare plotting data ------------------------------------
pdta <- subset(blueprint,
  Year %in% c(1990, 2012) & CcodeQOG %in% key.ccodeQog
)

pdta <- reshape2::melt(
  data = pdta,
  id.vars = c('Country', 'Year'),
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
  x = rep(1, 6),
  y = seq(0, 100, 20),
  labels = as.character(seq(0, 100, 20))
)

scale.grid <- data.frame(            ## define grid for plot
  x = rep(1:9, each = 10),       ## 9 functions of democracy
  y = rep(10, 90)
)

p <- ggplot(data = pdta,
  aes(x = variable, y = value, fill = fill)) +
  geom_bar(
    width = 1, stat = "identity"
  ) +
  scale_fill_manual(values = wzb.colors) +
  geom_bar(
    data = scale.grid,              ## implement manual grid
    aes(x = x, y = y, fill = NA), width = 1,
    colour = "grey95", position = "stack", stat = "identity"
  ) +
  geom_text(
    data = scale.labs, aes(x = x, y = y, label = labels,
      fill = NULL
    ),
    size = 4
  ) +
  scale_x_discrete(
    labels = c(                    ## define speaking labels
      "Individual Freedom", "Rule of Law",
      "Public\nSphere", "Competition",
      "Horizontal Control", "Government Capability",
      "Transparency", "Partici-\npation", "Representation"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 100), breaks = seq(0,100,10)
  ) +
  labs(x = "", y = "") +
  guides(
    fill = guide_legend(title = "Democratic Principle",
      override.aes = list(colour = NULL, alpha = 1)
    )
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal"
  ) +
  coord_polar(start = 340*0.0174532925) +
  facet_grid(Year ~ Country)

ggsave(
  plot = p,
  file = file.path(path.out, '04_contribution_roseDiagram.png'),
  width = plot.size, height = plot.size/1.618, dpi = 1200
)

# Housekeeping ---------------------------------------------
rm(p, pdta)
