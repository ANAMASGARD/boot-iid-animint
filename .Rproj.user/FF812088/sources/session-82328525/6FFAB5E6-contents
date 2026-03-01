# port of boot.iid() from the animation package to animint2
# original: https://yihui.org/animation/example/boot-iid/
# shows bootstrap resampling step by step — click any iteration to explore

library(animint2)

set.seed(42)
x    <- runif(20)
nmax <- 50
m    <- length(x)

# compute all bootstrap samples upfront instead of inside a loop
idx  <- replicate(nmax, sample(m, m, replace = TRUE))
xx   <- matrix(x[idx], nrow = m)
xest <- apply(xx, 2, mean)

# original 20 points — shown in every frame as background
df_original <- data.frame(
  x_val        = x,
  sample_index = seq_along(x)
)

# for each iteration, which points got picked and how many times
df_resample <- do.call(rbind, lapply(1:nmax, function(i) {
  counts <- table(idx[, i])
  data.frame(
    iteration    = i,
    sample_index = as.integer(names(counts)),
    x_val        = x[as.integer(names(counts))],
    count        = as.integer(counts)
  )
}))

# manually bin the means — geom_histogram conflicts with showSelected
n_bins <- 15
breaks <- seq(min(xest) - 0.001, max(xest) + 0.001, length.out = n_bins + 1)

df_stat <- do.call(rbind, lapply(1:nmax, function(i) {
  binned <- cut(xest[1:i], breaks = breaks, include.lowest = TRUE)
  counts <- table(binned)
  data.frame(
    iteration = i,
    xmin      = breaks[-length(breaks)],
    xmax      = breaks[-1],
    bin_count = as.integer(counts)
  )
}))

# one row per iteration, drives the clickable timeline
df_iter <- data.frame(
  iteration  = 1:nmax,
  stat_value = xest
)

# black dots are the original data, red dots are what got resampled
# bigger dot = that point was picked more times this round
gg_resample <- ggplot() +
  geom_point(
    data  = df_original,
    aes(x = x_val, y = sample_index),
    color = "black",
    size  = 3
  ) +
  geom_point(
    data         = df_resample,
    aes(x        = x_val, y = sample_index, size = count),
    color        = "red",
    showSelected = "iteration"
  ) +
  scale_size_continuous(range = c(4, 14), name = "Times picked") +
  coord_cartesian(xlim = c(0, 1.05)) +
  theme_bw() +
  theme(
    plot.title  = element_text(size = 11),
    plot.margin = unit(c(10, 20, 5, 5), "pt")
  ) +
  labs(
    title = "Bootstrap Sample",
    x     = "Sample Value",
    y     = "Original Index (1-20)"
  )

# histogram grows as iterations complete — blue line is the original mean
gg_distribution <- ggplot() +
  geom_rect(
    data         = df_stat,
    aes(xmin     = xmin, xmax = xmax, ymin = 0, ymax = bin_count),
    fill         = "bisque",
    color        = "black",
    showSelected = "iteration"
  ) +
  geom_vline(
    xintercept = mean(x),
    color      = "blue",
    linetype   = "dashed",
    linewidth  = 1
  ) +
  theme_bw() +
  theme(
    plot.title  = element_text(size = 11),
    plot.margin = unit(c(10, 20, 5, 5), "pt")
  ) +
  labs(
    title = "Distribution of Bootstrap Means (blue = original mean)",
    x     = "Bootstrap Mean",
    y     = "Count"
  )

# click any grey bar to jump to that iteration
# red dot follows whichever iteration is currently active
gg_progress <- ggplot() +
  geom_tallrect(
    data         = df_iter,
    aes(xmin     = iteration - 0.5, xmax = iteration + 0.5),
    clickSelects = "iteration",
    alpha        = 0.3,
    fill         = "grey"
  ) +
  geom_line(
    data = df_iter,
    aes(x = iteration, y = stat_value)
  ) +
  geom_point(
    data         = df_iter,
    aes(x        = iteration, y = stat_value),
    showSelected = "iteration",
    color        = "red",
    size         = 4
  ) +
  scale_x_continuous(breaks = seq(0, 50, 10)) +
  theme_bw() +
  theme(
    plot.title  = element_text(size = 11),
    plot.margin = unit(c(10, 20, 5, 5), "pt")
  ) +
  labs(
    title = "Bootstrap Mean per Iteration (click bar to select)",
    x     = "Iteration",
    y     = "Bootstrap Mean"
  )

# deployed on GitHub Pages
viz <- animint(
  title        = "Bootstrap i.i.d. Demo",
  resample     = gg_resample,
  distribution = gg_distribution,
  progress     = gg_progress,
  time         = list(variable = "iteration", ms = 600),
  source       = "https://github.com/ANAMASGARD/boot-iid-animint/blob/main/boot_iid_viz.R"
)

## local preview (uncomment to run locally instead)
# animint2dir(viz, out.dir = "boot-iid-animint-output")
