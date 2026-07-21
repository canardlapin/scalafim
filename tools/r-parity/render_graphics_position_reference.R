args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1L) args[[1L]] else "target/graphics-position-qa/ggplot2"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages(library(ggplot2))

palette <- c(red = "#467DB4", blue = "#DC8741")
bars <- data.frame(
  category = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
  value = c(3, 2, 1, 4),
  group = factor(c("red", "blue", "red", "blue"), levels = c("red", "blue"))
)

base_bars <- ggplot(bars, aes(category, value, group = group, fill = group)) +
  scale_fill_manual(values = palette) +
  labs(x = "category", y = "value") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

dodge <- base_bars +
  geom_col(width = 0.9, colour = "#232D37", position = position_dodge(width = 0.9)) +
  ggtitle("position-dodge")
stack <- base_bars +
  geom_col(width = 0.9, colour = "#232D37", position = position_stack()) +
  ggtitle("position-stack")

points <- data.frame(
  category = factor(c("A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
  value = c(1, 1, 1.6, 2, 2, 2.6),
  group = factor(c("red", "blue", "red", "blue", "red", "blue"), levels = c("red", "blue"))
)
jittered <- ggplot(points, aes(category, value, colour = group)) +
  geom_point(position = position_jitter(width = 0.22, height = 0.12, seed = 2026), size = 2) +
  scale_colour_manual(values = palette) +
  labs(title = "position-jitter", x = "category", y = "value") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

plots <- list(dodge = dodge, stack = stack, jitter = jittered)
for (name in names(plots)) {
  ggsave(
    filename = file.path(out_dir, paste0(name, ".png")),
    plot = plots[[name]],
    width = 6.4,
    height = 4.8,
    dpi = 100,
    bg = "white"
  )
}

dodge_data <- layer_data(dodge)
stack_data <- layer_data(stack)
jitter_data <- layer_data(jittered)
write.table(
  dodge_data[c("x", "xmin", "xmax", "y", "group")],
  file.path(out_dir, "dodge-layer.tsv"),
  sep = "\t", row.names = FALSE, quote = FALSE
)
write.table(
  stack_data[c("x", "xmin", "xmax", "y", "ymin", "ymax", "group")],
  file.path(out_dir, "stack-layer.tsv"),
  sep = "\t", row.names = FALSE, quote = FALSE
)
write.table(
  jitter_data[c("x", "y", "group")],
  file.path(out_dir, "jitter-layer.tsv"),
  sep = "\t", row.names = FALSE, quote = FALSE
)

cat("wrote ggplot2 position references to", out_dir, "\n")
