
# CORRELATION TABLE

# Select key variables for correlation analysis
cor_df <- master_df %>%
  select(
    profit_net,
    profit_per_area_net,
    edu_level_mean,
    literacy_rate,
    numeracy_rate,
    land_area_ha,
    soil_fertility,
    crop_output_qty,
    fertilizer_inorganic,
    purchased_seeds,
    irrigated
  )

# CORRELATION TABLE 

# Check summary (optional sanity check)
summary(cor_df)

# Check variance of each variable
var_check <- sapply(cor_df, function(x) {
  if (is.numeric(x)) var(x, na.rm = TRUE) else NA
})

# Keep only variables with non-zero and defined variance
valid_vars <- names(var_check)[!is.na(var_check) & var_check > 0]

# Create cleaned correlation dataset
cor_df_clean <- cor_df[, valid_vars]

# Compute correlation matrix
cor_matrix <- round(
  cor(cor_df_clean, use = "pairwise.complete.obs"),
  2
)

# View results
print(cor_matrix)
View(as.data.frame(cor_matrix))

# CORRELATION HEATMAP

library(ggplot2)

# Convert correlation matrix to long format
cor_long <- as.data.frame(as.table(cor_matrix))
colnames(cor_long) <- c("Variable1", "Variable2", "Correlation")

# Plot heatmap
ggplot(cor_long, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#2c7bb6",
    mid = "white",
    high = "#d7191c",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  geom_text(aes(label = Correlation), size = 3) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Correlation Heatmap of Key Variables"
  )

# SAVE OUTPUTS 

dir.create("figures", showWarnings = FALSE)

# Save correlation table
write.csv(cor_matrix, "figures/correlation_table.csv")

# Save heatmap
ggsave("figures/correlation_heatmap.png", width = 9, height = 7, dpi = 300)

