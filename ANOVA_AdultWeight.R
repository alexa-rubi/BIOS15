dat = read.csv("~/Bioinformatics/BIOS15/BIOS15/butterflies.csv")

# Calculate the mean adult weight time for each combination
means = tapply(dat$AdultWeight, 
               list(dat$MaternalHost, dat$LarvalHost), 
               mean)

# Calculate the standard deviation adult weight for each combination
SD = tapply(dat$AdultWeight, 
            list(dat$MaternalHost, dat$LarvalHost), 
            sd)

# Calculate the sample size 
n = tapply(dat$AdultWeight, 
           list(dat$MaternalHost, dat$LarvalHost), 
           function(x) sum(!is.na(x)))

# Calculate the standard error adult weight for each combination
SE = SD/sqrt(n)

# Fit the two-way ANOVA model
m = lm(AdultWeight ~ MaternalHost * LarvalHost,
       data = dat)

# Create a data frame for plotting
plot_data = data.frame(
  MaternalHost = rep(rownames(means), times = ncol(means)),
  LarvalHost = rep(colnames(means), each = nrow(means)),
  means = as.vector(means),
  SE = as.vector(SE)
)

# Create the plot
library(ggplot2)

ggplot(plot_data,
       aes(x = LarvalHost,
           y = means,
           group = MaternalHost,
           shape = MaternalHost,
           fill = MaternalHost,
           linetype = MaternalHost)) +
  
  geom_line(color = "black") + 
  
  geom_errorbar(aes(ymin = means - SE,
                    ymax = means + SE),
                width = 0.05,
                color = "black") +
  
  scale_linetype_manual(values = c("dashed", "solid")) +
  
  geom_point(size = 3) + 
  
  scale_shape_manual(values = c(21, 21)) +
  
  scale_fill_manual(values = c("firebrick", "#87CEFA")) +
  
  labs(
    x = "Larval host",
    y = "Adult weight (mg)",
    shape = "Maternal Host",
    linetype = "Maternal Host") +
  
  scale_y_continuous(breaks = seq(50, 70, by = 5)) +
  
  coord_cartesian(ylim = c(50, 70)) +
  
  theme_bw() +
  
  theme(
    aspect.ratio = 1,
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(
      margin = margin(t = 12)),
    axis.title.y = element_text(
      margin = margin(r = 12)),
    legend.position ="inside",
    legend.position.inside = c(0.8, 0.80)) +
  
  guides(
    shape = guide_legend(
      override.aes = list(
        fill = c("firebrick", "#87CEFA"),
        linetype = 0
      )
    ),
    fill = "none"
  )

ggsave(filename = "weight_plot.png", 
       width = 12, 
       height = 12, 
       units = "cm", 
       dpi = 300)
