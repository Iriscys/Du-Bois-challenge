library(ggpubr)
library(here)
library(png)
library(scales)
library(tidyverse)

## create dataset from original work
df <- data.frame(
  class = c("$100-200", "$200-300", "$300-400", "$400-500", "$500-750", "$750-1000", "$1000 AND OVER"),
  avr_income = c(139.1, 249.45, 335.66, 433.82, 547, 880, 1125),
  rent = c(19, 22, 23, 18, 13, 0, 0),
  food = c(43, 47, 43, 37, 31, 37, 29),
  clothes = c(28, 23, 18, 15, 17, 19, 16),
  tax = c(0.1, 4, 4.5, 5.5, 5, 8, 4.5),
  other = c(9.9, 4, 11.5, 24.5, 34, 36, 50.5)
  )

df_plot <- df |>
  pivot_longer( 
    cols = c("rent", "food", "clothes", "tax", "other"), 
    names_to = "type",
    values_to = "pct") |>
  mutate( 
    type = fct_relevel(type, c("rent", "food", "clothes", "tax", "other")),
    class = fct_relevel(class, c("$1000 AND OVER", "$750-1000", "$500-750",
                                 "$400-500", "$300-400", "$200-300", "$100-200"))
  ) |>
  filter(pct != 0)

## df for legend above the bar chart
df_legend_col <- data.frame(
  x = rep.int(8, 5),
  y = seq(0, 80, 20),
  xend = rep.int(8, 5),
  yend = seq(20, 100, 20),
  color = c("#cab8a6", "#9f9a97", "#e39d90", "#896687", "#0d110a")
)

df_legend_text <- data.frame(
  x = rep.int(8.3, 5),
  y = seq(10, 90, 20),
  type = c("OTHER", "TAX", "CLOTHES", "FOOD", "RENT")
)

## df for table on the left
# df for outlines
df_table <- data.frame( 
  x = c(0.5, 0.5, 0.5, 7.8),
  y = c(105, 120, 135, 105),
  xend = rep.int(7.8, 4),
  yend = c(105, 120, 135, 135)
)

df_table_arrow <- data.frame( # horizontal arrows
  x = seq(7.5, 0.5, -1),
  y = rep.int(135, 8),
  xend = seq(7.5, 0.5, -1),
  yend = rep.int(95, 8)
)

# df for text
df_table_text <- data.frame(
  x = seq(1, 7, 1),
  y = rep.int(127.5, 7),
  label = c("$1000\nAND OVER", "$750-1000", "$500-750",
            "$400-500", "$300-400", "$200-300", "$100-200")
)

df_table_avg <- data.frame(
  x = seq(1, 7, 1),
  y = rep.int(112.5, 7),
  label = sapply(df$avr_income,
                 function(x) paste("$", x, sep = ""))
)
df_table_title <- data.frame(
  x = c(7.65, 7.65),
  y = c(127.5, 112.5),
  label = c("CLASS", "ACTUAL AVERAGE")
)

## df for text on the right y axis
df_right_text <- data.frame(
  x = c(1, 2.5, 4.5, 6.5),
  y = rep.int(-4, 4),
  label = c("WELL-TO-DO", "CONFORTABLE", "FAR", "POOR")
)

## import and lad fonts
sysfonts::font_add_google(name = "Delius", 
                            family = "Delius") 
showtext::showtext_auto() 

## import the image for background
img.file <- here("images", "parchment_paper.png")
img <- png::readPNG(img.file)

## create plot
## first plot the bar chart
ggplot(df_plot, aes(x = class, y = pct, fill = type)) +
  background_image(img) + 
  geom_bar(stat = "identity",
           width = 0.5) +
  coord_flip() + 
  scale_y_reverse() + 
  scale_fill_manual(values = c("#0d110a", "#896687", "#e39d90", 
                               "#9f9a97", "#cab8a6")) +
  labs(caption = "FOR FURTHER STATISTICS RAISE THIS FRAME.") + 
  theme_void(base_family = "Delius") + 
  theme(
    legend.position = "none",
    axis.title = element_blank(), 
    axis.text = element_blank(), 
    plot.caption = element_text(hjust = 0.5, 
                                vjust = 9, 
                                color = "#2F4F4F",
                                size = 7)
  ) +
  ## add details
  # add % within bar 
  geom_text(
    aes(label = ifelse(type == "rent", paste0(pct, "%"), "")), 
    position = position_stack(vjust = 0.5), 
    color = "white",
    size = 2.5) +
  geom_text(
    aes(label = ifelse(type != "rent" & pct > 0.1, paste0(pct, "%"), "")), 
    position = position_stack(vjust = 0.5), 
    color = "black",
    size = 2.5) +
  # add legend above the bar
  geom_segment(
    data = df_legend_col,
    aes(x = x, y = y, xend = xend, yend = yend, color = color),
    inherit.aes = FALSE,
    color = c("#cab8a6", "#9f9a97", "#e39d90", "#896687", "#0d110a"),
    size = 5
  ) + 
  geom_text(
    data = df_legend_text,
    aes(x = x, y = y, label = type),
    inherit.aes = FALSE,
    color = "#2F4F4F",
    size = 3) +
  expand_limits(x= c(0, 8.5)) +
  # add left table outline
  geom_segment(
    data = df_table,
    aes(x = x, y = y, xend = xend, yend = yend),
    inherit.aes = FALSE,
    color = "#2F4F4F",
    size = 0.3
  ) +
  geom_segment(
    data = df_table_arrow,
    aes(x = x, y = y, xend = xend, yend = yend),
    inherit.aes = FALSE,
    color = "#2F4F4F",
    size = 0.3, 
    arrow = arrow(length = unit(0.2, "cm"))) +
  # add left table text
  geom_text(
    data = df_table_text,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    color = "#2F4F4F",
    size = 3,
    hjust = 0.5
  ) +
  geom_text(
    data = df_table_avg,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    color = "#2F4F4F",
    size = 3,
    hjust = 0.5
  ) +
  geom_text(
    data = df_table_title,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    color = "#2F4F4F",
    size = 1.8,
    hjust = 0.5
  ) +
  # add the text on the right y-axis
  geom_text(
    data = df_right_text,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    color = "#2F4F4F",
    size = 1.8,
    hjust = 0.5,
    angle = 90
  ) 
