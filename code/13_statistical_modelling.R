library(tidyverse)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(broom)
library(tidyverse)
library(dotwhisker)
library(gapminder)

# Hypotheses ----


df_analysis <- bind_rows(read_csv("data/paper/tw_df_mps.csv") |> mutate(data = "Twitter Dataset"), 
                         read_csv("data/paper/fb_df_mps.csv") |> mutate(data = "Facebook Dataset"))
df_analysis$elected_via <- factor(df_analysis$elected_via, levels = c("Direct","List"))
df_analysis <- df_analysis |> mutate(incumbent = case_when(
  incumbent == 1 ~ "Incumbent",
  incumbent == 0 ~ "No Incumbent"
))

# Identify numeric and categorical columns
numeric_cols <- c("area_district", "list_no", "birthyear")
categorical_cols <- c("list_state", "gender","incumbent")

# Standardize numeric predictors
df_analysis <- df_analysis %>%
  mutate(across(all_of(numeric_cols), ~ scale(.) %>% as.numeric())) %>%
  mutate(across(all_of(categorical_cols), as.factor))

df_analysis <- df_analysis %>%
  mutate(across(all_of(numeric_cols), ~ scale(.) %>% as.numeric())) %>%
  mutate(across(all_of(categorical_cols), as.factor))


## H1: Direct Candidates are more likely to mention places within their constituency ----

#FB: without controls
tw_mod_10 <-lm(mean_share_mp ~ elected_via, df_analysis |> filter(data == "Twitter Dataset"))
#FB: with controls
tw_mod_11 <-lm(mean_share_mp ~ elected_via + incumbent+area_district+list_no+ gender+birthyear, df_analysis |> filter(data == "Twitter Dataset"))

#FB: without controls
fb_mod_10 <-lm(mean_share_mp ~ elected_via, df_analysis |> filter(data == "Facebook Dataset"))
#FB: with controls
fb_mod_11 <-lm(mean_share_mp ~ elected_via+incumbent+area_district +list_no+ gender+birthyear, df_analysis |> filter(data == "Facebook Dataset"))

## H2: Direct Candidates are more likely to mention places closer to their constituency ----

#FB: without controls
tw_mod_20 <-lm(mean_distance ~ elected_via, df_analysis |> filter(data == "Twitter Dataset"))
#FB: with controls
tw_mod_21 <-lm(mean_distance ~ elected_via + incumbent+area_district +list_no+ gender+birthyear, df_analysis |> filter(data == "Twitter Dataset"))

#FB: without controls
fb_mod_20 <-lm(mean_distance ~ elected_via, df_analysis |> filter(data == "Facebook Dataset"))
#FB: with controls
fb_mod_21 <-lm(mean_distance ~ elected_via+incumbent+area_district +list_no+ gender+birthyear, df_analysis |> filter(data == "Facebook Dataset"))

#H1
summary(tw_mod_10)
summary(tw_mod_11)
summary(fb_mod_10)
summary(fb_mod_11)
#H2
summary(tw_mod_20)
summary(tw_mod_21)
summary(fb_mod_20)
summary(fb_mod_21)

terms <- c("elected_viaList", "incumbent","list_no","gender","birthyear","area_district" ,"incumbentNo Incumbent")

tidy_model1 <- tidy(tw_mod_11)|> filter(term %in% terms)|> mutate(model = "Twitter Dataset", hypothesis = "H1: District Match")
tidy_model2 <- tidy(fb_mod_11)|> filter(term %in% terms)|> mutate(model = "Facebook Dataset",hypothesis = "H1: District Match")
tidy_model3 <- tidy(tw_mod_21)|> filter(term %in% terms)|> mutate(model = "Twitter Dataset",hypothesis = "H2: District Distance")
tidy_model4 <- tidy(fb_mod_21)|> filter(term %in% terms)|> mutate(model = "Facebook Dataset",hypothesis = "H2: District Distance")


tidy_models <- rbind(tidy_model1,tidy_model2,tidy_model3,tidy_model4) |> 
  mutate(term = case_when(
    term == "elected_viaList" ~ "Elected (List)",
    term == "list_no" ~ "List Number",
    term == "birthyear" ~ "Age",
    term == "incumbent" ~ "Incumbent",
    term == "area_district" ~ "District Size",
    term == "incumbentNo Incumbent" ~ "Incumbent (No)"
  ) )


dwplot(tidy_models, 
       dot_args = list(aes(shape= model, colour = model),size = 4), 
       whisker_args = list(size = 1)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~hypothesis,scales = "free_x")+
  theme_bw() + 
  labs(       x = "Estimate",
       y = "Terms")+
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5)+
  scale_shape_discrete(name  ="Data", breaks = c(0, 1)) + 
  scale_colour_grey(start = .1, end = .4, name = "Models")

ggsave("output/coefficients.png", dpi = 1200, width = 7, height = 5)
  


#### Predicted Values

tidy_model1 <- tidy(tw_mod_11)|> filter(term %in% terms)|> mutate(model = "Twitter Dataset", hypothesis = "H1: District Match")
tidy_model2 <- tidy(fb_mod_11)|> filter(term %in% terms)|> mutate(model = "Facebook Dataset",hypothesis = "H1: District Match")
tidy_model3 <- tidy(tw_mod_21)|> filter(term %in% terms)|> mutate(model = "Twitter Dataset",hypothesis = "H2: District Distance")
tidy_model4 <- tidy(fb_mod_21)|> filter(term %in% terms)|> mutate(model = "Facebook Dataset",hypothesis = "H2: District Distance")

#H1
plot_data_mod1_1 <- ggeffects::ggpredict(tw_mod_11, terms = c("elected_via")) |> as_tibble() |> mutate(model = "Twitter Dataset", hypothesis = "H1: District Match")
plot_data_mod1_2 <- ggeffects::ggpredict(fb_mod_11, terms = c("elected_via")) |> as_tibble() |> mutate(model = "Facebook Dataset",hypothesis = "H1: District Match")
plot_data_mod2_1 <- ggeffects::ggpredict(tw_mod_21, terms = c("elected_via")) |> as_tibble() |> mutate(model = "Twitter Dataset",hypothesis = "H2: District Distance")
plot_data_mod2_2 <- ggeffects::ggpredict(fb_mod_22, terms = c("elected_via")) |> as_tibble() |> mutate(model = "Facebook Dataset",hypothesis = "H2: District Distance")

#H2

#H1
plot_data_mod1_1$x <- factor(plot_data_mod1_1$x, levels = c("Direct", "List"))
plot_data_mod1_2$x <- factor(plot_data_mod1_2$x, levels = c("Direct", "List"))
plot_data_mod2_1$x <- factor(plot_data_mod2_1$x, levels = c("Direct", "List"))
plot_data_mod2_2$x <- factor(plot_data_mod2_2$x, levels = c("Direct", "List"))

##################################################################################################


# Combine data
plot_data <- bind_rows(plot_data_mod1_1, plot_data_mod1_2, plot_data_mod2_1, plot_data_mod2_2)

# Factor the x variable for consistency
plot_data$x <- factor(plot_data$x, levels = c("Direct", "List"))

common_color_scale <- scale_color_manual(name = "Data", values = c("Facebook Dataset" = "#2066a8", "Twitter Dataset" = "#ae282c"))
common_shape_scale <- scale_shape_manual(name = "Data", values = c("Facebook Dataset" = 16, "Twitter Dataset" = 17))

# H1 Plot
plot_h1 <- ggplot(plot_data %>% filter(hypothesis == "H1: District Match"), aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, color = model, shape = model)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.5)) +
  labs(title = "H1: District Match", y = "Share") +
  theme_bw() +
  theme(legend.position = "bottom") +
  common_color_scale +
  common_shape_scale +
  rremove("x.title")

# H2 Plot
plot_h2 <- ggplot(plot_data %>% filter(hypothesis == "H2: District Distance"), aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, color = model, shape = model)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.5)) +
  labs(title = "H2: District Distance", y = "Distance") +
  theme_bw() +
  theme(legend.position = "bottom") +
  common_color_scale +
  common_shape_scale +
  rremove("x.title")

# Arrange plots with a common legend
figure <- ggpubr::ggarrange(plot_h1, plot_h2, align = "v", labels = NULL, common.legend = TRUE, legend = "bottom")

# Annotate the figure
annotate_figure(
  figure,
  bottom = text_grob("Mandate Mode", size = 15),
  fig.lab.size = 24
)


ggsave("output/predicted_values.png", dpi = 1200, width = 7, height = 5)


