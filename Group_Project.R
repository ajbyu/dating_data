library("tidyverse")

matches <- read_csv("https://www.dropbox.com/scl/fi/4pptj15s4czpv52dhcjej/10_train.csv?rlkey=iqjivkpptxavii3zghigcrlr0&dl=1")

matches %>% 
  filter(is.na(has_null)) %>% 
  count(has_null)

matches %>% 
  distinct(race)

clean_matches <- matches %>% 
  mutate(match = parse_number(match))

matches_visual <- matches %>% 
  mutate(match = as.factor(parse_number(match))) %>%
  count(match) %>%
  mutate(percentage = n / sum(n) * 100) %>% 
  ggplot(aes(x = match, y = n, fill = match)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 3.5) +
  labs(x = "Match", y = "Decision Count", title = "Distribution of Matches") +
  scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) +  # Change labels for x-axis
  scale_fill_manual(values=c("0" = "red", "1" = "green"), labels=c("0" = "No", "1" = "Yes")) +  # Optional: Adjust fill legends
  theme_minimal()

average_attributes <- clean_matches %>%
  mutate(match = factor(match, levels = c(0, 1), labels = c("No", "Yes")),
         attractive_diff = attractive - attractive_o,
         intelligence_diff = intelligence - intelligence_o,
         sincere_diff = sincere - sinsere_o,  # Corrected typo
         funny_diff = funny - funny_o,
         ambition_diff = ambition - ambitous_o) %>%  # Corrected typo
  gather(key = "attribute", value = "difference", attractive_diff, intelligence_diff, sincere_diff, funny_diff, ambition_diff) %>%
  group_by(match, attribute) %>%
  summarise(average_difference = mean(difference, na.rm = TRUE), .groups = 'drop') %>%
  ungroup() %>%
  ggplot(aes(x = attribute, y = average_difference, fill = match)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("No" = "red", "Yes" = "green")) +
  labs(title = "Average Attribute Differences by Match Outcome",
       x = "Attribute",
       y = "Average Difference (Self - Partner)",
       fill = "Match Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

