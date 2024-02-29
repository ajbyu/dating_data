library("tidyverse")

matches <- read_csv("https://www.dropbox.com/scl/fi/4pptj15s4czpv52dhcjej/10_train.csv?rlkey=iqjivkpptxavii3zghigcrlr0&dl=1")

matches %>% 
  select(gender, attractive, attractive_o, match) %>% 
  print(n = 50)

# Binary Matches
clean_matches <- matches %>% 
  mutate(match = parse_number(match))

# Graph to see distribution of yes and no matches
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

# Graph to see average distribution of match outcome to average differences of attributes
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

matches %>%
  select(relevant_cols) %>% 
  glimpse()

# Clean stupid crappy names
matches <- matches %>% 
  rename("sincere_o" = "sinsere_o") %>% 
  rename("ambitious_o" = "ambitous_o") %>% 
  mutate(gender = gsub("b'", "", gender)) %>%
  mutate(gender = gsub("'", "", gender))

# Clean values
matches <- matches %>% 
  mutate(attractive = if_else(attractive > 10, 10, attractive)) %>% 
  mutate(attractive_o = if_else(attractive_o > 10, 10, attractive_o)) %>% 
  mutate(sincere = if_else(sincere > 10, 10, sincere)) %>% 
  mutate(sincere_o = if_else(sincere_o > 10, 10, sincere_o)) %>% 
  mutate(intelligence = if_else(intelligence > 10, 10, intelligence)) %>% 
  mutate(intelligence_o = if_else(intelligence_o > 10, 10, intelligence_o)) %>% 
  mutate(funny = if_else(funny > 10, 10, funny)) %>% 
  mutate(funny_o = if_else(funny_o > 10, 10, funny_o)) %>% 
  mutate(ambition = if_else(ambition > 10, 10, ambition)) %>% 
  mutate(ambitious_o = if_else(ambitious_o > 10, 10, ambitious_o))

# List of relevant columns:
# attractive, attractive_o, sincere, sincere_o, intelligence, intelligence_o, funny, funny_o, ambition, ambitious_o
relevant_cols <- c("gender", "age", "field", "race", "attractive", "attractive_o", "sincere", "sincere_o", "intelligence", "intelligence_o", "funny", "funny_o", "ambition", "ambitious_o")
numeric_cols <- c("attractive", "attractive_o", "sincere", "sincere_o", "intelligence", "intelligence_o", "funny", "funny_o", "ambition", "ambitious_o")

# Round the values in the relevant columns to the nearest integer
matches <- matches %>%
  mutate_at(vars(numeric_cols), ~round(., digits = 0))

# Attractive_o = what the partner said about me
# Attractive = what I said about myself
# Attractive_partner = my rating of my partner

# Graphs:
# Difference of self-rating in gender
matches %>% 
  select(relevant_cols) %>% 
  ggplot(aes(x = attractive, fill = gender)) +
  scale_x_discrete(name = "Attractive: Self", limits = as.numeric(0:10)) +
  geom_bar(position = 'dodge')

matches %>% 
  select(relevant_cols) %>% 
  ggplot(aes(x = sincere, fill = gender)) +
  scale_x_discrete(name = "Sincere: Self", limits = as.numeric(0:10)) +
  geom_bar(position = 'dodge')

matches %>% 
  select(relevant_cols) %>% 
  ggplot(aes(x = intelligence, fill = gender)) +
  scale_x_discrete(name = "Intelligent: Self", limits = as.numeric(0:10)) +
  geom_bar(position = 'dodge')

matches %>% 
  select(relevant_cols) %>% 
  ggplot(aes(x = funny, fill = gender)) +
  scale_x_discrete(name = "Funny: Self", limits = as.numeric(0:10)) +
  geom_bar(position = 'dodge')

matches %>% 
  select(relevant_cols) %>% 
  ggplot(aes(x = ambition, fill = gender)) +
  scale_x_discrete(name = "Ambitious: Self", limits = as.numeric(0:10)) +
  geom_bar(position = 'dodge')

# Distribution of ratings of other
matches %>% 
  select(relevant_cols) %>% 
  ggplot(aes(x = attractive_o, fill = gender)) +
  scale_x_discrete(name = "Attractive: Other", limits = as.numeric(0:10)) +
  geom_bar(position = 'dodge')

matches %>% 
  select(relevant_cols) %>% 
  ggplot(aes(x = sincere_o, fill = gender)) +
  scale_x_discrete(name = "Sincere: Other", limits = as.numeric(0:10)) +
  geom_bar(position = 'dodge')

matches %>% 
  select(relevant_cols) %>% 
  ggplot(aes(x = intelligence_o, fill = gender)) +
  scale_x_discrete(name = "Intelligent: Other", limits = as.numeric(0:10)) +
  geom_bar(position = 'dodge')

matches %>% 
  select(relevant_cols) %>% 
  ggplot(aes(x = funny_o, fill = gender)) +
  scale_x_discrete(name = "Funny: Other", limits = as.numeric(0:10)) +
  geom_bar(position = 'dodge')

matches %>% 
  select(relevant_cols) %>% 
  ggplot(aes(x = ambitious_o, fill = gender)) +
  scale_x_discrete(name = "Ambitous: Other", limits = as.numeric(0:10)) +
  geom_bar(position = 'dodge')

# Combined!
matches %>%
  select(relevant_cols, gender) %>% 
  glimpse()

matches %>%
  select(numeric_cols, gender) %>%
  pivot_longer(cols = -gender, names_to = "trait", values_to = "rating") %>%
  ggplot(aes(x = factor(rating), fill = gender)) +
  scale_x_discrete(name = "", limits = as.character(0:10)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~ trait, scales = "free_x") +
  labs(x = "Rating", y = "Count")

# Self and rating from other overlaid
matches %>%
  select(numeric_cols) %>%
  pivot_longer(cols = everything(), names_to = "category", values_to = "value") %>%
  ggplot(aes(x = value, fill = category)) +
  scale_x_discrete(name = "Intelligent: Other", limits = as.numeric(0:10)) +
  geom_bar(position = 'dodge')


# Self rating by age
age_groups <- matches %>% 
  mutate(age_group = cut(age, breaks = c(18, 25, 35, 45, 55, Inf), labels = c("18-25", "26-35", "36-45", "46-55", "56+"), right = FALSE)) %>%
  select(relevant_cols, age_group)

age_groups %>%
  ggplot(aes(x = attractive, fill = gender)) +
  scale_x_discrete(name = "Attractive by Age: Self", limits = as.character(0:10)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~ age_group, scales = "free") +
  labs(y = "Count")

age_groups %>%
  ggplot(aes(x = sincere, fill = gender)) +
  scale_x_discrete(name = "Sincerity by Age: Self", limits = as.character(0:10)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~ age_group, scales = "free") +
  labs(y = "Count")

age_groups %>%
  ggplot(aes(x = intelligence, fill = gender)) +
  scale_x_discrete(name = "Intelligence by Age: Self", limits = as.character(0:10)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~ age_group, scales = "free") +
  labs(y = "Count")

age_groups %>%
  ggplot(aes(x = funny, fill = gender)) +
  scale_x_discrete(name = "Funniness by Age: Self", limits = as.character(0:10)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~ age_group, scales = "free") +
  labs(y = "Count")

age_groups %>%
  ggplot(aes(x = ambition, fill = gender)) +
  scale_x_discrete(name = "Ambition by Age: Self", limits = as.character(0:10)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~ age_group, scales = "free") +
  labs(y = "Count")

# Rating of other by 
age_groups %>%
  ggplot(aes(x = attractive_o, fill = gender)) +
  scale_x_discrete(name = "Attractive by Age: Other", limits = as.character(0:10)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~ age_group, scales = "free") +
  labs(y = "Count")

age_groups %>%
  ggplot(aes(x = sincere_o, fill = gender)) +
  scale_x_discrete(name = "Sincerity by Age: Other", limits = as.character(0:10)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~ age_group, scales = "free") +
  labs(y = "Count")

age_groups %>%
  ggplot(aes(x = intelligence_o, fill = gender)) +
  scale_x_discrete(name = "Intelligence by Age: Other", limits = as.character(0:10)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~ age_group, scales = "free") +
  labs(y = "Count")

age_groups %>%
  ggplot(aes(x = funny_o, fill = gender)) +
  scale_x_discrete(name = "Funniness by Age: Other", limits = as.character(0:10)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~ age_group, scales = "free") +
  labs(y = "Count")

age_groups %>%
  ggplot(aes(x = ambitious_o, fill = gender)) +
  scale_x_discrete(name = "Ambition by Age: Other", limits = as.character(0:10)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~ age_group, scales = "free") +
  labs(y = "Count")

# These hopefully together
age_groups <- matches %>% 
  mutate(age_group = cut(age, breaks = c(18, 25, 35, 45, 55, Inf), labels = c("18-25", "26-35", "36-45", "46-55", "56+"), right = FALSE)) %>%
  select(relevant_cols, age_group)

age_groups %>%
  select(numeric_cols, age_group) %>% 
  pivot_longer(cols = -c(age_group), names_to = "trait", values_to = "rating") %>%
  ggplot(aes(x = factor(rating), fill = age_group)) +
  scale_x_discrete(name = "", limits = as.character(0:10)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~ trait, scales = "free_x") +
  labs(x = "Rating", y = "Count")


# Self and rating from other faceted by field
# Looks like most people rate others as average regardless of how they rate themselves
matches %>%
  ggplot(aes(x = factor(attractive), fill = factor(attractive_o))) +
  geom_bar(position = "dodge") +
  labs(x = "Attractive: Self", y = "Count", fill = "Attractive: Other")

matches %>%
  select(attractive, attractive_o) %>% 
  pivot_longer(cols = c(attractive, attractive_o), names_to = "Rating", values_to = "Value") %>%
  ggplot(aes(x = factor(Value))) +
  geom_bar(fill = "red", position = "dodge") +
  facet_wrap(~ Rating, scales = "free") +
  labs(x = "Rating", y = "Count") +
  theme_minimal()

matches %>%
  select(sincere, sincere_o) %>% 
  select(sincere, sincere_o) %>% 
  pivot_longer(cols = c(sincere, sincere_o), names_to = "Rating", values_to = "Value") %>%
  ggplot(aes(x = factor(Value))) +
  geom_bar(fill = "blue", position = "dodge") +
  facet_wrap(~ Rating, scales = "free") +
  labs(x = "Rating", y = "Count") +
  theme_minimal()

# Self and rating from other faceted by race
