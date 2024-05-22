# latent profile analysis of newham data

library(mclust)

interests_clustering <- interests_clean %>%
  na.omit() %>%
  mutate_all(list(scale))

BIC <- mclustBIC(interests_clustering)

plot(BIC)

summary(BIC)

mod1 <- Mclust(interests_clustering, modelNames = "VEE", G = 3, x = BIC)

summary(mod1)

ICL <- mclustICL(interests_clustering)

plot(ICL)

summary(ICL)

mclustBootstrapLRT(interests_clustering, modelName = "VEE")

## visualise the clusters

means <- data.frame(mod1$parameters$mean) %>%
  rownames_to_column() %>%
  rename(Interest = rowname) %>%
  pivot_longer(cols = c(X1, X2, X3), names_to = "Profile", values_to = "Mean") %>%
  mutate(Mean = round(Mean, 2),
         Mean = ifelse(Mean > 1, 1, Mean))

p <- means %>%
  mutate(Profile = recode(Profile,
                          X1 = "Science: 16%",
                          X2 = "Disinterest: 60%",
                          X3 = "Arts & Humanities: 24%")) %>%
  ggplot(aes(Interest, Mean, group = Profile, color = Profile)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  scale_x_discrete(limits = c("Active sport", "Adrenaline sports", "Passive sport",
                              "Countryside, outdoors", "Gardening", "Cars",
                              "Art exhibitions", "Dancing", "Musical instruments", "Theatre", "Writing", "Reading",
                              "Geography", "History", "Law", "Politics", "Psychology", "Religion", "Foreign languages",
                              "Biology", "Chemistry", "Mathematics", "Medicine", "Physics", "Science and technology",
                              "Internet", "PC",
                              "Celebrities", "Economy Management", "Fun with friends", "Shopping", "Pets")) +
  labs(x = NULL, y = "Standardized mean interest") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
