library(dplyr)

exper_data <- read.csv("../data/treatment_metrics.csv", header=TRUE, sep=",")
contr_data <- read.csv("../data/control_metrics.csv", header=TRUE, sep=",")

exper_data['isGeneratedByAgent'] <- TRUE
contr_data['isGeneratedByAgent'] <- FALSE
data <- rbind(exper_data, contr_data)
data$isMerged <- data$state == "MERGED"
model_data <- data.frame(data$reviewTime, data$reviewersTotalCount, data$reviewersComments, data$authorComments,
                         data$commentsTotalCount, data$additions, data$deletions, data$prSize,
                         data$commitsTotalCount, data$changedFiles, data$prExperience, data$isGeneratedByAgent, data$bodyLength, 
                         data$purpose,data$repoLanguage,data$forkCount,data$stargazerCount,data$repoAge,data$isMember,data$isMerged)
names(model_data)[1] <- "reviewTime"
names(model_data)[2] <- "reviewersTotalCount"
names(model_data)[3] <- "reviewersComments"
names(model_data)[4] <- "authorComments"
names(model_data)[5] <- "commentsTotalCount"
names(model_data)[6] <- "additions"
names(model_data)[7] <- "deletions"
names(model_data)[8] <- "prSize"
names(model_data)[9] <- "commitsTotalCount"
names(model_data)[10] <- "changedFiles"
names(model_data)[11] <- "prExperience"
names(model_data)[12] <- "isGeneratedByAgent"
names(model_data)[13] <- "bodyLength"
names(model_data)[14] <- "purpose"
names(model_data)[15] <- "repoLanguage"
names(model_data)[16] <- "forkCount"
names(model_data)[17] <- "stargazerCount"
names(model_data)[18] <- "repoAge"
names(model_data)[19] <- "isMember"
names(model_data)[20] <- "isMerged"
model_data$isMember <- as.integer(as.logical(model_data$isMember))
model_data$reviewTime <- as.numeric(model_data$reviewTime)

value_counts <- table(model_data[["repoLanguage"]])

top_values <- names(value_counts)[order(value_counts, decreasing = TRUE)][1:10]

model_data$repoLanguage <- ifelse(!(model_data[["repoLanguage"]] %in% top_values), "Other", model_data[["repoLanguage"]])

library("tidyverse")
library("broom")
library("magrittr")
library("WeightIt")
library("cobalt")
W.out <- weightit(isGeneratedByAgent ~ reviewersTotalCount + reviewersComments + authorComments + commentsTotalCount   +
                    additions + deletions + prSize + commitsTotalCount + changedFiles + prExperience +
                    bodyLength + purpose + repoLanguage + forkCount + stargazerCount + repoAge + isMember ,
                  data = model_data, estimand = "ATT", method = "ebal")
summary(W.out)
bal.tab(W.out,threshold=0.1)
plot(bal.tab(W.out,threshold=0.1, un = TRUE), abs = TRUE)

library("marginaleffects")
fit <- glm( isMerged ~ isGeneratedByAgent  +
       reviewersTotalCount + reviewersComments + authorComments + commentsTotalCount   +
       additions + deletions + prSize + commitsTotalCount + changedFiles + prExperience +
       bodyLength + purpose + repoLanguage + forkCount + stargazerCount + repoAge + isMember ,
     data = model_data, weights = W.out$weights) 

# Section 5.2 RQ2: How are the code reviews affected by the use of Copilot for PRs? - (RQ2.2)
avg_comparisons(fit,
                  variables = "isGeneratedByAgent",
                  vcov = "HC3",
                  wts = W.out$weights,
                  comparison = "lnoravg",
                  transform = "exp")

PSW_result <- model_data %>%
  lm(isMerged ~ isGeneratedByAgent +  
       reviewersTotalCount + reviewersComments + authorComments + commentsTotalCount   +
       additions + deletions + prSize + commitsTotalCount + changedFiles + prExperience +
       bodyLength + purpose + repoLanguage + forkCount + stargazerCount + repoAge + isMember ,
     data = ., weights = W.out$weights) %>%
  tidy()

print(n = 38,PSW_result)