# loading 1st dataset (number of language known)
lang <- read.csv("/Users/sobil/Downloads/Stats_Data_sets/xxx/edat_aes_l21/edat_aes_l21_1_Data.csv", stringsAsFactors = FALSE, na.strings = ":")
str(lang)
lang <- lang[,- c(3,4,5)]
lang$N_LANG <- as.factor(lang$N_LANG)

lang.list <- split(lang , lang$N_LANG)

removeColumn <- function(df) {
  df <- df[,- c(1,2)]
  return(df)
}
names <- lang.list$`1 language`$GEO
lang.list <- lapply(lang.list, removeColumn)

lang.dist <- data.frame(`No languages` = lang.list$`No languages`, `One language` = lang.list$`1 language`, `Two language` =lang.list$`2 languages`, `Three language or more` = lang.list$`3 languages or more`)
row.names(lang.dist) <- names
lang.dist = lang.dist[order(rownames(lang.dist)),]
lang.dist$GEO <- row.names(lang.dist)


skills <- read.csv("/Users/sobil/Downloads/Stats_Data_sets/xxx/isoc_sk_dskl_i/isoc_sk_dskl_i_1_Data.csv", stringsAsFactors = FALSE, na.strings = ":")
str(skills)
row.names(skills) <- skills$GEO
skills <- skills[,- c(1,3,4,5)]
skills = skills[order(rownames(skills)),]

merged <- merge(lang.dist, skills, by.x="GEO", by.y="GEO")
merged <- merged[- c(34),]
str(merged)

library(psych)
psych::pairs.panels(merged)


