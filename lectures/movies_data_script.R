

# Data obtained from https://datasets.imdbws.com/
# Munging here: https://rpubs.com/tjstarak/movie-assoc-rule-mining


file_path = "C:/Users/anhsmith/OneDrive - Massey University/Work/Teaching/161.324/"

movies <- data.table::fread(
  paste0(file_path,'title.basics.tsv/data.tsv'),
  quote = '',
  na.strings = '\\N',
  encoding = 'UTF-8',
  colClasses = c(
    startYear = 'double',
    runtimeMinutes = 'double')) |>
  filter(titleType == 'movie', startYear >= 2020, !isAdult, !is.na(genres)) |>
  select(genres)

write_csv(movies, file="data/movie_genres.csv")

movt <- movies |>
  pull(genres) |>
  strsplit(",\\s*") |>
  as("transactions")

summary(movt)
itemFrequencyPlot(movt,topN=27,horiz=T)

inspect(movt)

rules <- movt |>
  apriori(parameter=list(support=0.01, confidence=0.2))

rules |>
  sort(by="confidence") |>
  head(n = 20) |>
  inspect()

inspect(rules)

plot(rules,
     method = "paracoord",
     shading = "confidence")

plot(rules,
     method = "graph",
     engine = "htmlwidget")

####
#
# set.seed(456)
#
# princ <- data.table::fread(
#   paste0(file_path,'title.principals.tsv/data.tsv'),
#   na.strings = '\\N',
#   encoding = 'UTF-8',
#   quote = '')
#
# movies_princ <- princ |>
#   inner_join(select(movies, tconst, titleType, primaryTitle, startYear),
#              by = c('tconst'))
#
# rm(princ)
#
# #
# names_per_movie <- movies_princ |>
#   group_by(tconst) |>
#   summarise(n_names = n())
#
#
# movies <- movies %>%
#   inner_join(select(filter(names_per_movie, n_names == 10), tconst), by = c('tconst'))
#
#
# movies_per_name <- movies_princ %>%
#   group_by(nconst) %>%
#   summarise(n = n())
#
# names_universe <- movies_per_name %>%
#   filter(n > 2) %>%
#   select(nconst)
#
# movies_princ <- semi_join(movies_princ, names_universe, by = c('nconst')) %>%
#   semi_join(movies, by = c('tconst'))
#
# movies <- semi_join(movies, movies_princ, by = c('tconst'))
#
# names <- data.table::fread(
#   paste0(file_path,'name.basics.tsv/data.tsv'),
#   na.strings = '\\N',
#   quote = '',
#   encoding = 'UTF-8')
#
# movies_trans <- movies_princ %>%
#   select(tconst, nconst) %>%
#   left_join(select(movies, tconst, primaryTitle), by = c('tconst')) %>%
#   mutate(title = paste(tconst, str_trunc(primaryTitle, 15))) %>%
#   left_join(select(names, nconst, primaryName), by = c('nconst')) %>%
#   mutate(name = paste(substr(nconst, 8, 9), str_trunc(primaryName, 20)))
#
#
#
# # save(basics_sample, file='basics_sample.RData')
#
# # basics_sample %>%
# #   group_by(titleType) %>%
# #   summarise(n = n()) %>%
# #   mutate(freq = n/sum(n)) %>%
# #   ggplot(aes(x = fct_reorder(titleType, freq),
# #              y = freq
# #   )) +
# #   geom_col(col='black', fill='coral') +
# #   coord_flip() +
# #   geom_text(aes(label = paste0(round(freq*100,2),'%')), nudge_y = 0.05) +
# #   scale_y_continuous(expand = expansion(mult = c(0,0.3))) +
# #   labs(
# #     title = 'Title type proportion',
# #     x = 'Title type',
# #     y = 'Count'
# #   ) +
# #   theme_bw() +
# #   theme(axis.title.x = element_blank(),
# #         axis.text.x = element_blank(),
# #         axis.ticks.x = element_blank(),
# #         panel.grid.major = element_blank(),
# #         panel.grid.minor = element_blank())
#
#
