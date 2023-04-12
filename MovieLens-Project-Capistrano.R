##########################################################
# MovieLens Project of Francis "Kapi" Capistrano
##########################################################

### 1. Set Up

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(knitr)
library(gridExtra)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

# Before joining the ratings and movies data sets, I first tried to:
## 1) modify the "movies" data set by (a) creating a new variable "movieyear" (b) turning "genres" into a series of logical variables

movies <- movies %>% 
  mutate(movieyear = str_extract(title,"\\([1|2][0|9][0-9][0-9]\\)")) %>% 
  mutate(movieyear = str_extract(movieyear, "[0-9][0-9][0-9][0-9]")) %>% 
  mutate(moviedecade = ifelse(as.integer(movieyear)<1920,"1910s",
                        ifelse(as.integer(movieyear)<1930,"1920s",
                        ifelse(as.integer(movieyear)<1940,"1930s",
                        ifelse(as.integer(movieyear)<1950,"1940s",
                        ifelse(as.integer(movieyear)<1960,"1950s",
                        ifelse(as.integer(movieyear)<1970,"1960s",
                        ifelse(as.integer(movieyear)<1980,"1970s",
                        ifelse(as.integer(movieyear)<1990,"1980s",
                        ifelse(as.integer(movieyear)<2000,"1990s","2000s"))))))))))

genrelist <- movies %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(genres)
genrelist <- genrelist$genres

movies <- movies %>% mutate(nogenres = str_detect(genres,genrelist[1]),
                            action = str_detect(genres,genrelist[2]),
                            adventure = str_detect(genres,genrelist[3]),
                            animation = str_detect(genres,genrelist[4]),
                            children = str_detect(genres,genrelist[5]),
                            comedy = str_detect(genres,genrelist[6]),
                            crime = str_detect(genres,genrelist[7]),
                            documentary = str_detect(genres,genrelist[8]),
                            drama = str_detect(genres,genrelist[9]),
                            fantasy = str_detect(genres,genrelist[10]),
                            filmnoir = str_detect(genres,genrelist[11]),
                            horror = str_detect(genres,genrelist[12]),
                            imax = str_detect(genres,genrelist[13]),
                            musical = str_detect(genres,genrelist[14]),
                            mystery = str_detect(genres,genrelist[15]),
                            romance = str_detect(genres,genrelist[16]),
                            scifi = str_detect(genres,genrelist[17]),
                            thriller = str_detect(genres,genrelist[18]),
                            war = str_detect(genres,genrelist[19]),
                            western = str_detect(genres,genrelist[20]))

## 2) modify the "ratings" data set by adding a "ratingdate" variable 
ratings <- ratings %>% mutate(ratingdate = as.Date.POSIXct(timestamp))

#Time to join!
movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

# Kept some of the data frames for use later
rm(dl, test_index, temp, removed, movies_file, ratings_file)


### 2. Exploratory Analyses

## Basic Descriptives: Movie Ratings

descriptives_movielens <- movielens %>% summarize(title = "Full MovieLens 10M Data Set",
                                     NoMovies = n_distinct(movieId), 
                                     NoRaters = n_distinct(userId),
                                     RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                     AveRating = round(mean(rating),3),
                                     SDRating = round(sd(rating),3),
                                     AveRatingDate = mean(ratingdate))
descriptives_train <- edx %>% summarize(title = "EDX Training Data Set",
                                  NoMovies = n_distinct(movieId), 
                                  NoRaters = n_distinct(userId),
                                  RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                  AveRating = round(mean(rating),3),
                                  SDRating = round(sd(rating),3),
                                  AveRatingDate = mean(ratingdate))
descriptives_holdout <- final_holdout_test %>% summarize(title = "Final Holdout Test Set", 
                                                         NoMovies = n_distinct(movieId), 
                                                         NoRaters = n_distinct(userId),
                                                         RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                         AveRating = round(mean(rating),3),
                                                         SDRating = round(sd(rating),3),
                                                         AveRatingDate = mean(ratingdate))
descriptives <- bind_rows(descriptives_movielens, descriptives_train, descriptives_holdout) 
rm(descriptives_movielens, descriptives_train, descriptives_holdout)
kable(t(descriptives[2:7]), format = "rst", align = "c", col.names = c("Full MovieLens 10M Data Set", "EDX Training Data Set", "Final Holdout Test Set"))

edx_hist <- edx %>% ggplot(aes(rating)) + geom_histogram(binwidth = 0.5, fill = "navy") + labs(title = "Distribution of Ratings",x = "Training Set")
holdout_hist <- final_holdout_test %>% ggplot(aes(rating)) + geom_histogram(binwidth = 0.5, fill = "orange3") + labs(x = "Test Set", y="", title = "")
grid.arrange(edx_hist, holdout_hist, ncol = 2)
rm(edx_hist, holdout_hist)

## Exploring Movie and User Effects on Ratings

#Movie Effects
edx %>% group_by(movieId) %>% summarize(meanRating = mean(rating)) %>% 
  ggplot(aes(meanRating)) + geom_histogram(binwidth = 0.25, fill = "navy") + 
  labs(x = "Mean Rating", y = "", title = "Distribution of Mean Movie Ratings")
edx %>% group_by(moviedecade) %>% summarize(noMovies = n_distinct(movieId)) %>% 
  ggplot(aes(moviedecade, noMovies)) + geom_col(fill = "orange3") + 
  labs(x = "", y = "Number of Movies", title = "Number of Movies by Decade")
edx %>% group_by(movieId) %>% summarize(Action = ifelse(sum(action)>0,1,0),
                                        Adventure = ifelse(sum(adventure)>0,1,0),
                                        Animation = ifelse(sum(animation)>0,1,0),
                                        Children = ifelse(sum(children)>0,1,0),
                                        Comedy = ifelse(sum(comedy)>0,1,0),
                                        Crime = ifelse(sum(crime)>0,1,0),
                                        Documentary = ifelse(sum(documentary)>0,1,0),
                                        Drama = ifelse(sum(drama)>0,1,0),
                                        Fantasy = ifelse(sum(fantasy)>0,1,0),
                                        FilmNoir = ifelse(sum(filmnoir)>0,1,0),
                                        Horror = ifelse(sum(horror)>0,1,0),
                                        IMAX = ifelse(sum(imax)>0,1,0),
                                        Musical = ifelse(sum(musical)>0,1,0),
                                        Mystery = ifelse(sum(mystery)>0,1,0),
                                        Romance = ifelse(sum(romance)>0,1,0),
                                        SciFi = ifelse(sum(scifi)>0,1,0),
                                        Thriller = ifelse(sum(thriller)>0,1,0),
                                        War = ifelse(sum(war)>0,1,0),
                                        Western = ifelse(sum(western)>0,1,0)) %>% 
  summarize(Action = sum(Action), Adventure = sum(Adventure), Animation = sum(Animation),
            Children = sum(Children), Comedy = sum(Comedy), Crime = sum(Crime),
            Documentary = sum(Documentary), Drama = sum(Drama), Fantasy = sum(Fantasy),
            FilmNoir = sum(FilmNoir), Horror = sum(Horror), IMAX = sum(IMAX),
            Musical = sum(Musical), Mystery = sum(Mystery), Romance = sum(Romance),
            SciFi = sum(SciFi), Thriller = sum(Thriller), War = sum(War), Western = sum(Western)) %>% 
  t() %>% as.data.frame() %>% rownames_to_column(var = "Genre") %>% rename(noMovies = V1) %>% 
  ggplot(aes(noMovies, fct_reorder(Genre, noMovies))) + geom_col(fill="green3") + 
  labs(x = "Number of Movies", y = "Movie Genres", title = "Number of Movies per Genre")
  

#User Effects
edx %>% group_by(userId) %>% summarize(meanRating = mean(rating)) %>% 
  ggplot(aes(meanRating)) + geom_histogram(binwidth = 0.25, fill = "navy") + 
  labs(x = "Mean Rating", y = "", title = "Distribution of Mean Ratings by Users")
edx %>% group_by(moviedecade) %>% summarize(noUsers = n_distinct(userId)) %>% 
  ggplot(aes(moviedecade, noUsers)) + geom_col(fill = "orange3") + 
  labs(x = "", y = "Number of Users", title = "Number of Users by Decade")
edx %>% group_by(userId) %>% summarize(Action = ifelse(sum(action)>0,1,0),
                                        Adventure = ifelse(sum(adventure)>0,1,0),
                                        Animation = ifelse(sum(animation)>0,1,0),
                                        Children = ifelse(sum(children)>0,1,0),
                                        Comedy = ifelse(sum(comedy)>0,1,0),
                                        Crime = ifelse(sum(crime)>0,1,0),
                                        Documentary = ifelse(sum(documentary)>0,1,0),
                                        Drama = ifelse(sum(drama)>0,1,0),
                                        Fantasy = ifelse(sum(fantasy)>0,1,0),
                                        FilmNoir = ifelse(sum(filmnoir)>0,1,0),
                                        Horror = ifelse(sum(horror)>0,1,0),
                                        IMAX = ifelse(sum(imax)>0,1,0),
                                        Musical = ifelse(sum(musical)>0,1,0),
                                        Mystery = ifelse(sum(mystery)>0,1,0),
                                        Romance = ifelse(sum(romance)>0,1,0),
                                        SciFi = ifelse(sum(scifi)>0,1,0),
                                        Thriller = ifelse(sum(thriller)>0,1,0),
                                        War = ifelse(sum(war)>0,1,0),
                                        Western = ifelse(sum(western)>0,1,0)) %>% 
  summarize(Action = sum(Action), Adventure = sum(Adventure), Animation = sum(Animation),
            Children = sum(Children), Comedy = sum(Comedy), Crime = sum(Crime),
            Documentary = sum(Documentary), Drama = sum(Drama), Fantasy = sum(Fantasy),
            FilmNoir = sum(FilmNoir), Horror = sum(Horror), IMAX = sum(IMAX),
            Musical = sum(Musical), Mystery = sum(Mystery), Romance = sum(Romance),
            SciFi = sum(SciFi), Thriller = sum(Thriller), War = sum(War), Western = sum(Western)) %>% 
  t() %>% as.data.frame() %>% rownames_to_column(var = "Genre") %>% rename(noMovies = V1) %>% 
  ggplot(aes(noMovies, fct_reorder(Genre, noMovies))) + geom_col(fill="green3") + 
  labs(x = "Number of User Ratings", y = "Movie Genres", title = "Number of Users per Genre")

## Ratings per Year and Decade

edx %>% group_by(movieyear) %>% summarize(meanRating = mean(rating)) %>% 
  ggplot(aes(movieyear, meanRating)) + geom_point(color = "navy") + geom_smooth()

stats_decade <- edx %>% group_by(MovieDecade = moviedecade) %>% summarize(NoMovies = n_distinct(movieId), 
                                                            NoRaters = n_distinct(userId),
                                                            RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                            AveRating = round(mean(rating),3),
                                                            SDRating = round(sd(rating),3),
                                                            AveRatingDate = mean(ratingdate))
stats_decade %>% kable(format = "rst")

stats_decade %>% ggplot(aes(fct_reorder(MovieDecade, MovieDecade), AveRating, fill = MovieDecade)) +
  geom_col() + geom_text(aes(label = round(AveRating,2)), vjust = -0.2, size = 3) +
  labs(x = "Average Movie Rating per Decade", y = "") +
  coord_cartesian(ylim = c(0.1,4.1)) + 
  theme(legend.position = "none")

stats_decade %>% ggplot(aes(fct_reorder(MovieDecade, MovieDecade), AveRating, fill = MovieDecade)) +
  geom_col() + geom_text(aes(label = round(AveRating,2)), vjust = -0.2, size = 3) +
  labs(x = "Average Movie Rating per Decade", y = "") +
  coord_cartesian(ylim = c(0.1,4.1)) + 
  theme(legend.position = "none")

## Descriptive Statistics per Genre

stats_action <- edx %>% filter(action == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_adventure <- edx %>% filter(adventure == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_animation <- edx %>% filter(animation == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_children <- edx %>% filter(children == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_comedy <- edx %>% filter(comedy == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_crime <- edx %>% filter(crime == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_documentary <- edx %>% filter(documentary == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_drama <- edx %>% filter(drama == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_fantasy <- edx %>% filter(fantasy == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_filmnoir <- edx %>% filter(filmnoir == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_horror <- edx %>% filter(horror == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_imax <- edx %>% filter(imax == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_musical <- edx %>% filter(musical == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_mystery <- edx %>% filter(mystery == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_romance <- edx %>% filter(romance == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_scifi <- edx %>% filter(scifi == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_thriller <- edx %>% filter(thriller == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_war <- edx %>% filter(war == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))
stats_western <- edx %>% filter(western == TRUE) %>% summarize(NoMovies = n_distinct(movieId), 
                                                             AveReleaseYear = round(mean(as.integer(movieyear)),0),
                                                             NoRaters = n_distinct(userId),
                                                             RatersPerMovie = round(n_distinct(userId)/n_distinct(movieId), 0),
                                                             AveRating = mean(rating),
                                                             SDRating = sd(rating),
                                                             AveRatingDate = mean(ratingdate))

stats_genre <- bind_rows(stats_action, stats_adventure, stats_animation, stats_children, stats_comedy, 
                         stats_crime, stats_documentary, stats_drama,stats_fantasy, stats_filmnoir,
                         stats_horror, stats_imax, stats_musical, stats_mystery, stats_romance,
                         stats_scifi, stats_thriller, stats_war, stats_western)
stats_genre <- bind_cols(Genre = genrelist[2:20],stats_genre)
rm(stats_action, stats_adventure, stats_animation, stats_children, stats_comedy, stats_crime, 
   stats_documentary, stats_drama, stats_fantasy, stats_filmnoir, stats_horror, stats_imax, 
   stats_musical, stats_mystery, stats_romance, stats_scifi, stats_thriller, stats_war, stats_western)
stats_genre <- as_tibble(stats_genre)

stats_genre %>% arrange(desc(AveRating))

stats_genre %>% ggplot(aes(AveRating, fct_reorder(Genre, AveRating), fill = Genre)) + geom_col() +
  geom_text(aes(label = round(AveRating,2)), hjust = -0.2, size = 3) +
  labs(x = "Average Movie Rating per Genre", y = "") +
  coord_cartesian(xlim = c(0.1,4.1)) + 
  theme(legend.position = "none")

## Top Rated Movies 

# Top All Time (Only of Movies with 10 or more Raters)

stats_movie <- edx %>% group_by(movieId) %>% 
  summarize(AveRating = mean(rating), NoRaters = n_distinct(userId)) %>% 
  inner_join(movies)
  
top_all_time <- stats_movie[1:7] %>% filter(NoRaters >= 10) %>% arrange(desc(AveRating)) %>% head(top_all_time, n = 30) %>% select(-movieyear)

top_all_time %>% ggplot(aes(AveRating, fct_reorder(title, AveRating), fill = moviedecade)) + geom_col() +
  geom_text(aes(label = round(AveRating,2)), hjust = -0.5, size = 3) +
  labs(title = "Top 30 Movies 1910s-2000s", x = "", y = "") + 
  scale_y_discrete(labels = function(y) str_wrap(y, width = 40)) + 
  coord_cartesian(xlim = c(0.1,4.8))
  
# Top Rated Movies Per Decade
top_10s <- stats_movie[1:7] %>% filter(NoRaters >= 10 & moviedecade == "1910s") %>% arrange(desc(AveRating)) %>% head(n = 1)
top_20s <- stats_movie[1:7] %>% filter(NoRaters >= 10 & moviedecade == "1920s") %>% arrange(desc(AveRating)) %>% head(n = 1)
top_30s <- stats_movie[1:7] %>% filter(NoRaters >= 10 & moviedecade == "1930s") %>% arrange(desc(AveRating)) %>% head(n = 1)
top_40s <- stats_movie[1:7] %>% filter(NoRaters >= 10 & moviedecade == "1940s") %>% arrange(desc(AveRating)) %>% head(n = 1)
top_50s <- stats_movie[1:7] %>% filter(NoRaters >= 10 & moviedecade == "1950s") %>% arrange(desc(AveRating)) %>% head(n = 1)
top_60s <- stats_movie[1:7] %>% filter(NoRaters >= 10 & moviedecade == "1960s") %>% arrange(desc(AveRating)) %>% head(n = 1)
top_70s <- stats_movie[1:7] %>% filter(NoRaters >= 10 & moviedecade == "1970s") %>% arrange(desc(AveRating)) %>% head(n = 1)
top_80s <- stats_movie[1:7] %>% filter(NoRaters >= 10 & moviedecade == "1980s") %>% arrange(desc(AveRating)) %>% head(n = 1)
top_90s <- stats_movie[1:7] %>% filter(NoRaters >= 10 & moviedecade == "1990s") %>% arrange(desc(AveRating)) %>% head(n = 1)
top_00s <- stats_movie[1:7] %>% filter(NoRaters >= 10 & moviedecade == "2000s") %>% arrange(desc(AveRating)) %>% head(n = 1)

top_per_decade <- bind_rows(top_10s, top_20s, top_30s, top_40s, top_50s, 
                            top_60s, top_70s, top_80s, top_90s, top_00s)
rm(top_10s, top_20s, top_30s, top_40s, top_50s, 
   top_60s, top_70s, top_80s, top_90s, top_00s)

top_per_decade %>% arrange(desc(AveRating))

top_per_decade %>% ggplot(aes(AveRating, fct_reorder(title, moviedecade, .desc = TRUE), fill = moviedecade)) + geom_col() +
  geom_text(aes(label = round(AveRating,2)), hjust = -0.2, size = 3) +
  labs(x = "Top Rated Movies of Each Decade", y = "") + 
  coord_cartesian(xlim = c(0.1,4.8)) + 
  scale_y_discrete(labels = function(y) str_wrap(y, width = 40))

## Top Rated Movies per Genre

top_action <- stats_movie %>% filter(NoRaters >= 10 & action == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Action")
top_adventure <- stats_movie %>% filter(NoRaters >= 10 & adventure == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Adventure")
top_animation <- stats_movie %>% filter(NoRaters >= 10 & animation == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Animation")
top_children <- stats_movie %>% filter(NoRaters >= 10 & children == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Children")
top_comedy <- stats_movie %>% filter(NoRaters >= 10 & comedy == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Comedy")
top_crime <- stats_movie %>% filter(NoRaters >= 10 & crime == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Crime")
top_documentary <- stats_movie %>% filter(NoRaters >= 10 & documentary == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Documentary")
top_drama <- stats_movie %>% filter(NoRaters >= 10 & drama == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Drama")
top_fantasy <- stats_movie %>% filter(NoRaters >= 10 & fantasy == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Fantasy")
top_filmnoir <- stats_movie %>% filter(NoRaters >= 10 & filmnoir == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Film Noir")
top_horror <- stats_movie %>% filter(NoRaters >= 10 & horror == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Horror")
top_imax <- stats_movie %>% filter(NoRaters >= 10 & imax == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "IMAX")
top_musical <- stats_movie %>% filter(NoRaters >= 10 & musical == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Musical")
top_mystery <- stats_movie %>% filter(NoRaters >= 10 & mystery == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Mystery")
top_romance <- stats_movie %>% filter(NoRaters >= 10 & romance == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Romance")
top_scifi <- stats_movie %>% filter(NoRaters >= 10 & scifi == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Sci-Fi")
top_thriller <- stats_movie %>% filter(NoRaters >= 10 & thriller == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Thriller")
top_war <- stats_movie %>% filter(NoRaters >= 10 & war == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "War")
top_western <- stats_movie %>% filter(NoRaters >= 10 & western == TRUE) %>% arrange(desc(AveRating)) %>% head(n = 1) %>% mutate(genretop = "Western")

top_per_genre <- bind_rows(top_action, top_adventure, top_animation, top_children, top_comedy, 
                           top_crime, top_documentary, top_drama, top_fantasy, top_filmnoir, 
                           top_horror, top_imax, top_musical, top_mystery, top_romance, 
                           top_scifi, top_thriller, top_war, top_western) %>% mutate(genretitle = str_c(genretop, ": ", title))
rm(top_action, top_adventure, top_animation, top_children, top_comedy, 
   top_crime, top_documentary, top_drama, top_fantasy, top_filmnoir, 
   top_horror, top_imax, top_musical, top_mystery, top_romance, 
   top_scifi, top_thriller, top_war, top_western)

top_per_genre %>% arrange(desc(AveRating))

top_per_genre %>% ggplot(aes(AveRating, fct_reorder(genretitle, AveRating), fill = genretop)) + geom_col() +
  geom_text(aes(label = round(AveRating,2)), hjust = -0.2, size = 3) +
  labs(x = "Top Rated Movies per Genre", y = "") + 
  coord_cartesian(xlim = c(0.1,5)) + 
  scale_y_discrete(labels = function(y) str_wrap(y, width = 40))

### 3. Machine Learning Analyses

## repartition edx data set between a training and testing set
## since the instruction was to save the final_holdout_test set for the final testing

test_index <- createDataPartition(edx$rating, p = 0.1, list = FALSE)

edx_train <- edx[test_index,]
edx_test <- edx[-test_index,]

RMSEs <- tibble()

meanRating <- mean(edx_train$rating)
baselineRMSE <- RMSE(meanRating,edx_test$rating)
RMSEs <- tibble(Model = "Baseline Using Mean Rating", RMSE = baselineRMSE)

kable(RMSEs, digits = 4)

## 3.1 Movie & User Effects

# Movie Effects

movieMeanRatings <- edx_train %>% group_by(movieId) %>% 
  summarize(AveRating = mean(rating)) %>% 
  mutate(movieEffect = AveRating - meanRating)

edx_test <- edx_test %>% 
  left_join(movieMeanRatings) %>% 
  mutate(movieEffect = ifelse(is.na(movieEffect), 0, movieEffect)) %>% 
  mutate(movieEffectPred = movieEffect + meanRating)

movieEffectsModelRMSE <- RMSE(edx_test$movieEffectPred, edx_test$rating)
RMSEs <- bind_rows(RMSEs,tibble(Model = "Movie Effects Model", RMSE = movieEffectsModelRMSE))

# User Effects

userMeanRatings <- edx_train %>% 
  left_join(movieMeanRatings) %>%
  group_by(userId) %>% 
  summarize(userEffect = mean(rating - meanRating - movieEffect))

edx_test <- edx_test %>% 
  left_join(userMeanRatings) %>% 
  mutate(userEffect = ifelse(is.na(userEffect),0,userEffect)) %>%
  mutate(userEffectPred = userEffect + meanRating)

userEffectsModelRMSE <- RMSE(edx_test$userEffectPred, edx_test$rating)
RMSEs <- bind_rows(RMSEs, tibble(Model = "User Effects Model", RMSE = userEffectsModelRMSE)) 

# Movie & User Effects

edx_test <- edx_test %>% 
  mutate(movieuserEffectPred = movieEffect + userEffect + meanRating)

movieuserEffectsModelRMSE <- RMSE(edx_test$movieuserEffectPred, edx_test$rating)
RMSEs <- bind_rows(RMSEs, tibble(Model = "Movie & User Effects Model", RMSE = movieuserEffectsModelRMSE))

## Regularization
lambdas <- seq(0, 10, 0.25)
regularizedRMSEs <- sapply(lambdas, function(lambda){
  Movies <- edx_train %>%
    group_by(movieId) %>%
    summarize(bMovie = sum(rating - meanRating)/(n()+lambda))
  Users <- edx_train %>% 
    left_join(Movies, by="movieId") %>%
    group_by(userId) %>%
    summarize(bUser = sum(rating - bMovie - meanRating)/(n()+lambda))
  predicted_ratings <- edx_test %>% 
    left_join(Movies, by = "movieId") %>%
    left_join(Users, by = "userId") %>% 
    mutate(bMovie = ifelse(is.na(bMovie), 0, bMovie), 
           bUser = ifelse(is.na(bUser),0,bUser)) %>%
    mutate(pred = meanRating + bMovie + bUser)
  return(RMSE(predicted_ratings$pred, edx_test$rating))
})

min(regularizedRMSEs)

best_lambda = lambdas[which.min(regularizedRMSEs)]
best_lambda

RMSEs <- bind_rows(RMSEs, tibble(Model="Regularized Movie + User Effect Model",
                                 RMSE = min(regularizedRMSEs)))

kable(RMSEs, digits = 4)

## 3.2 Linear Regression Models

# Regress Against Decade Dummies

edx_train <- edx_train %>% mutate(dummy = TRUE, moviedecade2 = str_c("d",moviedecade)) %>%
  spread(key=moviedecade2, value=dummy, fill=FALSE)
edx_test <- edx_test %>% mutate(dummy = TRUE, moviedecade2 = str_c("d",moviedecade)) %>%
  spread(key=moviedecade2, value=dummy, fill=FALSE)

lm_fit_decades <- lm(rating ~ d1920s + d1930s + d1940s + d1950s + 
                       d1960s + d1970s + d1980s + d1990s + d2000s, data = edx_train)
#alternative specification: lm(rating ~ as.factor(moviedecade), data = edx)

p_hat <- predict(lm_fit_decades, edx_test)
edx_test <- edx_test %>% bind_cols(as_tibble(p_hat)) %>% rename(lmDecadesPred = value)
rm(p_hat)

lmDecadesModelRMSE <- RMSE(edx_test$lmDecadesPred, edx_test$rating)
RMSEs <- bind_rows(RMSEs, tibble(Model = "Linear Model on Decade Dummies", RMSE = lmDecadesModelRMSE))

# Regress Against Year

#First, let me use "year" as a numeric value

edx_train <- edx_train %>% mutate(movieyear2 = as.numeric(movieyear))
edx_test <- edx_test %>% mutate(movieyear2 = as.numeric(movieyear))

lm_fit_years <- lm(rating ~ movieyear2, data = edx_train)

p_hat <- predict(lm_fit_years, edx_test)
edx_test <- edx_test %>% bind_cols(as_tibble(p_hat)) %>% rename(lmYearsPred = value)
rm(p_hat)

lmYearsModelRMSE <- RMSE(edx_test$lmYearsPred, edx_test$rating)
RMSEs <- bind_rows(RMSEs, tibble(Model = "Linear Model on Year as Numeric", RMSE = lmYearsModelRMSE))

#Can I attempt at a linear model with years transformed into dummies?

lm_fit_years_dummies <- lm(rating ~ as.factor(movieyear), data=edx_train)

p_hat <- predict(lm_fit_years_dummies, edx_test)
edx_test <- edx_test %>% bind_cols(as_tibble(p_hat)) %>% rename(lmYearsDumPred = value)
rm(p_hat)

lmYearsModelDumRMSE <- RMSE(edx_test$lmYearsDumPred, edx_test$rating)
RMSEs <- bind_rows(RMSEs, tibble(Model = "Linear Model on Year as Dummies", RMSE = lmYearsModelDumRMSE))

# Regress Against Genre Dummies

lm_fit_genres <- lm(rating ~ action + adventure + animation + children + comedy +
                      crime + documentary + drama + fantasy + filmnoir + horror +
                      imax + musical + mystery + romance + scifi + thriller + war +
                      western, data = edx_train)

p_hat <- predict(lm_fit_genres, edx_test)
edx_test <- edx_test %>% bind_cols(as_tibble(p_hat)) %>% rename(lmGenresPred = value)
rm(p_hat)

lmGenresModelRMSE <- RMSE(edx_test$lmGenresPred, edx_test$rating)
RMSEs <- bind_rows(RMSEs, tibble(Model = "Linear Model on Genre Dummies", RMSE = lmGenresModelRMSE))

# Regress Against Decade & Genre Dummies

lm_fit_genresdecades <- lm(rating ~ action + adventure + animation + children + comedy +
                             crime + documentary + drama + fantasy + filmnoir + horror +
                             imax + musical + mystery + romance + scifi + thriller + war +
                             western + d1920s + d1930s + d1940s + d1950s + 
                             d1960s + d1970s + d1980s + d1990s + d2000s, data = edx_train)

p_hat <- predict(lm_fit_genresdecades, edx_test)
edx_test <- edx_test %>% bind_cols(as_tibble(p_hat)) %>% rename(lmGenreDecadesPred = value)
rm(p_hat)

lmGenreDecadesModelRMSE <- RMSE(edx_test$lmGenreDecadesPred, edx_test$rating)
RMSEs <- bind_rows(RMSEs, tibble(Model = "Linear Model on Decade & Genre Dummies", RMSE = lmGenreDecadesModelRMSE))

# Regressions with Movie & User Fixed Effects

edx_test <- edx_test %>% 
  mutate(lmGenreDecadesFEPred = lmGenreDecadesPred + movieEffect + userEffect)

lmGenreDecadesFEModelRMSE <- RMSE(edx_test$lmGenreDecadesFEPred, edx_test$rating)
RMSEs <- bind_rows(RMSEs, tibble(Model = "Final Linear Model with Movie & User Fixed Effects"
                                 , RMSE = lmGenreDecadesFEModelRMSE))

kable(RMSEs, digits = 4)

### 4 Final RMSE on Best Model

## Reimplementing the Final Model and Testing Against Final Holdout Set

Movies <- edx_train %>%
  group_by(movieId) %>%
  summarize(bMovie = sum(rating - meanRating)/(n()+best_lambda))
Users <- edx_train %>% 
  left_join(Movies, by="movieId") %>%
  group_by(userId) %>%
  summarize(bUser = sum(rating - bMovie - meanRating)/(n()+best_lambda))
predicted_ratings <- 
  final_holdout_test %>% 
  left_join(Movies, by = "movieId") %>%
  left_join(Users, by = "userId") %>% 
  mutate(bMovie = ifelse(is.na(bMovie), 0, bMovie), 
         bUser = ifelse(is.na(bUser),0,bUser)) %>%
  mutate(pred = meanRating + bMovie + bUser)

final_RMSE <- RMSE(predicted_ratings$pred, final_holdout_test$rating)

kable(tibble(Model = "Regularized Movie & User Effects" ,RMSE = final_RMSE), digits = 4)


