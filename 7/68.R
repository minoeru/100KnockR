library(magrittr)
library(jsonlite)

Func68 <- function(path){
  data <- jsonlite::stream_in(file(path))
  sum <- length(data[[1]])
  name <- as.vector(data[[1]])
  tags <- as.vector(data[[2]])
  tags_count <- lapply(1:sum,function(x){
    ifelse(is.null(tags[[x]][[1]]),0,tags[[x]][[1]])
  }) %>% unlist()
  tags_value <- lapply(1:sum,function(x){
    ifelse(is.null(tags[[x]][[2]]),0,tags[[x]][[2]])
  }) %>% unlist()
  sort_name <- as.vector(data[[3]])
  ended <- as.vector(data[[4]])
  gid <- as.vector(data[[5]])
  id <- as.vector(data[[6]])
  type <- as.vector(data[[7]])
  begin_year <- data[[8]]$year
  begin_date <- data[[8]]$date
  begin_month <- data[[8]]$month
  end_year <- data[[9]]$year
  end_date <- data[[9]]$date
  end_month <- data[[9]]$month
  area <- data[[10]]
  aliases <- as.vector(data[[11]])
  aliases_name <- lapply(1:sum,function(x){
    ifelse(is.null(aliases[x][[1]]$name),0,aliases[x][[1]]$name)
  }) %>% unlist()
  sort_name <- lapply(1:sum,function(x){
    ifelse(is.null(aliases[x][[1]]$sort_name),0,aliases[x][[1]]$sort_name)
  }) %>% unlist()
  gender <- as.vector(data[[12]])
  rating_count <- as.vector(data[[13]])$count %>% unlist()
  rating_value <- as.vector(data[[13]])$value %>% unlist()
  database <<- data.frame(
    name=name,
    tags.count=tags_count,
    tags.value=tags_value,
    sort.name=sort_name,
    ended=ended,
    gid=gid,
    id=id,
    type=type,
    begin.year=begin_year,
    begin.date=begin_date,
    begin.month=begin_month,
    end.year=end_year,
    end.date=end_date,
    end.month=end_month,
    area=area,
    aliases.name=aliases_name,
    sort.name=sort_name,
    gender=gender,
    rating.count=rating_count,
    rating.value=rating_value
  )
  hogege <- database$tags.value
  hogehoge <- grep(".*dance.*",hogege)
  dance_data <- database[hogehoge,]
  piyo <- order(dance_data$rating.count, decreasing=T)[1:10]
  dance_data$name[piyo] %>% as.vector() %>% writeLines()
}

Func68("artist.json")