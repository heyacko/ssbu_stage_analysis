# use to generate list of tournaments to pull game data from

## to pull custom date range
start_date <- as.numeric(as.POSIXct('2022-01-01', '%Y-%m-%d'))  
end_date <- as.numeric(as.POSIXct('2022-02-01', '%Y-%m-%d'))  

pull_tournaments_from_pages(start_date, end_date)

## to pull 2021 yr
tourneys_df <-
  c(
    pull_tournaments_from_pages('2021-01-01', '2021-02-01'),
    pull_tournaments_from_pages('2021-02-02', '2021-03-01'),
    pull_tournaments_from_pages('2021-03-02', '2021-04-01'),
    pull_tournaments_from_pages('2021-04-02', '2021-05-01'),
    pull_tournaments_from_pages('2021-05-02', '2021-06-01'),
    pull_tournaments_from_pages('2021-06-02', '2021-07-01'),
    pull_tournaments_from_pages('2021-07-02', '2021-08-01'),
    pull_tournaments_from_pages('2021-08-02', '2021-09-01'),
    pull_tournaments_from_pages('2021-09-02', '2021-10-01'),
    pull_tournaments_from_pages('2021-10-02', '2021-11-01'),
    pull_tournaments_from_pages('2021-11-02', '2021-12-01'),
    pull_tournaments_from_pages('2021-12-02', '2022-01-01')
  ) %>%
  bind_rows() %>%
  unnest()

pull_tournaments_from_pages <-
  function(start_date, stop_date, page_start) {
    search_tourneys_query <- '
query TournamentsByVideogame($beforeDate: Timestamp!, $afterDate: Timestamp!,
  $page:Int!, $perPage: Int!, $videogameId: ID!) {
  tournaments(query: {
    perPage: $perPage
    page: $page
    filter: {
      afterDate: $afterDate,
      beforeDate: $beforeDate,
      published: true,
      videogameIds: [
        $videogameId
      ]
    }
  }) {
    nodes {
      id
      name
      numAttendees
      startAt
      endAt
    }
  }
}

'

start_date_formatted <-
  as.numeric(as.POSIXct(start_date, '%Y-%m-%d'))
end_date_formatted <-
  as.numeric(as.POSIXct(stop_date, '%Y-%m-%d'))

if (missing(page_start)) {
  i <- 1
} else {
  i <- page_start
}

list_of_res_json <- list()

q <- Query$new()$query('url', search_tourneys_query)

while (T) {
  q_var <- list(
    afterDate = start_date_formatted,
    beforeDate = end_date_formatted,
    page = i,
    perPage = 500,
    videogameId = 1386
  )
  
  res_json <- con$exec(q$url, variables = q_var) %>%
    fromJSON(flatten = F) %>%
    .[['data']]
  
  if (res_json %>%
      .[['tournaments']] %>%
      .[['nodes']] %>%
      length() == 0) {
    print(paste0("Found end of query at page: ", i))
    break
  } else {
    list_of_res_json[[length(list_of_res_json) + 1]] <- res_json
    print(paste0("Successfully appended page: ", i))
    i <- i + 1
    Sys.sleep(0.5)
  }
}

return(list_of_res_json)

  }