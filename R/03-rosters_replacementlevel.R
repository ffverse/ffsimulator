#' Add replacement level players to each roster
#'
#' Adds the same N free-agent starters to each roster to represent being able to
#' churn the waiver wire for starters, where N is the maximum number of players
#' that could start in a given position
#'
#' @param rosters a dataframe of rosters as created by `ffs_rosters()`
#' @param franchises a dataframe of franchises as created by `ffs_franchises()`
#' @param latest_rankings a dataframe of latest rankings as created by `ff_latest_rankings()`
#' @param lineup_constraints a dataframe of lineup constraints as created by `ffs_starter_positions`
#' @param pos_filter a character vector of positions to filter to, defaults to c("QB","RB","WR","TE","K")
#'
#' @export
#' @return a dataframe of rosters with replacements
ffs_add_replacement_level <- function(rosters,
                                      latest_rankings,
                                      franchises,
                                      lineup_constraints,
                                      pos_filter = c("QB","RB","WR","TE")
                                      ){

  assert_df(franchises, c("franchise_id", "franchise_name", "league_id"))
  assert_df(rosters, c("pos","fantasypros_id", "franchise_id","franchise_name"))
  assert_df(latest_rankings, c("fantasypros_id"))
  assert_df(lineup_constraints, c("pos","min","max"))

  pos <- NULL
  franchise_id <- NULL
  pos_rank <- NULL
  max <- NULL
  ecr <- NULL
  player <- NULL
  fantasypros_id <- NULL

  r <- data.table::as.data.table(rosters)
  f <- data.table::as.data.table(franchises)[
    ,c("franchise_id","franchise_name","league_id")
  ][,`:=`(joinkey = 1)]

  lr <- data.table::as.data.table(latest_rankings)[pos %in% pos_filter]
  lc <- data.table::as.data.table(lineup_constraints)[
    pos %in% pos_filter,
    c("pos","min","max")]

  fa <- r[
    ,c("fantasypros_id","franchise_id")
  ][lr, on = c("fantasypros_id")
  ][is.na(franchise_id)
  ][order(pos,ecr)
  ][,pos_rank := seq_len(.N), by = c("pos")
  ][lc, on = "pos"
  ][pos_rank <= max
  ][,list(
    player_id = paste(pos, pos_rank, sep = "_"),
    player_name = paste("Replacement",pos, "-", pos_rank, player),
    pos,
    team = NA,
    age = NA,
    fantasypros_id,
    joinkey = 1
  )]

  fa <- merge(f, fa, by = "joinkey", allow.cartesian = TRUE)[,-"joinkey"]

  out <- data.table::rbindlist(list(r, fa), use.names = TRUE, fill = TRUE)

  return(out)
}
