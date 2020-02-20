library(dplyr)
library(ggraph)
library(tidygraph)
library(stringr)


map_episode <- function(show_selected, season_selected, episode_selected) {
   characters %>%
    dplyr::group_by(character, show) %>%
    dplyr::count() %>%
    dplyr::group_by(character) %>%
    dplyr::mutate(total_shows = n()) %>%
    dplyr::arrange(-total_shows) %>%
    dplyr::ungroup() %>%
    dplyr::filter(total_shows > 1) %>%
    dplyr::rename(to = character, from = show) %>%
    dplyr::arrange(from) %>%
    tidygraph::as_tbl_graph() %>%
    tidygraph::activate(nodes) %>%
    dplyr::left_join(characters %>%
                       dplyr::filter(show == show_selected,
                                     season == season_selected,
                                     episode == episode_selected) %>%
                       dplyr::group_by(character, show) %>%
                       dplyr::count() %>%
                       dplyr::group_by(character) %>%
                       dplyr::mutate(total_shows = n()) %>%
                       dplyr::arrange(-total_shows) %>%
                       dplyr::ungroup() %>%
                       dplyr::distinct(character, total_shows) %>%
                       dplyr::rename(name = character)) %>%
    dplyr::mutate(show_label = ifelse(stringr::str_detect(name, characters %>% dplyr::distinct(show) %>%
                                                            dplyr::pull(show) %>%
                                                            paste(collapse = "|")), name, NA),
                  character_label = ifelse(name %in% (
                    characters %>%
                      dplyr::filter(show == show_selected,
                                    season == season_selected,
                                    episode == episode_selected) %>%
                      dplyr::distinct(character) %>% pull(character)
                  ), name, NA)) %>%
    activate(edges) %>%
    left_join(
      characters %>%
        dplyr::group_by(character, show) %>%
        dplyr::count() %>%
        dplyr::group_by(character) %>%
        dplyr::mutate(total_shows = n()) %>%
        dplyr::arrange(-total_shows) %>%
        dplyr::ungroup() %>%
        dplyr::filter(total_shows > 1) %>%
        distinct(character) %>%
        mutate(to = 1:n())
    ) %>%
    left_join(
      characters %>%
        dplyr::group_by(character, show) %>%
        dplyr::count() %>%
        dplyr::group_by(character) %>%
        dplyr::mutate(total_shows = n()) %>%
        dplyr::arrange(-total_shows) %>%
        dplyr::ungroup() %>%
        dplyr::filter(total_shows > 1) %>%
        distinct(show) %>%
        mutate(from = 1:n())
    ) %>%
    mutate(selected = ifelse(character %in% (
      characters %>%
        dplyr::filter(show == show_selected,
                      season == season_selected,
                      episode == episode_selected) %>%
        dplyr::distinct(character) %>% pull(character)
      ), 1, 1/50)) %>%
    arrange(-selected) %>%
    ggraph(layout = "kk") +
    # geom_edge_diagonal(alpha = 1/50,
    #                    show.legend = FALSE) +
    geom_edge_diagonal(aes(color = selected, alpha = selected),
                       show.legend = FALSE) +
    geom_node_label(aes(label = show_label), fill = "black", color = "white", alpha = 3/5) +
    geom_node_label(aes(label = character_label), size = 3)+
    theme_void()
}

# characters %>%
#   dplyr::group_by(character, show) %>%
#   dplyr::count() %>%
#   dplyr::group_by(character) %>%
#   dplyr::mutate(total_shows = n()) %>%
#   dplyr::arrange(-total_shows) %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(total_shows > 1) %>%
#   filter(character %in% (
#     characters %>%
#       dplyr::filter(show == "Vera",
#                     season == 2,
#                     episode == 1) %>%
#       dplyr::distinct(character) %>% pull(character)
#   ) & show == "Vera")


# characters %>%
#   dplyr::group_by(character, show) %>%
#   dplyr::count() %>%
#   dplyr::group_by(character) %>%
#   dplyr::mutate(total_shows = n()) %>%
#   dplyr::arrange(-total_shows) %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(total_shows > 1) %>%
#   dplyr::rename(to = character, from = show) %>%
#   dplyr::arrange(from)  %>%
#   left_join(
#     characters %>%
#       dplyr::group_by(character, show) %>%
#       dplyr::count() %>%
#       dplyr::group_by(character) %>%
#       dplyr::mutate(total_shows = n()) %>%
#       dplyr::arrange(-total_shows) %>%
#       dplyr::ungroup() %>%
#       dplyr::filter(total_shows > 1) %>%
#       dplyr::rename(to = character, from = show) %>%
#       dplyr::arrange(from)  %>%
#       distinct(to) %>%
#       mutate(to_index = 1:n())
#   ) %>%
#   left_join(
#     characters %>%
#       dplyr::group_by(character, show) %>%
#       dplyr::count() %>%
#       dplyr::group_by(character) %>%
#       dplyr::mutate(total_shows = n()) %>%
#       dplyr::arrange(-total_shows) %>%
#       dplyr::ungroup() %>%
#       dplyr::filter(total_shows > 1) %>%
#       dplyr::rename(to = character, from = show) %>%
#       dplyr::arrange(from)  %>%
#       distinct(from) %>%
#       mutate(from_index = 1:n())
#   ) -> prepared_tbl
#
#
# prepared_tbl %>%
#   tidygraph::as_tbl_graph() %>%
#   tidygraph::activate(nodes) %>%
#   dplyr::mutate(show_label = ifelse(stringr::str_detect(name, characters %>% dplyr::distinct(show) %>%
#                                                           dplyr::pull(show) %>%
#                                                           paste(collapse = "|")), name, NA),
#                 character_label = ifelse(name %in% (
#                   characters %>%
#                     dplyr::filter(show == "Vera",
#                                   season == 2,
#                                   episode == 1) %>%
#                     dplyr::distinct(character) %>% pull(character)
#                 ), name, NA)) %>%
#   activate(edges) %>%
#   left_join(prepared_tbl %>%
#               distinct(from, from_index) %>%
#               rename(show = from,
#                    from  = from_index)) %>%
#   left_join(prepared_tbl %>%
#               distinct(to, to_index) %>%
#               rename(character = to,
#                      to = to_index)) %>%
#   mutate(selected = ifelse(character %in% (
#     characters %>%
#       dplyr::filter(show == "Vera",
#                     season == 2,
#                     episode == 1) %>%
#       dplyr::distinct(character) %>% pull(character)
#   ), 1, 1/50)) %>%
#   ggraph(layout = "kk") +
#   # geom_edge_diagonal(alpha = 1/50,
#   #                    show.legend = FALSE) +
#   geom_edge_diagonal(aes(color = factor(selected), alpha = selected)) +
#   geom_node_label(aes(label = show_label), fill = "black", color = "white", alpha = 3/5) +
#   geom_node_label(aes(label = character_label), size = 3)+
#   theme_void()



#' @export
find_characters <- function(show_selected, season_selected, episode_selected) {
  characters %>% filter(show == show_selected, season == season_selected,
                        episode == episode_selected) %>%
    distinct(character) %>% pull(character)
}

#' @export
find_distinct_shows <- function(character_list){
  characters %>%
    filter(character %in% character_list) %>%
    group_by(character) %>%
    mutate(n_show  = n_distinct(show)) %>%
    arrange(-n_show)
}

# find_distinct_shows(find_characters("Bletchley Circle", 1, 3)) %>%
#   select(show, character) %>%
#   distinct(character) %>%
#   pull(character) %>%
#   paste(collapse = "|") %>%
#   prepare_graph() %>%
#   graph_prepared_graph()


#' @export
prepare_graph <- function(selected_name){
  prepared_tbl %>%
  select(from, to) %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  mutate(selected_node = ifelse(str_detect(name, selected_name) | name %in% unique(prepared_tbl$from) , name, NA),
         show_node = ifelse(name %in% unique(prepared_tbl$from[which(str_detect(prepared_tbl$to, selected_name))]) | str_detect(name, selected_name), "show", NA),
         selected_node_alpha = ifelse(!is.na(show_node) | str_detect(name, selected_name), 1, 1/5)) %>%
  activate(edges) %>%
  left_join(prepared_tbl %>%
              mutate(to_index = to_index + n_distinct(from)) %>%
              distinct(to, to_index), by = c("to" = "to_index")) %>%
    left_join(prepared_tbl %>%
                distinct(from, from_index), by = c("from" = "from_index")) %>%
  mutate(selected_edge = ifelse(str_detect(to.y, selected_name), 1, 1/50)) %>%
  arrange(-selected_edge)
}

#' @export
graph_prepared_graph <- function(prepared_tbl_graph){
  prepared_tbl_graph %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(color = factor(from.y), alpha = selected_edge)) +
  geom_node_label(aes(label = selected_node, fill = show_node, alpha = selected_node_alpha), repel = TRUE)
}



