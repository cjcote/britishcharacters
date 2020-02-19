library(tidyverse)
library(rvest)
library(tidygraph)
library(ggraph)

#' @importFrom magrittr %>%

#' @export
episode_search <- function(show_select, season_select, episode_select) {
  characters %>%
    dplyr::group_by(character, show) %>%
    dplyr::count() %>%
    dplyr::group_by(character) %>%
    dplyr::mutate(total_shows = n()) %>%
    dplyr::arrange(-total_shows) %>%
    dplyr::ungroup() %>%
    dplyr::filter(total_shows > 1) %>%
    # dplyr::distinct(character, total_shows) %>%
    dplyr::rename(to = character, from = show) %>%
    dplyr::arrange(from) %>%
    tidygraph::as_tbl_graph() %>%
    tidygraph::activate(nodes) %>%
    dplyr::left_join(characters %>%
                dplyr::filter(show == show_select,
                       season == season_select,
                       episode == episode_select) %>%
                dplyr::group_by(character, show) %>%
                dplyr::count() %>%
                dplyr::group_by(character) %>%
                dplyr::mutate(total_shows = n()) %>%
                dplyr::arrange(-total_shows) %>%
                dplyr::ungroup() %>%
                # dplyr::filter(total_shows > 1) %>%
                dplyr::distinct(character, total_shows) %>%
                dplyr::rename(name = character)) %>%
    dplyr::mutate(show_label = ifelse(stringr::str_detect(name, characters %>% dplyr::distinct(show) %>%
                                            dplyr::pull(show) %>%
                                            paste(collapse = "|")), name, NA),
           character_label = ifelse(!is.na(total_shows), name, NA)) %>%
    ggraph(layout = "kk") +
    geom_edge_diagonal(alpha = 1/50,
                   show.legend = FALSE) +
    geom_node_label(aes(label = show_label), alpha = 3/5) +
    geom_node_label(aes(label = character_label), size = 3)+
    theme_void()
}

#' @export
map_episode <- function(show_selected, season_selected, episode_selected) {
  characters %>%
    dplyr::left_join(characters%>%
                dplyr::filter(character %in% (
                  characters %>%
                    dplyr::filter(show == show_selected,
                           season == season_selected,
                           episode == episode_selected) %>%
                    dplyr::pull(character)
                )) %>% dplyr::mutate(selected = 1))  %>%
    dplyr::filter(selected == 1) %>%
    tidygraph::as_tbl_graph() %>%
    ggraph(layout = "graphopt") +
    geom_edge_diagonal() +
    geom_node_label(aes(label = name))+
    # geom_node_label(aes(label = ifelse(name %in% unique(characters$show), name, NA), color = name), show.legend = FALSE, size = 5) +
    # geom_node_label(aes(label = ifelse(!(name %in% unique(characters$show)), name, NA)), size = 3, show.legend = FALSE) +
    theme_void()
}


#' @export
map_character <- function(character_selected,...) {
  characters %>%
    dplyr::filter(stringr::str_detect(character, character_selected)) %>%
    tidygraph::as_tbl_graph() %>%
    ggraph(layout = "graphopt") +
    geom_edge_link() +
    geom_node_label(aes(label = name)) +
    # geom_node_label(aes(label = ifelse(name %in% unique(characters$show), name, NA), color = name), show.legend = FALSE, size = 4) +
    # geom_node_label(aes(label = ifelse(!(name %in% unique(characters$show)), name, NA)), size = 3, show.legend = FALSE) +
    theme_void()
}


