ggcor <- function(data, lb = -1, ub = 1, ...){
  data %>%
    filter_all(any_vars(!is.na(.))) %>% 
    cor(...) %>% 
    as.data.frame() %>%  
    rownames_to_column() %>% 
    as.tibble() %>% 
    gather(
      key = Column
      ,value = Correlation
      ,-rowname
    ) %>% 
    rename(Row = rowname) %>% 
    ggplot(
      aes(
        x = Column
        ,y = Row
        ,fill = Correlation
      )
    ) +
    geom_raster() +
    scale_fill_distiller(
      type = "div"
      ,palette = "RdBu"
      ,limits = c(lb, ub)
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
      ,axis.title.x = element_blank()
      ,axis.title.y = element_blank()
      ,panel.grid = element_blank()
      ,panel.background = element_blank()
    )
}
