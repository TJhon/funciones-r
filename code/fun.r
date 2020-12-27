source(here::here("code", "pkg.r"))
source(here::here("code", "datos.r"))

# codfigo de referencia


create_bar_chart <- function(column_var, df){
  column_var <- enquo(column_var)
  df %>% 
    select(!!column_var) %>% 
    count(!!column_var) %>% 
    mutate(!!column_var := reorder(!!column_var, n)) %>% 
    ggplot(aes(x = !!column_var, y = n, fill = !!column_var)) +
    geom_col() +
    coord_flip() +
    theme(legend.position = "none")
}

# must

save_chart <- function(df, filename){
  temp_chart <-  df %>% 
    mutate(category = reorder(category, n)) %>% 
    ggplot() +
    aes(category, y = n, fill = category) +
    geom_col() +
    coord_flip() +
    theme(legend.position = "none") +
    ggtitle(paste0(filename, " Attacks"))
  
  ggsave(filename = paste0(filename, ".pdf"), plot = temp_chart, 
         width = 11, height = 8.5, units = "in")
}



vari <- function(df, disti = "c", var = "variable"){
  
  df %>% 
  # filter(Distrito == case_when(disti == "c" ~ "Chilca",
    #                             disti == "t" ~ "El Tambo",
     #                            disti == "h" ~ "Huancayo"
    #                              )) %>% 
    count(Distrito, name, variable, value) %>% 
    group_by(Distrito) %>% 
    #ungroup() %>% 
    filter(name == var) %>% 
    drop_na(n) %>% 
    mutate(Porcentaje = (n / sum(n) * 100 %>% 
                    round(4)),
           por = paste0(round(Porcentaje, 1), "%"), 
           value = factor(value),
           value = reorder(value, n),
           tree_label = paste(variable, Porcentaje)) %>% 
    ungroup() %>% 
    
    mutate(value = reorder(value, n))
  }

b_plot <- function(df){
    ggplot(df) + 
    theme_bw() +
    theme(legend.position = "none") +
    labs(title = "None") +
    aes(value, Porcentaje, fill = value) +
    geom_col() +
    labs(x = "") +
    coord_flip() +
    facet_wrap(~Distrito, scales = "free_x", ncol = 2)
    
    
    #geom_tech() +
#    scale_fill_tech("twitter") +
}

t_plot <- function(df){
  ggplot(df) + 
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = "") +
    labs(title = "None") +
    aes(area = n, fill = value, label = tree_label) +
    geom_treemap(color = "white", size = 2) +
    facet_wrap(~Distrito, ncol = 2, scales = "free") +
    geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                      grow = TRUE)
}

t_vari <- function(df, t = TRUE){
  t = as.logical(t)
  df %>% 
  select(!c(name, variable, n, Porcentaje, tree_label)) %>% 
  pivot_wider(names_from = value, values_from = por) %>% 
    knitr::kable()
  
}

me_fiter <- function(df, var = c()){
  df %>% 
    filter(name %in% var) %>% 
    select(!name) %>% 
    mutate(value = parse_number(value))
}


df1 %>% 
  mutate(Distrito = "Huancayo Metropolitano") %>% 
  bind_rows(df1) %>% 
  #filter(Distrito == "Chilca")
  vari("t", "satisfaccion")  %>% 
  b_plot() 

  

  
  

