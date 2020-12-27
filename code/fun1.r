source(here::here("code", "pkg.r"))
source(here::here("code", "pkg.r"))

# asi se deben hacer las tablas orden col, row

# pivot_wider tiempo, distrito $ tal vez cambiar los nombres anteriores 
# pivot-wider covid (prevension, tratar) -> intervalo row(distritos)
# pibot_wider covid lo mismo para internados y muertes (prevension, tratar) -> intervalo row(distritos)
# pivot_wider vivienda cuartos : banio :numero s distritos
# pivot_wider f_, pivot_wider, 
# pivot wider especificar que son servicios que tiene, 
# pivot_wider servicios pivot_wider
# pivot_wider alrededores pivot wider, 

# intervalos edad ya esta
# intervalo ingrelso familiar histogram (intervalors)

# count satisfaccion, agregar un subtitulo con la leyenda 1 [poco satisfecho] : 10 [muy satisfecho]
# count covid _ higiene col distritos, row, valores
# count histogram vivienda anios_ aparte puede ser con un count
# count hogar ingresos cuanto count
# count idiom treemap
# count sentimiento count
# count genero count
# count jefe_ocu count
# count calidad


# no definido vivienda_m2:vieiena precio, valoreacion de la vivienda, dejas si no hay recomendaciones


# crear una funcion para con pivot wider count, intervalo

library(glue)

me_filter <- function(df, fil = "none"){
  #esta funcion filtra creando valores de los cuales se pueden hacer graficos
  df1 %>% 
    filter(str_detect(name, fil)) %>% 
    select(!c(id, name)) %>% 
    group_by(Distrito, variable) %>% 
    count(value) %>% 
    drop_na(value) %>% 
    mutate(por = round(n / sum(n) * 100, 2), 
           value = reorder(value, n),
           por1 = paste(por, "%")) %>% 
    ungroup() 
}

me_intervalo <- function(df, filter1 = "edad", len = 10) {
  df1 %>% 
    filter(str_detect(name, filter1)) %>% 
    mutate(value = as.numeric(value),
           value = cut_interval(value, length = len)) %>%
    group_by(Distrito, variable) %>% 
    count(value) %>% 
    mutate(por = round(n / sum(n) * 100, 2), 
           #value = reorder(value, n),
           por1 = paste(por, "%")) 
}

me_pivot <- function(df, name = "insertar nombre"){
  # esto listo para hacer una tabla
  
  df  %>% 
    select(!c(n, por)) %>% 
    pivot_wider(names_from = Distrito, values_from = por1) %>% 
    rename({{name}} := value) %>%  
    arrange(1) %>% 
    mutate_if(is.character, replace_na, replace = "0 %") 
  }



meb_plot <- function(df, title = "titulo", sub = "", pos = "bottom", tipo = "dodge2", x = "", coll = 2, value){
  ggplot(df) +
    aes(value, por, fill = {value}) +
    coord_flip() +
    geom_col(position = "dodge2") +
    facet_wrap(~Distrito, scales = "free", ncol = coll) +
    geom_text(aes(label = por1), position = position_stack(vjust = .5), color = "White", size = 2.5) +
    labs(title = {{title}},
         subtitle = sub, 
         caption = "Fuente: Encuesta realizada octubre 2020",
          y = "(%)",
         x = {x}) +
    theme_bw() +
    theme(legend.position = pos) 
}





