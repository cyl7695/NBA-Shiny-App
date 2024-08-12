# Create a custom theme with the Conagra Brands colors
custom_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "darkly",
  bg = "#231F20",
  fg = "white",
  primary = NULL,
  secondary = "#E82C2A",
  font_scale = 1,
  heading_font = bslib::font_google("Lato"),
  base_font = bslib::font_google("Lato"),
  code_font = bslib::font_google("Source Code Pro")
)

## function to create a selectize input
create_selectizeInput <- function(id, mylabel, my_choices = character(0), my_selected = '', my_multiple = TRUE) {

  selectizeInput(id, mylabel, choices = my_choices, selected = my_selected, multiple = my_multiple)
}

## function to update a selectize input
create_updateSelectizeInput <- function(session, id, my_choices = character(0), my_selected = ' ', my_multiple = TRUE) {

  updateSelectizeInput(session, id, choices = my_choices, selected = my_selected, server = TRUE)
}

# function to create the input cats dataset for use for update of selectize input
create_input_cats <- function(data, target_col) {

  data %>%
    select({{target_col}}) %>%
    distinct() %>%
    as.vector() %>%
    unname() %>%
    unlist() %>%
    sort()
}

filter_input_cats <- function(data, main_col, input_cat) {
  
  if(is.null(input_cat)) {
    return(data)
  }
  data %>%
    filter(!!as.name(main_col) %in% input_cat)
}

create_max_lvl <- function(df, current_n_cols) {

  material_lvls <- df %>%
    rename(!!paste("MATERIAL", current_n_cols, sep="_") := MATERIAL)

  max_lvl <- NULL

  for (x in 1:current_n_cols) {

    if(x < current_n_cols) {

      prop_match <- material_lvls %>%
        mutate(!!sym(paste("NEXT_MATCH", x, sep="_")) :=
                 ifelse(!!sym(paste("MATERIAL", x, sep="_")) == !!sym(paste("MATERIAL", x+1, sep="_")), 1, 0)) %>%
        summarise(prop := mean(!!sym(paste("NEXT_MATCH", x, sep="_"))))

      if(prop_match$prop==1) {
        max_lvl <- c(max_lvl, x)
      }
    }
    else {
      max_lvl <- c(max_lvl, x)
    }
  }
  min(max_lvl)
}

summarise_productivity_costs <- function(df, my_by) {
  df %>%
    summarise(COST_PER_LB = sum(COST_PER_LB*PERFECT_LB_PER_LB) / sum(PERFECT_LB_PER_LB),
              PERFECT = sum(PERFECT_LB_PER_LB),
              COMP= sum(COMP_LB_PER_LB),
              OPS = sum(OPS_LB_PER_LB),
              .by = all_of(c(my_by)))
}

# Function to clean a flattened bom and calculate component costs
top_down_clean_bom <- function(df, batch) {
  
  if(is.null(df)) {
    return(NULL)
  }
  
  df <- df %>%
    mutate(USAGE = round(PERFECT*batch, digits = 5),
           COMPONENT_SCRAP= round(100*(COMP/PERFECT), digits = 5),
           OPERATIONAL_SCRAP = round(100*(OPS/PERFECT), digits = 5),
           COST_PER_LB = round(COST_PER_LB, digits = 5),
           COST = round(COST_PER_LB * USAGE * (1 + ((COMPONENT_SCRAP + OPERATIONAL_SCRAP) / 100)), digits = 5),
           MVP_COST = COST,
           DIFF = COST - MVP_COST) %>% 
    select(c(HEADER, PLANT, starts_with("MATERIAL"), COST_PER_LB, USAGE, COMPONENT_SCRAP, OPERATIONAL_SCRAP, COST, MVP_COST, DIFF))
  }

bottom_up_clean_bom <- function(df1, df2) {
  
  if(is.null(df1) | is.null(df2)) {
    return(NULL)
  }
  
  df <- df1 %>% 
    mutate(COMPONENT_SCRAP= 100*(COMP/PERFECT),
           OPERATIONAL_SCRAP = 100*(OPS/PERFECT),
           CURRENT_COST = round(COST_PER_LB * PERFECT * (1 + ((COMPONENT_SCRAP + OPERATIONAL_SCRAP) / 100)), digits = 5)) %>% 
    select(c(HEADER, PLANT, starts_with("MATERIAL"), COST_PER_LB, PERFECT, COMPONENT_SCRAP, OPERATIONAL_SCRAP, CURRENT_COST)) %>% 
    rename(CURRENT_COST_PER_LB = COST_PER_LB, CURRENT_PERFECT = PERFECT) %>% 
    right_join(df2) %>%
    select(HEADER, PLANT, starts_with("MATERIAL"), CURRENT_COST_PER_LB, CURRENT_PERFECT, COMPONENT_SCRAP, OPERATIONAL_SCRAP, CURRENT_COST, MVP_COST_PER_LB, MVP_USAGE) %>%
    mutate(MVP_PERFECT = MVP_USAGE*sum(CURRENT_PERFECT)/100,
           MVP_COST = round(MVP_COST_PER_LB * MVP_PERFECT * (1 + ((COMPONENT_SCRAP + OPERATIONAL_SCRAP) / 100)), digits = 5),
           .by = c(HEADER, PLANT, MATERIAL_1)) %>%
    summarise(CURRENT_PERFECT = sum(CURRENT_PERFECT),
              CURRENT_COST = sum(CURRENT_COST),
              MVP_PERFECT = sum(MVP_PERFECT),
              MVP_COST = sum(MVP_COST),
              .by = c(HEADER, PLANT, MATERIAL_1))
  }

diff_flag_column <- function(df, col, diff) {
  if(diff!=0) {
    df %>%
      formatStyle(col, color = 'white', backgroundColor = '#E82C2A', fontWeight = 'bold')
  }
  else {
    df %>%
      formatStyle(col, color = 'white', backgroundColor = "#231F20")
  }
}

render_report <- function(input, output, params) {
  # library(dplyr)
  # library(markdown)
  library(DT)
  rmarkdown::render(input,
                    output_file = output,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}

scale_and_flag <- function(x) {
  ifelse(is.na(scale(x)), 0, scale(x))[,1]
}

create_z <- function(df, x, by) {

  x <- enquo(x)

  df %>%
    mutate(Z_SCORE = scale_and_flag(!!x), .by = all_of(by))
}
