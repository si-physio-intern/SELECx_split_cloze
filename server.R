# Split Cloze Responses

library(shiny)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(rlang)

library(shiny)

# Function ----------------------------------------------------------------

### Validate Response file
is.valid_names_df <- function(df, valid_regex){
    
    map_lgl(valid_regex , ~ str_detect(names(df), .x) %>% any() ) %>% all()
    
}


clean_selecx <- function(df) {
    
    require(dplyr)
    require(tidyr)
    
    df_clean <- df %>% 
        select(-Institution,-Department,-`Started on`,-`Time taken`,-Completed,
               -starts_with("Grade")) %>% 
        rename(Email="Email address") %>% 
        unite("First name","Surname",sep = " ",col = "Name") %>% 
        filter(!is.na(Email)) %>% 
        mutate( ID = str_extract(Email,"[:digit:]+") %>% as.character(), 
                .keep = "unused") %>% 
        relocate(Name,ID)
    
    #names(df_clean) <- names(df_clean) %>% snakecase::to_snake_case()
    df_clean
}

### Get column names that has cloze answer

get_cloze_cols <- function(df) {
    
    require(purrr)
    require(stringr)
    
    regex <- "part [:digit:]+:"
    is_cloze_lgl <- df %>% 
        map(~str_detect(.x, regex)) %>% 
        map_lgl(any)
    
    which(is_cloze_lgl) %>% names()
    
}

### Split cloze answer

split_part <- function(df, var, long = F) {
    
    require(rlang)
    require(dplyr)
    require(purrr)
    
    var <- ensym(var)
    var_lab <- as_label(var)
    regex <- "part [:digit:]+:"
    
    ex <- df[1, ] %>% pull(!!var)
    parts_len <- seq_len(str_count(ex, regex))
    
    split_col_df <- df %>% 
        mutate(!!var := str_remove_all(!!var, regex)) %>% 
        separate(!!var, into = paste0(var_lab, "_part_", parts_len), sep = "; ") 
    
    out <- if(long == T){
        require(tidyr)
        col_nm <- paste0(var_lab, "_parts")
        split_col_df %>%   
            pivot_longer(cols = starts_with(paste0(var_lab, "_part_")), 
                         names_to = col_nm, 
                         names_prefix = paste0(var_lab, "_part_"),
                         values_to = paste0(var_lab,"_answers")) 
        #mutate(!!col_nm := as.factor(.data[[col_nm]]))
    }else{
        split_col_df
    }
    
    out
}

### Split cloze answer (vectorized, input vars as string)

split_parts_2 <- function(df, vars = NULL, long = F) {
    
    require(stringr)
    require(purrr)
    
    not_vars_i <- str_which(names(df), vars, negate = T)
    not_vars <- names(df)[not_vars_i]
    
    df <- map_df(df, as.character)
    
    ls_df <- map(vars, ~split_part(select(df, all_of(.x), not_vars), 
                                   !!.x, long = long)) 
    
    ls_df %>% reduce(full_join, by = not_vars)
    
}

### Join ID file
join_id <- function(ids, df, ... ){
    
    clean_id <- ids %>% 
        map_df(as.character) %>% 
        mutate(ID = str_extract(ID,"[:digit:]+")) %>% 
        select(ID,Name, ... )
    
    full_join(clean_id, df, by = "ID") %>% 
        rename(Name_from_ID = "Name.x", 
               Name_from_SELECx = "Name.y") %>% 
        relocate(... , .after = Name_from_SELECx) %>% 
        arrange(ID)
    
}


# Server ------------------------------------------------------------------



server <- function(input, output, session) {
    
    # Upload files IDs ---------------------------------------------------------------
    
    ids <- reactive({
        
        req(input$file_id) # Require - code wait until file uploaded
        
        ext <- tools::file_ext(input$file_id$name)
        switch(ext,
               csv = readr::read_csv(input$file_id$datapath),
               xls = readxl::read_excel(input$file_id$datapath),
               xlsx = readxl::read_excel(input$file_id$datapath),
               validate("Invalid file; Please upload a .csv, .xls or .xlsx file")
        )
    })
    
    proper_id <- reactive({  
        
        all(c("ID","Name") %in% colnames( ids() ) && all(str_detect(ids() %>% pull(ID),"[:digit:]+"))) 
        
    })
    
    
    observeEvent(input$file_id,
                 shinyFeedback::feedbackWarning(
                     "file_id", 
                     !proper_id(),
                     "Incorrect ID file specification"
                 )  
    )
    
    # Validate & Clean Input DF -------------------------------------------------------------------
    
    
    df_pre <- reactive({
        
        req(input$file) # Require - code wait until file uploaded
        
        ext <- tools::file_ext(input$file$name)
        switch(ext,
               csv = readr::read_csv(input$file$datapath),
               xls = readxl::read_excel(input$file$datapath),
               xlsx = readxl::read_excel(input$file$datapath),
               validate("Invalid file; Please upload a .csv, .xls or .xlsx file")
        )
        
    })
    
    
    proper_df <- reactive({
        
        valid_regex <- c("First name","Surname","Email address","State","Response")
        
        df_pre() %>% is.valid_names_df(valid_regex)
        
    })
    
    df_raw <- reactive({
        
        shinyFeedback::feedbackWarning("file", !proper_df(), "Incorrect file specification")
        
        req(proper_df())
        
        df_pre()
        
    })
    
    
    
    df_cleaned <- reactive({
        
        df_raw() %>% clean_selecx()
        
    })
    
    
    # Choose Cloze cols and Split DF -------------------------------------------------------
    
    cloze_cols <- reactive({
        
        df_cleaned() %>% get_cloze_cols()
        
    })
    
    observeEvent(input$file,{
        
        updateSelectInput(session, "cloze_cols", 
                          choices = cloze_cols(), selected = NULL)
        
    })
    
    
    df_splited <- reactive({
        
        not_choose_yet <- ( !isTruthy(input$cloze_cols) && isTruthy(input$file) )
        
        shinyFeedback::feedbackWarning("cloze_cols", 
                                       not_choose_yet,
                                       "Please select Cloze column(s)")
        
        req(input$cloze_cols)
        
        df_cleaned() %>% 
            select(Name, ID, all_of(input$cloze_cols)) %>% 
            split_parts_2(vars = input$cloze_cols, long = input$pivot ) %>% 
            relocate(Name, ID)
        
        
    })
    
    
    #output$raw <- renderPrint({  input$cloze_cols })
    
    # Join DF to ids ---------------------------------------------------------------
    
    id_cols <- reactive({ 
        
        req(proper_id())
        ids() %>% select(-ID,-Name) %>% colnames() 
        
    })
    
    output$select <- renderUI({
        
        if(input$add_cols == T){
            selectInput("cols","Choose column",choices = id_cols(), multiple = TRUE)
        }
        
    })
    
    
    df_splited_joined_ids <- reactive({
        
        if(( !isTruthy(input$file_id)) || ( !proper_id()) ){  
            
            df_splited()
            
        }else if(input$add_cols == T){
            
            join_id(ids = ids() ,df = df_splited(), input$cols)
            
        }else{
            join_id(ids = ids() ,df = df_splited())}
        
        
    })  
    
    df_cleaned_joined_ids <- reactive({
        
        if(( !isTruthy(input$file_id)) || ( !proper_id()) ){  
            
            df_cleaned()
            
        }else if(input$add_cols == T){
            
            join_id(ids = ids() ,df = df_cleaned(), input$cols)
            
        }else{
            join_id(ids = ids() ,df = df_cleaned())}
        
        
    })  
    
    # Display table -----------------------------------------------------------
    
    
    output$table_split <- renderDataTable({
        
        df_splited_joined_ids()
        
    },  options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ))
    
    
    output$table_cleaned <- renderDataTable({
        
        df_cleaned_joined_ids()
        
    },  options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ))
    
    
    # Download ---------------------------------------------------------------
    
    output$download <- downloadHandler(
        
        filename = function() {
            paste0("Splited_cloze",".xlsx") #remove .xxx
        },
        content = function(file) {
            
            openxlsx::write.xlsx(df_splited_joined_ids(), file)
        }
    )
    
    output$download_2 <- downloadHandler(
        
        filename = function() {
            paste0("Response",".xlsx") #remove .xxx
        },
        content = function(file) {
            
            openxlsx::write.xlsx( df_cleaned_joined_ids(), file)
        }
    )
    
    
    
}



