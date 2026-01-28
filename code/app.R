# app.R
# Medicare Topic Explorer - Shiny app (FINAL update per user's requests)
# Place this file in the same folder as the "shiny_data" directory

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(igraph)
library(ggraph)
library(stringr)
library(htmltools)
library(reticulate)  # used to load scipy sparse TF-IDF matrix for similarity
library(Matrix)

DATA_DIR <- "../data/shiny-data"

# -------------------------
# Load precomputed data
# -------------------------
top_terms <- read_csv(file.path(DATA_DIR, "top_terms_by_topic.csv"), show_col_types = FALSE)
topic_term_long <- read_csv(file.path(DATA_DIR, "topic_term_long.csv"), show_col_types = FALSE)
doc_topic <- read_csv(file.path(DATA_DIR, "doc_topic_probs.csv"), show_col_types = FALSE)
documents <- read_csv(file.path(DATA_DIR, "documents.csv"), show_col_types = FALSE)
cooc <- read_csv(file.path(DATA_DIR, "cooccurrence.csv"), show_col_types = FALSE)
examples <- read_csv(file.path(DATA_DIR, "examples_by_word.csv"), show_col_types = FALSE)

# Ensure consistent column names
if (!"doc_id" %in% names(documents)) documents <- documents %>% mutate(doc_id = row_number()-1)

# Topic labels and CMS-style summaries (already expanded earlier)
topic_labels <- list(
  "0" = "Costs, Deductibles, and Supplement Plans",
  "1" = "Enrollment, Eligibility, and Social Security Coordination",
  "2" = "Plan Comparisons, Networks, and Dual Eligibility"
)

topic_summaries <- list(
  "0" = paste(
    "This topic covers questions about Medicare deductibles, Medigap supplement plans,",
    "cost-sharing, drug coverage, hospital bills, and navigating official Medicare.gov resources.",
    "Practical steps:",
    "1) Check your Part A/B deductible and coinsurance on Medicare.gov or in your plan documents;",
    "2) Review your Part D formulary and tier structure for prescription costs;",
    "3) Compare Medigap plans for out-of-pocket limits and provider coverage;",
    "4) For unexpected bills, request itemized statements, file appeals, or contact your plan's member services."
  ),
  "1" = paste(
    "This topic covers questions about Medicare enrollment periods, eligibility based on income or employer coverage,",
    "applying for Medicare or related disability benefits, and coordination with Social Security.",
    "Practical steps:",
    "1) Verify your Initial Enrollment Period dates and Special Enrollment rules;",
    "2) Coordinate employer benefits with Medicare to avoid penalties;",
    "3) Contact Social Security to confirm Part A/B eligibility and enrollment;",
    "4) Use local SHIP counselors for in-person guidance."
  ),
  "2" = paste(
    "This topic covers questions comparing Original Medicare and Medicare Advantage, provider networks,",
    "QMB/Extra Help programs, dual eligibility with Medicaid, and finding in-network doctors or plan details (e.g., Humana, Aetna).",
    "Practical steps:",
    "1) Compare provider networks and prior authorization rules;",
    "2) Check star ratings and member reviews;",
    "3) For dual-eligible individuals, confirm state Medicaid wraparound benefits;",
    "4) Contact plan customer service for provider directories."
  )
)


# Number of topics inferred from files
topics_available <- sort(unique(topic_term_long$topic))
topics_chr <- as.character(topics_available)


# Tokens for dropdown
THRESHOLD <- 0.01

valid_terms <- topic_term_long %>%
  group_by(term) %>%
  summarise(total_weight = sum(weight, na.rm = TRUE)) %>%
  filter(total_weight > THRESHOLD) %>%    # exclude very small probabilities
  arrange(term) %>%
  pull(term)


# -------------------------
# Helper functions
# -------------------------
split_sentences <- function(text) {
  sents <- unlist(str_split(text, "(?<=[.!?])\\s+"))
  sents[sents != ""]
}

get_highlighted_context <- function(full_text, word, max_sentences = 2) {
  word_lower <- str_to_lower(word)
  sents <- split_sentences(full_text)
  hit_idx <- which(str_detect(str_to_lower(sents), fixed(word_lower)))
  if (length(hit_idx) == 0) {
    ctx <- paste(head(sents, max_sentences), collapse = " ")
  } else {
    idx <- hit_idx[[1]]
    start <- max(1, idx - 1)
    end <- min(length(sents), idx + (max_sentences - 1))
    ctx <- paste(sents[start:end], collapse = " ")
  }
  pattern <- regex(word, ignore_case = TRUE)
  highlighted <- str_replace_all(ctx, pattern, function(m) paste0("<b>", m, "</b>"))
  HTML(highlighted)
}

predict_topic_from_text <- function(text, topic_term_df) {
  toks <- unlist(str_split(tolower(text), "\\s+"))
  toks <- toks[toks != ""]
  toks <- str_replace_all(toks, "^[^a-z0-9']+|[^a-z0-9']+$", "")
  toks <- toks[toks != ""]
  if (length(toks) == 0) {
    return(list(pred = NA, probs = rep(1/length(topics_chr), length(topics_chr))))
  }
  scores <- sapply(topics_chr, function(tid) {
    sum(topic_term_df %>% filter(topic == as.integer(tid) & term %in% toks) %>% pull(weight), na.rm = TRUE)
  })
  if (all(scores == 0)) {
    probs <- rep(1/length(scores), length(scores))
  } else {
    probs <- scores / sum(scores)
  }
  pred <- topics_chr[which.max(probs)]
  list(pred = pred, probs = probs)
}

# Build doc assignment & topic counts
doc_assigned <- doc_topic %>%
  mutate(assign = apply(select(., starts_with("topic_")), 1, function(r) which.max(r)-1)) %>%
  select(doc_id, assign)

topic_counts <- doc_assigned %>% count(assign) %>% rename(topic = assign, num_posts = n)

# -------------------------
# Topic descriptive stats
# -------------------------
# create fallback columns if missing
if (!"word_count" %in% names(documents)) documents$word_count <- NA
if (!"text_length" %in% names(documents)) documents$text_length <- NA
if (!"score" %in% names(documents)) documents$score <- NA
if (!"num_comments" %in% names(documents)) documents$num_comments <- NA

topic_stats <- doc_assigned %>%
  left_join(documents, by = "doc_id") %>%
  group_by(assign) %>%
  summarise(
    avg_word_count = mean(word_count, na.rm = TRUE),
    median_word_count = median(word_count, na.rm = TRUE),
    avg_text_length = mean(text_length, na.rm = TRUE),
    avg_score = mean(score, na.rm = TRUE),
    avg_num_comments = mean(num_comments, na.rm = TRUE),
    n_posts = n(),
    .groups = "drop"
  ) %>%
  rename(topic = assign) %>%
  mutate(
    avg_word_count = round(avg_word_count, 2),
    median_word_count = round(median_word_count, 2),
    avg_text_length = round(avg_text_length, 2),
    avg_score = round(avg_score, 2),
    avg_num_comments = round(avg_num_comments, 2)
  )

# Ensure all topics present
all_topics_df <- tibble(topic = as.integer(topics_chr))
topic_stats <- all_topics_df %>% left_join(topic_stats, by = "topic")

# -------------------------
# TF-IDF similarity assets (loaded via reticulate/scipy if available)
# -------------------------
# We'll try to load:
# - shiny_data/tfidf_sparse.npz   (scipy.sparse matrix of shape (n_docs, n_terms))
# - shiny_data/tfidf_feature_names.json  (vocab in order matching tfidf columns)
# - shiny_data/tfidf_doc_norms.csv (per-doc norms)
# If these are unavailable or fail to load, we'll fall back to topic-based "most probable posts".
tfidf_available <- FALSE
tfidf_py <- NULL
tfidf_vocab <- NULL
tfidf_doc_norms <- NULL

try({
  np <- import("numpy", convert = FALSE)
  sp <- import("scipy.sparse", convert = FALSE)
  # load sparse TF-IDF
  tfidf_path <- file.path(DATA_DIR, "tfidf_sparse.npz")
  vocab_path <- file.path(DATA_DIR, "tfidf_feature_names.json")
  norms_path <- file.path(DATA_DIR, "tfidf_doc_norms.csv")
  if (file.exists(tfidf_path) && file.exists(vocab_path) && file.exists(norms_path)) {
    # load sparse matrix (CSR) using scipy
    tfidf_py <- sp$load_npz(tfidf_path)  # a scipy.sparse matrix (docs x terms)
    # vocabulary
    tfidf_vocab <- jsonlite::fromJSON(vocab_path)
    tfidf_doc_norms <- read_csv(norms_path, show_col_types = FALSE)
    # compute term document-frequencies from sparse matrix
    # getnnz(axis=0) returns array-like; convert to R numeric if needed
    tfidf_available <- TRUE
  } else {
    tfidf_available <- FALSE
  }
}, silent = TRUE)

# -------------------------
# UI
# -------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Medicare Topic Explorer (CMS-style)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Project Summary", tabName = "summary", icon = icon("info-circle")),
      menuItem("Visualizations", tabName = "viz", icon = icon("chart-bar")),
      menuItem("Word Explorer", tabName = "word", icon = icon("search")),
      menuItem("Topic Chatbot", tabName = "chat", icon = icon("comments"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .dataTables_wrapper .dataTables_scrollBody { white-space: normal !important; }
      .chat-result { white-space: normal; word-wrap: break-word; }
      /* Increase boldness for axis labels in ggplot outputs */
      .shiny-plot-output { max-width: 100%; }
    "))),
    tabItems(
      # Summary
      tabItem(
        tabName = "summary",
        fluidRow(
          box(
            width = 12,
            title = "Project Summary",
            status = "primary",
            solidHeader = TRUE,
            HTML(paste0(
              "<h2>Medicare Subreddit Topic Modeling Dashboard</h2>",
              "<h4>Created by: <b>Sara Murley</b></h4>",
              "<p>This dashboard presents an interactive analysis of discussions from the ",
              "<b>r/Medicare</b> subreddit using machine learning-based topic modeling. ",
              "The goal is to help users—patients, caregivers, policy analysts, and CMS-style information seekers—",
              "better understand the major themes, concerns, and informational needs appearing across posts.</p>",
              "<h4>Purpose of this project</h4>",
              "<p>Using Latent Dirichlet Allocation (LDA), this tool identifies core topics in ",
              "public discussion about Medicare. These topics are converted into easy-to-understand CMS-style labels ",
              "and summaries that mimic the tone of official guidance resources. ",
              "This allows users to explore how real people talk about Medicare benefits, coverage, appeals, ",
              "drug plans, enrollment, and other key themes.</p>",
              "<h4>How to use this dashboard</h4>",
              "<ul>",
              "<li><b>Topics & CMS Guidance:</b> Browse each modeled topic along with a CMS-style ",
              "description of what the topic represents and its top associated words.</li>",
              "<li><b>Visualizations:</b> Explore word distributions, dominant terms, and example posts to see how topics form.",
              "</li>",
              "<li><b>Word Explorer:</b> Search any word to see contextual sentences and its strongest associations within ",
              "the dataset. Relevant search terms are highlighted for clarity.</li>",
              "<li><b>Topic Chatbot:</b> Enter a question or statement to receive a topic prediction and friendly CMS-style ",
              "guidance summarizing where a user could go for reliable Medicare information related to that topic.</li>",
              "</ul>",
              "<h4>Project Context</h4>",
              "<p>This analysis was developed as part of a data science and public policy project. ",
              "The broader aim is to understand how individuals seek information, where confusion arises, ",
              "and how policymakers and CMS might improve communication by aligning guidance with public discourse. ",
              "All machine learning components were trained on publicly available Reddit data.</p>",
              "<p>Use the navigation tabs on the left to explore the full functionality of the dashboard.</p>"
            ))
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Topics & CMS Guidance",
            status = "info",
            solidHeader = TRUE,
            uiOutput("topic_cards")
          )
        )
      ),
      
      # Visualizations
      tabItem(tabName = "viz",
              fluidRow(
                box(width = 4, title = "Select topic", status = "primary",
                    selectInput("viz_topic", "Topic:", choices = topics_chr, selected = topics_chr[1]),
                    sliderInput("n_terms", "Number of top terms to show:", min = 5, max = 40, value = 15)
                ),
                box(width = 8, title = "Topic top terms & summary", status = "primary",
                    plotOutput("topic_top_terms_plot", height = "420px"),
                    htmlOutput("term_weight_expl"),
                    DTOutput("topic_top_terms_table")
                )
              ),
              fluidRow(
                box(width = 12, title = "Example documents for topic", status = "primary",
                    DTOutput("topic_example_docs")
                )
              )
      ),
      
      # Word Explorer
      tabItem(tabName = "word",
              fluidRow(
                box(width = 4, title = "Search a word", status = "primary",
                    # Word Explorer input with valid_terms
                    selectizeInput(
                      "query_word",
                      "Enter or choose a token:",
                      choices = valid_terms,
                      selected = "drug",
                      options = list(
                        create = TRUE,
                        placeholder = 'Type or choose a token (e.g., "drug")'
                      )
                    ),
                    actionButton("search_btn", "Search"),
                    br(), br(),
                    helpText("Enter any token from the vocabulary; suggestions: 'drug', 'deductible', 'nursing'.")
                ),
                box(width = 8, title = "Word topic distribution", status = "primary",
                    plotOutput("word_topic_plot"),
                    br(),
                    htmlOutput("word_weight_expl")
                )
              ),
              fluidRow(
                box(width = 7, title = "Co-occurrence network (top neighbors)", status = "primary",
                    plotOutput("cooc_plot", height = "480px"),
                    htmlOutput("cooc_weight_expl")
                ),
                box(width = 5, title = "Example contexts", status = "primary",
                    DTOutput("example_contexts")
                )
              )
      ),
      
      # Chatbot / Topic predictor
      tabItem(tabName = "chat",
              # Keep the input on top, results below as full-width box
              fluidRow(
                box(width = 8, title = "Ask a question", status = "primary",
                    textAreaInput("chat_input", "Enter a question or sentence about Medicare:", rows = 5),
                    actionButton("chat_go", "Predict Topic")
                )
              ),
              fluidRow(
                # FULL WIDTH result box per your request
                box(width = 12, title = "Prediction & CMS guidance", status = "primary",
                    htmlOutput("chat_result_ui"),
                    br(),
                    tableOutput("chat_probs"),
                    br(),
                    plotOutput("chat_probs_plot", height = "250px"),
                    br(),
                    h4("Most similar posts (by TF-IDF cosine similarity)"),
                    DTOutput("chat_similar")
                )
              )
      )
    )
  )
)

# -------------------------
# SERVER
# -------------------------
server <- function(input, output, session) {
  
  # Topic cards for summary tab - show descriptive stats (rounded to 2 decimals)
  output$topic_cards <- renderUI({
    cards <- lapply(topics_chr, function(tid) {
      lbl <- topic_labels[[tid]]
      summ <- topic_summaries[[tid]]
      n <- topic_counts %>% filter(topic == as.integer(tid)) %>% pull(num_posts)
      if (length(n) == 0) n <- 0
      
      stats_row <- topic_stats %>% filter(topic == as.integer(tid))
      avg_wc <- ifelse(is.na(stats_row$avg_word_count), "N/A", sprintf("%.2f", stats_row$avg_word_count))
      med_wc <- ifelse(is.na(stats_row$median_word_count), "N/A", sprintf("%.2f", stats_row$median_word_count))
      avg_txt <- ifelse(is.na(stats_row$avg_text_length), "N/A", sprintf("%.2f", stats_row$avg_text_length))
      avg_sc <- ifelse(is.na(stats_row$avg_score), "N/A", sprintf("%.2f", stats_row$avg_score))
      avg_cm <- ifelse(is.na(stats_row$avg_num_comments), "N/A", sprintf("%.2f", stats_row$avg_num_comments))
      
      box(width = 12, status = "info", title = paste0("Topic ", tid, ": ", lbl),
          HTML(paste0(
            "<b>Estimated posts:</b> ", n,
            "<br/><b>Average word count:</b> ", avg_wc,
            "<br/><b>Median word count:</b> ", med_wc,
            "<br/><b>Average text length (chars):</b> ", avg_txt,
            "<br/><b>Average score:</b> ", avg_sc,
            "<br/><b>Average number of comments:</b> ", avg_cm,
            "<br/><b>Summary:</b> ", summ
          ))
      )
    })
    do.call(tagList, cards)
  })
  
  # TOPIC VISUALIZATIONS
  output$topic_top_terms_plot <- renderPlot({
    req(input$viz_topic)
    tid <- as.integer(input$viz_topic)
    n <- input$n_terms
    df <- top_terms %>% filter(topic == tid) %>% arrange(rank) %>% head(n)
    # Round weights for display to 2 decimals (plot uses original numeric, but labels can show rounded)
    df$weight_r <- round(df$weight, 2)
    ggplot(df, aes(x = reorder(term, weight), y = weight)) +
      geom_col(fill = "#2C7FB8") +
      coord_flip() +
      labs(x = NULL, y = "Weight", title = paste0("Top ", n, " terms for: ", topic_labels[[as.character(tid)]])) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(face = "bold", size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(face = "bold", size = 12)
      )
  })
  
  # Explanation for weight on topic top terms
  output$term_weight_expl <- renderUI({
    HTML("<small><b>What 'weight' means:</b> The 'weight' reported for a term is the model-derived relevance (or contribution) of that term to this topic.
         Higher weight means the term is more strongly associated with the topic in the model. Values shown in tables are rounded to two decimals.</small>")
  })
  
  output$topic_top_terms_table <- renderDT({
    req(input$viz_topic)
    tid <- as.integer(input$viz_topic)
    df <- top_terms %>% filter(topic == tid) %>% arrange(rank) %>%
      mutate(
        term = as.character(term),
        weight = round(weight, 2)
      ) %>%
      select(rank, term, weight)
    datatable(df, options = list(pageLength = 15), rownames = FALSE) %>%
      formatRound(columns = "weight", digits = 2)
  })
  
  output$topic_example_docs <- renderDT({
    req(input$viz_topic)
    tid <- as.integer(input$viz_topic)
    doc_ids <- doc_topic %>%
      mutate(assigned = apply(select(., starts_with("topic_")), 1, which.max) - 1) %>%
      filter(assigned == tid) %>%
      arrange(desc(!!sym(paste0("topic_", tid)))) %>%
      pull(doc_id)
    
    top_docs <- documents %>% filter(doc_id %in% doc_ids) %>%
      select(doc_id, text, score, num_comments, text_length, word_count) %>%
      head(50) %>%
      mutate(
        score = round(as.numeric(score), 2),
        num_comments = round(as.numeric(num_comments), 2),
        text_length = round(as.numeric(text_length), 2),
        word_count = round(as.numeric(word_count), 2)
      )
    
    datatable(top_docs, options = list(pageLength = 10), escape = FALSE) %>%
      formatRound(columns = c("score", "num_comments", "text_length", "word_count"), digits = 2)
  })
  
  # WORD EXPLORER
  word_search_results <- eventReactive(input$search_btn, {
    req(input$query_word)
    w <- tolower(str_trim(input$query_word))
    dfwt <- topic_term_long %>% filter(term == w)
    topic_dist <- dfwt %>% 
      group_by(topic) %>% 
      summarise(weight = sum(weight)) %>% 
      ungroup() %>%
      mutate(weight = weight / sum(weight)) %>%  # normalize to sum 1
      arrange(desc(weight))
    
    # Round weights for display
    topic_dist <- topic_dist %>% mutate(weight = round(weight, 2))
    
    csub <- cooc %>% filter(word == w | neighbor == w) %>%
      mutate(neighbor_term = if_else(word == w, neighbor, word)) %>%
      group_by(neighbor_term) %>%
      summarise(count = sum(count)) %>%
      arrange(desc(count)) %>%
      head(25)
    
    ex <- examples %>% filter(term == w) %>% left_join(documents, by = "doc_id")
    list(word = w, topic_dist = topic_dist, cooc = csub, examples = ex)
  })
  
  output$word_topic_plot <- renderPlot({
    res <- word_search_results()
    if (is.null(res)) return(NULL)
    if (nrow(res$topic_dist) == 0) {
      plot.new(); text(0.5,0.5,paste("Word", res$word, "not found in topics")); return()
    }
    ggplot(res$topic_dist, aes(x = factor(topic), y = weight)) +
      geom_col(fill = "#66C2A5") +
      labs(x = "Topic", y = "Relative weight", title = paste0("Topic distribution for '", res$word, "'")) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(face = "bold", size = 13),
        axis.title.y = element_text(face = "bold", size = 13),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 11)
      )
  })
  
  output$word_weight_expl <- renderUI({
    HTML("<small><b>What 'relative weight' means:</b> This chart shows the proportion of the selected token's total topic weights contributed by each topic. 
Higher values indicate a stronger association of the token with that topic in the model. 
Weights are derived from the topic-term matrix and are normalized so that the sum across all topics equals 1. 
Values shown in tables are rounded to two decimals.</small>
")
  })
  
  # Cooccurrence network plotting tweaks
  output$cooc_plot <- renderPlot({
    res <- word_search_results()
    if (is.null(res)) return(NULL)
    d <- res$cooc
    if (nrow(d) == 0) {
      plot.new(); text(0.5,0.5,"No co-occurrences found."); return()
    }
    edge_list <- res$cooc %>% transmute(from = res$word, to = neighbor_term, weight = count)
    g <- graph_from_data_frame(edge_list, directed = FALSE)
    set.seed(123)
    tryCatch({
      ggraph(g, layout = "fr") +
        geom_edge_link(aes(width = weight), colour = "grey80", alpha = 0.6) +
        geom_node_point(size = 4, color = "black") +
        geom_node_text(aes(label = name), repel = TRUE, size = 4.5, fontface = "bold", color = "black") +
        scale_edge_width(range = c(0.2, 2)) +
        theme_void() +
        ggtitle(paste0("Co-occurrence neighbors for '", res$word, "'")) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }, error = function(e) {
      plot(g, vertex.label.cex = 1, vertex.label.font = 2, vertex.label.color = "black")
    })
  })
  
  output$cooc_weight_expl <- renderUI({
    HTML("<small><b>What edge 'weight' means:</b> Edge thickness corresponds to the number of co-occurrences observed between the main token and the neighbor in the corpus.
         Thicker edges indicate more frequent co-occurrence.</small>")
  })
  
  output$example_contexts <- renderDT({
    res <- word_search_results()
    if (is.null(res)) return(NULL)
    ex <- res$examples
    if (nrow(ex) == 0) {
      df <- data.frame(Context = "No examples found for this word.")
      datatable(df, options = list(dom = 't'), escape = FALSE)
    } else {
      contexts <- ex %>% rowwise() %>% mutate(
        context_html = as.character(get_highlighted_context(text, res$word, max_sentences = 2))
      ) %>% ungroup() %>% select(doc_id, context_html) %>% distinct() %>% head(10)
      datatable(contexts %>% rename(example = context_html), escape = FALSE, options = list(pageLength = 5))
    }
  })
  
  # CHATBOT / PREDICTOR + TF-IDF cosine similarity to find most similar posts
  # CHATBOT / PREDICTOR + TF-IDF cosine similarity + topic probability bar chart
  observeEvent(input$chat_go, {
    req(input$chat_input)
    res <- predict_topic_from_text(input$chat_input, topic_term_long)
    pred_tid <- res$pred
    probs <- res$probs
    
    chat_html <- if (is.na(pred_tid)) {
      "<div class='chat-result'><b>Not enough signal to classify.</b></div>"
    } else {
      lbl <- topic_labels[[pred_tid]]
      summ <- topic_summaries[[pred_tid]]
      paste0("<div class='chat-result'>",
             "<h4>Predicted Topic: ", pred_tid, " — ", lbl, "</h4>",
             "<p><b>CMS Guidance:</b> ", summ, "</p>",
             "<p> The predicted topic probabilities are computed by comparing the words in your query to the topic-term matrix: ",
             "each topic has a set of terms weighted by relevance, and the weights for matching terms in your input are summed and normalized to produce probabilities across topics. ",
             "The 'Most similar posts' section is calculated using TF-IDF cosine similarity: your query is converted into a TF-IDF vector and compared to all documents in the corpus, ",
             "with similarity scores indicating how closely each document's content matches your input. Higher scores indicate closer similarity.</p>",
             "</div>")
    }
    
    output$chat_result_ui <- renderUI({ HTML(chat_html) })
    
    output$chat_probs <- renderTable({
      df <- data.frame(topic = topics_chr, probability = round(probs, 2))
      df$topic_label <- sapply(df$topic, function(t) topic_labels[[t]])
      df %>% select(topic, topic_label, probability)
    }, rownames = FALSE)
    
    output$chat_probs_plot <- renderPlot({
      df <- data.frame(topic = topics_chr, probability = probs)
      df$topic_label <- sapply(df$topic, function(t) topic_labels[[t]])
      ggplot(df, aes(x = reorder(topic_label, probability), y = probability, fill = probability)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(x = "Topic", y = "Predicted Probability", title = "Topic Probabilities for Your Query") +
        scale_fill_gradient(low = "#C6DBEF", high = "#08306B") +
        theme_minimal() +
        theme(axis.text.y = element_text(face = "bold"),
              plot.title = element_text(size = 14, face = "bold"))
    })
    
    # ---- Compute most similar posts using TF-IDF if available ----
    # ---- Compute most similar posts using TF-IDF + cosine similarity ----
    if (tfidf_available) {
      
      try({
        np  <- import("numpy")
        sp  <- import("scipy.sparse")
        
        # --- PREP: load TF-IDF matrix ---
        tfidf_mat <- tfidf_py
        if (!tfidf_mat$getformat() %in% c("csr", "csc")) {
          tfidf_mat <- tfidf_mat$tocsr()
        }
        
        shape <- tfidf_mat$shape
        n_docs <- as.integer(shape[[1]])
        n_terms <- as.integer(shape[[2]])
        
        # --- Compute idf ---
        df_arr <- np$asarray(tfidf_mat$getnnz(axis = 0), dtype="float64")
        idf <- np$log((n_docs + 1.0) / (df_arr + 1.0)) + 1.0
        
        # --- Tokenize user text ---
        vocab <- tfidf_vocab
        vocab_map <- setNames(seq_along(vocab) - 1, vocab)
        
        txt <- tolower(input$chat_input)
        toks <- unlist(strsplit(txt, "\\s+"))
        toks <- toks[toks != ""]
        toks <- gsub("^[^a-z0-9']+|[^a-z0-9']+$", "", toks)
        toks <- toks[toks != ""]
        
        # --- Term counts aligned to vocabulary ---
        user_counts_r <- numeric(length(vocab))
        if (length(toks) > 0) {
          for (tk in toks) {
            if (tk %in% names(vocab_map)) {
              idx <- vocab_map[[tk]] + 1  # R indexing
              user_counts_r[idx] <- user_counts_r[idx] + 1
            }
          }
        }
        
        user_counts_np <- np$array(user_counts_r, dtype="float64")
        
        # --- Compute user TFIDF ---
        user_tfidf_np <- user_counts_np * idf
        user_norm <- as.numeric(np$linalg$norm(user_tfidf_np))
        
        if (user_norm == 0) {
          sims <- rep(0, n_docs)
        } else {
          
          # --- Build sparse user row vector ---
          nz <- which(user_tfidf_np > 0)
          user_vec_csr <- sp$csr_matrix(
            tuple(
              list(as.numeric(user_tfidf_np[nz])),
              list(as.integer(nz - 1)),      # zero-based indices
              list(np$array(c(0, length(nz)), dtype="int32"))
            ),
            shape = tuple(1L, n_terms)
          )
          
          # --- Compute similarities ---
          doc_scores_np <- user_vec_csr$dot(tfidf_mat$T)
          doc_scores_arr <- as.numeric(doc_scores_np$toarray())
          
          # --- Normalize ---
          doc_norms_np <- np$array(tfidf_doc_norms$norm, dtype="float64")
          sims <- doc_scores_arr / (doc_norms_np * user_norm)
          sims[is.na(sims)] <- 0
        }
        
        # --- Return top documents ---
        top_idx <- order(sims, decreasing=TRUE)[1:5]
        output$chat_similar_posts <- renderTable({
          data.frame(
            rank = 1:5,
            similarity = round(sims[top_idx], 3),
            post_title = tfidf_doc_titles[top_idx],
            post_id = tfidf_doc_ids[top_idx]
          )
        }, rownames = FALSE)
        
      }, silent = TRUE)
      
    }
    else {
      # Fallback: Not able to do TF-IDF similarity. Use topic-probability top docs (old behavior).
      sim_docs <- tibble()
      if (!is.na(pred_tid)) {
        topic_col <- paste0("topic_", pred_tid)
        if (topic_col %in% names(doc_topic)) {
          sim_docs <- doc_topic %>%
            select(doc_id, !!sym(topic_col)) %>%
            arrange(desc(!!sym(topic_col))) %>%
            left_join(documents, by = "doc_id") %>%
            mutate(probability = round(!!sym(topic_col), 4)) %>%
            select(doc_id, probability, text) %>%
            mutate(text_snippet = substr(text, 1, 300)) %>%
            select(doc_id, probability, text_snippet) %>%
            head(10)
        }
      }
      output$chat_similar <- renderDT({
        if (nrow(sim_docs) == 0) {
          datatable(data.frame(Message = "No similar posts found or TF-IDF assets missing."), options = list(dom = 't'))
        } else {
          datatable(sim_docs, options = list(pageLength = 5), escape = FALSE) %>%
            formatRound(columns = "probability", digits = 2)
        }
      })
    } # end tfidf_available branch
    
  }) # end observeEvent chat_go
  
}

# Run the app
shinyApp(ui, server)
