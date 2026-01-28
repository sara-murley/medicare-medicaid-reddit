# medicare-medicaid-reddit
Topic modeling of Reddit posts to identify patterns in user experience with Medicare and Medicaid.

---

## Project Overview
This project performs topic modeling and clustering analysis on Reddit posts from selected subreddits (`Medicare`, `Medicaid`, and `HealthInsurance`) to uncover common themes and patterns in user experiences. An interactive dashboard summarizes the findings and allows exploration of key topics and associated terms.

**Interactive Dashboard:** [View Dashboard](https://smurley3.shinyapps.io/final_proj/)

> **Note:** The code results in this repository do **not** exactly match the final project report or dashboard. The code was rerun after the report submission, so minor differences in topic weights or clusters may occur.

---

## Directory Structure
```
project_root/
│
├── code/
│ ├── 01-reddit-api.ipynb # Collect Reddit posts and top comments using Reddit API
│ ├── 02-EDA.ipynb # Exploratory data analysis on collected posts and comments
│ ├── 03-preprocessing.ipynb # Text preprocessing and TF-IDF/CountVectorizer transformations
│ ├── 04-clustering.ipynb # Topic modeling (LDA), clustering, and generation of topic-term matrices
│ └── app.R # Shiny app code for interactive visualization
│
├── data/
│ └── shiny-data/ # Contains all preprocessed data files needed for the Shiny app
│
├── figures/ # Generated plots and visualizations from EDA and clustering
│
└── medicare_medicaid_insight_report.pdf # Final project report
```

## Notebook Descriptions

### `01-reddit-api.ipynb`
- Collects Reddit posts and top comments using PRAW (Python Reddit API Wrapper).  
- Filters posts by keywords (`Medicare`, `Medicaid`).  
- Stores data as CSVs for downstream analysis.

### `02-EDA.ipynb`
- Performs exploratory data analysis on collected Reddit data.  
- Includes:
  - Post and comment counts over time
  - Distribution of post lengths
  - Top contributors and active subreddits
- Generates summary tables and figures.

### `03-preprocessing.ipynb`
- Prepares text for topic modeling:
  - Tokenization, lowercasing
  - Stopword removal
  - Optional removal of overly frequent terms
- Converts text into CountVectorizer or TF-IDF matrices.

### `04-clustering.ipynb`
- Performs Latent Dirichlet Allocation (LDA) for topic modeling.
- Generates:
  - Topic-term matrices
  - Topic probabilities
  - Long-form CSVs for topic-term associations
- Visualizes clustering results using heatmaps and bar plots.

### `app.R`
- Shiny app for interactive exploration of topic modeling results.
- Reads preprocessed files from `data/shiny-data`.
- Provides interactive plots, tables, and topic search functionality.

---

## Reproducibility

### Requirements
- Python 3.9+
- R with Shiny installed
- Python packages: `pandas`, `numpy`, `scikit-learn`, `praw`, `tqdm`, `matplotlib`, `seaborn`
- Install R packages for Shiny: `shiny`, `shinythemes`, `DT`, `plotly`

1. Clone the repository
2. Set up Reddit API crednetials in a `.env` file at the project root.
```text
REDDIT_CLIENT_ID=your_client_id_here
REDDIT_CLIENT_SECRET=your_client_secret_here
REDDIT_USER_AGENT=healthcare-analysis by u/yourusername
```

3. Install Python dependencies
4. Run notebooks in order
5. Launch Shiny app

## Data Notes

- The raw Reddit data and preprocessed data are not included in the repository due to size and privacy considerations.
- If you want to collect fresh Reddit data, follow the API setup above.














