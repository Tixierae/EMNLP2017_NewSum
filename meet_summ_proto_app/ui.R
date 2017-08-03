library(metricsgraphics)
library(wordcloud2)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("journal"),

	titlePanel("Graph degeneracy and submodularity for unsupervised extractive summarization"),
	h5("Demo of this", a("EMNLP'17 paper",href = "http://www.lix.polytechnique.fr/~anti5662/emnlp_2017_summ_workshop_final_tixier.pdf",target="_blank")),

	fluidRow(
		column(2, 
			selectInput("corpus", "Select data source",choices = c("AMI_corpus","ICSI_corpus","DUC01_corpus","custom"), selected="DUC01_corpus"
			)
		),
		column(4,
			uiOutput("ui")
		),
		column(2,
			uiOutput("ui_slider")
		),
		column(1,
			actionButton("goButton", "Summarize!")
		)
	),

	fluidRow(
		tableOutput("meeting_info")
	),

	fluidRow(
		textOutput("detected_language")
	),
	
	br(),

	fluidRow(
		tabsetPanel(
			tabPanel("Summaries",
				fluidRow(
					wellPanel(
						h5("ROUGE scores"),
						textOutput("rouge")
					)
				),

				fluidRow(
					conditionalPanel("input.corpus == 'AMI_corpus' || input.corpus == 'DUC01_corpus'",
						column(6,
							wellPanel(
								h5("System"),
								htmlOutput("summary")
							)
						),
						column(6,
							wellPanel(
								h5("Human"),
								htmlOutput("golden_summary")
							)
						)
					),

					conditionalPanel("input.corpus == 'custom'",
						column(6,
							wellPanel(
								h5("System"),
								htmlOutput("summary_custom")
							)
						)
					),

					conditionalPanel("input.corpus == 'ICSI_corpus'",

						tabsetPanel(
							tabPanel("1",
								column(6,
									wellPanel(
										h5("System"),
										htmlOutput("summary_1")
									)
								),
								column(6,
									wellPanel(
										h5("Human"),
										htmlOutput("golden_summary_1")
									)
								)
							),
							tabPanel("2",
								column(6,
									wellPanel(
										h5("System"),
										htmlOutput("summary_2")
									)
								),
								column(6,
									wellPanel(
										h5("Human"),
										htmlOutput("golden_summary_2")
									)
								)
							),
							tabPanel("3",
								column(6,
									wellPanel(
										h5("System"),
										htmlOutput("summary_3")
								)
								),
								column(6,
									wellPanel(
										h5("Human"),
										htmlOutput("golden_summary_3")
									)
								)
							)
						)
					)
				)
			),

			tabPanel("Keywords",
				fluidRow(
					downloadButton('keywords_scores', 'keywords_scores')
				),
				fluidRow(
					column(12,
						wordcloud2Output("wordcloud")
					)
				),
				fluidRow(
					column(10,
						metricsgraphicsOutput('keywords_barplot',width="50%", height="800px")
					)
				)
			)
		)
	),
	
	fluidRow(
		h5("- Created by", a(" Antoine Tixier",href = "http://www.lix.polytechnique.fr/Labo/Antoine.Tixier/",target="_blank"), "for", a("DaSciM",href = "http://www.lix.polytechnique.fr/dascim/",target="_blank"), " with", a("RShiny",href = "http://shiny.rstudio.com/",target="_blank"),a("hash",href = "https://cran.r-project.org/web/packages/hash/index.html",target="_blank"),a("stringr",href = "https://cran.r-project.org/web/packages/stringr/vignettes/stringr.html",target="_blank"),a("SnowballC",href = "https://cran.r-project.org/web/packages/SnowballC/index.html",target="_blank"),a("igraph",href = "http://igraph.org/r/",target="_blank"),a("textcat",href = "https://cran.r-project.org/web/packages/textcat/index.html",target="_blank"),a("wordcloud2",href = "https://cran.rstudio.com/web/packages/wordcloud2/",target="_blank"), "and", a("metricsgraphics",href ="https://cran.r-project.org/web/packages/metricsgraphics/index.html",target="_blank")),
		p(),
		h5("- Code available at:", a("https://github.com/Tixierae/EMNLP2017_NewSum",href = "https://github.com/Tixierae/EMNLP2017_NewSum",target="_blank")),
		p(),
		em("- Last updated: August 2017 (added support for traditional documents)")
	)
))