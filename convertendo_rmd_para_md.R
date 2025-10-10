rmarkdown::render("./tabelas_modelos_lineares/README.Rmd",
                  rmarkdown::md_document(variant = "gfm"),
                  output_options = list(fig_path = "man/figures/"))
