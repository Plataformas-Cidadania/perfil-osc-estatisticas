

# Preview de capítulo
bookdown::preview_chapter("03-territorio.Rmd")

bookdown::preview_chapter("06-natjur.Rmd", output_format = "bookdown::word_document2")
bookdown::preview_chapter("06-natjur.Rmd", output_format = bookdown::pdf_book(fig_crop = FALSE))


?bookdown::word_document2

# Rendereza o livro
bookdown::render_book()
bookdown::render_book(output_format = "all")
bookdown::render_book(output_format = "bookdown::word_document2")
bookdown::render_book(output_format = bookdown::pdf_book(fig_crop = FALSE))


# Resolve problemas de conexão:
# Conta murilooj@gmail.com
rsconnect::connectUser(account = "mosc_ipea", server = 'bookdown.org')

# Publica no site
bookdown::publish_book(account = "mosc_ipea", 
                       server = 'bookdown.org', 
                       render = "local")


# Tentativas de resolver a renderização LateX/PDF (ainda não bem sucedida)
update.packages(ask = FALSE, checkBuilt = TRUE)
tinytex::tlmgr_update()

tinytex::reinstall_tinytex()

install.packages("cli")

# No terminal:
# quarto tools install tinytex


install.packages('tinytex')

# setup the external stuff and configure
tinytex::install_tinytex()


library(utils)
remove.packages('tinytex')
