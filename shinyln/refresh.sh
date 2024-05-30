guix package -r shinyln
guix package --delete-generations
guix gc
guix package --install-from-file=guix.scm
