SITE_CMD = Rscript -e "altdoc::render_docs()"

# BiocCheck the built tarball, as Bioconductor's builder does: .Rbuildignore
# keeps local site folders such as _quarto/ out of it.
CHECK_BIOC_CMD = Rscript -e "BiocCheck::BiocCheck('$(TARBALL)')"
check-bioc: build
