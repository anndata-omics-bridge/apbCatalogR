SITE_CMD = Rscript -e "altdoc::render_docs()"

# BiocCheck the built tarball, as Bioconductor's builder does: .Rbuildignore
# keeps local site folders such as _quarto/ out of it.
CHECK_BIOC_CMD = Rscript -e "BiocCheck::BiocCheck('$(TARBALL)')"
check-bioc: build

# Ship an apb-catalog release's bundles: unpack the two archives attached to
# release TAG (default: the latest) in place of inst/extdata/bundle/ and
# inst/extdata/test-bundle/, run the full check and commit both folders.
# Pushing stays a separate step. The recipe runs make check's commands itself:
# a recipe line with $(MAKE) in it runs even under make -n.
APB_CATALOG_REPO := anndata-omics-bridge/apb-catalog
.PHONY: update-bundle
update-bundle:
	@set -e; \
	tag="$(TAG)"; \
	[ -n "$$tag" ] || tag=$$(gh release view -R $(APB_CATALOG_REPO) --json tagName -q .tagName); \
	rm -rf inst/extdata/bundle inst/extdata/test-bundle; \
	for name in bundle test-bundle; do \
		gh release download "$$tag" -R $(APB_CATALOG_REPO) \
			-p "apb-catalog-$$name.tar.gz" -O - | tar xzf - -C inst/extdata; \
		mv "inst/extdata/apb-catalog-$$name" "inst/extdata/$$name"; \
	done; \
	git add -A inst/extdata/bundle inst/extdata/test-bundle; \
	if git diff --cached --quiet -- inst/extdata/bundle inst/extdata/test-bundle; then \
		echo "apb-catalog $$tag: bundles unchanged"; exit 0; \
	fi; \
	$(DOCUMENT_CMD); \
	$(BUILD_CMD); \
	$(CHECK_CMD); \
	git commit -m "Bundles of the apb-catalog $$tag release" \
		-- inst/extdata/bundle inst/extdata/test-bundle; \
	echo "Committed the apb-catalog $$tag bundles; push when ready"

help-package:
	@echo ""
	@echo "Package:"
	@echo "  make update-bundle    - ship an apb-catalog release's bundles"
	@echo "                          (TAG=vX.Y.Z, default latest), check, commit"
