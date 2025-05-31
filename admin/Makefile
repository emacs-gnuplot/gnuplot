src = gnuplot-source

regen:
	# Download and extract the Gnuplot source
	[ -e $(src).tar.xz ] || curl -o $(src).tar.xz http://deb.debian.org/debian/pool/main/g/gnuplot/gnuplot_6.0.2+dfsg1.orig.tar.xz
	rm -rf $(src)/
	mkdir $(src)
	tar --strip-components=1 -C $(src) -xf $(src).tar.xz

	# Run doc2texi.el inside the $(src)/docs directory
	cp doc2texi.el $(src)/docs
	(cd $(src)/docs; emacs --batch -l doc2texi.el -f d2t-doc-to-texi)

	# Make sure that the info file compiles cleanly
	(cd $(src)/docs; makeinfo --no-split gnuplot.texi)

	# Copy the updated gnuplot-eldoc.el and gnuplot.texi to the package directory
	cp $(src)/docs/gnuplot.texi $(src)/docs/gnuplot-eldoc.el ..
