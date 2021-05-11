compile:
	gplc --no-top-level package.pl ${XDG_CONFIG_HOME}/pkgs/* -o pl-equivalent-pkgs

buildPacman:
	@echo "Generating pacman knowledge base"
	@pacman -Qs -q > packages.temp
	@sort -u < packages.temp\
	| sed -E\
	      -e 's/^/start__/g'\
	      -e 's/\-/__minus/g'\
	      -e 's/\+/__plus/g'\
	      -e 's/\./__dot/g'\
	| sed -E 's/(.*)/pacman(\1)./g' > ${XDG_CONFIG_HOME}/pkgs/pacman.pl
	@rm packages.temp
