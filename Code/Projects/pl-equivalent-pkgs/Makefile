compile: buildPkg
	gplc --no-top-level package.pl equiv-main.pl  ${XDG_CONFIG_HOME}/pkgs/* -o pl-equivalent-pkgs
	gplc --no-top-level package.pl main-local.pl  ${XDG_CONFIG_HOME}/pkgs/* -o pl-pkgs-local
	gplc package.pl ${XDG_CONFIG_HOME}/pkgs/* -o pl-equivalent-pkgs -o pl-pkgs-interactive

buildPkg:
	@echo "Generating knowledge base"
	make-package-db
