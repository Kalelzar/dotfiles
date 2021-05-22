STACK_SIZE='262144' #Increase stack size to 256 MiB

compile: buildPkg
	GLOBALSZ=${STACK_SIZE} gplc --no-top-level package.pl equiv-main.pl  ${XDG_CONFIG_HOME}/pkgs/* -o pl-equivalent-pkgs
	GLOBALSZ=${STACK_SIZE} gplc --no-top-level package.pl alt-main.pl  ${XDG_CONFIG_HOME}/pkgs/* -o pl-alternative-pkgs
	GLOBALSZ=${STACK_SIZE} gplc --no-top-level package.pl main-local.pl  ${XDG_CONFIG_HOME}/pkgs/* -o pl-pkgs-local
	GLOBALSZ=${STACK_SIZE} gplc package.pl ${XDG_CONFIG_HOME}/pkgs/* -o pl-equivalent-pkgs -o pl-pkgs-interactive

buildPkg:
	@echo "Generating knowledge base"
	make-package-db
