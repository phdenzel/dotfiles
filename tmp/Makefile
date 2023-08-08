BUILD ?= .config

CONF_DIR ?= $(HOME)/.config

PYEXE ?= $(shell which python)
GENPY ?= generate_scheme.py
OPT ?= 

.DEFAULT_GOAL := build

.PHONY: clean
clean:
	rm -rf $(BUILD) __pycache__

.PHONY: uninstall
uninstall: clean
	echo "NotYetImplemented"

.PHONY: install
install: build
	echo "NotYetImplemented"

.PHONY: build
build: alacritty bat dircolors


ALACRITTYDIR = $(BUILD)/alacritty/themes
alacritty-themes = $(ALACRITTYDIR)/phd-ark-dark.toml \
									 $(ALACRITTYDIR)/phd-ark-default.toml \
									 $(ALACRITTYDIR)/phd-ark-light.toml
.PHONY: alacritty
alacritty: $(alacritty-themes)


BATDIR = $(BUILD)/bat/themes
bat-themes = $(BATDIR)/phd-ark-default.tmTheme \
						 $(BATDIR)/phd-ark-dark.tmTheme \
						 $(BATDIR)/phd-ark-light.tmTheme
.PHONY: bat
bat: $(bat-themes)

DIRCOLORSDIR = $(BUILD)/dircolors
dircolors-themes = $(DIRCOLORSDIR)/dircolors-default \
				  				 $(DIRCOLORSDIR)/dircolors-dark \
									 $(DIRCOLORSDIR)/dircolors-light
.PHONY: dircolors
dircolors: $(dircolors)
	( test -f $(DIRCOLORSDIR)/dircolors-default && \
		mv $(DIRCOLORSDIR)/dircolors-default $(DIRCOLORSDIR)/dircolors )


$(alacritty-themes) $(bat-themes) $(dircolors-themes):
	mkdir -p $(@D)
	@$(eval template=$(subst default,template,$(@F)))
	@$(eval template=$(subst dark,template,$(template)))
	@$(eval template=$(subst light,template,$(template)))
	@echo "$(templ)"
	$(PYEXE) $(GENPY) $(OPT) -i $(template) -o $@

