BUILD ?= build
BUILDC ?= build/.config
TARGETDIR ?= ..
CONFDIR ?= $(HOME)/.config

PYEXE ?= $(shell which python)
GENPY ?= generate_scheme.py
OPT ?= --save
FLAVOR ?= default

.DEFAULT_GOAL := build

.PHONY: build
build: svg alacritty bat btop dircolors dunst emacs fzf git sublime vim xmonad xresources

.PHONY: install
install: build
	@rsync -avh $(BUILD)/ $(TARGETDIR)

.PHONY: clean
clean:
	rm -rf $(BUILD) __pycache__


ALACRITTYDIR = $(BUILDC)/alacritty/themes
alacritty-themes = $(ALACRITTYDIR)/phd-ark-dark.toml \
									 $(ALACRITTYDIR)/phd-ark-default.toml \
									 $(ALACRITTYDIR)/phd-ark-light.toml
.PHONY: alacritty
alacritty: $(alacritty-themes)


BATDIR = $(BUILDC)/bat/themes
bat-themes = $(BATDIR)/phd-ark-default.tmTheme \
						 $(BATDIR)/phd-ark-dark.tmTheme \
						 $(BATDIR)/phd-ark-light.tmTheme
.PHONY: bat
bat: $(bat-themes)


BTOPDIR = $(BUILDC)/btop/themes
btop-themes = $(BTOPDIR)/btop-phd-ark-default.theme \
							$(BTOPDIR)/btop-phd-ark-dark.theme \
							$(BTOPDIR)/btop-phd-ark-light.theme
.PHONY: btop
btop: $(btop-themes)
	@if [ -s "$(BTOPDIR)/btop-phd-ark-default.theme" ]; then mv $(BTOPDIR)/btop-phd-ark-default.theme $(BTOPDIR)/phd-ark-default.theme; fi
	@if [ -s "$(BTOPDIR)/btop-phd-ark-dark.theme" ]; then mv $(BTOPDIR)/btop-phd-ark-dark.theme $(BTOPDIR)/phd-ark-dark.theme; fi
	@if [ -s "$(BTOPDIR)/btop-phd-ark-light.theme" ]; then mv $(BTOPDIR)/btop-phd-ark-light.theme $(BTOPDIR)/phd-ark-light.theme; fi


DIRCOLORSDIR = $(BUILDC)/dircolors
dircolors-themes = $(DIRCOLORSDIR)/dircolors-default \
				  				 $(DIRCOLORSDIR)/dircolors-dark \
									 $(DIRCOLORSDIR)/dircolors-light
.PHONY: dircolors
dircolors: $(dircolors-themes)
	@if [ -s "$(DIRCOLORSDIR)/dircolors-$(FLAVOR)" ]; then cp $(DIRCOLORSDIR)/dircolors-$(FLAVOR) $(DIRCOLORSDIR)/dircolors; fi


DUNSTDIR = $(BUILDC)/dunst
dunst-themes = $(DUNSTDIR)/dunstrc-default \
							 $(DUNSTDIR)/dunstrc-dark \
							 $(DUNSTDIR)/dunstrc-light
.PHONY: dunst
dunst: $(dunst-themes)
	@if [ -s "$(DUNSTDIR)/dunstrc-$(FLAVOR)" ]; then cp $(DUNSTDIR)/dunstrc-$(FLAVOR) $(DUNSTDIR)/dunstrc; fi


EMACSDIR = $(BUILDC)/emacs
emacs-themes = $(EMACSDIR)/phd-ark-theme-all.el
.PHONY: emacs
emacs: $(emacs-themes)
	@if [ -s "$(emacs-themes)" ]; then mv $(emacs-themes) $(EMACSDIR)/phd-ark-theme.el; fi


FZFDIR = $(BUILDC)/fzf/default_opts
fzf-themes = $(FZFDIR)/phd-ark-default.fzf \
						 $(FZFDIR)/phd-ark-dark.fzf \
						 $(FZFDIR)/phd-ark-light.fzf
.PHONY: fzf
fzf: $(fzf-themes)


GITDIR = $(BUILDC)/git/themes
git-themes = $(GITDIR)/phd-ark-default.gitcolor \
						 $(GITDIR)/phd-ark-dark.gitcolor \
						 $(GITDIR)/phd-ark-light.gitcolor
.PHONY: git
git: $(git-themes)


SUBLDIR = $(BUILDC)/sublime-text/Packages
subl-themes = $(SUBLDIR)/phd-ark-default.sublime-color-scheme \
							$(SUBLDIR)/phd-ark-dark.sublime-color-scheme \
							$(SUBLDIR)/phd-ark-light.sublime-color-scheme
.PHONY: sublime
sublime: $(subl-themes)


VIMCOLODIR = $(BUILDC)/vim/colors
vim-themes = $(VIMCOLODIR)/phd-ark-default.vim \
						 $(VIMCOLODIR)/phd-ark-dark.vim \
						 $(VIMCOLODIR)/phd-ark-light.vim
.PHONY: vim
vim: $(vim-themes)


XMONADDIR = $(BUILDC)/xmonad/lib/Colors
xmonad-themes = $(XMONADDIR)/PhDArkDefault.hs \
								$(XMONADDIR)/PhDArkDark.hs \
								$(XMONADDIR)/PhDArkLight.hs
.PHONY: xmonad
xmonad: $(xmonad-themes)


xresources-themes = $(BUILD)/.Xresources-$(FLAVOR)
.PHONY: xresources
xresources: $(xresources-themes)
	sed '0,/^\!---\ Color\ config*/I!d' $(TARGETDIR)/.Xresources | sed '$$d' > $(BUILD)/.Xresources
	cat $(xresources-themes) >> $(BUILD)/.Xresources
	rm $(xresources-themes)


SVGDIR = $(BUILD)/imgs/palettes
svg-themes = $(SVGDIR)/phd-ark-default \
						 $(SVGDIR)/phd-ark-dark \
						 $(SVGDIR)/phd-ark-light
.PHONY: svg
svg: $(svg-themes)
$(svg-themes):
	mkdir -p $@
	$(PYEXE) $(GENPY) $(OPT) --palette -o $@
	convert -background none -resize 200x200 $@/*.svg -set filename:fn '%[basename]' $@/'%[filename:fn].png'



$(alacritty-themes) $(bat-themes) $(btop-themes) $(dircolors-themes) $(dunst-themes) $(emacs-themes) $(fzf-themes) $(git-themes) $(subl-themes) $(vim-themes) $(xmonad-themes) $(xresources-themes):
	mkdir -p $(@D)
	@$(eval template=$(subst Default,Template,$(@F)))
	@$(eval template=$(subst default,template,$(template)))
	@$(eval template=$(subst Dark,Template,$(template)))
	@$(eval template=$(subst dark,template,$(template)))
	@$(eval template=$(subst Light,Template,$(template)))
	@$(eval template=$(subst light,template,$(template)))
	@$(eval template=$(subst all,template,$(template)))
	@echo "$(template)"
	$(PYEXE) $(GENPY) $(OPT) -i $(template) -o $@
