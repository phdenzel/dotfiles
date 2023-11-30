BUILD ?= build
BUILDC ?= build/.config
TARGETDIR ?= ..
CONFDIR ?= $(HOME)/.config

PYEXE ?= $(shell which python)
GENPY ?= generate_scheme.py
OPT ?= --save
FLAVOR ?= iridis

.DEFAULT_GOAL := build

.PHONY: build
build: alacritty bat btop dircolors dunst emacs fzf freshrss git grep highlight hyprland mailspring shell sublime thunderbird rofi tofi vim waybar wlogout xmonad xresources

.PHONY: install
install: build
	@rsync -avh $(BUILD)/ $(TARGETDIR)

.PHONY: clean
clean:
	rm -rf $(BUILD) __pycache__


ALACRITTYDIR = $(BUILDC)/alacritty/themes
alacritty-themes = $(ALACRITTYDIR)/phd-ark-dark.toml \
                   $(ALACRITTYDIR)/phd-ark-iridis.toml \
                   $(ALACRITTYDIR)/phd-ark-light.toml
.PHONY: alacritty
alacritty: $(alacritty-themes)


BATDIR = $(BUILDC)/bat/themes
bat-themes = $(BATDIR)/phd-ark-iridis.tmTheme \
             $(BATDIR)/phd-ark-dark.tmTheme \
             $(BATDIR)/phd-ark-light.tmTheme
.PHONY: bat
bat: $(bat-themes)


BTOPDIR = $(BUILDC)/btop/themes
btop-themes = $(BTOPDIR)/btop-phd-ark-iridis.theme \
              $(BTOPDIR)/btop-phd-ark-dark.theme \
              $(BTOPDIR)/btop-phd-ark-light.theme
.PHONY: btop
btop: $(btop-themes)
	@if [ -s "$(BTOPDIR)/btop-phd-ark-iridis.theme" ]; then mv $(BTOPDIR)/btop-phd-ark-iridis.theme $(BTOPDIR)/phd-ark-iridis.theme; fi
	@if [ -s "$(BTOPDIR)/btop-phd-ark-dark.theme" ]; then mv $(BTOPDIR)/btop-phd-ark-dark.theme $(BTOPDIR)/phd-ark-dark.theme; fi
	@if [ -s "$(BTOPDIR)/btop-phd-ark-light.theme" ]; then mv $(BTOPDIR)/btop-phd-ark-light.theme $(BTOPDIR)/phd-ark-light.theme; fi


DIRCOLORSDIR = $(BUILDC)/dircolors
dircolors-themes = $(DIRCOLORSDIR)/dircolors-iridis \
                   $(DIRCOLORSDIR)/dircolors-dark \
                   $(DIRCOLORSDIR)/dircolors-light
.PHONY: dircolors
dircolors: $(dircolors-themes)
	@if [ -s "$(DIRCOLORSDIR)/dircolors-$(FLAVOR)" ]; then cd $(DIRCOLORSDIR) && ln -sf dircolors-$(FLAVOR) dircolors; fi


DUNSTDIR = $(BUILDC)/dunst
dunst-themes = $(DUNSTDIR)/dunstrc-iridis \
               $(DUNSTDIR)/dunstrc-dark \
               $(DUNSTDIR)/dunstrc-light
.PHONY: dunst
dunst: $(dunst-themes)
	@if [ -s "$(DUNSTDIR)/dunstrc-$(FLAVOR)" ]; then cd $(DUNSTDIR) && ln -sf dunstrc-$(FLAVOR) dunstrc; fi


EMACSDIR = $(BUILDC)/emacs
emacs-themes = $(EMACSDIR)/phd-ark-theme-all.el
.PHONY: emacs
emacs: $(emacs-themes)
	@if [ -s "$(emacs-themes)" ]; then mv $(emacs-themes) $(EMACSDIR)/phd-ark-theme.el; fi


FRESHRSSDIR = $(BUILDC)/freshrss/themes
freshrss-themes = $(FRESHRSSDIR)/phd-ark-iridis \
                  $(FRESHRSSDIR)/phd-ark-dark \
                  $(FRESHRSSDIR)/phd-ark-light
.PHONY: freshrss
freshrss: $(freshrss-themes)
$(freshrss-themes):
	mkdir -p "$@"
	cp -r freshrss-phd-ark-theme/icons $@/
	cp freshrss-phd-ark-theme/LICENSE $@/
	cp freshrss-phd-ark-theme/loader.gif $@/
	$(PYEXE) $(GENPY) $(OPT) \
		-i freshrss-phd-ark-theme/metadata.json -o $@/metadata.json
	$(PYEXE) $(GENPY) $(OPT) \
		-i freshrss-phd-ark-theme/README.md -o $@/README.md
	@$(eval template=$(subst -iridis,-template,$(@F)))
	@$(eval template=$(subst -dark,-template,$(template)))
	@$(eval template=$(subst -light,-template,$(template)))
	@echo "$(template)"
	$(PYEXE) $(GENPY) $(OPT) -i freshrss-phd-ark-theme/$(template).css -o $@/$(@F).css
	$(PYEXE) $(GENPY) $(OPT) -i freshrss-phd-ark-theme/$(template).rtl.css -o $@/$(@F).rtl.css


FZFDIR = $(BUILDC)/fzf/default_opts
fzf-themes = $(FZFDIR)/phd-ark-iridis.fzf \
             $(FZFDIR)/phd-ark-dark.fzf \
             $(FZFDIR)/phd-ark-light.fzf
.PHONY: fzf
fzf: $(fzf-themes)


GITDIR = $(BUILDC)/git/themes
git-themes = $(GITDIR)/phd-ark-iridis.gitcolor \
             $(GITDIR)/phd-ark-dark.gitcolor \
             $(GITDIR)/phd-ark-light.gitcolor
.PHONY: git
git: $(git-themes)


GREPDIR = $(BUILDC)/grep/colors
grep-themes = $(GREPDIR)/phd-ark-iridis.grep \
              $(GREPDIR)/phd-ark-dark.grep \
              $(GREPDIR)/phd-ark-light.grep
.PHONY: grep
grep: $(grep-themes)

HIGHLIGHTDIR = $(BUILD)/.themes
highlight-themes = $(HIGHLIGHTDIR)/phd-ark-highlight-iridis.theme \
                   $(HIGHLIGHTDIR)/phd-ark-highlight-dark.theme \
                   $(HIGHLIGHTDIR)/phd-ark-highlight-light.theme
.PHONY: highlight
highlight: $(highlight-themes)

HYPRLANDDIR = $(BUILDC)/hypr/themes
hyprland-themes = $(HYPRLANDDIR)/hyprland-phd-ark-iridis.conf \
                  $(HYPRLANDDIR)/hyprland-phd-ark-dark.conf \
                  $(HYPRLANDDIR)/hyprland-phd-ark-light.conf
.PHONY: hyprland
hyprland: $(hyprland-themes)

MAILSPRINGDIR = $(BUILDC)/Mailspring/packages
mailspring-themes = $(MAILSPRINGDIR)/phd-ark-iridis \
                    $(MAILSPRINGDIR)/phd-ark-dark \
                    $(MAILSPRINGDIR)/phd-ark-light
.PHONY: mailspring
mailspring: $(mailspring-themes)
$(mailspring-themes):
	mkdir -p $@/styles
	$(PYEXE) $(GENPY) $(OPT) \
		-i mailspring-phd-ark-theme/package.json \
		-o $@/package.json
	$(PYEXE) $(GENPY) $(OPT) \
		-i mailspring-phd-ark-theme/styles/main.less \
		-o $@/styles/main.less
	$(PYEXE) $(GENPY) $(OPT) \
		-i mailspring-phd-ark-theme/styles/email-frame.less \
		-o $@/styles/email-frame.less
	$(PYEXE) $(GENPY) $(OPT) \
		-i mailspring-phd-ark-theme/styles/ui-variables.less \
		-o $@/styles/ui-variables.less


SHELLDIR = $(BUILDC)/shell/themes
shell-themes = $(SHELLDIR)/phd-ark-iridis.sh \
               $(SHELLDIR)/phd-ark-dark.sh \
               $(SHELLDIR)/phd-ark-light.sh
.PHONY: shell
shell: $(shell-themes)


SUBLDIR = $(BUILDC)/sublime-text/Packages
subl-themes = $(SUBLDIR)/phd-ark-iridis.sublime-color-scheme \
              $(SUBLDIR)/phd-ark-dark.sublime-color-scheme \
              $(SUBLDIR)/phd-ark-light.sublime-color-scheme
.PHONY: sublime
sublime: $(subl-themes)


TBIRDDIR = $(BUILDC)/thunderbird/themes
thunderbird-themes = $(TBIRDDIR)/phd-ark-iridis.xpi \
                     $(TBIRDDIR)/phd-ark-dark.xpi \
                     $(TBIRDDIR)/phd-ark-light.xpi
.PHONY: thunderbird
thunderbird: $(thunderbird-themes)
$(thunderbird-themes):
	mkdir -p $(basename $@)/images
	cp $(TARGETDIR)/imgs/logos/icon{16,48,128}.png $(basename $@)/images/
	cp thunderbird-phd-ark-theme/style.css $(basename $@)/style.css
	$(PYEXE) $(GENPY) $(OPT) \
		-i thunderbird-phd-ark-theme/manifest.json \
		-o $(basename $@)/manifest.json
	cd $(basename $@) && zip -r -q -9 ../$(@F) . && cd - && rm -r $(basename $@)


TOFIDIR = $(BUILDC)/tofi/themes
tofi-themes = $(TOFIDIR)/phd-ark-iridis.tofi \
              $(TOFIDIR)/phd-ark-dark.tofi \
              $(TOFIDIR)/phd-ark-light.tofi
.PHONY: tofi
tofi: $(tofi-themes)


ROFIDIR = $(BUILDC)/rofi
rofi-themes = $(ROFIDIR)/phd-ark-iridis.rasi \
              $(ROFIDIR)/phd-ark-dark.rasi \
              $(ROFIDIR)/phd-ark-light.rasi
.PHONY: rofi
rofi: $(rofi-themes)


VIMCOLODIR = $(BUILDC)/vim/colors
vim-themes = $(VIMCOLODIR)/phd-ark-iridis.vim \
             $(VIMCOLODIR)/phd-ark-dark.vim \
             $(VIMCOLODIR)/phd-ark-light.vim
.PHONY: vim
vim: $(vim-themes)


WAYBARDIR = $(BUILDC)/waybar/themes
waybar-themes = $(WAYBARDIR)/phd-ark-iridis.css \
                $(WAYBARDIR)/phd-ark-dark.css \
                $(WAYBARDIR)/phd-ark-light.css
.PHONY: waybar
waybar: $(waybar-themes)


WLOGOUTDIR = $(BUILDC)/wlogout/themes
wlogout-themes = $(WLOGOUTDIR)/phd-ark-iridis.css \
                 $(WLOGOUTDIR)/phd-ark-dark.css \
                 $(WLOGOUTDIR)/phd-ark-light.css
.PHONY: wlogout
wlogout: $(wlogout-themes)


XMONADDIR = $(BUILDC)/xmonad/lib/Colors
xmonad-themes = $(XMONADDIR)/PhDArkIridis.hs \
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
svg-themes = $(SVGDIR)/phd-ark-iridis \
             $(SVGDIR)/phd-ark-dark \
             $(SVGDIR)/phd-ark-light
.PHONY: svg
svg: $(svg-themes)
$(svg-themes):
	mkdir -p $@
	$(PYEXE) $(GENPY) $(OPT) --palette -o $@
	convert -background none -resize 200x200 $@/*.svg -set filename:fn '%[basename]' $@/'%[filename:fn].png'



$(alacritty-themes) $(bat-themes) $(btop-themes) $(dircolors-themes) $(dunst-themes) $(emacs-themes) $(fzf-themes) $(git-themes) $(grep-themes) $(highlight-themes) $(hyprland-themes) $(shell-themes) $(subl-themes) $(tofi-themes) $(rofi-themes) $(vim-themes) $(waybar-themes) $(wlogout-themes) $(xmonad-themes) $(xresources-themes):
	mkdir -p $(@D)
	@$(eval template=$(subst Iridis,Template,$(@F)))
	@$(eval template=$(subst -iridis,-template,$(template)))
	@$(eval template=$(subst Dark,Template,$(template)))
	@$(eval template=$(subst -dark,-template,$(template)))
	@$(eval template=$(subst Light,Template,$(template)))
	@$(eval template=$(subst -light,-template,$(template)))
	@$(eval template=$(subst -all,-template,$(template)))
	@echo "$(template)"
	$(PYEXE) $(GENPY) $(OPT) -i $(template) -o $@
