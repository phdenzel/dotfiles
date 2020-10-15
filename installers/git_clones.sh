#!/bin/bash
####################################################### Git clone installer

mkdir -p $HOME/forks                   # for github-forks
mkdir -p $HOME/git-clones              # for simple repo clones
mkdir -p $HOME/Documents/papers        # for paper repos
mkdir -p $HOME/Documents/stds          # for templates
mkdir -p $HOME/Documents/how-tos       # for repo tutorials
mkdir -p $HOME/Documents/git-archive   # for archived repos

GITCLONES=$HOME/git-clones
FORKS=$HOME/forks


### phdenzel HOME repos
cd $HOME
git clone git@github.com:phdenzel/phdenzel.github.io.git
git clone git@github.com:phdenzel/dotfiles.git
git clone git@github.com:phdenzel/gleam.git
git clone git@github.com:phdenzel/glass.git
git clone git@github.com:phdenzel/model-zapper.git
git clone git@github.com:phdenzel/sw05.git
git clone git@github.com:phdenzel/adler.git
git clone git@github.com:phdenzel/delays.git
git clone git@github.com:phdenzel/slacs-match.git
git clone git@github.com:phdenzel/tdlmc.git
git clone git@github.com:phdenzel/lensing.js.git
git clone git@github.com:phdenzel/camengine.js.git
git clone git@github.com:phdenzel/streaming-lens.git
git clone git@github.com:phdenzel/zurich-lens.git
git clone git@github.com:phdenzel/aesobscurum.git
git clone git@github.com:phdenzel/mymtg.git
git clone git@github.com:phdenzel/garden.git
git clone git@github.com:phdenzel/tenebrae.git
git clone git@github.com:phdenzel/finasset.git
git clone git@github.com:phdenzel/ollam.git
git clone git@github.com:phdenzel/pentaplex.git
git clone git@github.com:phdenzel/twilio-sms-bot.git
git clone git@github.com:phdenzel/tophil.git
git clone git@github.com:phdenzel/l-brain.git

### DOCUMENT repos
cd $HOME/Documents
git clone git@github.com:phdenzel/pylib.git
git clone git@github.com:phdenzel/euler-project.git
git clone git@github.com:phdenzel/teaching.git
git clone git@github.com:phdenzel/PhDCV.git
git clone git@github.com:phdenzel/master-thesis.git
git clone --recurse-submodules git@github.com:phdenzel/phd-thesis.git
cd $HOME/Documents/papers
git clone git@github.com:phdenzel/heat-spike-paper.git
git clone git@github.com:phdenzel/adler-paper.git
git clone git@github.com:phdenzel/time-delays-paper.git
git clone git@github.com:phdenzel/slacs-matching-paper.git
git clone git@github.com:phdenzel/sw05-paper.git
cd $HOME/Documents/stds
git clone git@github.com:phdenzel/c-info.git
git clone git@github.com:phdenzel/c-std.git
git clone git@github.com:phdenzel/dev-std.git
git clone git@github.com:phdenzel/paper-template.git
cd $HOME/Documents/how-tos
git clone git@github.com:phdenzel/local-python-install.git
git clone git@github.com:phdenzel/about-git.git
#git clone git@github.com:phdenzel/hello-world.git
cd $HOME/Documents/git-archive
#git clone git@github.com:phdenzel/fk20-proposal.git
git clone git@github.com:phdenzel/dm-sheets.git
git clone git@github.com:phdenzel/dydama.git
git clone git@github.com:phdenzel/fibonacci-flower.git
git clone git@github.com:phdenzel/julia.git
git clone git@github.com:phdenzel/ising.git
git clone git@github.com:phdenzel/kdtree-p.git
git clone git@github.com:phdenzel/fire-halos.git

### phdenzel FORK repos
cd $FORKS
git clone git@github.com:phdenzel/neofetch.git
git clone git@github.com:phdenzel/alacritty.git
git clone git@github.com:phdenzel/ckb-next.git
git clone git@github.com:phdenzel/piper.git
git clone git@github.com:phdenzel/powerline.git
git clone git@github.com:phdenzel/ripgrep.git
git clone git@github.com:phdenzel/dust.git
git clone git@github.com:phdenzel/reveal.js.git
git clone git@github.com:phdenzel/arxiv-latex-cleaner.git
git clone git@github.com:phdenzel/libimobiledevice.git
# git clone git@github.com:phdenzel/EuclidEmulator2.git
# mkdir -p $HOME/forks/macos-kexts       # for macOS kext forks
# cd $FORKS/macos-kexts
# git clone git@github.com:phdenzel/WhateverGreen.git
# git clone git@github.com:phdenzel/Lilu.git
# git clone git@github.com:phdenzel/IntelMausiEthernet.git
# git clone git@github.com:phdenzel/OS-X-FakeSMC-kozlek.git
# git clone git@github.com:phdenzel/OS-X-USB-Inject-All.git
# git clone git@github.com:phdenzel/RTL8111_driver_for_OS_X.git

### GITCLONES repos
cd $GITCLONES
git clone git@github.com:psaha/magicenv.git
git clone git@github.com:RafiKueng/SpaghettiLens.git
git clone git@github.com:yjwen/org-reveal.git
git clone https://gitlab.com/git-latexdiff/git-latexdiff.git
# git clone git@github.com:fniessen/org-html-themes.git
# git clone git@github.com:domtronn/all-the-icons.el.git
# git clone git@github.com:google/fonts.git google-fonts
# git clone git@github.com:powerline/fonts.git powerline-fonts
# git clone git@github.com:pynbody/pynbody.git
# git clone git@github.com:GalacticDynamics-Oxford/Agama.git agama
