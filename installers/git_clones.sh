#!/bin/bash
####################################################### Git clone installer

### phdenzel repos
cd $HOME
git clone git@github.com:phdenzel/dotfiles.git
git clone git@github.com:phdenzel/powerline.git
git clone git@github.com:phdenzel/glfits.git
git clone git@github.com:phdenzel/glass.git
git clone git@github.com:phdenzel/tdlmc.git

git clone git@github.com:phdenzel/zurich_lens.git
git clone git@github.com:phdenzel/dm-sheets.git

git clone git@github.com:phdenzel/dydama.git
git clone git@github.com:phdenzel/fibonacci-flower.git
git clone git@github.com:phdenzel/julia.git
git clone git@github.com:phdenzel/ising.git
git clone git@github.com:phdenzel/kdtree-p.git
git clone git@github.com:phdenzel/l-brain.git
git clone git@github.com:phdenzel/tophil.git

git clone git@github.com:phdenzel/pentaplex.git
git clone git@github.com:phdenzel/twilio-sms-bot.git

mkdir -p $HOME/stds
cd $HOME/stds
git clone git@github.com:phdenzel/c-info.git
git clone git@github.com:phdenzel/c-std.git
git clone git@github.com:phdenzel/dev-std.git

cd $HOME/Documents
git clone git@github.com:phdenzel/master-thesis.git
git clone git@github.com:psaha/magicenv.git

mkdir -p $HOME/Documents/how-tos
cd $HOME/Documents/how-tos
git clone git@github.com:phdenzel/local-python-install.git
git clone git@github.com:phdenzel/hello-world.git

mkdir -p $HOME/papers
cd $HOME/papers/
git clone git@github.com:phdenzel/sw-05-42.git
git clone git@github.com:phdenzel/heat-spike.git

### Misc
cd $HOME
git clone git@github.com:RafiKueng/SpaghettiLens.git
git clone git@github.com:pynbody/pynbody.git

#### Other git(hub) clones
#mkdir -p $HOME/git-clones;
#cd $HOME/git-clones;
#
## Google fonts (for Fira, Roboto, and Ubuntu Mono)
#git clone git@github.com:google/fonts.git
#mv fonts google-fonts
#
## Powerline fonts (for Fira, Roboto, and Ubuntu Mono)
# git clone git@github.com:powerline/fonts.git
# mv fonts powerline-fonts
#
## Emacs icons
#git clone git@github.com:domtronn/all-the-icons.el.git
#
## Org mode themes
#git clone git@github.com:fniessen/org-html-themes.git