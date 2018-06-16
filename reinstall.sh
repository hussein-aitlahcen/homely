cd ~/git/gihub/.spacemacs.d
git add .
git commit -am "fix: autocommit"
git push origin master
cd ~/git/github/dotfiles/.spacemacs.d
git pull origin master
cd ~/git/github/dotfiles
git add .spacemacs.d
git commit -am "fix: update spacemacs config"
git push origin master
cd ~/git/github/homely
./install.sh
