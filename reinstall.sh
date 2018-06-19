cd ~/git/github/.spacemacs.d && \
    git add . && \
    git commit -am "fix: autocommit" && \
    git push http master && \
    cd ~/git/github/dotfiles/.spacemacs.d && \
    git fetch origin master && \
    git reset --hard origin/master && \
    cd ~/git/github/dotfiles && \
    git add .spacemacs.d && \
    git commit -am "fix: update spacemacs config" && \
    git push http master && \
    cd ~/git/github/homely && \
    ./install.sh
