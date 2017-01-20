#!/bin/bash

dest_dir=${1:-$HOME}
script_dir=$(cd $(dirname $0); pwd)

config_file_links=(
    '.ackrc                     => .ackrc'
    'emacs-config/.agenda_files => .agenda_files'
    '.agignore                  => .agignore'
    'secrets/.authinfo          => .authinfo'
    '.autolayouts.yaml          => .autolayouts.yaml'
    '.bash_profile              => .bash_profile'
    '.bashrc                    => .bashrc'
    '.bundle-config             => .bundle/config'
    '.dns_switcher_options.yaml => .dns_switcher_options.yaml'
    '.gemrc                     => .gemrc'
    'secrets/.gitconfig         => .gitconfig'
    '.gitignore                 => .gitignore'
    '.gitignore_global          => .gitignore_global'
    '.gnu_globalrc              => .globalrc'
    '.gvimrc                    => .gvimrc'
    '.htoprc                    => .htoprc'
    '.httpie-config.json        => .httpie/config.json'
    '.hyper.js                  => .hyper.js'
    '.icrt.conf                 => .icrt.conf'
    '.ideavimrc                 => .ideavimrc'
    'secrets/.imapnotifyrc.json => .imapnotifyrc.json'
    '.irbrc                     => .irbrc'
    '.linkdispatch              => .linkdispatch'
    '.lldb-ruby.py              => .lldb/ruby.py'
    '.m2-settings.xml           => .m2/settings.xml'
    'secrets/.offlineimaprc     => .offlineimaprc'
    '.profile                   => .profile'
    '.proton                    => .proton'
    '.proxyswitcher.rc          => .proxyswitcher.rc'
    '.pryrc                     => .pryrc'
    '.rails.gitignore           => .rails.gitignore'
    '.rails.template            => .rails.template'
    '.railsrc                   => .railsrc'
    '.rotrc                     => .rotrc'
    '.rspec-git                 => .rspec-git'
    '.sbt-repositories          => .sbt/repositories'
    '.screenrc                  => .screenrc'
    'emacs-config/.spacemacs    => .spacemacs'
    '.spring.rb                 => .spring.rb'
    '.sublimious                => .sublimious'
    '.vimrc                     => .vimrc'
    '.windowmaxrc               => .windowmaxrc'
    '.zlogin                    => .zlogin'
    '.zprofile                  => .zprofile'
    '.zshenv                    => .zshenv'
    '.zshrc                     => .zshrc'
    '.rvm-config-db             => .rvm/config/db'
)

for link in "${config_file_links[@]}"; do
    echo ln -nfs "$script_dir/${link%% *}" "$dest_dir/${link##* }"
done

