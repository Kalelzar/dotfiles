define(`dot_secure', `esyscmd(`dot-decrypt ~/.config/kaltemplate/capabilities/$1.gpg')')dnl
changecom(`/*', `*/')dnl
