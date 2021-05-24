define(`dot_secure', `esyscmd(`dot-decrypt ~/.config/kaltemplate/capabilities/$1.cpb.gpg')')dnl
define(`dot_user', `esyscmd(`whoami')')dnl
changecom(`/*', `*/')dnl
