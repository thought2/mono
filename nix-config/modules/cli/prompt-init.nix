{ pkgs, ... }:
let
  gitRepo = fetchGit {
    url = "https://github.com/git/git";
    rev = "965798d1f2992a4bdadb81eba195a7d465b6454a";
  };

  getGitInfo = pkgs.writeShellScriptBin "get-git-info" ''
    source ${gitRepo}/contrib/completion/git-prompt.sh
    __git_ps1
  '';

  withFace = pkgs.writeScriptBin "with-face" ''
    STYLE=$1
    COLOR=$2
    CONTENT=$3
    OPEN_FACE="\[\033[$STYLE;$COLOR\]"
    CLOSE_FACE="\[\033[00m\]"
    echo -e $OPEN_FACE$CONTENT$CLOSE_FACE
  '';
in
{
  programs.bash.promptInit = ''

    # STYLES
    NORMAL=0
    BOLD=1
    ITALIC=3

    # COLORS
    RED="31m"
    BLUE="34m"

    # UTIL
    withFace=${withFace}/bin/with-face

    # MAIN

    if [ "$UID" -eq 0 ]
    then
      COLOR="$RED"
      SIGN='#'
    else
      COLOR="$BLUE"
      SIGN='$'
    fi

    INFO=`${withFace}/bin/with-face $BOLD $COLOR "[\u:\h:\w]"`
    GIT_INFO='$(${getGitInfo}/bin/get-git-info "(%s)")'
    GIT_INFO=`${withFace}/bin/with-face $NORMAL $COLOR "$GIT_INFO"`
    SIGN=`${withFace}/bin/with-face $BOLD $COLOR "$SIGN"`

    PS1="\n$INFO$GIT_INFO\n$SIGN "
  '';
}
