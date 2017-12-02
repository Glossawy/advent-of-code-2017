#!/usr/bin/env bash
# shellcheck disable=SC2034,SC2059

cecho() {
  end='\n'
  [ "$1" == '-n' ] && shift && end=''
  printf "$*${RESET}${end}"
}

loop_prompt() {
  local result
  while true; do
    echo -e -n "$1"
    read -r choice

    case "$choice" in
      [yY]) result=0; break;;
      [nN]) result=1; break;;
    esac
  done

  return ${result}
}

prompt() {
  loop_prompt "$1"
  return $?
}

venv_missing_error() {
  cecho "${Red}Virtualenv Not Found Error"
  [ $# -gt 0 ] && cecho "${RED}Expected at: ${1}"
  cecho "${Yel}Did you rememebr to run\`${Whi}script/install${Yel}\`?"
  exit 1
}

venv_activation_command() {
  local path commands
  path="$1/bin/activate"
  commands=(
    'resetbound=false'
    '[[ "$-" == *u* ]] && set +u && resetbound=true'
    "source $path"
    '${resetbound} && set -u'
    'resetbound='
  )

  printf "%s\n" "${commands[@]}"
}

venv_validate() {
  [ -d "$1" ] || venv_missing_error "$1"
  [ -f "$1/bin/activate" ] || venv_missing_error "$1"
}

is_cibuild() {
  # shellcheck disable=SC2086
  [ ${CIBUILD:-1} -eq 0 ] && return 0 || return 1
}

Yel='\033[0;33m'; Gre='\033[0;32m'; Red='\033[0;31m'; Whi='\033[0;37m'; Blu='\033[0;34m'
Bold='\033[1m'; RESET='\033[0m'

PYVERSION=python2.7
[ ! -z "${VIRTUAL_ENV:-}" ] && INVENV=0 || INVENV=1
