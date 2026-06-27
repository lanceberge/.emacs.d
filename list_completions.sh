#!/usr/bin/env bash
set -euo pipefail

if [[ ${LIST_COMPLETIONS_SOURCED:-} != 1 ]]; then
  if [[ $# -ne 1 ]]; then
    printf 'usage: %s "command line to complete"\n' "$0" >&2
    exit 2
  fi

  LIST_COMPLETIONS_SOURCED=1 exec bash -ic 'source "$1" "$2"' bash "$0" "$1" 2>/dev/null
fi

if [[ $# -ne 1 ]]; then
  printf 'usage: %s "command line to complete"\n' "$0" >&2
  exit 2
fi

COMP_LINE=$1
COMP_POINT=${#COMP_LINE}

words=()
if [[ $COMP_LINE == *[[:space:]] ]]; then
  read -r -a words <<<"$COMP_LINE"
  words+=("")
else
  read -r -a words <<<"$COMP_LINE"
fi

if [[ ${#words[@]} -eq 0 ]]; then
  exit 0
fi

COMP_WORDS=("${words[@]}")
COMP_CWORD=$((${#COMP_WORDS[@]} - 1))
COMP_TYPE=9
COMP_KEY=9

cmd=${COMP_WORDS[0]}
cur=${COMP_WORDS[$COMP_CWORD]}
prev=
if (( COMP_CWORD > 0 )); then
  prev=${COMP_WORDS[$((COMP_CWORD - 1))]}
fi

completion_spec=$(complete -p -- "$cmd" 2>/dev/null || complete -p -D 2>/dev/null || true)
if [[ -z $completion_spec ]]; then
  exit 0
fi

set -- ${completion_spec#complete }
completion_function=
while [[ $# -gt 0 ]]; do
  case $1 in
    -F)
      completion_function=$2
      shift 2
      ;;
    -A|-C|-G|-P|-S|-W|-X|-o)
      shift 2
      ;;
    --)
      shift
      break
      ;;
    *)
      shift
      ;;
  esac
done

if [[ -z $completion_function ]]; then
  exit 0
fi

"$completion_function" "$cmd" "$cur" "$prev"

if [[ ${#COMPREPLY[@]} -gt 0 ]]; then
  printf '%s\n' "${COMPREPLY[@]}"
fi
