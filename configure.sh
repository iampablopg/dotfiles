#!/bin/bash

programs="${@}"

check_program() {
    local dir
    local program
    program="${1}"
    dir="$HOME/dotfiles/${program}/"

    printf "\\nChecking for ${program}... "

    if [ ! -d $dir ]; then
	printf "\\nI do not have config for ${program} :(\\n"
	exit 1
    fi

    which "${program}"

    if [[ "${?}" -ne 0 ]]; then
	printf "\\n${program} is not installed. Please install it before apply my config.\\n"
    else
	apply_config "${program}"
    fi
}

apply_config() {
    local program
    program="${1}"

    printf "Creating config for ${program}...\\n"
    cp ~/dotfiles/${program}/.${program} ~/.${program}
    printf "Done.\\n"
}

usage() {
    printf "|-------------------------|\\n"
    printf "| Pablo's dotfiles script |\\n"
    printf "|-------------------------|\\n"
    printf "\\nUsage:\\n"
    printf "  ${0} <programs>\\n"
    printf "Examples:\\n"
    printf "  ${0} emacs zsh i3lock\\n"
}

main() {
    if [[ -z "${programs}" ]]; then
        usage
	exit 1
    fi

    for p in ${programs}; do
	check_program "${p}"
    done
}

main
