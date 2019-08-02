#!/bin/sh

# release.sh is a simple release script for Emacs packages.
# It updates declared version, tags the release, and prints a reminder
# to push the tag and master branch.

# © 2019 Nicholas D Steeves
# Distributed under the terms of the CC0
# No warranty, no guaranty, no rights reserved

# Exit immediately if anything fails.
set -e

main_file="fountain-mode.el"

# Make sure we're on the master branch, because tagging a detached
# HEAD isn't useful or release-worthy.
git checkout master
printf "What version are you releasing? "
read new_version
sed -i "s/^;; Version:.*/;; Version: $new_version/" "$main_file"
while true; do
    git diff -- fountain-mode.el
    printf "Release $new_version after making these changes? (y/n) "
    read confirm_release
    case "$confirm_release" in
        yes | y | Yes | YES )
            break
            ;;
        no | n | No | NO )
            echo "Ok, exiting without releasing new version."
            exit 1
            ;;
        * )
            printf "Please answer yes or no\n\n"
            ;;
    esac
done
git commit -m "Update declared version to $new_version" -- "$main_file"
git tag -a -m "Release v$new_version" "v$new_version"
printf "\n\nShare it with the world with:  git push origin master v$new_version\n"
