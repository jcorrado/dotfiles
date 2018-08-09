#!/bin/bash

# Recreate mutt cache dirs, which are empty by default, and not copied
# over by rcup
for p in zoion birchbox; do
    profile_dir=~/.mutt/profiles/$p
    if [ -d  $profile_dir ] && [ ! -d $profile_dir/cache ]; then
	mkdir $profile_dir/cache
    fi
done
