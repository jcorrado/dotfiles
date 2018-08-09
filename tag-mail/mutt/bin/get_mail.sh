#!/bin/bash

set -e

mailboxes=${@--a}

echo Synchronizing mail
mbsync $mailboxes
echo

mu index --nocolor \
   --my-address=jereme@zoion.net \
   --my-address=jereme@birchbox.com \
   --my-address=jereme.corrado@gmail.com
echo

# build recent mail caches
for acct in zoion birchbox; do
    echo Rebuilding ${acct}/recent_mail
    mu find --skip-dups --include-related --format=links --linksdir=~/Maildir/${acct}/recent_mail --clearlinks date:3m..now maildir:/${acct}/all_mail
done

# collect email addresses
echo Rebuilding mutt aliases from mu index

# Anything involving Birchbox
mu cfind --format=mutt-alias '@(contractors\.)?birchbox.com' > ~/.mutt/aliases/mu_birchbox

# Addrs that I've directly corresponded with
mu cfind --personal --format=mutt-alias | \
   sed -e  's/`//g' \
   > ~/.mutt/aliases/mu_zoion

echo

exit 0
