#!/bin/bash

mailboxes=${@--a}

echo Synchronizing mail
mbsync $mailboxes
echo

mu index --nocolor \
   --my-address=jereme@zoion.net \
   --my-address=jereme@birchbox.com \
   --my-address=jereme.corrado@gmail.com \
   --my-address=jereme@teammobot.com
echo

# build recent mail caches
for acct in zoion teammobot; do
    echo Rebuilding ${acct}/recent_mail
    mu find --skip-dups --include-related --format=links --linksdir=~/Maildir/${acct}/recent_mail --clearlinks date:1m..now maildir:/${acct}/all_mail
done

# collect email addresses
echo Rebuilding mutt aliases from mu index

# Anything involving TeamMobot
mu cfind --format=mutt-alias '@teammobot.com' > ~/.mutt/aliases/mu_teammobot

# Addrs that I've directly corresponded with
mu cfind --personal --format=mutt-alias | \
    sed -e  's/`//g' \
        > ~/.mutt/aliases/mu_zoion

echo

exit 0
