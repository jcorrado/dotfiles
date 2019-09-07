#!/usr/bin/perl

use warnings;
use strict;

use Getopt::Std;
use Term::ReadLine;

our ($opt_d, $opt_f, $opt_r);

getopts('d:f:r:');

my $RESULTS_DIR = $opt_d;
my $MAILDIR_ROOT = $opt_r;
my $MU_CMD="mu find --skip-dups --format=links --linksdir=$RESULTS_DIR --clearlinks m:$MAILDIR_ROOT ";
my $PROMPT = 'mu> ';
my $APP_NAME = 'mu_mutt_search';
my $HISTORY_FILE = $opt_f || "$ENV{HOME}/.${APP_NAME}_history";

my $term = Term::ReadLine->new($APP_NAME, *STDIN, *STDERR);
$term->read_history($HISTORY_FILE);

my $out_fh = *STDOUT;
my $ui_fh = $term->OUT;

$term->ornaments(0);

print $ui_fh help();
my $query = $term->readline($PROMPT);
$term->write_history($HISTORY_FILE);

# TODO: Build a command input loop, so we can support a help command
# and such, then final search execution.

if ($query) {
    if (system("$MU_CMD $query") == 0) {
        print $out_fh "push <change-folder-readonly>$RESULTS_DIR<enter>";
    } else {
        print $ui_fh "could not fork() for mu(1): $!\n";
        print $out_fh "push <esc>";
    }
} else {
    print $out_fh "push <esc>";
}

exit 0;


# subs

sub help {
    return '
        mu-find(1)

        Fields
        ------
        f from          Message sender
        t to            To: recipient(s)
        c cc            Cc (carbon-copy) recipient(s)
        h bcc           Bcc (blind-carbon-copy) recipient(s)
        contact         from, to, cc and bcc
        recip           to, cc, bcc

        s subject       Message subject
        d date          Date-Range
        z size          Message size
        i msgid         Message-ID

        e embed         Search inside embedded text parts (messages, attachments)
        j file          Attachment filename
        y mime          MIME-type of one or more message parts

        g flag          Message Flags
        p prio          Message priority ("low", "normal" or "high")
        x tag           Tags for the message (X-Label and/or X-Keywords)
v list          Mailing list (e.g. the List-Id value)
m maildir       Maildir


Flags for `g` (use multiple times)
----------------------------------
a attach        Has attachment
d draft         Draft Message
f flagged       Flagged
n new           New message (in new/ Maildir)
p passed        Passed ("Handled")
r replied       Replied
s seen          Seen
t thrashed      Marked for deletion
z signed        Signed message
x encrypted     Encrypted message
l list          Mailing-list message

';
}
