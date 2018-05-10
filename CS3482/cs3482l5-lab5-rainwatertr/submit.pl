#!/usr/bin/perl
#######################################################################
# submit.pl - script that submits result to scoreboard
#
# run by typing: ./submit.pl -u "Nickname"
# file needs to be executable: chmod a+x submit.pl
#######################################################################
use strict 'vars';
use Getopt::Std;
use IO::Socket::INET;

# Generic settings 
$| = 1;      # Flush stdout each time, also on socket
umask(0077); # Files created by the user in tmp readable only by that user
$ENV{PATH} = "/usr/local/bin:/usr/bin:/bin";
my $port = 15003;

my $numArgs = $#ARGV + 1;
if ($numArgs != 2 || $ARGV[0] != "-u") {
    print "\nUsage: submit.pl -u nick_name\n";
    exit;
}

my $nickName = $ARGV[1];
my $login = (getpwuid($<))[0] || "unknown";
my $tmpdir = "/var/tmp/cachelab.$login.$$";
my $diemsg = "The files are in $tmpdir.";
my $infile;

$infile = "./driver.py";

system("mkdir $tmpdir") == 0
    or die "$0: Could not make scratch directory $tmpdir.\n";

system("$infile > $tmpdir/cachelab.output");

open(my $fh, "$tmpdir/cachelab.output") or
die("Unable to open $tmpdir/cachelab.output");
my $csim = 0;
my $trans32 = 0;
my $trans64 = 0;
my $trans67 = 0;
while (my $line = <$fh>)
{
   if ($line =~ /^Csim correctness\s+([^\s]*)/)
   {
      $csim = $1;
   }
   if ($line =~ /^Trans perf 32x32\s+([^\s]*)/)
   {
      $trans32 = $1;
   }
   if ($line =~ /^Trans perf 64x64\s+([^\s]*)/)
   {
      $trans64 = $1;
   }
   if ($line =~ /^Trans perf 61x67\s+([^\s]*)/)
   {
      $trans67 = $1;
   }
}
system("rm -rf $tmpdir");

my $socket = new IO::Socket::INET (
   PeerHost => 'localhost',
   PeerPort => $port,
   Proto => 'tcp'
);

die "cannot connect to the server $!\n" unless $socket;
# print "connected to the server\n";

# data to send to a server
my $req = "|$login|$nickName|$csim|$trans32|$trans64|$trans67|";
my $size = $socket->send($req);
    # print "sent data of length $size\n";

    # notify server that request has been sent
shutdown($socket, 1);

    # receive a response of up to 1024 characters from server
my $response = "";
$socket->recv($response, 32);
print "$response";

$socket->close();

