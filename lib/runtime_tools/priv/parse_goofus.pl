#!/usr/bin/env perl

while (<>) {
    chomp;
    if (/^Pid <([.0-9]+)>/) {
        $pid = $1;
    ## CP doesn't always give top stack frame, so ignore it:
    ## } elsif (/^CP/ || / Return addr /) {
    } elsif (/ Return addr /) {
        s/.* \(//;
        s/[ )].*//;
        push(@a, $_) if ($_ ne "<terminate" && $_ ne "unknown" && $_ ne "invalid");
    } elsif (/^\./) {
        print $pid, ";", join(";", reverse(@a)), "\n";
        @a = split(//, ""); ### for ($i = 0; $i <= $#a; $i++) { pop(@a); }
    }
}
