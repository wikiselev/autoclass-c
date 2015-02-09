#!/usr/bin/perl -w

# (c) Mikhail Spivakov 2011

use strict;

if(scalar(@ARGV<1)){
	print "\nautoclass2tab.pl <report.class-data-n> [pmin=\"any\"] [rmin=\"none\"] [id:file=<db2-fname>,col=<colN>]\n";
	print "pmin - minimum class association probability\n";
	print "rmin - minimum ratio in probabilities between the most probable and next probable class\n\n";
	print "id - instead of case N, for each case, list a value from col colN (numbers start with 1) from the original db2 file (good for adding IDs)\n";
	exit;
}

my $fname = shift @ARGV;

my $rmin = -1;
my $pmin = 0;

my $idfile = undef;
my $idcol = undef;

foreach my $arg (@ARGV){
	if ($arg=~/pmin=(\d+\.?\d*)/){
		$pmin = $1;
	}
	if ($arg=~/rmin=(\d+\.?\d*)/){
		$rmin = $1;
	}
	if ($arg=~/id\:file=(\S+)\,col=(\d+)/){
		$idfile = $1;
		$idcol = $2;
		print STDERR "Using IDs from file $idfile, column $idcol\n";
	}
}

my @ids;

if (defined $idfile){
		
	open ID, $idfile or die "couldn't open db2 file $idfile for reading\n";
	
	my $line;
	
	while ($line=<ID>){
		chomp $line;
		
		if ($line=~/^(!|#|;)/){
			next;
		}
		
		my @line = split (/\,|\s+/,$line);
		if (scalar(@line)){ #skip blank lines
			push @ids, $line[$idcol-1];
		}
		
	}
	
	close ID;
	
}

open IN, $fname or die "couldn't open input file $fname for reading\n";

my $line;

my $curclass ="";

my @discarded_p; #i= class
my @discarded_r; #i= class
my @discarded_p_r; #i=class
my @total; #i = class
 
while ($line=<IN>){
	chomp $line;
	if ($line=~/DATA_CLASS (\d+)/){

		$curclass = $1;
		
		$total[$curclass]=0;
		$discarded_p[$curclass]=0;
		$discarded_r[$curclass]=0;
		$discarded_p_r[$curclass]=0;	

	}
	
	my $id;
	my $thisp;
	my $otherp;
	my $discardp = 0;
	my $discardr = 0;
	my $go=0;
	
		
	if ($line=~/^(\d+)\s+(\d+\.?\d*)/){
		$id = (defined $idfile) ? $ids[$1-1] : $1;
		$thisp = $2;
		$total[$curclass]++;
		$go = 1;

		if ($thisp<$pmin){
			$discardp = 1;
			$discarded_p[$curclass]++;
#			print STDERR "discarded: p $thisp too small\n";
		}

	}

	if ($line=~/^\d+\s+\d+\.?\d*\s+\d+\s+(\d+\.?\d*)/){

#		print STDERR "\$1 = $1, \$1-1=", $1-1, " ids[\$-1]=", $ids[$1-1]; <STDIN>;

		$otherp = $1;	

#		print STDERR "otherp_check passed\n";

		if ($thisp/$otherp<$rmin){
			$discardr = 1;
			$discarded_r[$curclass]++;
#			print STDERR "discarded: p/otherp ", $thisp/$otherp, " too small\n";
#			print STDERR "discarded_r incremented; discardp=$discardp\n";
		}
		
		if ($discardp && $discardr){
			$discarded_p_r[$curclass]++;
			$discarded_p[$curclass]--;
			$discarded_r[$curclass]--;
			
#			print STDERR "both criteria failed\n";
#			print STDERR "discarded_r and discarded_p decremented\n";
		}
				
	}
	
	if($go && !$discardp && !$discardr){
		print "$id\t$thisp\t$curclass\n";
	}
	
		
}
close IN;

print STDERR "\n-------------------------\n";
print STDERR "pmin=$pmin, rmin=$rmin\n\n";
print STDERR "class total passed %passed discarded_p discarded_r discarded_p_r\n";


my $t_total=0;
my $t_passed=0;
my $t_discarded_p=0;
my $t_discarded_r=0;
my $t_discarded_p_r=0;
for (my $i=0; $i<=$#total; $i++){
	if (! defined $total[$i]){next}
	
	my $passed = $total[$i]-$discarded_p[$i]-$discarded_r[$i]-$discarded_p_r[$i];
	my $p_passed = sprintf "%.1f", $passed/$total[$i]*100;

	$t_passed+=$passed;
	$t_total+=$total[$i];
	$t_discarded_p+=$discarded_p[$i];
	$t_discarded_r+=$discarded_r[$i];
	$t_discarded_p_r+=$discarded_p_r[$i];
	
	print STDERR "$i\t${total[$i]}\t$passed\t${p_passed}\t${discarded_p[$i]}\t${discarded_r[$i]}\t${discarded_p_r[$i]}\n";
	
	
}

my $t_p_passed = sprintf "%.1f", $t_passed/$t_total*100;
print STDERR "\nTotal:\n";
print STDERR "\t${t_total}\t${t_passed}\t${t_p_passed}\t${t_discarded_p}\t${t_discarded_r}\t${t_discarded_p_r}\n";

