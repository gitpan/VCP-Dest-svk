package VCP::Dest::svk ;

=head1 NAME

VCP::Dest::svk - svk destination driver

=head1 SYNOPSIS

   vcp <source> svk:/path/to/repos:path
   vcp <source> svk:/path/to/repos:path --init-repos
   vcp <source> svk:/path/to/repos:path --init-repos --delete-repos

   # source could be cvs:/path/to/cvsrepos:module/... or
   # cvs::pserver:anonymous@cvs.server.org:/repos:module/...

=head1 DESCRIPTION

This driver allows L<vcp|vcp> to insert revisions in to a Subversion
repository via the svk interface. You could use the L<vcp> command
line interface or the integrated L<SVK> mirror command.

=head1 OPTIONS

=over

=item --init-repos

Initializes a SVN repository in the directory indicated.
Refuses to init a non-empty directory.

=item --delete-repos

If C<--init-repos> is passed and the target directory is not empty, it
will be deleted.  THIS IS DANGEROUS AND SHOULD ONLY BE USED IN TEST
ENVIRONMENTS.

=item --nolayout

Do not create conventional layout for trunk and branches.

=item --trunk-dir

The directory for trunk. default is trunk.

=item --branch-dir

The directory for branches. default is branches.

=back

=cut
use strict;
our $VERSION = '0.28' ;
our @ISA = qw( VCP::Dest );

use SVN::Core;
use SVN::Repos;
# XXX: 584 doesn't like version requirement here
use SVK;
use SVK::XD;
use SVK::Util qw( md5 );
use Data::Hierarchy;
use VCP::Logger qw( pr lg pr_doing pr_did );
use VCP::Rev ('iso8601format');
use VCP::Utils qw( empty is_win32 escape_filename);
use File::Path ;
use VCP::Debug ':debug' ;

use vars qw( $debug ) ;

$debug = 0 ;

sub _db_store_location {
   my $self = shift ;

   my $loc = $self->{SVK_REPOSPATH};

   return File::Spec->catdir( $loc, 'vcp_state',
			      escape_filename ($self->{SVK_TARGETPATH}), @_ );
}

sub new {
   my $self = shift->SUPER::new( @_ ) ;

   ## Parse the options
   my ( $spec, $options ) = @_ ;

   $self->parse_repo_spec( $spec )
      unless empty $spec;

   $self->parse_options( $options );

   return $self ;
}

sub parse_svk_depot_spec {

}

sub options_spec {
   my $self = shift;
   return (
      $self->SUPER::options_spec,
      "init-repos"     => \$self->{SVK_INIT_REPOS},
      "delete-repos"   => \$self->{SVK_DELETE_REPOS},
      "nolayout"       => \$self->{SVK_NOLAYOUT},
      "trunk-dir=s"    => \$self->{SVK_TRUNK_DIR},
      "branch-dir=s"   => \$self->{SVK_BRANCH_DIR},
   );
}

sub init_repos {
    my $self = shift;

    $self->{SVK_REPOS} = SVN::Repos::create ($self->{SVK_REPOSPATH},  undef, undef, undef,
					     {'bdb-txn-nosync' => '1',
					      'fs-type' => $ENV{SVNFSTYPE} || 'bdb',
					      'bdb-log-autoremove' => '1'});
}

sub init_layout {
    my $self = shift;
    my $fs = $self->{SVK_REPOS}->fs;
    my $pool = SVN::Pool->new_default;
    my $root = $fs->revision_root ($fs->youngest_rev);

    my ($trunk, $branch) = @{$self}{qw/SVK_TRUNKPATH SVK_BRANCHPATH/};
    return unless $root->check_path ($trunk) == $SVN::Node::none ||
	$root->check_path ($branch) == $SVN::Node::none;

    my $editor = [$self->{SVK_REPOS}->get_commit_editor ('',
							 '',
							 'VCP', 'VCP: initializing layout',
							 undef)];
    my $edit = SVN::Simple::Edit->new
	( _editor => $editor,
	  missing_handler =>
	  &SVN::Simple::Edit::check_missing ($root)
	);

    $edit->open_root (0);
    $edit->add_directory ($branch);
    $edit->add_directory ($trunk) if $trunk ne $branch;
    $edit->close_edit ();
}

sub init {
   my $self = shift;

   $self->{SVK_REPOSPATH} = $self->repo_server;
   $self->{SVK_TARGETPATH} = $self->repo_filespec;

   ## Set default repo_id.
   $self->repo_id( "svk:" . $self->repo_server )
      if empty $self->repo_id && ! empty $self->repo_server ;

#   $self->deduce_rev_root( $self->repo_filespec ) ;

   $self->{SVK_TRUNK_DIR} ||= 'trunk';
   $self->{SVK_BRANCH_DIR} = 'branches'
       unless defined $self->{SVK_BRANCH_DIR};

   if ( $self->{SVK_INIT_REPOS} ) {
      if ( $self->{SVK_DELETE_REPOS} ) {
         $self->rev_map->delete_db;
         $self->head_revs->delete_db;
	 rmtree [ $self->{SVK_REPOSPATH} ];
      }
      $self->init_repos;
   }
   else {
      pr "ignoring --delete-repos, which is only useful with --init-repos"
         if $self->{SVK_DELETE_REPOS};
      $self->{SVK_REPOS} ||= SVN::Repos::open ($self->{SVK_REPOSPATH});
   }
   $self->{SVK_TRUNKPATH} = $self->{SVK_NOLAYOUT} ? $self->{SVK_TARGETPATH} :
       ($self->{SVK_TARGETPATH} eq '/' ? '/' : $self->{SVK_TARGETPATH}.'/').
	   $self->{SVK_TRUNK_DIR};

   unless ($self->{SVK_NOLAYOUT}) {
       $self->{SVK_BRANCHPATH} = $self->{SVK_TARGETPATH} eq '/' ? '/' : $self->{SVK_TARGETPATH}.'/';
       if ($self->{SVK_BRANCH_DIR} eq '.') {
	   chop $self->{SVK_BRANCHPATH};
       }
       else {
	   $self->{SVK_BRANCHPATH} .= $self->{SVK_BRANCH_DIR};
       }
       $self->init_layout;
   }

   $self->{SVK} = SVK->new ( output => \$self->{SVK_OUTPUT},
			     xd => SVK::XD->new
			     ( depotmap => {'' => $self->{SVK_REPOSPATH}},
			       svkpath => $self->{SVK_REPOSPATH},
			       checkout => Data::Hierarchy->new ));

   my $coroot = $self->work_path ("co");
   $self->mkpdir ($coroot);
#   $self->{SVK}->checkout ('//', $coroot);

   $self->rev_map->open_db;
   $self->head_revs->open_db;
}

sub compare_base_revs {
   my $self = shift ;
   my ( $r, $source_path ) = @_ ;

   die "\$source_path not set at ", caller
      unless defined $source_path;

   my ($prefix, $name, $rev ) =
       $self->rev_map->get ( [ $r->source_repo_id, $r->id ] );
   my $pool = SVN::Pool->new_default;
   my $root = $self->{SVK_REPOS}->fs->revision_root ($rev);

   if (-z $source_path) {
       # dead rev is fine
       return if $root->check_path ("$prefix/$name") == $SVN::Node::none;
       # placeholder
       return if $r->rev_id =~ m/\.0$/;
   }

   open FH, '<', $source_path or die "$!: $source_path" ;
   my $source_digest = md5( \*FH ) ;

   my $dest_digest = $root->file_md5_checksum ("$prefix/$name");

   lg "$r checking out ", $r->as_string, " as $prefix/$name\@$rev from svk dest repo";

   die( "vcp: base revision\n",
	$r->as_string, "\n",
       "differs from the last version in the destination p4 repository.\n",
       "    source digest: $source_digest (in ", $source_path, ")\n",
       "    dest. digest:  $dest_digest (in $prefix/$name\@$rev)\n"
   ) unless $source_digest eq $dest_digest ;
}

sub handle_header {
   my $self = shift ;
   my ( $h ) = @_;

   $self->{SVK_PENDING}         = [] ;
   $self->{SVK_PREV_COMMENT}    = undef ;
   $self->{SVK_PREV_CHANGE_ID}  = undef ;
   $self->{SVK_COMMIT_COUNT}    = 0 ;

   $self->SUPER::handle_header( @_ ) ;
}

sub handle_rev {
   my $self = shift ;
   my $r ;
   ( $r ) = @_ ;

   debug "got ", $r->as_string if debugging;
   my $change_id = $r->change_id;

   $self->commit
      if @{$self->{SVK_PENDING}}
         && $change_id ne $self->{SVK_PREV_CHANGE_ID};

   $self->{SVK_PREV_CHANGE_ID} = $change_id;
   $self->{SVK_PREV_COMMENT}   = $r->comment;

   if ( $r->is_base_rev ) {
      $self->compare_base_revs( $r, $r->get_source_file );
      pr_doing;
      return;
   }

   push @{$self->{SVK_PENDING}}, $r;
}

sub handle_footer {
   my $self = shift ;

   $self->commit if @{$self->{SVK_PENDING}};

   $self->SUPER::handle_footer ;

   pr "committed ", $self->{SVK_COMMIT_COUNT}, " revisions";
}

sub update_revision_prop {
    my ($self, $rev, $r) = @_;
    my $fs = $self->{SVK_REPOS}->fs;
    my $pool = SVN::Pool->new_default;
    if ($r->time) {
	my $time = iso8601format($r->time);
	$time =~ s/\s/T/;
	$time =~ s/Z/\.00000Z/;
	$fs->change_rev_prop($rev, 'svn:date', $time);
    }
    $fs->change_rev_prop($rev, 'svn:author', $r->user_id || 'unknown_user');

    $self->{SVK_COMMIT_CALLBACK}->($rev, defined $r->source_change_id ? $r->source_change_id : $rev)
	if $self->{SVK_COMMIT_CALLBACK};
}

sub commit {
    my $self = shift;
    my $revs = $self->{SVK_PENDING};
    $self->{SVK_PENDING} = [];
    my $fs = $self->{SVK_REPOS}->fs;
    my $pool = SVN::Pool->new_default;
    my $root = $fs->revision_root ($fs->youngest_rev);
    my $branch = $revs->[0]->branch_id || '';
    my $coroot = $self->work_path ("co");

    unless ($self->{SVK_NOLAYOUT} || defined $self->{SVK_TRUNK}) {
	unless (defined ($self->{SVK_TRUNK} = $root->node_prop
			 ($self->{SVK_TRUNKPATH}, 'vcp:trunk'))) {
	    $self->{SVK}->propset ('--direct', '-m', "[vcp] select <$branch> as trunk",
				   'vcp:trunk', $branch, "/$self->{SVK_TRUNKPATH}");
	    $self->{SVK_TRUNK} = $branch;
	}
    }

    my $thisbranch = ($self->{SVK_NOLAYOUT} || $branch eq $self->{SVK_TRUNK})
	? "/$self->{SVK_TRUNKPATH}" : "/$self->{SVK_BRANCHPATH}/$branch";
    my $thisco = ($self->{SVK_NOLAYOUT} || $branch eq $self->{SVK_TRUNK})
	? $self->work_path ('trunk') : $self->work_path ('branches', $branch);

    if ($root->check_path ($thisbranch) == $SVN::Node::none) {
	$self->handle_branchpoint ($branch, $root, $revs);
    }
    else {
	if (!-e $coroot || ($self->{SVK_LAST_BRANCH} && $thisbranch ne $self->{SVK_LAST_BRANCH})) {
	    unlink ($coroot) if -e $coroot;
	    unless (-d $thisco) {
		$self->mkpdir ($thisco);
		$self->{SVK}->checkout ($thisbranch, $thisco);
		lg "checking out $thisbranch to $thisco";
	    }
	    symlink ($thisco, $coroot) or die "$thisco -> $coroot: $!";
	}
	$self->handle_branching ($thisbranch, $root, $revs);
	my $use_anchor;
	my $copath = $self->work_path ('co');
	my $anchor = $self->prepare_commit ($thisbranch, $root, $revs);
	lg "import $thisbranch$anchor $copath$anchor (".(1+$#{$revs})." files)";
	while ($anchor && $root->check_path ($thisbranch.$anchor) == $SVN::Node::none) {
	    (undef,$anchor,undef) = File::Spec->splitpath( $anchor );
	    $anchor =~ s|/$||g;
	    $use_anchor = 1;
	}

	$anchor = $anchor ? $copath.$anchor : $thisco;
	my @targets = ($use_anchor || $#{$revs} > 100) ? ($anchor)
	    : map { "$copath/".$_->name } @$revs;

        no warnings 'redefine';
        require SVK::Command::Commit;
        local *SVK::Command::Commit::loc = sub { $_[0] };
	$self->{SVK}->commit ('--direct', '--import',
			      '-m', $revs->[0]->comment || '** no comments **',
			      @targets);
	die $self->{SVK_OUTPUT}.YAML::Dump($self->{SVK}{xd})
	    unless $self->{SVK_OUTPUT} =~ m'Committed revision';
	debug "import result:\n$self->{SVK_OUTPUT}" if debugging;
	$self->{SVK}->update ($thisco);
	$self->{SVK_LAST_BRANCH} = $thisbranch;
    }

    my $rev = $fs->youngest_rev;
    $self->update_revision_prop ($rev, $revs->[0]);

    pr_did "revision", $rev;
    ++$self->{SVK_COMMIT_COUNT};
    for my $r (@$revs) {
	pr_doing;
	$self->rev_map->set
	    ( [ $r->source_repo_id, $r->id ],
	      $thisbranch, $r->name, $rev );

	$self->head_revs->set
	    ( [ $r->source_repo_id, $r->source_filebranch_id ],
	      $r->source_rev_id
	    );
     }
}

sub deduce_branchparent {
    my ($self, $revs) = @_;
    my $branchinfo;
    my $anchor;
    for my $r (@$revs) {
	my $pr_id = $r->previous_id;
	die "branchpoint has something without previous" if empty $pr_id;

	my ( $pprefix, $pname, $prev ) =
	     $self->rev_map->get( [ $r->source_repo_id, $pr_id ] );

	$branchinfo->{$pprefix} = $prev
	    if !$branchinfo->{$pprefix} || $prev > $branchinfo->{$pprefix};

	if (defined $anchor) {
	    while ($anchor && "$anchor/" ne substr ($r->name, 0, length ($anchor)+1)) {
		(undef,$anchor,undef) = File::Spec->splitpath( $anchor );
		$anchor =~ s|/$||g;
	    }
	}
	else {
	    (undef,$anchor,undef) = File::Spec->splitpath( $r->name );
	    $anchor =~ s|/$||g;
	}
    }

    if (keys %$branchinfo != 1) {
	debug YAML::Dump ($branchinfo);
	die "complicated branchpoint not handled yet:\n".join("\n", map $_->as_string, @$revs);
    }

    # XXX: verify the latest rev is still in range for all revs
    return ([%$branchinfo, $revs, $anchor]);
}

sub handle_branchpoint {
    my ($self, $branch, $root, $revs) = @_;
    my $coroot = $self->work_path ("co");
    my (@branchfrom) = $self->deduce_branchparent ($revs);
    my $work_path = $self->work_path ('branch');
    for (@branchfrom) {
	my ($branchfrom, $branchfromrev, $branchrevs, $anchor) = @$_;
	unless (-e "$work_path/$branch") {
	    $self->{SVK}->checkout ('-N', "/$self->{SVK_BRANCHPATH}", $work_path)
		unless -e $work_path;
	    die "branch already exist on branchpoint" if -d "$work_path/$branch";
	    if ($anchor) {
		my $anchor_path = $self->work_path( "branch", $branch, $anchor );
		$self->mkpdir ($anchor_path);
		$self->{SVK}->add ($self->work_path ( "branch", $branch));
		$self->{SVK}->copy ('-r', $branchfromrev, "$branchfrom/$anchor",
				    $self->work_path ( 'branch', $branch, $anchor));
	    }
	    else {
		$self->{SVK}->copy ('-r', $branchfromrev, $branchfrom,
				    $self->work_path ( 'branch', $branch));
	    }
	    lg "branch $branch copied from $branchfrom:$branchfromrev";
	    debug "copy result:\n$self->{SVK_OUTPUT}" if debugging;
	    # do fixup for things not in $branchrevs
	    my %copied;
	    $copied{$_->name}++ for @$branchrevs;
	    debug "files belong to this copy: ".join(',',keys %copied) if debugging;
	    for (split /\n/, $self->{SVK_OUTPUT}) {
		my (undef, $path) = split /\s+/;
		next if -d $path;
		my $npath = $path;
		$npath =~ s|^\Q$work_path/$branch\E/?||;
		next if exists $copied{$npath};
		debug "remove: $path" if debugging;
		$self->{SVK}->delete ($path) if -e $path;
	    }
	    unlink ($coroot) if -e $coroot;
	    symlink ("$work_path/$branch", $coroot) or die $!;
	}
	else {
	    # copy $branchrevs from $branch{from,rev} to co
	    die "complicated branching at a batch not implemented yet:\n".join("\n", @$revs);
	    $self->handle_branching ($branch, $root, $branchrevs);
	}
	$self->prepare_commit ($branch, $root, $branchrevs);
    }
    $self->{SVK}->commit ('--direct', '-m', $revs->[0]->comment || 'bzz', $self->work_path ('branch'));
    rmtree [$work_path];
    unlink ($coroot);
}

sub prepare_commit {
    my ($self, $prefix, $root, $revs) = @_;
    my $anchor;
    my @fetch_rev = grep {!$_->is_placeholder_rev && $_->action ne 'delete' } @$revs;

    my @source_fns = $revs->[0]->source->can ('get_source_files') ?
	$revs->[0]->source->get_source_files (@fetch_rev) :
	    map {$_->get_source_file} @fetch_rev;
    for my $r (@$revs) {
	next if $r->is_placeholder_rev;
	if (defined $anchor) {
	    while ($anchor && "$anchor/" ne substr ($r->name, 0, length ($anchor)+1)) {
		(undef,$anchor,undef) = File::Spec->splitpath( $anchor );
		$anchor =~ s|/$||g;
	    }
	}
	else {
	    (undef,$anchor,undef) = File::Spec->splitpath( $r->name );
	    $anchor =~ s|/$||g;
	}
	my $work_path = $self->work_path( "co", $r->name ) ;
	unlink $work_path if -e $work_path;

	if ($r->action ne 'delete') {
	    my $source_fn = shift @source_fns;
	    $self->mkpdir( $work_path );
	    link $source_fn, $work_path
		or die "$! linking '$source_fn' -> '$work_path'" ;
	}
    }
    return $anchor ? "/$anchor" : '';
}

sub handle_branching {
    my ($self, $prefix, $root, $revs) = @_;
    my $copied;
    # XXX: optimize this as the grouped copy
    for my $r (@$revs) {
	my $pr_id = $r->previous_id;
	next if empty $pr_id;
	my $fn = $r->name ;
	my $work_path = $self->work_path( "co", $fn ) ;

	local $@;
	my ( $pprefix, $pname, $prev ) = eval {
	     $self->rev_map->get( [ $r->source_repo_id, $pr_id ] ) };
	if ($@) {
	    pr "abandon branch source $pr_id for ".$r->as_string;
	    $r->action ('add');
	    next;
	}

	my $dir = $self->mkpdir ($work_path);
	my $anchor = $r->name;
	while (1) {
	    # XXX: file::spec::unix
	    my (undef,$newanchor,undef) = File::Spec->splitpath( $anchor );
	    $newanchor =~ s|/$||g;
	    last unless $root->check_path ("$prefix/$newanchor") == $SVN::Node::none;
	    $anchor = $newanchor;
	}
	$self->{SVK}->add ($self->work_path ('co', $anchor)) unless $anchor eq $r->name;
	++$copied;
	$self->{SVK}->copy ('-r', $prev, "$pprefix/$pname", $work_path);
	debug "copy from $pprefix/$pname\@$prev -> $work_path ($prefix)"
	    if debugging;
    }
    return $copied;
}

sub sort_filters {
    my $self = shift;
    require VCP::Filter::map;
    require VCP::Filter::stringedit;
    require VCP::Filter::changesets;;

    return ( @_ && $_[0] eq "change_id" ? () :
	     VCP::Filter::changesets->new
	     ( "",
	       [qw( time                   <=60
		    user_id                equal
		    comment                equal
		    source_branch_id       equal)]
	     ),
	     VCP::Filter::stringedit->new
	     ( "",
	       [ "user_id,name,labels", "@",   "_at_"   ,
		 "user_id,name,labels", "#",   "_pound_",
		 "user_id,name,labels", "*",   "_star_" ,
		 "user_id,name,labels", "%",   "_pcnt_" ,
		 "branch_id", "/", "_slash_" ,
	       ],
	     ),
	   );
}

=head1 AUTHORS

Chia-liang Kao E<lt>clkao@clkao.orgE<gt>

=head1 COPYRIGHT

Copyright 2004 by Chia-liang Kao E<lt>clkao@clkao.orgE<gt>.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

package VCP::Source::cvs;
use VCP::Debug qw( :debug ) ;
use VCP::Logger qw( lg );

# use cvs up -D for a derived timestamp, and if it does something bad that is discovered
# by cvs status output, fallback to individual fetching

sub _fix_state {
    my ($self, $state, $file) = @_;

    my $cvsroot = $self->repo_id;
    $cvsroot =~ s/^.*://;
    $state->{file} =~ s|^$cvsroot/||;
    $state->{file} =~ s|Attic/([^/]+)$|$1|;

    return unless exists $file->{$state->{file}};

    my $r = $file->{$state->{file}};
    debug "doing $state->{file} $state->{rev}" if debugging;
    if ($r->rev_id eq $state->{rev}) {
	my $wp = $self->work_path( "revs", $r->source_name, $r->source_rev_id );
	$self->mkpdir( $wp ) ;
	use File::Copy;
	copy($state->{file}, $wp) or die "copy failed: $! ";
    }
    else {
	lg "fallback to individual checkout: $state->{rev} vs ".$r->rev_id;
	my $fn = $self->get_source_file ($r);
    }
    delete $file->{$state->{file}};
}

sub _cvs_status_output {
    my ($self, $fh, $file) = @_;
    my $state;

    while (<$fh>) {
	if (m|^===================================================================|) {
	    $self->_fix_state($state, $file) if $state;
	    $state = {};
	}
	elsif (m/Working revision:\s+([\d\.]+)/) {
	    $state->{rev} = $1;
	}
	elsif (m/Repository revision:\s+[\d\.]+\s+(.*),v/) {
	    $state->{file} = $1;
	}
    }
    $self->_fix_state($state, $file) if $state;
}

sub get_source_files {
    my VCP::Source::cvs $self = shift ;
    my $max_time = 0;
    my %file;
    my $common_parent;
    my $common_offset;

    return map $self->get_source_file( $_ ), @_
	if $self->{CVS_PARSE_RCS_FILES} || $#_ < 3;

#    $self->create_cvs_workspace unless

    lg "fast checkout";

    my $samerev = $_[0]->rev_id;
    for my $r (@_) {
	my $t = $r->time ;
	$max_time = $t if $t >= $max_time;
	my $cvs_name = $self->SUPER::denormalize_name( $r->source_name );
	$file{$cvs_name} = $r;

	if (!$common_parent) {
	    $common_parent = $cvs_name;
	    $common_parent =~ s|/[^/]+$|/|;
	    $common_offset = index ($common_parent, '/') + 1;
	}
	elsif (index ($common_parent, '/', $common_offset) > 0) {
	    $common_parent =~ s|/[^/]+/$|/|
		while substr($cvs_name, 0, length($common_parent))
			     ne $common_parent;
	}

	undef $samerev if $samerev && $r->rev_id ne $_[0]->rev_id;
    }

    chop $common_parent;
    local $@;
    eval {
    $self->cvs(['update', '-d', $samerev ? ('-r', $_[0]->rev_id) :
		('-D', VCP::Rev::iso8601format($max_time), 
		 $_[0]->branch_id ? ('-r', $_[0]->branch_id) : ()), $common_parent]);
    $self->cvs(['status', $common_parent], undef, sub { $self->_cvs_status_output(@_, \%file) });
    };

    lg "fast update error: $@, fallback to iteration"
	if $@;

    return map $self->get_source_file( $_ ), @_ if $@;

    $self->get_source_file ($_) for values %file;

    return map { $self->work_path( "revs", $_->source_name, $_->source_rev_id ) ; } @_;
}

=head1 NOTES

=over

=item

Since VCP is not currently available on CPAN, you could get a dist
with fixes from L<http://wagner.elixus.org/~clkao/VCP-0.9-clkao.tar.gz>

=item

C<VCP::Dest::svk> uses branch info to decide the path in the resulting
repository.  So source repository with branches denoted by path names
like Perforce should normalize the path name and branch info. see
L<SVN::Mirror::VCP> for example.

=back

=head1 SEE ALSO

L<SVK>, L<SVK::Command::Mirror>, L<VCP>, L<VCP::Source::cvs>, L<VCP::Source::p4>

L<http://svk.elixus.org/index.cgi?MirrorVCP>

=head1 AUTHORS

Chia-liang Kao E<lt>clkao@clkao.orgE<gt>

=head1 COPYRIGHT

Copyright 2004 by Chia-liang Kao E<lt>clkao@clkao.orgE<gt>.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut

1;
