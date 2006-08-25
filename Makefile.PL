#!/usr/bin/perl
my $required_svn = '1.0.4';

use inc::Module::Install;

name		('VCP-Dest-svk');
author		('Chia-liang Kao <clkao@clkao.org>');
abstract	('VCP destination driver for svn/svk');
license		('perl');
version_from	('svk.pm');

requires(
        'VCP'   => '0.9',
	'SVK'   => '0.20',
	'YAML'  => '0',
);
include('ExtUtils::AutoInstall');
auto_install();

WriteAll( sign => 1 );

die "SVN::Core $required_svn must be manually installed"
    unless eval {require SVN::Core} && $SVN::Core::VERSION >= $required_svn;
