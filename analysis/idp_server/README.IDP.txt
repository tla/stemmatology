This directory contains the necessary scripts and machinery for running
the IDP graph calculation service. Installation is a little intricate as
it includes several components:

* The IDP system itself (available from 
  http://dtai.cs.kuleuven.be/krr/software/idp)
* The *.idp and *.lua scripts in this directory
* A Perl DBI-compatible database for the storage of calculation results
* A Gearman server for dispatching calculation requests
* The Perl script in this directory, which functions as a Gearman worker
* The CGI script in this directory, which functions as a Gearman client

INSTALLATION
============

0. Decide on a location for the following:
* The IDP system software (e.g. /usr/local/idp)
* The graph calculation IDP scripts (e.g. $HOME/graphcalc or
  /usr/local/idp/script)
* The database for storage of calculation results

Support software and packages
-----------------------------
1. Compile and install IDP for your platform into your chosen directory.
See http://dtai.cs.kuleuven.be/krr/software/idp for more information.

2. Install (or designate) a Gearman server. The relevant Ubuntu package is 'gearman-job-server'; installing it will set up a server on localhost.

3. Ensure that the following Perl (v5.12 or later) dependencies are
installed:
* Gearman::Client
* Gearman::Worker
* Graph
* Graph::Reader::Dot
* IPC::Run
* JSON
* Text::Tradition::Analysis
* Text::Tradition::Directory
* TryCatch

4. Install (or designate) a Supervisor daemon. The relevant Ubuntu
package is 'supervisor'; installing it will start a daemon on localhost.

Graph calculation service components
------------------------------------

5. Create a file /etc/graphcalc.conf as in the example here, substituting appropriate
values for your system:
--- START CONF FILE ---
DBTYPE = <database server type (e.g. mysql / Pg / SQLite / Sybase), 
		default mysql>
DBHOST = <database server host name or IP, default 127.0.0.1>
DBPORT = <database server port, default 3006>
DBNAME = <database for storage of results, default 'idpresult'>
DBUSER = <database username, default undef>
DBPASS = <password for database username, default undef>
GEARMAN_SERVER = <Gearman server host and port, 
		default 127.0.0.1:4730>
IDPBINPATH = <location of 'idp' binary, default /usr/local/idp/bin>
IDPSCRIPTPATH = <location of graph calculation scripts, 
		default /usr/local/idp/script>
TMPDIR = <location for temporary files, default /var/tmp>
---- END CONF FILE ----

The DBTYPE should be set to a value for which a Perl DBD::* module
exists. You may optionally specify a value for 'DSN' instead of listing
the type, host, port, and name separately, but this will break the
backup and restore scripts.

6. Copy the *.idp, *.lua, and *.pl scripts from this directory to the
directory specified in IDPSCRIPTPATH.

7. Create a database for the storage of calculation results. On MySQL
this can be done with the following sequence of statements, where
'user', 'host', and 'password' are chosen appropriately:

CREATE DATABASE idpresult; 
GRANT ALL PRIVILEGES ON idpresult.* TO "[user]"@"[host]" 
--> IDENTIFIED BY "[password]"; 
FLUSH PRIVILEGES;

8. Copy the graphcalc.cgi script to the cgi-bin directory of your
webserver, ensuring that it is executable and that the file permissions
are set appropriately.

9. Start one or more instances of the gearman_worker.pl script. It is
recommended that this be done through the Supervisor daemon. An example
Supervisor configuration script is given here:

--- START SUPERVISOR CONF ---
[program:graphcalc_worker]
command=/usr/local/idp/script/graphcalc_worker.pl
process_name=%(program_name)s.%(process_num)s
numprocs=4
user=www-data
---- END SUPERVISOR CONF ----

10. Run the idpinit.pl script to initialize the database.

11. Requests can now be sent via CGI to graphcalc - you're done!

Maintenance scripts
-------------------
Two maintenance scripts, idpbackup.pl and idprestore.pl, are included
with this distribution.  Each of these scripts can be invoked with no
arguments.

At the moment the restore script assumes that the database is on a MySQL
server; it does not know how to drop tables for any other database. The
backup script will dump all calculation results to
TMPDIR/idpresults.json; the restore script will wipe the database and
load them from the temp file. If nothing unexpected happens, the restore
script will also clean up the temp file.
