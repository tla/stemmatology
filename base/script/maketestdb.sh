#!/bin/bash

DBDIR=$1
DSN=dbi:SQLite:dbname=$DBDIR/traditions.db

( cd $DBDIR && rm -f traditions.db.test && rm -f traditions.db && ln -s traditions.db.test traditions.db )
script/make_tradition.pl -i self -o db --dsn $DSN -s t/data/florilegium.dot t/data/florilegium_graphml.xml
script/make_tradition.pl -i self -o db --dsn $DSN --dbid 'tradition:notre_besoin' -s t/data/besoin.dot t/data/besoin.xml
script/admin_users.pl --dsn $DSN -u 'admin@example.org' -p 'AdminPass' -r 'admin'
script/admin_users.pl --dsn $DSN -u 'user@example.org' -p 'UserPass'
script/admin_users.pl --dsn $DSN -c modify -u 'user@example.org' -t 'tradition:notre_besoin'

