#!/bin/bash

( cd stemmaweb/db && rm -f traditions.db.test && rm -f traditions.db && ln -s traditions.db.test traditions.db )
script/make_tradition.pl -i self -o db -s t/data/florilegium.dot t/data/florilegium_graphml.xml
script/make_tradition.pl -i self -o db --dbid 'tradition:notre_besoin' -s t/data/besoin.dot t/data/besoin.xml
script/admin_users.pl -u 'admin@example.org' -p 'AdminPass' -r 'admin'
script/admin_users.pl -u 'user@example.org' -p 'UserPass'
script/admin_users.pl -c modify -u 'user@example.org' -t 'tradition:notre_besoin'

