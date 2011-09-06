#! /bin/sh

find -type l -delete

ln -s server/ ./files/data/ocsigenserver

ln -s ../resources ./files/data/ocsimore/resources
ln -s ../resources ./files/data/obrowser/resources
ln -s ../resources/ ./files/data/eliom/resources
ln -s ../resources ./files/data/tyxml/resources
ln -s ../resources ./files/data/server/resources
ln -s ../resources ./files/data/oclosure/resources
ln -s ../resources ./files/data/main/resources
ln -s ../resources ./files/data/devarea/resources
ln -s ../resources ./files/data/ocsforge/resources
ln -s ../resources ./files/data/js_of_ocaml/resources
ln -s ../resources ./files/data/occduce/resources
ln -s ../resources ./files/data/macaque/resources
ln -s ../resources ./files/data/tutorial/resources
ln -s ../resources ./files/data/tmp_css/resources
ln -s ../resources ./files/data/lwt/resources
