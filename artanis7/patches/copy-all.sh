#! /bin/bash

SOURCE_DIR="/home/mbc/projects/autostall/artanis7/patches/"
DEST_DIR="/home/mbc/projects/artanis/artanis/"

cp "$SOURCE_DIR""env.scm" "$DEST_DIR""env.scm"
cp "$SOURCE_DIR""utils.scm" "$DEST_DIR""utils.scm"
cp "$SOURCE_DIR""tpl/parser.scm" "$DEST_DIR""tpl/parser.scm"
cp "$SOURCE_DIR""commands/work.scm" "$DEST_DIR""commands/work.scm"
cp "$SOURCE_DIR""mvc/controller.scm" "$DEST_DIR""mvc/controller.scm"
cp "$SOURCE_DIR""webapi/restful.scm" "$DEST_DIR""webapi/restful.scm"

cp "$SOURCE_DIR""session.scm" "$DEST_DIR""session.scm"
cp "$SOURCE_DIR""lpc.scm" "$DEST_DIR""lpc.scm"

