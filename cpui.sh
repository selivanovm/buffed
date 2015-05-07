#!/bin/sh

rm -rf static/js/app
mkdir -p static/js/app

cp -a ui/out static/js/app
cp -a ui/ui.js static/js/app/ui.js
