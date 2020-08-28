#!/bin/sh

cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html
  PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
   <head>
      <meta http-equiv="content-type" content="application/xhtml+xml; charset=utf-8"/>
      <link rel="stylesheet" type="text/css" href="style.css"/>
      <title>OPDS Acquisition Selection 1.0</title>
   </head>
   <body>
EOF

pandoc OPDS.lhs || exit 1

cat <<EOF
   </body>
</html>
EOF
