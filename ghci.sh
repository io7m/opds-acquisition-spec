#!/bin/sh
exec ghci -W -Wall -Werror -pgmL markdown-unlit OPDS.lhs
