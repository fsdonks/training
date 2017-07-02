#!/usr/bin/awk -f
#
# clj2org -- Convert Tom's annotated .clj files into org-mode goodness
# Marginalia is not going to lose a job because of this script. :D
#
# fs-rick, 1 July 2017, Unlicense (https://unlicense.org).

BEGIN {
    # Enumeration for input line types.
    blank = 0
    text  = 1
    code  = 2

    # Are we in a text block or a code block?
    block     = text
    prevblock = text
}

/^;/ {
    type = block = text
}
/^[\t ]*$/ {
    type = blank
}
!/^;/ && !/^[\t ]*$/ {
    type = block = code
}

type == text {
    sub(/^[;]+/,"",$0)
    if ( /^==+[\t ]*$/ ) {
         prevheaderlevel = 1
         $0 = ""
         prev = "* " prev
    }
    if ( /^```/ ) {
        fenced = !fenced
        if ( fenced )
            $0 = "#+BEGIN_EXAMPLE"
        else
            $0 = "#+END_EXAMPLE"
    }   
}

block == code && type == blank {
    numblanklinesinarow++
}
block == code && type == code {
    prevnumblanklinesinarow = numblanklinesinarow
    numblanklinesinarow = 0
}

!(block     == code && type     == blank &&
  prevblock == code && prevtype == blank) {
    # Ensure blank lines between a code block and the following text
    # block ends up in the text block.
    if (prevblock == text && prev2block == code)
        for (i=0; i < prevnumblanklinesinarow; i++)
            print ""
    # Suppress blank lines at the end of code blocks.
    if (!(block == text && prevblock == code && prevtype == blank))
        print prev
    # If the (previous) line printed (just above) was blank and part
    # of a code block which is still active, then print the original
    # vertical whitespace.
    if (block == code && prevblock == code && prevtype == blank)
        for (i=1; i < prevnumblanklinesinarow; i++)
            print ""
    prev2block = prevblock
    prev2fenced = prevfenced
    prev = $0
    prevtype = type
    prevblock = block
    prevheaderlevel = 0
    prevfenced = fenced
    prevnumblanklinesinarow = numblanklinesinarow
}

prevblock == text {
    if ( prev2block == code )
        print "#+END_SRC"
}

prevblock == code {
    if ( prev2block == text )
        print "#+BEGIN_SRC clojure"
}

END {
    print prev
    if ( prevblock == code )
        print "#+END_SRC"
}
