# trivial-nntp
Common lisp tools for connecting to and crawling around NNTP servers.  It uses usocket.

This is a minimalistic effort; however watch this:
    
     CL-USER> (tnntp:connect)
     #<USOCKET:STREAM-USOCKET {100E6BDDD3}>
     CL-USER> (tnntp:send-command "HELP")
     1
     "100 Legal commands^M"
     CL-USER> (tnntp:read-list)
     "Report problems to <usenet@fleegle.mixmin.net>.^M"
     "  XPAT header message-ID|range pattern [pattern ...]^M" "  XOVER [range]^M"
     "  XHDR header [message-ID|range]^M" "  XGTITLE [wildmat]^M"
     "  STAT [message-ID|number]^M" "  STARTTLS^M" "  QUIT^M" "  POST^M"
     "  OVER [range]^M" "  NEXT^M" "  NEWNEWS wildmat [yy]yymmdd hhmmss [GMT]^M"
     "  NEWGROUPS [yy]yymmdd hhmmss [GMT]^M" "  MODE READER^M"
     "  LISTGROUP [newsgroup [range]]^M"
     "  LIST [ACTIVE [wildmat]|ACTIVE.TIMES [wildmat]|COUNTS [wildmat]|DISTRIB.PATS|DISTRIBUTIONS|HEADERS [MSGID|RANGE]|MODERATORS|MOTD|NEWSGROUPS [wildmat]|OVERVIEW.FMT|SUBSCRIPTIONS [wildmat]]^M"
     "  LAST^M" "  IHAVE message-ID^M" "  HELP^M" "  HEAD [message-ID|number]^M"
     "  HDR header [message-ID|range]^M" "  GROUP newsgroup^M" "  DATE^M"
     "  CAPABILITIES [keyword]^M" "  BODY [message-ID|number]^M"
     "  AUTHINFO USER name|PASS password|GENERIC program [argument ...]^M"
     "  ARTICLE [message-ID|number]^M")
     CL-USER> (tnntp:disconnect)
    2
    "205 Bye!^M"
    CL-USER>

The system uses several special variables to keep defaults which come in handy in interactive work:
*acct*    structure contains server name, address, username and password
*socket*  contains the usocket opened by most recent connect.

Most functions can be called with :socket parameter; otherwise *socket* will be used.

Commands are sent with
    (send-command "commandstring" :expecting 2)
The expecting parameter, if specified, makes sure that the response in in 200-299 range (only first digit is checked).

Response are read with
(read-unit) - read one line
(read-list) - read lines terminated by ".^M" into a list

Lines are returned unprocessed, with control-M character.  Rationale: you will probably parse the lines anyway, so there is little reason to worry about that.

TODO: installable parsers?