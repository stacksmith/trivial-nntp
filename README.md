# trivial-nntp
Common lisp tools for connecting to and crawling around NNTP servers.

This is a minimalistic effort; however watch this:
    
     CL-USER> (tnntp:connect)
     2
     "200 news.mixmin.net InterNetNews NNRP server INN 2.7.0 (20151105 prerelease) ready (posting ok)^M"
     CL-USER> (tnntp:disconnect)
     2
     "205 Bye^M"
     CL-USER> (tnntp:connect)
     2
     "200 news.mixmin.net InterNetNews NNRP server INN 2.7.0 (20151105 prerelease) ready (posting ok)^M"
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
