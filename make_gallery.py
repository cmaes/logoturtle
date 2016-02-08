#!/usr/bin/env python
import os

head = """ <html>
           <head>
           <style>
           body {
             margin: 1em;
           }
           img {
            max-width: 320px;
            max-height: 320px
           }
           .code {
             width: 70%;
             float:right }
           .output {
             float: left;
             width: 25%; }
           .container {
             margin: 0;
             margin-bottom: 1em;
             width: 100%;
             max-height: 320px;
             overflow: auto
            }
            </style>
            </head>
            <body> """

with open("samples.html", "w") as out:
    print >>out, head
    for filename in os.listdir(os.getcwd()):
        if filename.endswith('.png'):
            srcname  = 'sample_programs/' + filename[:-4] + '.logo'
            print >>out, '<div class="container">'
            print >>out, '<div class="output"><img src="%s"/></div>' % filename
            with open(srcname, 'r') as logosrc:
                print >>out, '<div class="code"><pre>%s</pre></div>' % logosrc.read()
            print >>out, '</div>'
    print >>out, "</body>\n</html>"
