#!/usr/bin/env bash

./run-examples.sh

ssh gutenberg 'mkdir -p public_html/haray'

imgs=""
for png in output/*.png
do
  img=$(basename ${png})
  alt=$(basename ${png} .png)
  imgs="${imgs}<a href=\"${alt}.txt\"><img src=\"${img}\" alt=\"${alt}\" /></a>"
done

cat > output/index.html <<EOF
<html>
  <head>
    <title>Haray Images!</title>
    <meta http-equiv="refresh" content="60">
    <style type="text/css">
      img {
      margin: 0.5em;
      }
    </style>
  </head>
  <body>
    <div>
      ${imgs}
    </div>
  </body>
</html>
EOF

scp output/* gutenberg:public_html/haray/
for scn in scenes/*
do
  scp ${scn} gutenberg:public_html/haray/$(basename ${scn}).txt
done

ssh gutenberg 'chmod a+r public_html/haray/*'

