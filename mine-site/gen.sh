#!/usr/bin/env bash

gen_site() {
    image=$1
    filename=$(basename ${image%.png})
    mdfile=posts/${filename/_/-}.md
    if [ ! -f ${mdfile} ]; then
        echo $mdfile
        cat > ${mdfile} <<EOF
---
title: ${filename}
pos:
world:
cover: /${image}
---

![](/${image})
EOF
    fi
}

find images -name '*.png' | while read F; do
    gen_site $F
done
