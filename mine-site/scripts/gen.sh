#!/usr/bin/env bash

gen_site() {
    image=$1
    filename=$(basename ${image%.png})
    gm convert -crop 1000x30+90+330 ${image} temp/${filename}-xyz.png # 切出坐标图
    gm convert -crop 1000x30+90+330 ${image} temp/${filename}-world.png # 切出世界类型
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

mkdir -p temp

find images -name '*.png' | while read F; do
    gen_site $F
done
