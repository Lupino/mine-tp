#!/usr/bin/env bash

gen_site() {
    image=$1
    filename=$(basename ${image%.png})
    XYZ_PATH=temp/${filename}-xyz.png
    WORLD_PATH=temp/${filename}-world.png
    gm convert -crop 800x30+90+330 ${image} ${XYZ_PATH} # 切出坐标图
    pos=$(python scripts/ocr.py ${XYZ_PATH})
    rm ${XYZ_PATH}

    gm convert -crop 600x30+0+260 ${image} ${WORLD_PATH} # 切出世界类型
    world=$(python scripts/ocr.py ${WORLD_PATH} world)
    rm ${WORLD_PATH}

    mdfile=posts/${filename/_/-}.md
    if [ ! -f ${mdfile} ]; then
        echo $mdfile
        cat > ${mdfile} <<EOF
---
title: ${filename}
pos: ${pos}
world: ${world}
cover: /${image}
---

![](/${image})
EOF
    fi
}

mkdir -p temp
mkdir -p temp/nums

find images -name '*.png' | while read F; do
    gen_site $F
done
