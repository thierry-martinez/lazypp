#!/bin/sh
set -ex
for file in original/*.c; do
    basename="$(basename "$file")"
    target="preprocessed/$basename"
    dune exec ../../frontend/frontend.exe -- \
         -I original/include $file -o $target \
         --undef-all --gcc \
         -e listSET_FIRST_LIST_ITEM_INTEGRITY_CHECK_VALUE \
         -e listSET_SECOND_LIST_ITEM_INTEGRITY_CHECK_VALUE \
         -e listSET_LIST_INTEGRITY_CHECK_1_VALUE \
         -e listSET_LIST_INTEGRITY_CHECK_2_VALUE \
         -e listTEST_LIST_INTEGRITY \
         -e listTEST_LIST_ITEM_INTEGRITY \
         -D configSUPPORT_STATIC_ALLOCATION=1 \
         -D configUSE_TASK_NOTIFICATIONS=0
done
for file in original/*.c; do
    basename="$(basename "$file")"
    target="preprocessed/$basename"
    by_hand="simplified-by-hand/$basename"
    diff $target $by_hand || true
done
