#!/bin/bash


python -m crawler.mine \
    --source reddit \
    --submission_id 8s67ow \
    --mode users \
    --framework baf \
    --path ../data/prolog/kb/reddit/prova/ \
    --verbose