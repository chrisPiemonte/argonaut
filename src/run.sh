#!/bin/bash


DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Mining reddit example
python -m crawler.mine \
    --source reddit \
    --submission_id 8s67ow \
    --mode users \
    --framework baf \
    --path $DIR/../data/prolog/kb/reddit/ \
    --verbose


# Mining stackoverflow example
# python -m crawler.mine \
#     --source stackoverflow \
#     --question_id 29480099 \
#     --mode users \
#     --framework baf \
#     --path $DIR/../data/prolog/kb/stackoverflow/ \
#     --verbose

# Mining twitter example
# python -m crawler.mine \
#     --source twitter \
#     --query coronavirus \
#     --mode users \
#     --framework baf \
#     --path $DIR/../data/prolog/kb/twitter/ \
#     --verbose