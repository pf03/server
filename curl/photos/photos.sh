cd "$(dirname "$0")"
cd ..
# source tokens.sh
source host.sh

APIPATH=/photos
URL="$HOST$APIPATH"
BODY="page=1"

curl --data "$BODY" ${URL}