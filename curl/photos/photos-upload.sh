cd "$(dirname "$0")"
cd ..
# source tokens.sh
source host.sh

APIPATH=photos/upload?name=zvezdnoe_nebo_test.png
URL="$HOST$APIPATH"

cd "$(dirname "$0")"
curl -T zvezdnoe_nebo.png ${URL}