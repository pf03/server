cd "$(dirname "$0")"
cd ..
source tokens.sh
source host.sh

APIPATH=/tags/2/edit
URL="$HOST$APIPATH"
BODY="tag_name=edited_tag"

curl --data "$BODY" ${URL}
curl --data "$BODY" ${URL} -H "$FAKEUSER"
curl --data "$BODY" ${URL} -H "$USER3"
curl --data "$BODY" ${URL} -H "$USER4"
curl --data "$BODY" ${URL} -H "$USER5"
curl --data "$BODY" ${URL} -H "$USER6"
curl --data "$BODY" ${URL} -H "$USER7"
curl --data "$BODY" ${URL} -H "$ADMIN"