cd "$(dirname "$0")"
cd ..
source tokens.sh
source host.sh

APIPATH=/user/edit
URL="$HOST$APIPATH"
BODY="last_name=last_name2"
BODY+="&first_name=first_name2"
BODY+="&avatar=avatar2"
BODY+="&pass=pass2"

curl --data "$BODY" ${URL}
curl --data "$BODY" ${URL} -H "$FAKEUSER"
curl --data "$BODY" ${URL} -H "$USER3"
curl --data "$BODY" ${URL} -H "$USER4"
curl --data "$BODY" ${URL} -H "$USER5"
curl --data "$BODY" ${URL} -H "$USER6"
curl --data "$BODY" ${URL} -H "$USER7"
curl --data "$BODY" ${URL} -H "$ADMIN"