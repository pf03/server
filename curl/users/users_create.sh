cd "$(dirname "$0")"
cd ..
source tokens.sh
source host.sh

APIPATH=/users/create
URL="$HOST$APIPATH"
BODY="last_name=last_name"
BODY+="&first_name=first_name"
BODY+="&avatar=avatar"
BODY+="&user_login=login"
BODY+="&pass=pass"

curl --data "$BODY" ${URL}
curl --data "$BODY" ${URL} -H "$FAKEUSER"
curl --data "$BODY" ${URL} -H "$USER3"
curl --data "$BODY" ${URL} -H "$USER4"
curl --data "$BODY" ${URL} -H "$USER5"
curl --data "$BODY" ${URL} -H "$USER6"
curl --data "$BODY" ${URL} -H "$USER7"
curl --data "$BODY" ${URL} -H "$ADMIN"