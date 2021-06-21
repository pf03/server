cd "$(dirname "$0")"
cd ..
source tokens.sh
source host.sh

APIPATH=/drafts/create
URL="$HOST$APIPATH"
BODY="name=name"
BODY+="&category_id=1"
BODY+="&text=text"
BODY+="&photo=photo.jpg"
BODY+="&tag_id__all=[1,2,3]"
BODY+="&photos__all=[\"photo1.jpg\",\"photo2.jpg\"]"

curl --data "$BODY" ${URL}
curl --data "$BODY" ${URL} -H "$FAKEUSER"
curl --data "$BODY" ${URL} -H "$USER3"
curl --data "$BODY" ${URL} -H "$USER4"
curl --data "$BODY" ${URL} -H "$USER5"
curl --data "$BODY" ${URL} -H "$USER6"
curl --data "$BODY" ${URL} -H "$USER7"
curl --data "$BODY" ${URL} -H "$ADMIN"