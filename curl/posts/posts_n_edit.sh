cd "$(dirname "$0")"
cd ..
source tokens.sh
source host.sh

APIPATH=/posts/3/edit
URL="$HOST$APIPATH"
BODY="content_name=name"
BODY+="&category_id=3"
BODY+="&content_text=text3"
BODY+="&photo=photo3.jpg"
BODY+="&tag_id__all=[1,2,3,4,5]"
BODY+="&photos__all=[\"photo5.jpg\",\"photo6.jpg\"]"

curl --data "$BODY" ${URL}
curl --data "$BODY" ${URL} -H "$FAKEUSER"
curl --data "$BODY" ${URL} -H "$USER3"
curl --data "$BODY" ${URL} -H "$USER4"
curl --data "$BODY" ${URL} -H "$USER5"
curl --data "$BODY" ${URL} -H "$USER6"
curl --data "$BODY" ${URL} -H "$USER7"
curl --data "$BODY" ${URL} -H "$ADMIN"