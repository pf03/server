cd "$(dirname "$0")"
cd ..
source tokens.sh
source host.sh

APIPATH=/posts
URL="$HOST$APIPATH"

# single filters
curl --data "tag_id__in=[1,2,3]" ${URL}
curl --data "created_at__bt=(2018-05-21,2030-05-21)" ${URL}
curl --data "category_id__in=[1,2,3]" ${URL}
curl --data "content_name=Ya pomnyu chudnoe mgnovenye" ${URL}
curl --data "content_name__like=mgnovenye" ${URL}
curl --data "content_text__like=glasgow" ${URL}
curl --data "author_name__like=Denis" ${URL}
curl --data "author_name=Moskvin Denis" ${URL}
curl --data "contains__like=haskell" ${URL}
curl --data "order_by=fake" ${URL}
curl --data "order_by=created_at" ${URL}
curl --data "order_by=author_name" ${URL}
curl --data "order_by=author_name" ${URL}
curl --data "order_by=category_id" ${URL}
curl --data "order_by=photos" ${URL}
curl --data "page=1" ${URL}

#  all filters together
BODY="tag_id__in=[1,2,3]"
BODY+="&created_at__bt=(2018-05-21,2030-05-21)"
BODY+="&category_id__in=[1,2,3]"
BODY+="&content_name__like=GHC"
BODY+="&content_text__like=glasgow"
BODY+="&author_name__like=Denis"
BODY+="&contains__like=haskell"
BODY+="&order_by=created_at"
BODY+="&page=1"

curl --data "$BODY" ${URL}
curl --data "$BODY" ${URL} -H "$FAKEUSER"
curl --data "$BODY" ${URL} -H "$USER3"
curl --data "$BODY" ${URL} -H "$USER4"
curl --data "$BODY" ${URL} -H "$USER5"
curl --data "$BODY" ${URL} -H "$USER6"
curl --data "$BODY" ${URL} -H "$USER7"
curl --data "$BODY" ${URL} -H "$ADMIN"