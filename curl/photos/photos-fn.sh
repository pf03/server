cd "$(dirname "$0")"
cd ..
# source tokens.sh
source host.sh


APIPATH=photos/zvezdnoe_nebo_test.png
URL="$HOST$APIPATH"
BODY=""

echo  "This function is better tested in browser or postman because it returns an image!"
echo  "Copy this URL fot test:"
echo  ${URL}

# curl --data "$BODY" ${URL}