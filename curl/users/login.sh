cd "$(dirname "$0")"
cd ..
# source tokens.sh
source host.sh

APIPATH=/login
URL="$HOST$APIPATH"

curl --data "user_login=fake&pass=fake" ${URL}
curl --data "user_login=DELETED_USER&pass=DELETED_USER" ${URL}
curl --data "user_login=admin&pass=123456" ${URL}
curl --data "user_login=pivan&pass=equalpass" ${URL}
curl --data "user_login=ysergey&pass=equalpass" ${URL}
curl --data "user_login=psergey&pass=psergeypass" ${URL}
curl --data "user_login=vmayakovskiy&pass=vmayakovskiypass" ${URL}
curl --data "user_login=dmoskvin&pass=dmoskvinpass" ${URL}