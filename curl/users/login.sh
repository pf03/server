cd "$(dirname "$0")"
cd ..
# source tokens.sh
source host.sh

APIPATH=/login
URL="$HOST$APIPATH"

curl --data "login=fake&pass=fake" ${URL}
curl --data "login=DELETED_USER&pass=DELETED_USER" ${URL}
curl --data "login=admin&pass=123456" ${URL}
curl --data "login=pivan&pass=equalpass" ${URL}
curl --data "login=ysergey&pass=equalpass" ${URL}
curl --data "login=psergey&pass=psergeypass" ${URL}
curl --data "login=vmayakovskiy&pass=vmayakovskiypass" ${URL}
curl --data "login=dmoskvin&pass=dmoskvinpass" ${URL}