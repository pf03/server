
# source /d/Projects/Haskell/Server2/server/curl/users/tokens.sh
# echo $PWD
# BASEDIR=../$(dirname $(realpath "$0"))
# BASEDIR=$(realpath "$0")
# BASEDIR=../test.sh
# source $BASEDIR
# echo $BASEDIR
cd "$(dirname "$0")"
cd ..
echo $PWD
source tokens.sh

curl http://localhost/users/
curl http://localhost/users -H "Authorization: 2_admin_2021-04-24_5f0a06b2dcb5f11a62aea56d1197513e"
curl http://localhost/users -H "Authorization: 3_user_2021-04-24_23604cef9d6aeced41f136184af05eb7"
