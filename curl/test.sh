

# read -p 'НАЗВАНИЕ: ' NAME
# echo ${NAME}

# токены актуальны на 24.04.2021
# auth

# curl --data "login=fake&pass=fake" http://localhost/login

# curl --data "login=DELETED_USER&pass=DELETED_USER" http://localhost/login

# 2_admin_2021-04-24_5f0a06b2dcb5f11a62aea56d1197513e
# curl --data "login=admin&pass=123456" http://localhost/login

# 3_user_2021-04-24_23604cef9d6aeced41f136184af05eb7
# curl --data "login=pivan&pass=equalpass" http://localhost/login

# 4_user_2021-04-24_8e7eef03791cf45472a4b73e8502b56f
# curl --data "login=ysergey&pass=equalpass" http://localhost/login

# 5_user_2021-04-24_c498c0a7a1976ccff7bc9eba244155c7
# curl --data "login=psergey&pass=psergeypass" http://localhost/login

# 6_user_2021-04-24_ba4871393c6a06c0a1a6a048688a5d50
# curl --data "login=vmayakovskiy&pass=vmayakovskiypass" http://localhost/login

# 7_user_2021-04-24_3dfe19bc82f5fb06ab5b1150ce50939f
# curl --data "login=dmoskvin&pass=dmoskvinpass" http://localhost/login

# curl https://api.telegram.org/bot1410314989:AAGntdCR8ERCumCcs-JohMKqg0JmgTh5kzE/getUpdates

# select by id

# curl http://localhost/users/1
# fake token
# curl http://localhost/users/1 -H "Authorization: 2_admin_2021-04-24_5f0a06b2dcb5f11a62aea56d11975444" 
# curl http://localhost/users/1 -H "Authorization: 2_admin_2021-04-24_5f0a06b2dcb5f11a62aea56d1197513e"
# curl http://localhost/users/1 -H "Authorization: 3_user_2021-04-24_23604cef9d6aeced41f136184af05eb7"

# curl http://localhost/authors/1
# curl http://localhost/authors/1 -H "Authorization: 2_admin_2021-04-24_5f0a06b2dcb5f11a62aea56d1197513e"

# curl http://localhost/categories/1

# curl http://localhost/tags/1

# curl http://localhost/posts/1

# curl http://localhost/drafts/1
# curl http://localhost/drafts/1 -H "Authorization: 2_admin_2021-04-24_5f0a06b2dcb5f11a62aea56d1197513e"
# curl http://localhost/drafts/1 -H "Authorization: 3_user_2021-04-24_23604cef9d6aeced41f136184af05eb7"
# curl http://localhost/drafts/1 -H "Authorization: 4_user_2021-04-24_8e7eef03791cf45472a4b73e8502b56f"
# curl http://localhost/drafts/1 -H "Authorization: 5_user_2021-04-24_c498c0a7a1976ccff7bc9eba244155c7"
# curl http://localhost/drafts/1 -H "Authorization: 6_user_2021-04-24_ba4871393c6a06c0a1a6a048688a5d50"
# curl http://localhost/drafts/1 -H "Authorization: 7_user_2021-04-24_3dfe19bc82f5fb06ab5b1150ce50939f"

# curl http://localhost/posts/1/comments

# select
# curl http://localhost/users/
# curl http://localhost/users -H "Authorization: 2_admin_2021-04-24_5f0a06b2dcb5f11a62aea56d1197513e"
# curl http://localhost/users -H "Authorization: 3_user_2021-04-24_23604cef9d6aeced41f136184af05eb7"

# curl http://localhost/user
# curl http://localhost/user -H "Authorization: 2_admin_2021-04-24_5f0a06b2dcb5f11a62aea56d1197513e"
# curl http://localhost/user -H "Authorization: 3_user_2021-04-24_23604cef9d6aeced41f136184af05eb7"
# curl http://localhost/user -H "Authorization: 4_user_2021-04-24_8e7eef03791cf45472a4b73e8502b56f"
# curl http://localhost/user -H "Authorization: 5_user_2021-04-24_c498c0a7a1976ccff7bc9eba244155c7"
# curl http://localhost/user -H "Authorization: 6_user_2021-04-24_ba4871393c6a06c0a1a6a048688a5d50"
# curl http://localhost/user -H "Authorization: 7_user_2021-04-24_3dfe19bc82f5fb06ab5b1150ce50939f"


# curl http://localhost/authors
# curl http://localhost/authors -H "Authorization: 2_admin_2021-04-24_5f0a06b2dcb5f11a62aea56d1197513e"

# curl http://localhost/categories

# curl http://localhost/tags

# curl http://localhost/posts
# curl http://localhost/posts?page=2

#  Параметры для select запросов могут быть как в строке запроса, так и в теле, для остальных запросов только в теле 
# curl --data "tag_id__in=[1,2,3]" http://localhost/posts
# curl --data "created_at__bt=(2018-05-21,2030-05-21)"  http://localhost/posts
# curl --data "category_id__in=[1,2,3]" http://localhost/posts
# curl --data "name=Ya pomnyu chudnoe mgnovenye" http://localhost/posts
# curl --data "name__like=mgnovenye" http://localhost/posts
# curl --data "text__like=glasgow" http://localhost/posts
# curl --data "author_name__like=Denis" http://localhost/posts
# curl --data "author_name=Moskvin Denis" http://localhost/posts
# curl --data "contains__like=haskell" http://localhost/posts
# curl --data "order_by=fake" http://localhost/posts
# curl --data "order_by=created_at" http://localhost/posts
# curl --data "order_by=author_name" http://localhost/posts
# curl --data "order_by=author_name" http://localhost/posts
# curl --data "order_by=category_id" http://localhost/posts
# curl --data "order_by=photos" http://localhost/posts
# curl --data "page=1" http://localhost/posts

curl --data "tag_id__in=[1,2,3]&created_at__bt=(2018-05-21,2030-05-21)&category_id__in=[1,2,3]&name__like=GHC&text__like=glasgow&author_name__like=Denis&contains__like=haskell&order_by=created_at&page=1" http://localhost/posts

    # --data "tag_id__in=[1,2,3]" 
    # --data "created_at__bt=(2018-05-21,2030-05-21)" 
    # --data "category_id__in=[1,2,3]"
    # --data "name__like=mgnovenye"
    # --data "text__like=glasgow"
    # --data "author_name=Denis"
    # --data "contains__like=haskell"
    # --data "order_by=created_at"
    # --data "order_by=author_name"
    # --data "order_by=category_id"
    # --data "order_by=photos"
    # --data "page=1"





# &category_id__in="[1,2,3]"
# &name=mgnovenye
    # &text__like=glasgow
    # &author_name=Denis
    # &contains__like=haskell

# ошибка JSON.evalParams category_id Map.!: given key is not an element in the map
# сравнить со старой версией!!
# curl http://localhost/drafts
# curl http://localhost/drafts -H "Authorization: 2_admin_2021-04-24_5f0a06b2dcb5f11a62aea56d1197513e"
# curl http://localhost/drafts -H "Authorization: 3_user_2021-04-24_23604cef9d6aeced41f136184af05eb7"
# curl http://localhost/drafts -H "Authorization: 4_user_2021-04-24_8e7eef03791cf45472a4b73e8502b56f"
# curl http://localhost/drafts -H "Authorization: 5_user_2021-04-24_c498c0a7a1976ccff7bc9eba244155c7"
# curl http://localhost/drafts -H "Authorization: 6_user_2021-04-24_ba4871393c6a06c0a1a6a048688a5d50"
# curl http://localhost/drafts -H "Authorization: 7_user_2021-04-24_3dfe19bc82f5fb06ab5b1150ce50939f"

# curl http://localhost/posts/1/comments

# insert 

# curl http://localhost/users/create?last_name
# curl --data "login=DELETED_USER&pass=DELETED_USER" http://localhost/login

