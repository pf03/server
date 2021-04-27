FAKEUSER="Authorization: 2_admin_2021-04-24_5f0a06b2dcb5f11a62aea56d11975444"
ADMIN="Authorization: 2_admin_2021-04-24_5f0a06b2dcb5f11a62aea56d1197513e"
USER3="Authorization: 3_user_2021-04-24_23604cef9d6aeced41f136184af05eb7"
USER4="Authorization: 4_user_2021-04-24_8e7eef03791cf45472a4b73e8502b56f"
USER5="Authorization: 5_user_2021-04-24_c498c0a7a1976ccff7bc9eba244155c7"
USER6="Authorization: 6_user_2021-04-24_ba4871393c6a06c0a1a6a048688a5d50"
USER7="Authorization: 7_user_2021-04-24_3dfe19bc82f5fb06ab5b1150ce50939f"

# echo "hello"

# read -p 'НАЗВАНИЕ: ' NAME
# echo ${NAME}

# токены актуальны на 24.04.2021
# auth


# в функции логин тоже нужно сделать подмену даты!!
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

# BODY="tag_id__in=[1,2,3]"
# BODY+="&created_at__bt=(2018-05-21,2030-05-21)"
# BODY+="&category_id__in=[1,2,3]"
# BODY+="&name__like=GHC"
# BODY+="&text__like=glasgow"
# BODY+="&author_name__like=Denis"
# BODY+="&contains__like=haskell"
# BODY+="&order_by=created_at"
# BODY+="&page=1"
# curl --data "$BODY" http://localhost/posts



# curl http://localhost/posts/1/comments

# insert 

# user

# BODY="last_name=last_name"
# BODY+="&first_name=first_name"
# BODY+="&avatar=avatar"
# BODY+="&login=login"
# BODY+="&pass=pass"
# curl --data "$BODY" http://localhost/users/create



# curl --data "" http://localhost/users/2/edit
# curl --data "" http://localhost/users/2/edit  -H "$USER3" 
# curl --data "" http://localhost/users/2/edit  -H "$ADMIN" 

# BODY="last_name=last_name"
# BODY+="&first_name=first_name"
# BODY+="&avatar=avatar"
# BODY+="&pass=pass"
# curl --data "$BODY" http://localhost/users/2/edit  -H "$ADMIN" 


# BODY="last_name=last_name"
# BODY+="&first_name=first_name"
# BODY+="&avatar=avatar"
# BODY+="&pass=pass"
# curl --data "$BODY" http://localhost/user/edit  -H "$ADMIN" 
# curl --data "$BODY" http://localhost/user/edit  -H "$USER3" 
# curl --data "$BODY" http://localhost/user/edit  -H "$USER4" 
# curl --data "$BODY" http://localhost/user/edit  -H "$USER5" 

# curl --data "$BODY" http://localhost/users/3/delete
# curl --data "$BODY" http://localhost/users/3/delete  -H "$USER5" 
# curl --data "$BODY" http://localhost/users/3/delete  -H "$USER3" 
# curl --data "$BODY" http://localhost/users/3/delete  -H "$ADMIN" 

# author

# BODY="user_id=4"
# BODY+="&description=description"
# curl --data "$BODY" http://localhost/authors/create
# curl --data "$BODY" http://localhost/authors/create  -H "$USER4" 
# curl --data "$BODY" http://localhost/authors/create  -H "$ADMIN" 

# BODY="user_id=4"
# BODY+="&description=description"
# curl --data "$BODY" http://localhost/authors/2/edit 
# curl --data "$BODY" http://localhost/authors/2/edit   -H "$USER4" 
# curl --data "$BODY" http://localhost/authors/2/edit   -H "$ADMIN" 

# curl --data "$BODY" http://localhost/authors/2/delete
# curl --data "$BODY" http://localhost/authors/2/delete   -H "$USER4" 
# curl --data "$BODY" http://localhost/authors/2/delete   -H "$ADMIN" 

#category

# BODY="parent_id=2"
# BODY+="&category_name=category_name"
# curl --data "$BODY" http://localhost/categories/create
# curl --data "$BODY" http://localhost/categories/create  -H "$USER4" 
# curl --data "$BODY" http://localhost/categories/create  -H "$ADMIN" 

# BODY="parent_id=2"
# BODY+="&category_name=category_name"
# curl --data "$BODY" http://localhost/categories/2/edit 
# curl --data "$BODY" http://localhost/categories/2/edit   -H "$USER4" 
# curl --data "$BODY" http://localhost/categories/2/edit   -H "$ADMIN" 

# curl --data "$BODY" http://localhost/categories/2/delete
# curl --data "$BODY" http://localhost/categories/2/delete   -H "$USER4" 
# curl --data "$BODY" http://localhost/categories/2/delete   -H "$ADMIN" 
# curl --data "$BODY" http://localhost/categories/9/delete   -H "$ADMIN" 

# tag
# BODY="name=some_tag"
# curl --data "$BODY" http://localhost/tags/create 
# curl --data "$BODY" http://localhost/tags/create   -H "$USER4" 
# curl --data "$BODY" http://localhost/tags/create   -H "$ADMIN"

# BODY="name=edited_tag"
# URL=http://localhost/tags/2/edit
# curl --data "$BODY" ${URL}
# curl --data "$BODY" ${URL} -H "$USER4" 
# curl --data "$BODY" ${URL} -H "$ADMIN" 

# URL=http://localhost/tags/2/delete
# curl --data "$BODY" ${URL}
# curl --data "$BODY" ${URL} -H "$USER4" 
# curl --data "$BODY" ${URL} -H "$ADMIN" 

# draft

# BODY="page=1"
# URL=http://localhost/drafts
# curl ${URL}
# curl ${URL} -H "$ADMIN"
# curl ${URL} -H "$USER3"
# curl ${URL} -H "$USER4"
# curl ${URL} -H "$USER5"
# curl ${URL} -H "$USER6"
# curl ${URL} -H "$USER7"

# BODY="name=name"
# BODY+="&category_id=1"
# BODY+="&text=text"
# BODY+="&photo=photo.jpg"
# BODY+="&tag_id__all=[1,2,3]"
# BODY+="&photos__all=[\"photo1.jpg\",\"photo2.jpg\"]"
# URL=http://localhost/drafts/create 
# curl --data "$BODY" ${URL}
# curl --data "$BODY" ${URL} -H "$USER4" 
# curl --data "$BODY" ${URL} -H "$ADMIN"
# curl --data "$BODY" ${URL} -H "$USER5" 

# BODY="name=name"
# BODY+="&category_id=2"
# BODY+="&text=text2"
# BODY+="&photo=photo2.jpg"
# BODY+="&tag_id__all=[1,2,3,4]"
# BODY+="&photos__all=[\"photo3.jpg\",\"photo4.jpg\"]"
# URL=http://localhost/drafts/2/edit 
# curl --data "$BODY" ${URL}
# curl --data "$BODY" ${URL} -H "$USER4" 
# curl --data "$BODY" ${URL} -H "$ADMIN"
# curl --data "$BODY" ${URL} -H "$USER5" 

# URL=http://localhost/drafts/2/delete
# curl --data "$BODY" ${URL}
# curl --data "$BODY" ${URL} -H "$USER4" 
# curl --data "$BODY" ${URL} -H "$ADMIN"
# curl --data "$BODY" ${URL} -H "$USER5"

# post

# BODY="page=1"
# URL=http://localhost/posts
# curl --data "$BODY" ${URL}

# URL=http://localhost/drafts/2/publish 
# curl ${URL}
# curl ${URL} -H "$USER4"
# curl ${URL} -H "$USER5"  
# curl ${URL} -H "$ADMIN"

# URL=http://localhost/posts/3/edit
# BODY="name=name"
# BODY+="&category_id=3"
# BODY+="&text=text3"
# BODY+="&photo=photo3.jpg"
# BODY+="&tag_id__all=[1,2,3,4,5]"
# BODY+="&photos__all=[\"photo5.jpg\",\"photo6.jpg\"]"
# curl --data "$BODY" ${URL}
# curl --data "$BODY" ${URL} -H "$USER4"
# curl --data "$BODY" ${URL} -H "$USER5"  
# curl --data "$BODY" ${URL} -H "$ADMIN"

# URL=http://localhost/posts/3/delete
# curl --data "$BODY" ${URL}
# curl --data "$BODY" ${URL} -H "$USER4" 
# curl --data "$BODY" ${URL} -H "$USER5"
# curl --data "$BODY" ${URL} -H "$ADMIN"

# comments

# BODY="page=1"
# URL=http://localhost/posts/1/comments
# curl --data "$BODY" ${URL}

# BODY="text=comment_text"
# URL=http://localhost/posts/1/comments/create
# curl --data "$BODY" ${URL}
# curl --data "$BODY" ${URL} -H "$USER4" 
# curl --data "$BODY" ${URL} -H "$USER5"
# curl --data "$BODY" ${URL} -H "$ADMIN"


# URL=http://localhost/comments/6/delete
# curl --data "$BODY" ${URL}
# curl --data "$BODY" ${URL} -H "$USER4" 
# curl --data "$BODY" ${URL} -H "$USER5"
# curl --data "$BODY" ${URL} -H "$ADMIN"






