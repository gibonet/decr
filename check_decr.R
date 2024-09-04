

library(devtools)

load_all()

document()
build()

check_man()
check()


# git
# git remote add github git@github.com:gibonet/decr.git

# git remote -v
# github	git@github.com:gibonet/decr.git (fetch)
# github	git@github.com:gibonet/decr.git (push)
# origin	git@gitlab.com:gibonet/decr.git (fetch)
# origin	git@gitlab.com:gibonet/decr.git (push)

# origin is gitlab, github is github

# Push to github and gitlab -----------
# git push -u github master
# git push -u origin master

