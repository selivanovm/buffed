# Buffed

vk(vk.com) audio content fetcher. Allows the following :

- keep track of new content on public walls on vk.com 
- download mp3 from public walls on vk.com

work in progress ...

- Dependencies

Windows

ICU libs, you can download it here: http://icu-project.org/download/4.0.html
http://site.icu-project.org/download/55#TOC-ICU4C-Download

stack solver --modify-stack-yaml

pcre libs

download pcre-8.33, copy pcre3.dll to pcre.dll & pcre3.lib to pcre.lib

Installation 

cabal install --only-dependencies --extra-lib-dirs=/c/Users/ubear/Documents/coding/libs/icu/lib64 --extra-include-dirs=/c/Users/ubear/Documents/coding/libs/icu/include
