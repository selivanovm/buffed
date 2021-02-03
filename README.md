# Buffed

Finally I admit that it's abandoned! 
I lost interest and don't have time/motivation to keep it up to date.
I'm keeping it here for nostalgic reasons :)

vk(vk.com) audio content fetcher. Allows the following :

- keep track of new content on public walls on vk.com 
- download mp3 from public walls on vk.com

# Building & Running

## Prerequisites 

 - *stack* - The Haskell Tool Stack (tested with 1.9.3)
 - *clojure* - Clojure compiler (tested with 1.10.0)
 - *leiningen* - Cloujre build tool (tested with 2.8.3)
 - *happy* - Parser generator for Haskell (tested with 1.20.0)

 Build is tested on Arch Linux on 2020-02-03,
 however the app wasn't updated for quite some time,
 so parsing doesn't work.

### To build server part:

  $ stack build
  
### To build client part:

  $ cd ./ui 
  
  $ lein cljsbuild once
  
  $ cd ..
  
  $ ./cpui.sh

### To run:
  
  $ stack run
  
  open http://localhost:8000/app in the browser
  

