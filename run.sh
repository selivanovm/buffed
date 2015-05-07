#!/bin/sh

rm buffed.log

# ./dist/build/buffed/buffed +RTS -K32m -M128m -xc -RTS "http://vk.com/baudiozapis" true
# ./dist/build/buffed/buffed +RTS -K32m -M256m -xc -RTS "http://vk.com/baudiozapis" true
# ./dist/build/buffed/buffed +RTS -K16m -M256m -p -RTS "http://vk.com/baudiozapis" true

#./dist/build/buffed/buffed +RTS -K16m -M512m -RTS "http://vk.com/baudiozapis" true
#./dist/build/buffed/buffed +RTS -K16m -M512m -RTS "http://vk.com/baudiozapis" fromFile

#./dist/build/buffed/buffed +RTS -K16m -M512m -RTS "http://vk.com/baudiozapis" fromFile
#./dist/build/buffed/buffed +RTS -K16m -M512m -RTS "http://vk.com/baudiozapis" toFile
#./dist/build/buffed/buffed +RTS -K16m -M512m -RTS "http://vk.com/baudiozapis" normal
./dist/build/buffed/buffed +RTS -K16m -M512m -RTS "http://vk.com/mymotherland" normal

# profile normal
#./dist/build/buffed/buffed +RTS -K16m -M512m -RTS "http://vk.com/baudiozapis" normal
