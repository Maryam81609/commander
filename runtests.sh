#!/bin/bash

./commander.sh delay 0 ad_counter
./commander.sh delay 1 ad_counter
./commander.sh delay 2 ad_counter

./commander.sh delay 0 wallet
./commander.sh delay 1 wallet
./commander.sh delay 2 wallet

./commander.sh delay 0 b2b_orders
./commander.sh delay 1 b2b_orders
./commander.sh delay 2 b2b_orders

./commander.sh delay 0 fmk
./commander.sh delay 1 fmk
./commander.sh delay 2 fmk

# Run test by random scheduler
./commander.sh random "{110, 220, 280}" ad_counter
./commander.sh random "{110, 220, 340}" ad_counter
./commander.sh random "{210, 235,280}" ad_counter
./commander.sh random "{11, 25, 38}" ad_counter
./commander.sh random "{120, 460, 680}" ad_counter
./commander.sh random "{330, 440, 550}" ad_counter
./commander.sh random "{320, 420, 520}" ad_counter
./commander.sh random "{1, 101, 301}" ad_counter
./commander.sh random "{400, 450, 500}" ad_counter
./commander.sh random "{260, 580, 850}" ad_counter
./commander.sh random "{630, 520, 410}" ad_counter
./commander.sh random "{999, 750, 300}" ad_counter
./commander.sh random "{15, 27, 38}" ad_counter
./commander.sh random "{1, 2, 3}" ad_counter
./commander.sh random "{32, 42, 52}" ad_counter

./commander.sh random "{110, 220, 280}" wallet
./commander.sh random "{110, 220, 340}" wallet
./commander.sh random "{210, 235,280}" wallet
./commander.sh random "{11, 25, 38}" wallet
./commander.sh random "{120, 460, 680}" wallet
./commander.sh random "{330, 440, 550}" wallet
./commander.sh random "{320, 420, 520}" wallet
./commander.sh random "{1, 101, 301}" wallet
./commander.sh random "{400, 450, 500}" wallet
./commander.sh random "{260, 580, 850}" wallet
./commander.sh random "{630, 520, 410}" wallet
./commander.sh random "{999, 750, 300}" wallet
./commander.sh random "{15, 27, 38}" wallet
./commander.sh random "{1, 2, 3}" wallet
./commander.sh random "{32, 42, 52}" wallet

./commander.sh random "{110, 220, 280}" b2b_orders
./commander.sh random "{110, 220, 340}" b2b_orders
./commander.sh random "{210, 235,280}" b2b_orders
./commander.sh random "{11, 25, 38}" b2b_orders
./commander.sh random "{120, 460, 680}" b2b_orders
./commander.sh random "{330, 440, 550}" b2b_orders
./commander.sh random "{320, 420, 520}" b2b_orders
./commander.sh random "{1, 101, 301}" b2b_orders
./commander.sh random "{400, 450, 500}" b2b_orders
./commander.sh random "{260, 580, 850}" b2b_orders
./commander.sh random "{630, 520, 410}" b2b_orders
./commander.sh random "{999, 750, 300}" b2b_orders
./commander.sh random "{15, 27, 38}" b2b_orders
./commander.sh random "{1, 2, 3}" b2b_orders
./commander.sh random "{32, 42, 52}" b2b_orders

./commander.sh random "{110, 220, 280}" fmk
./commander.sh random "{110, 220, 340}" fmk
./commander.sh random "{210, 235,280}" fmk
./commander.sh random "{11, 25, 38}" fmk
./commander.sh random "{120, 460, 680}" fmk
./commander.sh random "{330, 440, 550}" fmk
./commander.sh random "{320, 420, 520}" fmk
./commander.sh random "{1, 101, 301}" fmk
./commander.sh random "{400, 450, 500}" fmk
./commander.sh random "{260, 580, 850}" fmk
./commander.sh random "{630, 520, 410}" fmk
./commander.sh random "{999, 750, 300}" fmk
./commander.sh random "{15, 27, 38}" fmk
./commander.sh random "{1, 2, 3}" fmk
./commander.sh random "{32, 42, 52}" fmk