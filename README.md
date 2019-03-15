parking_system
=====

An OTP application 

Version
-----
Erlang/OTP 21

Build
-----

 Compiles the Source files

    $ rebar3 compile

Directs to Erlang shell

    $ rebar3 shell

```
(parking_sytem@127.0.0.1)1> parking_system:entry(front, four_wheeler).
"successfully vehicle parked"
(parking_sytem@127.0.0.1)2> parking_system:entry(rear, three_wheeler).
"successfully vehicle parked"
(parking_sytem@127.0.0.1)3> parking_system:entry(wrong_gate, three_wheeler).
"Sorry, You look different!. I can allow only front/ rear-gates"
(parking_sytem@127.0.0.1)4> parking_system:entry(rear, six_wheeler).
"Sorry, You look different!. I can accommodate parking space for only 4/ 3/ 2-wheelers."
(parking_sytem@127.0.0.1)5> parking_system:exit(1, four_wheeler).
"1 is now available to park"
(parking_sytem@127.0.0.1)5> parking_system:exit(-111, four_wheeler).
"Sorry, please enter proper slot number"
```
Runs the test cases

    $ rebar3 ct
