
#### GENERAL INFORMATION ABOUT REPOSITORY

- This repo contains PRESENT block cipher encription algorithm implemented in C language.
- For using over block size messages, CBC mode implemented.
- Run the implementation by using given run.sh bash script.
`./run.sh`
- Question 1 to 3 will be automaticaly run and will be print the result in terminal.
- 64MB of data initilized as 0.
- User may rewrite the main code to use vanilla.
- Created by Arda CEKIC

------------
#### PERFORMANCE COUNTER ON 64MB DATA ENCRIPTON
- On MacBook Pro (13-inch, 2022) Apple M2 @ 3.5 GHz (8 cores)
it takes nearly 5 minutes to do encryption of 64MB of data .
(4min43sec)
------------

#### One sample for question report:

````bash
ardacekic@Ardas-MacBook-Pro c_code % ./run.sh
The current start time is: Sun Jan 14 00:38:09 +03 2024
gcc -Wall main.c -o main

Calculating Question One 
------------------------
Given Key : 00 00 00 00 00 00 00 00 00 00 
Given Plain Text : 0
Resulting Cipher Text : 5579c1387b228445
------------------------

Calculating Question Two 
------------------------
Given HEX string with Propper Pading 10*
array[0] = 0x417264612043656b
array[1] = 0x6963800000000000
CBC Resulting Ciphertext
ciphertext 0 : 140ba5595b61e12e 
ciphertext 1 : f78427afcb351681 
------------------------

Calculating Question Three 
------------------------
CBC 64MB Encripton is started Padding is not issued...
CBC 64MB is done...
------------------------
The current finish time is: Sun Jan 14 00:42:52 +03 2024
````