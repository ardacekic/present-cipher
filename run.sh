#IN C code you can set CRYPTODEBUG to 1 to see ciphertext for each corresponding block
current_time=$(date)
echo "The current start time is: $current_time"
make
./main
current_time=$(date)
echo "The current finish time is: $current_time"