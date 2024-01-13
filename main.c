#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define ROUNDS 31      
#define BLOCK_SIZE 64
#define KEY_SIZE 80
#define ARRAY_SIZE 10
#define TOTAL_BITS (ARRAY_SIZE * 8)


void printArray(uint8_t* array, size_t size) {
    for (size_t i = 0; i < size; ++i) {
        printf("%02X ", array[i]);
    }
    printf("\n");
}

const uint8_t SBOX[16] = {
    0x0C, 0x05, 0x06, 0x0B, 0x09, 0x00, 0x0A, 0x0D, 0x03, 0x0E, 0x0F, 0x08, 0x04, 0x07, 0x01, 0x02
};

uint8_t permutation[64] = {
    0, 16, 32, 48, 1, 17, 33, 49, 2, 18, 34, 50, 3, 19, 35, 51,
    4, 20, 36, 52, 5, 21, 37, 53, 6, 22, 38, 54, 7, 23, 39, 55,
    8, 24, 40, 56, 9, 25, 41, 57, 10, 26, 42, 58, 11, 27, 43, 59,
    12, 28, 44, 60, 13, 29, 45, 61, 14, 30, 46, 62, 15, 31, 47, 63
};

void permute_bits(uint64_t input, uint64_t *output) {
    uint64_t inter = 0;
    for (int i = 0; i < 64; ++i) {
        if (input & (1ULL << i)) { // Check if the i-th bit of input is set
            inter |= (1ULL << permutation[i]); // Set the P(i)-th bit of output
        }
    }
    *output = inter;
}

void sBoxLayerKeyGeneration(uint8_t* key){
    uint8_t upper_nibble = (key[0] >> 4) & 0x0F;
    uint8_t sbox_val =  SBOX[upper_nibble];
    uint8_t cleared_value = key[0] & 0x0F;
    uint8_t shifted_input = sbox_val << 4;
    key[0] = shifted_input | cleared_value;
}

void uint64_to_uint8_array(uint64_t value, uint8_t *array) {
    for (int i = 0; i < 8; i++) {
        array[i] = (value >> (8 * (7 - i))) & 0xFF;
    }
}

void uint8_array_to_uint64(uint8_t *array, uint64_t *value) {
    *value = 0; // Reset the value
    for (int i = 0; i < 8; i++) {
        *value |= ((uint64_t)array[i]) << (8 * (7 - i));   
    }
}

void sBoxLayer(uint64_t *block){
    uint8_t bytearray[8];
    uint64_t result =0;
    uint64_to_uint8_array(*block,bytearray);

    for(int position = 0; position < 8; position++){
        printf(" array %llx\n", bytearray[position]);
        uint8_t upper_nibble = (bytearray[position] >> 4) & 0x0F;
        uint8_t lower_nibble = (bytearray[position]) & 0x0F;

        uint8_t sbox_val_upper =  SBOX[upper_nibble];
        uint8_t sbox_val_lower =  SBOX[lower_nibble];

        uint8_t cleared_value_upper = bytearray[position] & 0x00;
        uint8_t shifted_input_upper = sbox_val_upper << 4;

        uint8_t cleared_value_lower = bytearray[position] & 0x00;
        uint8_t shifted_input_lower = sbox_val_lower;


        bytearray[position] = shifted_input_upper | cleared_value_upper | cleared_value_lower | shifted_input_lower;
    }

    uint8_array_to_uint64(bytearray,&result);

    *block = result;
    printf(" sbox %llx\n", *block);
}

void rotateLeft(uint8_t* array, size_t size, unsigned int rotationCount) {
    for (unsigned int i = 0; i < rotationCount; ++i) {
        uint8_t firstBit = (array[0] & 0x80) != 0; // Save the leftmost bit of the first byte
        for (size_t j = 0; j < size - 1; ++j) {
            array[j] = (array[j] << 1) | (array[j + 1] >> 7);
        }
        array[size - 1] = (array[size - 1] << 1) | firstBit;
    }
}

void xorBits(uint8_t* key, uint8_t round){ ///          0     1      2    3     4     5     6     7     8     9
    // Extracting the bits k19, k18, k17, k16, k15   {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
    uint8_t k19 = (key[7] >> 3) & 0x01; // 3rd bit of key
    uint8_t k18 = (key[7] >> 2) & 0x01; // 2nd bit of key
    uint8_t k17 = (key[7] >> 1) & 0x01; // 1st bit of key
    uint8_t k16 = key[7] & 0x01;        // 0th bit of key
    uint8_t k15 = (key[8] >> 7) & 0x01; // 7th bit of key
    uint8_t extractedBits = (k15) | (k16 << 1) | (k17 << 2) | (k18 << 3) | (k19 << 4);
    uint8_t result = extractedBits ^ round;
    //printf("extrated bits %x \n", extractedBits);
    key[7] = (key[7] & 0xF0) | ((result >> 1) & 0x0F); 
    key[8] = (key[8] & 0x7F) | ((result & 0x01) << 7); 
}

void generateRoundKeys(uint8_t *key, uint8_t *round_keys[]){
    uint8_t resedual_key[10];
    for (int i = 0; i < 10; i++) {
        round_keys[0][i] = key[i];
        resedual_key[i] = key[i];
    }

    for(int i = 1; i<=ROUNDS; i++){
        rotateLeft(resedual_key, ARRAY_SIZE , 61);
        sBoxLayerKeyGeneration(resedual_key);
        xorBits(resedual_key,i);
        for (int j = 0; j < 10; j++) {
            round_keys[i][j] = resedual_key[j];
        }
    }
};

void addRoundKey(uint8_t *roundKey, uint64_t *state){
    uint64_t result = 0;
    uint8_array_to_uint64(roundKey,&result);
    printf("state %llx\n", *state);
    *state = result ^ *state;
    printf("%llx\n", *state);
};

void permutationLayer(uint64_t *state){
    uint64_t input = *state;
    permute_bits(input,state);
    printf("%llx\n",*state);
};

void presentENC(uint8_t *key, uint8_t *round_keys[],uint64_t *state){
    generateRoundKeys(key,round_keys);
    for(int i = 0; i < ROUNDS; i++){
        addRoundKey(round_keys[i],&state);
        sBoxLayer(&state);
        permutationLayer(&state);
    }
    addRoundKey(round_keys[ROUNDS],&state);
}

int main() {

    uint64_t plaintext = 0;
    uint64_t ciphertext = 0;
    ciphertext = plaintext;
    uint8_t key[ARRAY_SIZE] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
    uint8_t *round_keys[ROUNDS + 1];

    for(int i = 0; i < ROUNDS + 1; i++) {
        round_keys[i] = (uint8_t *)malloc(ARRAY_SIZE * sizeof(uint8_t));
    }

    //presentENC(key,round_keys,ciphertext);

    generateRoundKeys(key,round_keys);
    for(int i = 0; i < 2; i++){
        printf("round key :\n");
        printArray(round_keys[i],10);
        addRoundKey(round_keys[i],&ciphertext);
        sBoxLayer(&ciphertext);
        permutationLayer(&ciphertext);
    }
    //addRoundKey(round_keys[ROUNDS],&ciphertext);

    for(int i = 0; i < ROUNDS + 1; i++) {
        free(round_keys[i]);
    }

    return 0;
}
