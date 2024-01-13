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

void sBoxLayerKeyGeneration(uint8_t* key){
    uint8_t upper_nibble = (key[0] >> 4) & 0x0F;
    uint8_t sbox_val =  SBOX[upper_nibble];
    uint8_t cleared_value = key[0] & 0x0F;
    uint8_t shifted_input = sbox_val << 4;
    key[0] = shifted_input | cleared_value;
}

void sBoxLayer(uint64_t *block){
    uint8_t array[8];
    uint64_t result;
    for (int i = 0; i < 8; i++) {
        array[i] = (*block >> (8 * i)) & 0xFF;
    }
    
    for(int position = 0; position < 8; position++){

        uint8_t upper_nibble = (array[position] >> 4) & 0x0F;
        uint8_t lower_nibble = (array[position]) & 0x0F;

        uint8_t sbox_val_upper =  SBOX[upper_nibble];
        uint8_t sbox_val_lower =  SBOX[lower_nibble];

        uint8_t cleared_value_upper = array[position] & 0x0F;
        uint8_t shifted_input_upper = cleared_value_upper << 4;

        uint8_t cleared_value_lower = array[position] & 0xF0;
        uint8_t shifted_input_lower = cleared_value_lower >> 4;

        array[position] = shifted_input_upper | cleared_value_upper | cleared_value_lower  | shifted_input_lower;
    }

    for (int i = 0; i < 8; i++) {
        result |= ((uint64_t)array[i]) << (8 * (8 - 1 - i));
    }

    block = result;
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
    uint64_t roundKey64 = 0;
    for (int i = 0; i < 8; i++) {
        roundKey64 |= ((uint64_t)roundKey[i]) << (8 * (8 - 1 - i));
    }
    *state = roundKey64 ^ *state;
};

void permutationLayer(uint64_t *state){

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

    presentENC(key,round_keys,ciphertext);

    for(int i = 0; i < ROUNDS + 1; i++) {
        free(round_keys[i]);
    }

    return 0;
}
