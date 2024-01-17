// See LICENSE.txt for license details.
package present

import chisel3._
import chisel3.util._
import chisel3.util._
import chisel3.iotesters.{PeekPokeTester, Driver}
import chisel3.stage.ChiselStage
import chisel3.experimental._
import chisel3.testers._

object PresentConstants {
    val keysize: Int = 80
    val round: Int = 32
    val blocksize: Int = 64
}
  
class PresentRoundKeyGenerator extends Module {
  val io = IO(new Bundle {
    val key = Flipped(Decoupled(UInt(PresentConstants.keysize.W)))
    val out = Decoupled(Vec(PresentConstants.round,UInt((PresentConstants.keysize).W)))
  })

  val sIdle :: sRoundKeyRor :: sRoundKeySbox :: sRoundKeyXor :: sUpdate :: sSend :: Nil = Enum(6)
  val state = RegInit(sIdle)
  
  val roundkeys    = RegInit(VecInit(Seq.fill(PresentConstants.round + 1)(0.U((PresentConstants.keysize).W))))
  val roundCounter = RegInit(1.U(6.W))
  val select_Wire  = WireInit((0.U((4.W))))
  val modifiedValue = WireInit((0.U((5.W))))
  val lookupTable = List(
    0.U -> 0xC.U,
    1.U -> 0x5.U,
    2.U -> 6.U,
    3.U -> 0xB.U,
    4.U -> 9.U,
    5.U -> 0.U,
    6.U -> 0xA.U,
    7.U -> 0xD.U,
    8.U -> 3.U,
    9.U -> 0xE.U,
    10.U -> 0xF.U,
    11.U -> 8.U,
    12.U -> 4.U,
    13.U -> 7.U,
    14.U -> 1.U,
    15.U -> 2.U
  )
  val Sbox   = MuxLookup(select_Wire, 0.U,lookupTable)

  // 1 cycle for 19 right rotate cycle
  // 1 cycle for Sbox operation
  // 1 cycle xor operation
  // 1 cycle update next key
  roundCounter    := Mux(state === sUpdate, roundCounter + 1.U, roundCounter)

  io.key.ready := false.B
  io.out.valid := false.B
  for ( i <- 0 until 32){
    io.out.bits(i) := 0.U
    roundkeys(i) := roundkeys(i)
  }

  when(state === sIdle){
    io.key.ready := true.B
  }

  when(state === sIdle && io.key.fire()){
    state := sRoundKeyRor
    roundkeys(0.U) := io.key.bits
  }

  when(state === sRoundKeyRor){
    state := sRoundKeySbox
    val rotatedValue = (roundkeys(roundCounter) >> 19.U) | (roundkeys(roundCounter) << (80.U - 19.U))
    roundkeys(roundCounter) := rotatedValue
  }

  when(state === sRoundKeySbox){
    state := sRoundKeyXor
    select_Wire := MuxLookup(roundkeys(roundCounter)(79,76), 0.U,lookupTable) 
    roundkeys(roundCounter) := Cat(select_Wire,roundkeys(roundCounter)(75,0))
  }

  when(state === sRoundKeyXor){
    state := sUpdate
    modifiedValue := roundkeys(roundCounter)(19,15) ^ roundCounter
    val result = Cat(roundkeys(roundCounter)(79,20),modifiedValue,roundkeys(roundCounter)(14,0))
    roundkeys(roundCounter) := result
  }

  when(state === sUpdate){
    state := Mux(roundCounter === 31.U, sSend, sRoundKeyRor)
    roundkeys(roundCounter + 1.U) := roundkeys(roundCounter)
  }

  when(state === sSend){
    state := sIdle
    io.out.valid := true.B
    for ( i <- 0 until 32){
        io.out.bits(i) := roundkeys(i)
    }
  }

}

class PresentCipher extends Module {
  val io = IO(new Bundle {
    val key       = Flipped(Decoupled(UInt(PresentConstants.keysize.W)))
    val plaintext = Flipped(Decoupled(UInt(PresentConstants.blocksize.W)))
    val ciphertext = Decoupled(UInt(PresentConstants.blocksize.W))
  })

  val sIdle :: sRoundKeyGeneration :: sAddRoundKey :: sSubSbox :: sPermutate :: sUpdate ::sDone :: sSend :: Nil = Enum(8)
  val state = RegInit(sIdle)
  val key = RegInit(0.U(80.W))
  val keyRegistered = RegInit(false.B)
  val plaintextRegistered = RegInit(false.B)
  val roundcounter = RegInit(0.U(5.W))
  val ciphertext = RegInit(0.U(PresentConstants.blocksize.W))
  val plaintext = RegInit(0.U(PresentConstants.blocksize.W))
  val roundKeyGenerator = Module(new PresentRoundKeyGenerator)
  val permute_result    = RegInit(0.U(64.W))
  val roundkeys    = RegInit(VecInit(Seq.fill(PresentConstants.round )(0.U((PresentConstants.blocksize).W))))

  io.ciphertext.bits := ciphertext
  
  val lookupTable = List(
      0.U -> 0xC.U,
      1.U -> 0x5.U,
      2.U -> 6.U,
      3.U -> 0xB.U,
      4.U -> 9.U,
      5.U -> 0.U,
      6.U -> 0xA.U,
      7.U -> 0xD.U,
      8.U -> 3.U,
      9.U -> 0xE.U,
      10.U -> 0xF.U,
      11.U -> 8.U,
      12.U -> 4.U,
      13.U -> 7.U,
      14.U -> 1.U,
      15.U -> 2.U
  )

  io.key.ready := true.B
  io.plaintext.ready := true.B
  roundKeyGenerator.io.key.valid := io.key.fire()
  roundKeyGenerator.io.key.bits  := io.key.bits
  roundKeyGenerator.io.out.ready := true.B
  io.ciphertext.valid := false.B
  keyRegistered := io.key.fire()
  plaintextRegistered := Mux(io.plaintext.fire(),true.B,plaintextRegistered)
  plaintext := Mux(io.plaintext.fire(),io.plaintext.bits,plaintext)
  ciphertext := ciphertext
  
  state := Mux(state === sIdle && io.key.fire(),sRoundKeyGeneration,sIdle)
  state := Mux(state === sRoundKeyGeneration && roundKeyGenerator.io.out.fire() && plaintextRegistered, sAddRoundKey, sRoundKeyGeneration)
  
  for(i <- 0 until 32){
    roundkeys(i) := Mux(roundKeyGenerator.io.out.fire(),roundKeyGenerator.io.out.bits(i)(79,16),roundkeys(i))
  }
  
  when((state === sIdle || state === sRoundKeyGeneration) && plaintextRegistered){
    ciphertext := plaintext 
  }

  when(state === sAddRoundKey){
    state := sSubSbox
    ciphertext := ciphertext ^ roundkeys(roundcounter)
  }

  when(state === sSubSbox){
    state := sPermutate
    val Sbox_results  = WireInit(VecInit(Seq.fill(16)(0.U((4).W))))
    val cat_result    = WireInit(0.U(64.W))
    for(i <- 0 until 16){
        Sbox_results(i) := MuxLookup(ciphertext(3 + 4*i,0 + 4*i), 0.U,lookupTable)
    }
    cat_result := Cat(Sbox_results.reverse)
    ciphertext := cat_result
  }

  when(state === sPermutate){
    state := sUpdate

  permute_result := Cat(
    // 63                   62                   61              60
    ciphertext(63), ciphertext(59), ciphertext(55), ciphertext(51),
    ciphertext(47), ciphertext(43), ciphertext(39), ciphertext(35),
    ciphertext(31), ciphertext(27), ciphertext(23), ciphertext(19),
    ciphertext(15), ciphertext(11), ciphertext(7),  ciphertext(3),
    ciphertext(62), ciphertext(58), ciphertext(54), ciphertext(50),
    ciphertext(46), ciphertext(42), ciphertext(38), ciphertext(34),
    ciphertext(30), ciphertext(26), ciphertext(22), ciphertext(18),
    ciphertext(14), ciphertext(10), ciphertext(6),  ciphertext(2),
    ciphertext(61), ciphertext(57), ciphertext(53), ciphertext(49),
    ciphertext(45), ciphertext(41), ciphertext(37), ciphertext(33),
    ciphertext(29), ciphertext(25), ciphertext(21), ciphertext(17),
    ciphertext(13), ciphertext(9),  ciphertext(5),  ciphertext(1),
    ciphertext(60), ciphertext(56), ciphertext(52), ciphertext(48),
    ciphertext(44), ciphertext(40), ciphertext(36), ciphertext(32),
    ciphertext(28), ciphertext(24), ciphertext(20), ciphertext(16),
    ciphertext(12), ciphertext(8),  ciphertext(4),  ciphertext(0)
  )
    
  }

  when(state === sUpdate){
    state := Mux(roundcounter === 30.U, sDone, sAddRoundKey)
    roundcounter := roundcounter + 1.U
    ciphertext := permute_result
  }

  when(state === sDone){
    state := sSend
    ciphertext := ciphertext ^ roundkeys(roundcounter)
  }

  when(state === sSend){
    state := sIdle
    plaintextRegistered := false.B
    io.ciphertext.valid := true.B
    io.ciphertext.bits  := ciphertext
  }

}

object PresentCipher extends App {
  val myverilog = (new ChiselStage).emitVerilog(
    new PresentCipher,Array("--target-dir", "output/Present")
  )
}

