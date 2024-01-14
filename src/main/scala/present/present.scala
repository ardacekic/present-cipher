// See LICENSE.txt for license details.
package present

import chisel3._
import chisel3.util._
import chisel3.util._
import chisel3.iotesters.{PeekPokeTester, Driver}
import chisel3.stage.ChiselStage
import chisel3.experimental._
import chisel3.testers._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

object PresentConstants {
    val keysize: Int = 80
    val round: Int = 32
    val blocksize: Int = 64
}
  
class PresentRoundKeyGenerator extends Module {
  val io = IO(new Bundle {
    val key = Flipped(Decoupled(UInt(PresentConstants.keysize.W)))
    val out = Decoupled(Vec(PresentConstants.round,UInt((PresentConstants.blocksize).W)))
  })

  val sIdle :: sRoundKeyRor :: sRoundKeySbox :: sRoundKeyXor :: sUpdate :: sSend :: Nil = Enum(6)
  val state = RegInit(sIdle)
  
  val roundkeys    = RegInit(VecInit(Seq.fill(PresentConstants.round)(0.U((PresentConstants.keysize).W))))
  val roundCounter = RegInit(0.U(5.W))
  val select_Wire  = WireInit((0.U((4.W))))

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
  roundCounter    := Mux(state === sRoundKeyXor, roundCounter + 1.U, roundCounter)

  io.key.ready := false.B
  io.out.valid := false.B
  for ( i <- 0 until 32){
    io.out.bits(i) := 0.U
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
    select_Wire := roundkeys(roundCounter)(79,76)
    roundkeys(roundCounter) := Cat(select_Wire,roundkeys(roundCounter)(75,0))
  }

  when(state === sRoundKeyXor){
    state := sUpdate
    val xorVal = WireInit(0.U(5.W))
    val positionsToChange = Seq(19, 18, 17, 16, 15)
    val clearMask = ~(positionsToChange.map(i => 1.U << i.U).reduce(_ | _))
    val newValues = Cat(0.U(75.W), "b11111".U)
    val modifiedValue = (roundkeys(roundCounter) & clearMask) | (roundkeys(roundCounter)(19,15) ^ roundCounter)
    roundkeys(roundCounter) := modifiedValue
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

  val sIdle :: sRoundKeyGeneration :: sAddRoundKey :: sSubSbox :: sPermutate :: sDone :: Nil = Enum(6)
  val state = RegInit(sIdle)
  val key = RegInit(0.U(80.W))
  val keyRegistered = RegInit(false.B)
  val plaintextRegistered = RegInit(false.B)
  val roundcounter = RegInit(0.U(5.W))
  val ciphertext = RegInit(0.U(PresentConstants.blocksize.W))
  val plaintext = RegInit(0.U(PresentConstants.blocksize.W))
  val roundKeyGenerator = Module(new PresentRoundKeyGenerator)
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
  plaintextRegistered := io.plaintext.fire()
  plaintext := Mux(io.plaintext.fire(),io.plaintext.bits,plaintext)
  ciphertext := plaintext
  
  state := Mux(state === sIdle && io.key.fire(),sRoundKeyGeneration,sIdle)
  state := Mux(state === sRoundKeyGeneration && roundKeyGenerator.io.out.fire() && plaintextRegistered, sAddRoundKey,sRoundKeyGeneration)
  
  when(state === sAddRoundKey){
    state := sSubSbox
    ciphertext := ciphertext ^ roundKeyGenerator.io.out.bits(roundcounter)
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
    state := Mux(roundcounter === 32.U, sDone, sAddRoundKey)
    roundcounter := roundcounter + 1.U
    val permute_result    = WireInit(0.U(64.W))
    permute_result(0,0)   := ciphertext(0,0)
    permute_result(16,16) := ciphertext(1,1)
    permute_result(32,32) := ciphertext(2,2)
    permute_result(48,48) := ciphertext(3,3)
    permute_result(1,1)   := ciphertext(4,4)
    permute_result(17,17) := ciphertext(5,5)
    permute_result(33,33) := ciphertext(6,6)
    permute_result(49,49) := ciphertext(7,7)
    permute_result(2,2)   := ciphertext(8,8)
    permute_result(18,18) := ciphertext(9,9)
    permute_result(34,34) := ciphertext(10,10)
    permute_result(50,50) := ciphertext(11,11)
    permute_result(3,3)   := ciphertext(12,12)
    permute_result(19,19) := ciphertext(13,13)
    permute_result(35,35) := ciphertext(14,14)
    permute_result(51,51) := ciphertext(15,15)
    permute_result(4,4)   := ciphertext(16,16)
    permute_result(20,20) := ciphertext(17,17)
    permute_result(36,36) := ciphertext(18,18)
    permute_result(52,52) := ciphertext(19,19)
    permute_result(5,5)   := ciphertext(20,20)
    permute_result(21,21) := ciphertext(21,21)
    permute_result(37,37) := ciphertext(22,22)
    permute_result(53,53) := ciphertext(23,23)
    permute_result(6,6)   := ciphertext(24,24)
    permute_result(22,22) := ciphertext(25,25)
    permute_result(38,38) := ciphertext(26,26)
    permute_result(54,54) := ciphertext(27,27)
    permute_result(7,7)   := ciphertext(28,28)
    permute_result(23,23) := ciphertext(29,29)
    permute_result(39,39) := ciphertext(30,30)
    permute_result(55,55) := ciphertext(31,31)
    permute_result(8,8)   := ciphertext(32,32)
    permute_result(24,24) := ciphertext(33,33)
    permute_result(40,40) := ciphertext(34,34)
    permute_result(56,56) := ciphertext(35,35)
    permute_result(9,9)   := ciphertext(36,36)
    permute_result(25,25) := ciphertext(37,37)
    permute_result(41,41) := ciphertext(38,38)
    permute_result(57,57) := ciphertext(39,39)
    permute_result(10,10) := ciphertext(40,40)
    permute_result(26,26) := ciphertext(41,41)
    permute_result(42,42) := ciphertext(42,42)
    permute_result(58,58) := ciphertext(43,43)
    permute_result(11,11) := ciphertext(44,44)
    permute_result(27,27) := ciphertext(45,45)
    permute_result(43,43) := ciphertext(46,46)
    permute_result(59,59) := ciphertext(47,47)
    permute_result(12,12) := ciphertext(48,48)
    permute_result(28,28) := ciphertext(49,49)
    permute_result(44,44) := ciphertext(50,50)
    permute_result(60,60) := ciphertext(51,51)
    permute_result(13,13) := ciphertext(52,52)
    permute_result(29,29) := ciphertext(53,53)
    permute_result(45,45) := ciphertext(54,54)
    permute_result(61,61) := ciphertext(55,55)
    permute_result(14,14) := ciphertext(56,56)
    permute_result(30,30) := ciphertext(57,57)
    permute_result(46,46) := ciphertext(58,58)
    permute_result(62,62) := ciphertext(59,59)
    permute_result(15,15) := ciphertext(60,60)
    permute_result(31,31) := ciphertext(61,61)
    permute_result(47,47) := ciphertext(62,62)
    permute_result(63,63) := ciphertext(63,63)
    ciphertext := permute_result
  }

  when(state === sDone){
    state := sIdle
    io.ciphertext.valid := true.B
    io.ciphertext.bits  := ciphertext
  }

}


object PresentCipher extends App {
  val myverilog = (new ChiselStage).emitVerilog(
    new PresentRoundKeyGenerator,Array("--target-dir", "output/")
  )
}

