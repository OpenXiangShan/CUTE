package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
// import boom.acc._

trait YGJKParameters{
    val accNum = 1

    val JKDataNum = 16 //数据接口bufferLine个数
    val dataNum = 16
    val dataWidth = 32
//    val sourceNum = 16
    val sourceNum = 32
    val addrWidth = 64
    val ygjk_memWidth = 64   //单位byte,一个访存请求的数据量
    val regWidth = 64
}

class AccReq  extends Bundle with YGJKParameters{
    val addr = UInt(addrWidth.W)
    val cmd = UInt(1.W) //0-R 1-W
    val data = Vec(JKDataNum,Bits(dataWidth.W))   // 写数据, 也是16*32
}

class YGJKCommand extends Bundle{
  val acc_req_a = Flipped(Decoupled(new AccReq))
  val acc_req_b = Flipped(Decoupled(new AccReq))
  val req_id = Output(UInt((5+1).W))
}

class YGJKBuffer extends Bundle with YGJKParameters{
  val data = Vec(JKDataNum,UInt(dataWidth.W))   // 读取的信息 16*32 = 512bit = 64byte
  val id = UInt(6.W)
}

class YGJKControl extends Bundle{
  val reset = Output(Bool())
  val acc_running = Input(Bool())
  val InstFIFO_Info = Input((UInt(5.W)))
  val InstFIFO_Full = Input((Bool()))
  val InstFIFO_Finish = Input((UInt(5.W)))
  val cute_return_val = (Input(UInt(32.W)))
  val config  = Valid(new Bundle{
    val cfgData1 = UInt(64.W)
    val cfgData2 = UInt(64.W)
    val func = UInt(7.W)
  })
}

class YGJKIO extends Bundle {
  val cmd     = new YGJKCommand   // 访存请求
  val buffer0  = Valid(new YGJKBuffer)    // 数据返回通道
  val buffer1 = Valid(new YGJKBuffer)
  val ctl     = new YGJKControl   // 控制命令通道
}

case object BuildYGAC extends Field[Parameters => MyACCModule]
abstract class MyACCModule extends Module with YGJKParameters{
   val io = IO(Flipped(new YGJKIO))  
}
