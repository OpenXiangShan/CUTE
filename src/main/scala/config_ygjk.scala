package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import cute.Bundles._
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

class MreleaseIO extends Bundle{
    val tokenRd = Vec(32, Bool())
}

class YGJKControl(implicit p: Parameters) extends CuteBundle{
  val reset = Output(Bool())
  val amuCtrl = Decoupled(new AmuCtrlIO)
  val mrelease = Flipped(Valid(new MreleaseIO))
}

class YGJKIO(implicit p: Parameters) extends CuteBundle {
  val cmd     = new YGJKCommand   // 访存请求
  val buffer0  = Valid(new YGJKBuffer)    // 数据返回通道
  val buffer1 = Valid(new YGJKBuffer)
  val ctl     = new YGJKControl   // 控制命令通道
}

case object BuildYGAC extends Field[Parameters => MyACCModule]
abstract class MyACCModule(implicit p: Parameters) extends CuteModule with YGJKParameters{
   val io = IO(Flipped(new YGJKIO))  
}
