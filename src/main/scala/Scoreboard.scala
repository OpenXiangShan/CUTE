package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

// 寄存器状态枚举
object RegState {
    val Width = 2
    val Idle = 0.U(Width.W)      // 空闲，可以被分配
    val Writing = 1.U(Width.W)   // 正在被写入（Load或Compute写C）
    val Ready = 2.U(Width.W)     // 写入完成，可以被读取
    val Reading = 3.U(Width.W)   // 正在被读取（可以有多个读者）
}

// 单个寄存器的记分牌条目
class RegScoreboardEntry(implicit p: Parameters) extends CuteBundle {
    val state = UInt(RegState.Width.W)           // 寄存器当前状态
    val writer_valid = Bool()                     // 是否有写者
    val writer_fifo_idx = UInt(2.W)              // 写入者的FIFO索引
    val writer_type = UInt(2.W)                   // 写入者类型: 0=Load, 1=Compute, 2=Store
    val reader_count = UInt(3.W)                  // 当前读者数量（最多支持7个并发读者）
    val reader_mask = UInt(4.W)                   // 读者掩码（用于跟踪是哪些微指令在读）
}

// Load微指令查询请求
class LoadQueryReq(implicit p: Parameters) extends CuteBundle {
    val a_reg = UInt(2.W)
    val b_reg = UInt(2.W)
    val c_reg = UInt(2.W)
    val has_a = Bool()
    val has_b = Bool()
    val has_c = Bool()
}

// Compute微指令查询请求
class ComputeQueryReq(implicit p: Parameters) extends CuteBundle {
    val a_reg = UInt(2.W)
    val b_reg = UInt(2.W)
    val c_reg = UInt(2.W)
}

// Store微指令查询请求
class StoreQueryReq(implicit p: Parameters) extends CuteBundle {
    val c_reg = UInt(2.W)
}

// Scoreboard的查询接口：用于依赖检查（使用Valid-Ready握手）
// valid: TaskController有指令要发射
// ready: Scoreboard检查依赖通过，允许发射
class ScoreboardQueryIO(implicit p: Parameters) extends CuteBundle {
    // Load微指令依赖查询
    val load = Flipped(DecoupledIO(new LoadQueryReq))
    
    // Compute微指令依赖查询
    val compute = Flipped(DecoupledIO(new ComputeQueryReq))
    
    // Store微指令依赖查询
    val store = Flipped(DecoupledIO(new StoreQueryReq))
}

// Scoreboard的更新接口：用于更新寄存器状态
class ScoreboardUpdateIO(implicit p: Parameters) extends CuteBundle {
    // 分配寄存器给Load指令
    val load_allocate = Input(Bool())
    val load_alloc_a_reg = Input(UInt(2.W))
    val load_alloc_b_reg = Input(UInt(2.W))
    val load_alloc_c_reg = Input(UInt(2.W))
    val load_alloc_has_a = Input(Bool())
    val load_alloc_has_b = Input(Bool())
    val load_alloc_has_c = Input(Bool())
    val load_alloc_fifo_idx = Input(UInt(2.W))
    
    // Load完成，标记寄存器为Ready
    val load_finish_a = Input(Bool())
    val load_finish_a_reg = Input(UInt(2.W))
    val load_finish_b = Input(Bool())
    val load_finish_b_reg = Input(UInt(2.W))
    val load_finish_c = Input(Bool())
    val load_finish_c_reg = Input(UInt(2.W))
    
    // Compute发射，读取A/B/C，写入C
    val compute_issue = Input(Bool())
    val compute_issue_a_reg = Input(UInt(2.W))
    val compute_issue_b_reg = Input(UInt(2.W))
    val compute_issue_c_reg = Input(UInt(2.W))
    val compute_issue_fifo_idx = Input(UInt(2.W))
    
    // Compute完成读取A/B
    val compute_read_finish_a = Input(Bool())
    val compute_read_finish_a_reg = Input(UInt(2.W))
    val compute_read_finish_b = Input(Bool())
    val compute_read_finish_b_reg = Input(UInt(2.W))
    
    // Compute完成写入C
    val compute_write_finish_c = Input(Bool())
    val compute_write_finish_c_reg = Input(UInt(2.W))
    
    // Store发射，读取C
    val store_issue = Input(Bool())
    val store_issue_c_reg = Input(UInt(2.W))
    val store_issue_fifo_idx = Input(UInt(2.W))
    
    // Store完成，释放C
    val store_finish = Input(Bool())
    val store_finish_c_reg = Input(UInt(2.W))
}

// Scoreboard的调试接口
class ScoreboardDebugIO(implicit p: Parameters) extends CuteBundle {
    val ab_reg_states = Output(Vec(4, UInt(RegState.Width.W)))
    val c_reg_states = Output(Vec(2, UInt(RegState.Width.W)))
    val ab_reg_writers = Output(Vec(4, UInt(2.W)))
    val c_reg_writers = Output(Vec(2, UInt(2.W)))
    val ab_reg_reader_counts = Output(Vec(4, UInt(3.W)))
    val c_reg_reader_counts = Output(Vec(2, UInt(3.W)))
}

// 主Scoreboard模块
class Scoreboard(implicit p: Parameters) extends CuteModule {
    val io = IO(new Bundle {
        val query = new ScoreboardQueryIO
        val update = new ScoreboardUpdateIO
        val debug = new ScoreboardDebugIO
    })
    
    // ===========================================
    // 寄存器状态存储
    // ===========================================
    // AB寄存器：[0-1]对应A矩阵，[2-3]对应B矩阵
    val ab_scoreboard = RegInit(VecInit(Seq.fill(4)(0.U.asTypeOf(new RegScoreboardEntry))))
    // C寄存器：[0-1]对应C矩阵
    val c_scoreboard = RegInit(VecInit(Seq.fill(2)(0.U.asTypeOf(new RegScoreboardEntry))))
    
    // ===========================================
    // 依赖检查逻辑
    // ===========================================
    
    // Load指令依赖检查：需要的寄存器必须全部Idle
    def checkLoadDependency(a_reg: UInt, b_reg: UInt, c_reg: UInt, 
                           has_a: Bool, has_b: Bool, has_c: Bool): Bool = {
        val a_ok = !has_a || (ab_scoreboard(a_reg).state === RegState.Idle)
        val b_ok = !has_b || (ab_scoreboard(b_reg + 2.U).state === RegState.Idle)
        val c_ok = !has_c || (c_scoreboard(c_reg).state === RegState.Idle)
        a_ok && b_ok && c_ok
    }
    
    // Compute指令依赖检查：
    // - 读A/B/C：必须是Ready或Reading状态
    // - 写C：如果C正在被读，需要等待；如果C是Idle或Ready(没有其他写者)，可以写
    def checkComputeDependency(a_reg: UInt, b_reg: UInt, c_reg: UInt): Bool = {
        // A必须Ready（Load已完成）
        val a_ready = (ab_scoreboard(a_reg).state === RegState.Ready) || 
                     (ab_scoreboard(a_reg).state === RegState.Reading)
        
        // B必须Ready（Load已完成）
        val b_ready = (ab_scoreboard(b_reg + 2.U).state === RegState.Ready) || 
                     (ab_scoreboard(b_reg + 2.U).state === RegState.Reading)
        
        // C必须Ready（可以读）
        val c_read_ready = (c_scoreboard(c_reg).state === RegState.Ready) || 
                          (c_scoreboard(c_reg).state === RegState.Reading)
        
        // C必须没有其他写者（避免WAW冲突）
        val c_write_ok = !c_scoreboard(c_reg).writer_valid
        
        a_ready && b_ready && c_read_ready && c_write_ok
    }
    
    // Store指令依赖检查：C必须Ready（Compute已完成写入）
    def checkStoreDependency(c_reg: UInt): Bool = {
        (c_scoreboard(c_reg).state === RegState.Ready) || 
        (c_scoreboard(c_reg).state === RegState.Reading)
    }
    
    // ===========================================
    // 查询接口连接（Valid-Ready握手）
    // ===========================================
    
    // Load查询：ready信号表示依赖检查通过
    io.query.load.ready := checkLoadDependency(
        io.query.load.bits.a_reg, 
        io.query.load.bits.b_reg, 
        io.query.load.bits.c_reg,
        io.query.load.bits.has_a,
        io.query.load.bits.has_b,
        io.query.load.bits.has_c
    )
    
    // Compute查询：ready信号表示依赖检查通过
    io.query.compute.ready := checkComputeDependency(
        io.query.compute.bits.a_reg,
        io.query.compute.bits.b_reg,
        io.query.compute.bits.c_reg
    )
    
    // Store查询：ready信号表示依赖检查通过
    io.query.store.ready := checkStoreDependency(
        io.query.store.bits.c_reg
    )
    
    // ===========================================
    // 寄存器状态更新逻辑
    // ===========================================
    
    // Load分配：将寄存器标记为Writing
    when(io.update.load_allocate) {
        when(io.update.load_alloc_has_a) {
            val a_reg = io.update.load_alloc_a_reg
            ab_scoreboard(a_reg).state := RegState.Writing
            ab_scoreboard(a_reg).writer_valid := true.B
            ab_scoreboard(a_reg).writer_fifo_idx := io.update.load_alloc_fifo_idx
            ab_scoreboard(a_reg).writer_type := 0.U  // Load
            ab_scoreboard(a_reg).reader_count := 0.U
            ab_scoreboard(a_reg).reader_mask := 0.U
        }
        
        when(io.update.load_alloc_has_b) {
            val b_reg = io.update.load_alloc_b_reg + 2.U
            ab_scoreboard(b_reg).state := RegState.Writing
            ab_scoreboard(b_reg).writer_valid := true.B
            ab_scoreboard(b_reg).writer_fifo_idx := io.update.load_alloc_fifo_idx
            ab_scoreboard(b_reg).writer_type := 0.U  // Load
            ab_scoreboard(b_reg).reader_count := 0.U
            ab_scoreboard(b_reg).reader_mask := 0.U
        }
        
        when(io.update.load_alloc_has_c) {
            val c_reg = io.update.load_alloc_c_reg
            c_scoreboard(c_reg).state := RegState.Writing
            c_scoreboard(c_reg).writer_valid := true.B
            c_scoreboard(c_reg).writer_fifo_idx := io.update.load_alloc_fifo_idx
            c_scoreboard(c_reg).writer_type := 0.U  // Load
            c_scoreboard(c_reg).reader_count := 0.U
            c_scoreboard(c_reg).reader_mask := 0.U
        }
    }
    
    // Load完成A：标记为Ready
    when(io.update.load_finish_a) {
        val a_reg = io.update.load_finish_a_reg
        ab_scoreboard(a_reg).state := RegState.Ready
        ab_scoreboard(a_reg).writer_valid := false.B
    }
    
    // Load完成B：标记为Ready
    when(io.update.load_finish_b) {
        val b_reg = io.update.load_finish_b_reg + 2.U
        ab_scoreboard(b_reg).state := RegState.Ready
        ab_scoreboard(b_reg).writer_valid := false.B
    }
    
    // Load完成C：标记为Ready
    when(io.update.load_finish_c) {
        val c_reg = io.update.load_finish_c_reg
        c_scoreboard(c_reg).state := RegState.Ready
        c_scoreboard(c_reg).writer_valid := false.B
    }
    
    // Compute发射：增加A/B/C的读者计数，设置C的写者
    when(io.update.compute_issue) {
        val a_reg = io.update.compute_issue_a_reg
        val b_reg = io.update.compute_issue_b_reg + 2.U
        val c_reg = io.update.compute_issue_c_reg
        val fifo_idx = io.update.compute_issue_fifo_idx
        
        // A寄存器增加读者
        ab_scoreboard(a_reg).state := RegState.Reading
        ab_scoreboard(a_reg).reader_count := ab_scoreboard(a_reg).reader_count + 1.U
        ab_scoreboard(a_reg).reader_mask := ab_scoreboard(a_reg).reader_mask | (1.U << fifo_idx)
        
        // B寄存器增加读者
        ab_scoreboard(b_reg).state := RegState.Reading
        ab_scoreboard(b_reg).reader_count := ab_scoreboard(b_reg).reader_count + 1.U
        ab_scoreboard(b_reg).reader_mask := ab_scoreboard(b_reg).reader_mask | (1.U << fifo_idx)
        
        // C寄存器增加读者（读老值）
        c_scoreboard(c_reg).state := RegState.Reading
        c_scoreboard(c_reg).reader_count := c_scoreboard(c_reg).reader_count + 1.U
        c_scoreboard(c_reg).reader_mask := c_scoreboard(c_reg).reader_mask | (1.U << fifo_idx)
        
        // C寄存器设置写者（写新值）
        c_scoreboard(c_reg).writer_valid := true.B
        c_scoreboard(c_reg).writer_fifo_idx := fifo_idx
        c_scoreboard(c_reg).writer_type := 1.U  // Compute
    }
    
    // Compute完成读A：减少读者计数
    when(io.update.compute_read_finish_a) {
        val a_reg = io.update.compute_read_finish_a_reg
        ab_scoreboard(a_reg).reader_count := ab_scoreboard(a_reg).reader_count - 1.U
        // 如果没有读者了，恢复为Ready状态
        when(ab_scoreboard(a_reg).reader_count === 1.U) {
            ab_scoreboard(a_reg).state := RegState.Ready
            ab_scoreboard(a_reg).reader_mask := 0.U
        }
    }
    
    // Compute完成读B：减少读者计数
    when(io.update.compute_read_finish_b) {
        val b_reg = io.update.compute_read_finish_b_reg + 2.U
        ab_scoreboard(b_reg).reader_count := ab_scoreboard(b_reg).reader_count - 1.U
        // 如果没有读者了，恢复为Ready状态
        when(ab_scoreboard(b_reg).reader_count === 1.U) {
            ab_scoreboard(b_reg).state := RegState.Ready
            ab_scoreboard(b_reg).reader_mask := 0.U
        }
    }
    
    // Compute完成写C：标记为Ready，清除写者
    when(io.update.compute_write_finish_c) {
        val c_reg = io.update.compute_write_finish_c_reg
        c_scoreboard(c_reg).writer_valid := false.B
        // 如果还有读者，保持Reading状态；否则变为Ready
        when(c_scoreboard(c_reg).reader_count === 0.U) {
            c_scoreboard(c_reg).state := RegState.Ready
        }
    }
    
    // Store发射：增加C的读者计数
    when(io.update.store_issue) {
        val c_reg = io.update.store_issue_c_reg
        val fifo_idx = io.update.store_issue_fifo_idx
        
        c_scoreboard(c_reg).state := RegState.Reading
        c_scoreboard(c_reg).reader_count := c_scoreboard(c_reg).reader_count + 1.U
        c_scoreboard(c_reg).reader_mask := c_scoreboard(c_reg).reader_mask | (1.U << fifo_idx)
    }
    
    // Store完成：减少C的读者计数，如果是最后一个读者则释放
    when(io.update.store_finish) {
        val c_reg = io.update.store_finish_c_reg
        c_scoreboard(c_reg).reader_count := c_scoreboard(c_reg).reader_count - 1.U
        
        // 如果没有读者了，释放寄存器
        when(c_scoreboard(c_reg).reader_count === 1.U) {
            c_scoreboard(c_reg).state := RegState.Idle
            c_scoreboard(c_reg).reader_mask := 0.U
            c_scoreboard(c_reg).writer_valid := false.B
        }.otherwise {
            c_scoreboard(c_reg).state := RegState.Reading
        }
    }
    
    // ===========================================
    // 调试输出
    // ===========================================
    for (i <- 0 until 4) {
        io.debug.ab_reg_states(i) := ab_scoreboard(i).state
        io.debug.ab_reg_writers(i) := ab_scoreboard(i).writer_fifo_idx
        io.debug.ab_reg_reader_counts(i) := ab_scoreboard(i).reader_count
    }
    
    for (i <- 0 until 2) {
        io.debug.c_reg_states(i) := c_scoreboard(i).state
        io.debug.c_reg_writers(i) := c_scoreboard(i).writer_fifo_idx
        io.debug.c_reg_reader_counts(i) := c_scoreboard(i).reader_count
    }
    
    // ===========================================
    // 调试打印（可选）
    // ===========================================
    if (YJPDebugEnable) {
        printf("[Scoreboard] AB States: [%d,%d,%d,%d], C States: [%d,%d]\n",
            ab_scoreboard(0).state, ab_scoreboard(1).state,
            ab_scoreboard(2).state, ab_scoreboard(3).state,
            c_scoreboard(0).state, c_scoreboard(1).state
        )
        
        // 查询请求日志
        when(io.query.load.valid) {
            printf("[Scoreboard] Load Query: A%d, B%d, C%d -> ready=%d\n",
                io.query.load.bits.a_reg,
                io.query.load.bits.b_reg,
                io.query.load.bits.c_reg,
                io.query.load.ready
            )
        }
        
        when(io.query.compute.valid) {
            printf("[Scoreboard] Compute Query: A%d, B%d, C%d -> ready=%d\n",
                io.query.compute.bits.a_reg,
                io.query.compute.bits.b_reg,
                io.query.compute.bits.c_reg,
                io.query.compute.ready
            )
        }
        
        when(io.query.store.valid) {
            printf("[Scoreboard] Store Query: C%d -> ready=%d\n",
                io.query.store.bits.c_reg,
                io.query.store.ready
            )
        }
        
        // 状态更新日志
        when(io.update.load_allocate) {
            printf("[Scoreboard] Load Allocate: A%d=%d, B%d=%d, C%d=%d\n",
                io.update.load_alloc_a_reg, io.update.load_alloc_has_a,
                io.update.load_alloc_b_reg, io.update.load_alloc_has_b,
                io.update.load_alloc_c_reg, io.update.load_alloc_has_c
            )
        }
        
        when(io.update.compute_issue) {
            printf("[Scoreboard] Compute Issue: Read A%d, B%d, C%d; Write C%d\n",
                io.update.compute_issue_a_reg,
                io.update.compute_issue_b_reg,
                io.update.compute_issue_c_reg,
                io.update.compute_issue_c_reg
            )
        }
    }
}

