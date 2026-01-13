package testchipip.cosim

import org.chipsalliance.cde.config.{Field, Parameters}
import chisel3._
import chisel3.util._
//import freechips.rocketchip.diplomacy.{BundleBridgeNexusNode, LazyModuleImp}
import freechips.rocketchip.diplomacy.{BundleBridgeNexusNode, InModuleBody, LazyModuleImp}
import freechips.rocketchip.rocket.TraceDoctor
import freechips.rocketchip.subsystem.{HasTiles, BaseSubsystem}
import freechips.rocketchip.util.HeterogeneousBag
import midas.targetutils.TriggerSink

// A per-tile interface that includes the tile's clock and reset
class TileTraceDoctorIO(val traceWidth: Int) extends Bundle {
  val clock: Clock = Clock()
  val reset: Bool = Bool()
  val data = new TraceDoctor(traceWidth)
  val tracerVTrigger: Bool = Bool()
}

// The IO matched on by the TraceDoctor bridge: a wrapper around a heterogenous
// bag of TileTraceDoctorIO. Each entry is trace associated with a single tile
class TraceDoctorOutputTop(val traceWidths: Seq[Int]) extends Bundle {
  val tracedoctors: HeterogeneousBag[TileTraceDoctorIO] = Output(HeterogeneousBag(traceWidths.map(w => new TileTraceDoctorIO(w))))
}

object TraceDoctorOutputTop {
  def apply(proto: Seq[TraceDoctor]): TraceDoctorOutputTop =
    new TraceDoctorOutputTop(proto.map(t => t.traceWidth))
}

case class TraceDoctorPortParams(print: Boolean = false)
object TraceDoctorPortKey extends Field[Option[TraceDoctorPortParams]](None)

trait CanHaveTraceDoctorIO { this: BaseSubsystem with HasTiles =>
  implicit val p: Parameters

  // 1) 在 LazyModule 层把所有 tile 的 traceDoctorNode 汇总到一个 Nexus 上
  val traceDoctorNexus = BundleBridgeNexusNode[TraceDoctor]()
  tiles.foreach { t => traceDoctorNexus := t.traceDoctorNode }

  // 2) 用 InModuleBody 在 DigitalTopModule 中“下沉”生成真正的 IO 端口
  val traceDoctorIO = InModuleBody {
    p(TraceDoctorPortKey) map { traceParams =>
      // 拿到每个 tile 送上来的 TraceDoctor Bundle
      val traceDoctorSeq = traceDoctorNexus.in.map(_._1).toSeq

      // 根据每个 trace 的宽度生成输出 IO
      val tio = IO(Output(TraceDoctorOutputTop(traceDoctorSeq)))

      // 手动把 clock/reset/data/tracerVTrigger 填进去
      (tio.tracedoctors zip (tile_prci_domains.values zip traceDoctorSeq)).foreach {
        case (port, (prci, tracedoc)) =>
          port.clock := prci.module.clock
          port.reset := prci.module.reset.asBool
          port.data  := tracedoc

          // tracerVTrigger：和原来逻辑一致，用 TriggerSink 打一个脉冲
          port.tracerVTrigger := false.B
          TriggerSink.whenEnabled(false.B) {
            port.tracerVTrigger := true.B
          }
      }

      // 可选：按参数决定要不要在 RTL 里 printf 出 trace
      if (traceParams.print) {
        for ((trace, idx) <- tio.tracedoctors.zipWithIndex ) {
          withClockAndReset(trace.clock, trace.reset) {
            when (trace.data.valid) {
              printf(s"TraceDoctor $idx: %x\n", trace.data.bits.asUInt)
            }
          }
        }
      }

      tio
    }
  }
}

// // Use this trait:
// trait CanHaveTraceDoctorIO { this: HasTiles =>
//   val module: CanHaveTraceDoctorIOModuleImp
//   // Bind all the trace nodes to a BB; we'll use this to generate the IO in the imp
//   val traceDoctorNexus = BundleBridgeNexusNode[TraceDoctor]()
//   tiles.foreach { traceDoctorNexus := _.traceDoctorNode }
// }

// trait CanHaveTraceDoctorIOModuleImp extends LazyModuleImp {
//   val outer: CanHaveTraceDoctorIO with HasTiles

//   val traceDoctorIO = p(TraceDoctorPortKey) map ( traceParams => {
//     val traceDoctorSeq = (outer.traceDoctorNexus.in.map(_._1))
//     val tio = IO(Output(TraceDoctorOutputTop(traceDoctorSeq)))

//     (tio.tracedoctors zip (outer.tile_prci_domains zip traceDoctorSeq)).foreach { case (port, (prci, tracedoc)) =>
//       port.clock := prci.module.clock
//       port.reset := prci.module.reset.asBool
//       port.data := tracedoc

//       port.tracerVTrigger := false.B
//       midas.targetutils.TriggerSink.whenEnabled(false.B) {
//         port.tracerVTrigger := true.B
//       }
//     }

//     if (traceParams.print) {
//       for ((trace, idx) <- tio.tracedoctors.zipWithIndex ) {
//         withClockAndReset(trace.clock, trace.reset) {
//           when (trace.data.valid) {
//             printf(s"TraceDoctor $idx: %x\n", trace.data.bits.asUInt)
//           }
//         }
//       }
//     }
//     tio
//   })
// }



