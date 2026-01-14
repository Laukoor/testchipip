// See LICENSE.SiFive for license details.

package testchipip.cosim

import org.chipsalliance.cde.config.{Field, Parameters}
import chisel3._
import chisel3.util._

import freechips.rocketchip.diplomacy.{BundleBridgeNexusNode, InModuleBody}
import freechips.rocketchip.rocket.TraceDoctor
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._
import midas.targetutils.TriggerSink

// A per-tile interface that includes the tile's clock and reset
class TileTraceDoctorIO(val traceWidth: Int) extends Bundle {
  val clock: Clock = Clock()
  val reset: Bool  = Bool()
  val data         = new TraceDoctor(traceWidth)
  val tracerVTrigger: Bool = Bool()
}

// The IO matched on by the TraceDoctor bridge: a wrapper around a heterogenous
// bag of TileTraceDoctorIO. Each entry is trace associated with a single tile
class TraceDoctorOutputTop(val traceWidths: Seq[Int]) extends Bundle {
  // NOTE: do not put an explicit HeterogeneousBag[...] type here, otherwise
  // Scala 2.13 will see two overloaded HeterogeneousBag definitions and complain.
  val tracedoctors = Output(HeterogeneousBag.apply(traceWidths.map(w => new TileTraceDoctorIO(w))))
}

object TraceDoctorOutputTop {
  def apply(proto: Seq[TraceDoctor]): TraceDoctorOutputTop =
    new TraceDoctorOutputTop(proto.map(_.traceWidth))
}

case class TraceDoctorPortParams(print: Boolean = false)
object TraceDoctorPortKey extends Field[Option[TraceDoctorPortParams]](None)

// New-style implementation using the hierarchical-elements API,
// mirroring what TraceIO.scala does.
trait CanHaveTraceDoctorIO { this: HasHierarchicalElementsRootContext with InstantiatesHierarchicalElements =>
  implicit val p: Parameters

  // Collect per-tile TraceDoctor bundle-bridge nodes.
  // Assumes each tile exposes `traceDoctorNode: BundleBridgeOutwardNode[TraceDoctor]`.
  val tileTDNodes = traceDoctorNodes.values

  // Actually create the top-level IO on the concrete module.
  val traceDoctorIO = InModuleBody {
    p(TraceDoctorPortKey) map { traceParams =>
      // Grab the TraceDoctor bundles coming out of the nodes.
      val traceDoctorSeq = tileTDNodes.map(_.in(0)._1).toSeq

      // Build the IO bundle. The companion object converts Seq[TraceDoctor] -> widths.
      val tio = IO(Output(TraceDoctorOutputTop(traceDoctorSeq)))

      // Wire up clock/reset and the TraceDoctor payload for each tile.
      // `tile_prci_domains.values` is exactly what TraceIO.scala uses.
      (tio.tracedoctors zip (tile_prci_domains.values zip traceDoctorSeq)).foreach {
        case (port, (prci, tracedoc)) =>
          port.clock := prci.module.clock
          port.reset := prci.module.reset.asBool
          port.data  := tracedoc

          // Same TriggerSink-based pulse as in the original implementation.
          port.tracerVTrigger := false.B
          TriggerSink.whenEnabled(false.B) {
            port.tracerVTrigger := true.B
          }
      }

      // Optional printf of the raw TraceDoctor data.
      if (traceParams.print) {
        for ((trace, idx) <- tio.tracedoctors.zipWithIndex) {
          withClockAndReset(trace.clock, trace.reset) {
            when(trace.data.valid) {
              printf(p"TraceDoctor $idx: 0x${trace.data.bits.asUInt}%x\n")
            }
          }
        }
      }

      tio
    }
  }
}
