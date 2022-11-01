package co.topl.testnetsimulationorchestrator.algebras

trait SimulationController[F[_]] {

  /**
   * Terminate the simulation and dispose of any lingering resources
   */
  def terminate: F[Unit]

}
