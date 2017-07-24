package gnasher.reactive.examples

import gnasher.reactive.model.Arena

trait ScalableExampleFactory {
  def makeExample(numAgents: Int): Arena
}
