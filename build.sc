import mill._, scalalib._, scalafmt._

object magic extends RootModule with ScalaModule with ScalafmtModule {
  def scalaVersion = "3.4.2"
  def mainClass = Some("Main")
}
