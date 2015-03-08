package soccerstand.util

import org.slf4j.LoggerFactory

trait Slf4jLogging {
  private lazy val logger = LoggerFactory.getLogger(getClass)
  
  def info(mes: =>String) = if (logger.isInfoEnabled) logger.info(mes)
  def warn(mes: =>String) = if (logger.isWarnEnabled) logger.warn(mes)

  def infoBlock[T](msg: String)(block: => T): T = {
    info(msg + "...")
    val result = block
    info(msg + "... finished")
    result
  }
}
