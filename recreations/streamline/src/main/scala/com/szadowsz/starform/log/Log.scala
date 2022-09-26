package com.szadowsz.starform.log


import java.lang.System.Logger
import java.lang.System.Logger.Level
import java.lang.System.Logger.Level.*

trait Log {
  protected[this] lazy val logger: Logger = System.getLogger(this.getClass.getName)
  inline def error(inline message: Any): Unit = ${ Log.logImpl('logger, 'ERROR,'message) }
  inline def warn(inline message: Any): Unit = ${ Log.logImpl('logger, 'WARNING, 'message) }
  inline def info(inline message: Any): Unit = ${ Log.logImpl('logger, 'INFO, 'message) }
  inline def debug(inline message: Any): Unit = ${ Log.logImpl('logger, 'DEBUG, 'message) }
  inline def trace(inline message: Any): Unit = ${ Log.logImpl('logger, 'TRACE, 'message) }
}

private[log] object Log {
  import scala.quoted._

  def logImpl(logger: Expr[Logger], logLevel: Expr[Level], message: Expr[Any])(using q: Quotes): Expr[Unit] = {
    '{
      if (${ logger }.isLoggable(${ logLevel })) {
        ${ logger }.log(${ logLevel }, ${ message })
      }
    }
  }
}