/*
 * Copyright 2016 Alexey Kardapoltsev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.kardapoltsev.astparser.util

import org.slf4j.LoggerFactory

object Logger {
  import scala.collection.concurrent.{TrieMap => MMap}
  val times  = MMap[String, Long]()
  val counts = MMap[String, Int]()
}

trait Logger {

  protected val log = LoggerFactory.getLogger(getClass)

  protected def loggingTimeMuted[A](tag: String)(f: => A): A = {
    f
  }

  protected def loggingTime[A](tag: String)(f: => A): A = {
    val start      = System.currentTimeMillis()
    val result     = f
    val time       = System.currentTimeMillis() - start
    val totalTime  = Logger.times.getOrElse(tag, 0L) + time
    val totalCount = Logger.counts.getOrElse(tag, 0) + 1
    Logger.times += tag  -> totalTime
    Logger.counts += tag -> totalCount

    val reportMessage = s"$tag took ${time}ms (${totalTime}ms total, $totalCount times)"
    if (time < 10) {
      //skip it
    } else if (time < 100) {
      log.debug(reportMessage)
    } else if (time < 1000) {
      log.info(reportMessage)
    } else {
      log.warn(reportMessage)
    }

    result
  }

}
