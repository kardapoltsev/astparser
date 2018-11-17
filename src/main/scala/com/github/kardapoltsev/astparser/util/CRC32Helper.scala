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

import java.nio.charset.StandardCharsets
import java.util.zip.CRC32

object CRC32Helper {
  def crc32(string: String): Int = {
    val crc = new CRC32()
    crc.update(string.getBytes(StandardCharsets.UTF_8))
    crc.getValue.toInt
  }
}
