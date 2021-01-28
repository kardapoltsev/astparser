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

package com.github.kardapoltsev.astparser.model

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConstrainedSpec extends AnyWordSpec with Matchers {
  private def newConstrained(enabled: Seq[String] = Nil, disabled: Seq[String] = Nil): Constrained = {
    ExternalType(
      parent = "",
      name = "",
      constraint = Constraint(
        EnableConstraint(enabled),
        DisableConstraint(disabled)
      )
    )
  }

  "Constrained" should {
    "return correct isEnabled" in {
      newConstrained(enabled = Seq("A")).isEnabled(Seq("A")) shouldBe true
      newConstrained().isEnabled(Seq("A")) shouldBe true
      newConstrained(disabled = Seq("A")).isEnabled(Seq("A")) shouldBe false
    }
  }

}
