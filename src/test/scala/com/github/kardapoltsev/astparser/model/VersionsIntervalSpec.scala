/*
  Copyright 2016 Alexey Kardapoltsev

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
 */
package com.github.kardapoltsev.astparser.model

import org.scalatest.{Matchers, WordSpec}

class VersionsIntervalSpec extends WordSpec with Matchers {

  "VersionsIntervalSpec" should {

    "isIntersect" in {
      VersionsInterval(None, None).isIntersect(VersionsInterval(None, None)) shouldBe true
      VersionsInterval(Some(2), Some(2))
        .isIntersect(VersionsInterval(Some(2), Some(3))) shouldBe true
      VersionsInterval(Some(2), Some(3))
        .isIntersect(VersionsInterval(Some(3), Some(5))) shouldBe true
      VersionsInterval(Some(2), Some(3))
        .isIntersect(VersionsInterval(Some(4), Some(5))) shouldBe false
    }

    "intersect" in {
      VersionsInterval(Some(2), Some(5))
        .intersect(VersionsInterval(Some(3), Some(10))) shouldBe VersionsInterval(Some(3), Some(5))
    }

    "contains" in {
      VersionsInterval(Some(2), Some(5)).contains(1) shouldBe false
      VersionsInterval(Some(2), Some(5)).contains(2) shouldBe true
      VersionsInterval(Some(2), Some(5)).contains(6) shouldBe false
    }

    "isEmpty" in {
      VersionsInterval(Some(3), Some(2)).isEmpty shouldBe true
      VersionsInterval(Some(3), None).isEmpty shouldBe false
    }

  }
}
