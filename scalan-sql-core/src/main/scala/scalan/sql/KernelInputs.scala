package scalan.sql

import scalan._

trait KernelInputs extends ScalanDsl {
  self: KernelInputsDsl with ScalanSql =>

  // purely abstract type for now
  trait KernelInput extends Def[KernelInput]
}
